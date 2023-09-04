use "collections"
use "counter"
use persistent = "collections/persistent"
use "json"
use "time"

class val ChatMessageAndTokens
  let chat_message: ChatMessage
  let tokens: Array[String] val

  new val create(
    chat_message': ChatMessage,
    tokens': Array[String] val
  ) =>
    chat_message = chat_message'
    tokens = tokens'

  fun json(): JsonObject iso^ =>
    recover
      let this_json: Map[String, JsonType] =
        HashMap[String, JsonType, HashEq[String]](2)

      // Property: chatMessage
      this_json("chatMessage") = chat_message.json()

      // Property: tokens
      let tokens_json: Array[JsonType] =
        Array[JsonType](where len = tokens.size())
      for token in tokens.values() do
        tokens_json.push(token)
      end
      this_json("tokens") = JsonArray.from_array(tokens_json)

      JsonObject.from_map(this_json)
    end

class val Counts
  let chat_messages_and_tokens: persistent.Vec[ChatMessageAndTokens]
  let tokens_by_sender: Map[String, persistent.Vec[String]] val
  let tokens_by_count: persistent.Map[U64, persistent.Vec[String]]

  new val create(
    chat_messages_and_tokens': persistent.Vec[ChatMessageAndTokens],
    tokens_by_sender': Map[String, persistent.Vec[String]] val,
    tokens_by_count': persistent.Map[U64, persistent.Vec[String]]
  ) =>
    chat_messages_and_tokens = chat_messages_and_tokens'
    tokens_by_sender = tokens_by_sender'
    tokens_by_count = tokens_by_count'

  fun json(): JsonObject iso^ =>
    recover
      let this_json: Map[String, JsonType] =
        HashMap[String, JsonType, HashEq[String]](3)

      // Property: chatMessagesAndTokens
      let chat_messages_and_tokens_json: Array[JsonType] =
        Array[JsonType](where len = chat_messages_and_tokens.size())
      for chat_message_and_tokens in chat_messages_and_tokens.values() do
        chat_messages_and_tokens_json.push(chat_message_and_tokens.json())
      end
      this_json("chatMessagesAndTokens") =
        JsonArray.from_array(chat_messages_and_tokens_json)

      // Property: tokensBySender
      let tokens_by_sender_json: Map[String, JsonType] =
        HashMap[String, JsonType, HashEq[String]](
          where prealloc = tokens_by_sender.size()
        )
      for (sender', tokens') in tokens_by_sender.pairs() do
        let tokens_json: Array[JsonType] =
          Array[JsonType](where len = tokens'.size())
        for token' in tokens'.values() do
          tokens_json.push(token')
        end
        tokens_by_sender_json(sender') = JsonArray.from_array(tokens_json)
      end
      this_json("tokensBySender") =
        JsonObject.from_map(tokens_by_sender_json)

      // Property: tokensAndCounts
      let count_and_tokens_pairs_json: Array[JsonType] =
        Array[JsonType](where len = tokens_by_count.size())
      for (count', tokens') in tokens_by_count.pairs() do
        let tokens_json = Array[JsonType]
        for token' in tokens'.values() do
          tokens_json.push(token')
        end

        let count_and_tokens_pair_json = Array[JsonType]
        let count_json = count'.i64()
        // Property: tokensAndCounts[*]._1
        count_and_tokens_pair_json.push(
          if count_json < 0 then I64.max_value() else count_json end
        )
        // Property: tokensAndCounts[*]._2
        count_and_tokens_pair_json.push(
          JsonArray.from_array(tokens_json)
        )

        count_and_tokens_pairs_json.push(
          JsonArray.from_array(count_and_tokens_pair_json)
        )
      end
      this_json("tokensAndCounts") =
        JsonArray.from_array(count_and_tokens_pairs_json)

      JsonObject.from_map(this_json)
    end

interface val CountsSubscriber
  fun val counts_received(counts: Counts)

interface val Tokenizer
  fun val apply(text: String val): Array[String val] iso^

actor SendersByTokenCounter
  let _env: Env val
  let _name: String val
  let _extract_tokens: Tokenizer val
  let _tokens_per_sender: USize
  let _chat_messages: ChatMessageBroadcaster tag
  let _rejected_messages: ChatMessageBroadcaster tag
  let _expected_senders: USize val
  var _chat_messages_and_tokens: persistent.Vec[ChatMessageAndTokens]
  let _tokens_by_sender: Map[String, FIFOBoundedSet[String]] ref
  let _token_counts: MultiSet[String]
  let _subscribers: SetIs[CountsSubscriber val] ref =
    HashSet[CountsSubscriber val, HashIs[CountsSubscriber val]]
  let _message_subscriber: ChatMessageSubscriber =
    _ChatMessageSubscriberActorAdapter(this)
  let _timers: Timers tag = Timers
  let _quiet_period_nanos: U64 = 100_000_000
  var _in_quiet_period: Bool = false
  var _dirty: Bool = false

  new create(
    env: Env, name: String, 
    extract_tokens: Tokenizer, tokens_per_sender: USize,
    chat_messages: ChatMessageBroadcaster,
    rejected_messages: ChatMessageBroadcaster,
    expected_senders: USize
  ) =>
    _env = env
    _name = name
    _extract_tokens = extract_tokens
    _tokens_per_sender = tokens_per_sender
    _chat_messages = chat_messages
    _rejected_messages = rejected_messages
    _expected_senders = expected_senders
    _chat_messages_and_tokens = persistent.Vec[ChatMessageAndTokens]
    _tokens_by_sender =
      HashMap[String, FIFOBoundedSet[String], HashEq[String]](
        where prealloc = _expected_senders
      )
    _token_counts = MultiSet[String](where prealloc = _expected_senders)

  fun _current_counts(): Counts =>
    let tokens_by_sender: Map[String, persistent.Vec[String]] trn =
      HashMap[String, persistent.Vec[String], HashEq[String]](
        where prealloc = _tokens_by_sender.size()
      )
    for (sender, tokens) in _tokens_by_sender.pairs() do
      tokens_by_sender(sender) = tokens.insertion_order
    end

    Counts(
      _chat_messages_and_tokens,
      consume tokens_by_sender,
      _token_counts.values_by_count
    )

  fun ref _notify_subscribers() =>
    if not _in_quiet_period then
      for subscriber in _subscribers.values() do
        subscriber.counts_received(_current_counts())
      end
      _in_quiet_period = true
      _dirty = false

      let self: SendersByTokenCounter tag = this
      _timers(
        Timer(
          object is TimerNotify
            fun ref apply(timer: Timer, count: U64): Bool =>
              self._awake_from_quiet_period()
              false
          end,
          _quiet_period_nanos
        )
      )
    else
      _dirty = true
    end

  be _awake_from_quiet_period() =>
    _in_quiet_period = false
    if _dirty then _notify_subscribers() end

  be message_received(message: ChatMessage) =>
    let sender: (String | None) =
      if message.sender != "" then message.sender end
    let extracted_tokens: Array[String] val = _extract_tokens(message.text)

    if extracted_tokens.size() > 0 then
      _env.out.print(
        "Extracted token \"" + "\", \"".join(extracted_tokens.values()) + "\""
      )
      let prioritized_tokens: Array[String] val =
        recover extracted_tokens.reverse() end // Prioritize earlier tokens
      _chat_messages_and_tokens = _chat_messages_and_tokens.push(
        ChatMessageAndTokens(message, extracted_tokens)
      )
      match sender
      | let sender': String =>
        let sender_tokens: FIFOBoundedSet[String] =
          _tokens_by_sender.get_or_else(
            sender', FIFOBoundedSet[String](_tokens_per_sender)
          )
        let effects: Array[Effect[String]] =
          sender_tokens.union(prioritized_tokens.values())
        effects.reverse_in_place()
        for effect in effects.values() do
          match effect
          | let pushed: Pushed[String] =>
            _token_counts.update(pushed.value)
          | let pushed_evicting: PushedEvicting[String] =>
            _token_counts.update(
              pushed_evicting.value, pushed_evicting.evicting
            )
          end
        end

        _tokens_by_sender(sender') = sender_tokens

      | None =>
        for new_token in extracted_tokens.values() do
          _token_counts.update(new_token)
        end
      end

      _notify_subscribers()
    else
      _env.out.print("No token extracted")
      _rejected_messages.new_message(message)
    end

  be reset() =>
    _tokens_by_sender.clear()
    _token_counts.clear()
    _notify_subscribers()

  be subscribe(subscriber: CountsSubscriber val) =>
    subscriber.counts_received(_current_counts())
    if _subscribers.size() == 0 then
      _chat_messages.subscribe(_message_subscriber)
    end
    _subscribers.set(subscriber)
    _env.out.print(
      "+1 " + _name + " subscriber (=" + _subscribers.size().string() + ")"
    )

  be unsubscribe(subscriber: CountsSubscriber val) =>
    _subscribers.unset(subscriber)
    if _subscribers.size() == 0 then
      _chat_messages.unsubscribe(_message_subscriber)
    end
    _env.out.print(
      "-1 " + _name + " subscriber (=" + _subscribers.size().string() + ")"
    )
