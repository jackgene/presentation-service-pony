use "collections"
use "counter"
use persistent = "collections/persistent"

interface val Tokenizer
  fun val apply(text: String val): Array[String val] iso^

interface val CountsSubscriber
  fun val counts_received(counts: persistent.Map[U64, persistent.Vec[String]])

actor SendersByTokenCounter
  let _env: Env val
  let _name: String val
  let _extract_tokens: Tokenizer val
  let _tokens_per_sender: USize
  let _chat_messages: ChatMessageBroadcaster tag
  let _rejected_messages: ChatMessageBroadcaster tag
  let _expected_senders: USize val
  let _tokens_by_sender: Map[String, FIFOBoundedSet[String]] ref
  let _token_counts: MultiSet[String]
  let _subscribers: SetIs[CountsSubscriber val] ref =
    HashSet[CountsSubscriber val, HashIs[CountsSubscriber val]]
  let _message_subscriber: ChatMessageSubscriber =
    _ChatMessageSubscriberActorAdapter(this)

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
    _tokens_by_sender = HashMap[String, FIFOBoundedSet[String], HashEq[String]](
      where prealloc = _expected_senders
    )
    _token_counts = MultiSet[String](where prealloc = _expected_senders)

  fun _notify_subscribers() =>
    for subscriber in _subscribers.values() do
      subscriber.counts_received(_token_counts.values_by_count)
    end

  be message_received(message: ChatMessage) =>
    let sender: (String | None) =
      if message.sender != "Me" then message.sender end
    let extracted_tokens: Array[String] = _extract_tokens(message.text)

    if extracted_tokens.size() > 0 then
      _env.out.print("Extracted token \"" + "\", \"".join(extracted_tokens.values()) + "\"")
      extracted_tokens.reverse_in_place() // Prioritize earlier tokens
      // TODO add to _chat_messages_and_tokens
      match sender
      | let sender': String =>
        let sender_tokens: FIFOBoundedSet[String] =
          _tokens_by_sender.get_or_else(sender', FIFOBoundedSet[String](_tokens_per_sender))
        let effects: Array[Effect] = sender_tokens.union(extracted_tokens.values())
        effects.reverse_in_place()
        for effect in effects.values() do
          match effect
          | let pushed: Pushed =>
            _token_counts.update(pushed.value)
          | let pushed_evicting: PushedEvicting =>
            _token_counts.update(pushed_evicting.value, pushed_evicting.evicting)
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
    subscriber.counts_received(_token_counts.values_by_count)
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
