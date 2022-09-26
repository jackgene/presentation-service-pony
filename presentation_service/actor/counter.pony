use "collections"
use persistent = "collections/persistent"

interface val TokenExtractor
  fun val apply(text: String val): (String val | None)

interface val TokensByCountListener
  fun val counts_received(counts: persistent.Map[I64, persistent.Vec[String]])

actor SendersByTokenCounter
  let _env: Env val
  let _name: String val
  let _extract_token: TokenExtractor val
  let _chat_messages: ChatMessageBroadcaster tag
  let _rejected_messages: ChatMessageBroadcaster tag
  let _expected_senders: USize val
  let _tokens_by_sender: Map[String, String] ref
  let _token_frequencies: Frequencies
  let _listeners: SetIs[TokensByCountListener val] ref =
    HashSet[TokensByCountListener val, HashIs[TokensByCountListener val]]
  let _message_receiver: ChatMessageListener =
    _SendersByTokenCounterMessageReceiver(this)

  new create(
    env: Env, name: String, extract_token: TokenExtractor,
    chat_messages: ChatMessageBroadcaster,
    rejected_messages: ChatMessageBroadcaster,
    expected_senders: USize
  ) =>
    _env = env
    _name = name
    _extract_token = extract_token
    _chat_messages = chat_messages
    _rejected_messages = rejected_messages
    _expected_senders = expected_senders
    _tokens_by_sender = HashMap[String, String, HashEq[String]](
      where prealloc = _expected_senders
    )
    _token_frequencies = Frequencies(where prealloc = _expected_senders)

  fun _notify_listeners() =>
    for listener in _listeners.values() do
      listener.counts_received(_token_frequencies.items_by_count)
    end

  be _message_received(message: ChatMessage) =>
    let sender: (String | None) =
      if message.sender != "Me" then message.sender end
    let old_token: (String | None) =
      match sender
      | let sender': String => try _tokens_by_sender(sender')? end
      end
    let new_token: (String | None) = _extract_token(message.text)

    match new_token
    | let new_token': String =>
      _env.out.print("Extracted token \"" + new_token' + "\"")

      match old_token
      | let old_token': String if new_token' == old_token' =>
        _notify_listeners()
      else
        match sender
        | let sender': String =>
          _tokens_by_sender(sender') = new_token'
        end

        _token_frequencies.update(
          where increment = new_token', decrement = old_token
        )
        _notify_listeners()
      end
    else
      _env.out.print("No token extracted")
      _rejected_messages.new_message(message)
    end

  be reset() =>
    _tokens_by_sender.clear()
    _token_frequencies.clear()
    _notify_listeners()

  be register(listener: TokensByCountListener val) =>
    listener.counts_received(_token_frequencies.items_by_count)
    if _listeners.size() == 0 then
      _chat_messages.register(_message_receiver)
    end
    _listeners.set(listener)
    _env.out.print(
      "+1 " + _name + " listener (=" + _listeners.size().string() + ")"
    )

  be unregister(listener: TokensByCountListener val) =>
    _listeners.unset(listener)
    if _listeners.size() == 0 then
      _chat_messages.unregister(_message_receiver)
    end
    _env.out.print(
      "-1 " + _name + " listener (=" + _listeners.size().string() + ")"
    )

class val _SendersByTokenCounterMessageReceiver is ChatMessageListener
  let _counter: SendersByTokenCounter tag

  new val create(counter: SendersByTokenCounter tag) =>
    _counter = counter

  fun val message_received(message: ChatMessage) =>
    _counter._message_received(message)
