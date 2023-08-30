use "collections"

class val Messages
  let chat_text: Array[String] val

  new val create(chat_text': Array[String] val) =>
    chat_text = chat_text'

interface val ApprovedMessagesListener
  fun val messages_received(messages: Messages)

actor MessageApprovalRouter
  let _env: Env val
  let _name: String val
  let _chat_messages: ChatMessageBroadcaster tag
  let _rejected_messages: ChatMessageBroadcaster tag
  let _chat_text_reversed: Array[String] ref
  let _listeners: SetIs[ApprovedMessagesListener val] ref =
    HashSet[ApprovedMessagesListener val, HashIs[ApprovedMessagesListener val]]
  let _message_receiver: ChatMessageListener =
    _ChatMessageListenerActorAdapter(this)

  new create(
    env: Env, name: String,
    chat_messages: ChatMessageBroadcaster,
    rejected_messages: ChatMessageBroadcaster,
    expected_messages: USize
  ) =>
    _env = env
    _name = name
    _chat_messages = chat_messages
    _rejected_messages = rejected_messages
    _chat_text_reversed = Array[String](expected_messages)

  fun _messages(): Messages val =>
    let len: USize = _chat_text_reversed.size()
    let chat_text: Array[String] iso = recover Array[String](len) end
    for text in _chat_text_reversed.reverse().values() do
      chat_text.push(text)
    end
    Messages(consume chat_text)

  fun _notify_listeners() =>
    let msgs = _messages()
    for listener in _listeners.values() do
      listener.messages_received(msgs)
    end

  be message_received(message: ChatMessage) =>
    match message.sender
    | "Me" =>
      _chat_text_reversed.push(message.text)
      _notify_listeners()
    else
      _rejected_messages.new_message(message)
    end

  be reset() =>
    _chat_text_reversed.clear()

  be register(listener: ApprovedMessagesListener val) =>
    listener.messages_received(_messages())
    if _listeners.size() == 0 then
      _chat_messages.register(_message_receiver)
    end
    _listeners.set(listener)
    _env.out.print(
      "+1 " + _name + " listener (=" + _listeners.size().string() + ")"
    )

  be unregister(listener: ApprovedMessagesListener val) =>
    _listeners.unset(listener)
    if _listeners.size() == 0 then
      _chat_messages.unregister(_message_receiver)
    end
    _env.out.print(
      "-1 " + _name + " listener (=" + _listeners.size().string() + ")"
    )
