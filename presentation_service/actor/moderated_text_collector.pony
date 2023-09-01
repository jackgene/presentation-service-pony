use "collections"

class val ModeratedText
  let chat_text: Array[String] val

  new val create(chat_text': Array[String] val) =>
    chat_text = chat_text'

interface val ModeratedTextSubscriber
  fun val moderated_text_received(messages: ModeratedText)

actor ModeratedTextCollector
  let _env: Env val
  let _name: String val
  let _chat_messages: ChatMessageBroadcaster tag
  let _rejected_messages: ChatMessageBroadcaster tag
  let _chat_text_reversed: Array[String] ref
  let _subscribers: SetIs[ModeratedTextSubscriber val] ref =
    HashSet[ModeratedTextSubscriber val, HashIs[ModeratedTextSubscriber val]]
  let _message_subscriber: ChatMessageSubscriber =
    _ChatMessageSubscriberActorAdapter(this)

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

  fun _messages(): ModeratedText val =>
    let len: USize = _chat_text_reversed.size()
    let chat_text: Array[String] iso = recover Array[String](len) end
    for text in _chat_text_reversed.reverse().values() do
      chat_text.push(text)
    end
    ModeratedText(consume chat_text)

  fun _notify_subscribers() =>
    let msgs = _messages()
    for subscriber in _subscribers.values() do
      subscriber.moderated_text_received(msgs)
    end

  be message_received(message: ChatMessage) =>
    match message.sender
    | "Me" =>
      _chat_text_reversed.push(message.text)
      _notify_subscribers()
    else
      _rejected_messages.new_message(message)
    end

  be reset() =>
    _chat_text_reversed.clear()

  be subscribe(subscriber: ModeratedTextSubscriber val) =>
    subscriber.moderated_text_received(_messages())
    if _subscribers.size() == 0 then
      _chat_messages.subscribe(_message_subscriber)
    end
    _subscribers.set(subscriber)
    _env.out.print(
      "+1 " + _name + " subscriber (=" + _subscribers.size().string() + ")"
    )

  be unsubscribe(subscriber: ModeratedTextSubscriber val) =>
    _subscribers.unset(subscriber)
    if _subscribers.size() == 0 then
      _chat_messages.unsubscribe(_message_subscriber)
    end
    _env.out.print(
      "-1 " + _name + " subscriber (=" + _subscribers.size().string() + ")"
    )
