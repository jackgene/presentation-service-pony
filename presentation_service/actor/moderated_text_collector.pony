use "collections"
use "json"

class val ModeratedText
  let chat_text: Array[String] val

  new val create(chat_text': Array[String] val) =>
    chat_text = chat_text'

  fun box json(): JsonObject iso^ =>
    recover
      let this_json: Map[String, JsonType] =
        HashMap[String, JsonType, HashEq[String]](where prealloc = 1)

      let chat_text_json: Array[JsonType] =
        Array[JsonType](where len = chat_text.size())
      for text in chat_text.values() do
        chat_text_json.push(text)
      end
      this_json("chatText") = JsonArray.from_array(chat_text_json)

      JsonObject.from_map(this_json)
    end

interface val ModeratedTextSubscriber
  fun val moderated_text_received(messages: ModeratedText)

actor ModeratedTextCollector
  let _env: Env val
  let _name: String val
  let _chat_messages: ChatMessageBroadcaster tag
  let _rejected_messages: ChatMessageBroadcaster tag
  let _chat_text: Array[String] ref
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
    _chat_text = Array[String](expected_messages)

  fun _current_messages(): ModeratedText =>
    let chat_text: Array[String] trn =
      Array[String](where len = _chat_text.size())
    for text in _chat_text.values() do
      chat_text.push(text)
    end

    ModeratedText(consume chat_text)

  fun _notify_subscribers() =>
    let msgs = _current_messages()
    for subscriber in _subscribers.values() do
      subscriber.moderated_text_received(msgs)
    end

  be message_received(message: ChatMessage) =>
    match message.sender
    | "" =>
      _chat_text.push(message.text)
      _notify_subscribers()
    else
      _rejected_messages.new_message(message)
    end

  be reset() =>
    _chat_text.clear()

  be subscribe(subscriber: ModeratedTextSubscriber val) =>
    subscriber.moderated_text_received(_current_messages())
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
