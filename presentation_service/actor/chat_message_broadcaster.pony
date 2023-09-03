use "collections"
use "json"

class val ChatMessage
  let sender: String
  let recipient: String
  let text: String

  new val create(sender': String, recipient': String, text': String) =>
    sender = sender'
    recipient = recipient'
    text = text'

  fun box string(): String iso^ =>
    sender + " to " + recipient + ": " + text

  fun box json(): JsonObject iso^ =>
    recover
      let this_json: Map[String, JsonType] =
        HashMap[String, JsonType, HashEq[String]](where prealloc = 3)
      this_json("s") = sender
      this_json("r") = recipient
      this_json("t") = text

      JsonObject.from_map(this_json)
    end

interface val ChatMessageSubscriber
  fun val message_received(message: ChatMessage)

interface val ChatMessageSubscriberActor
  be message_received(message: ChatMessage)

class val _ChatMessageSubscriberActorAdapter is ChatMessageSubscriber
  let _target: ChatMessageSubscriberActor tag

  new val create(target: ChatMessageSubscriberActor tag) =>
    _target = target

  fun val message_received(message: ChatMessage) =>
    _target.message_received(message)

actor ChatMessageBroadcaster
  let _env: Env val
  let _name: String val
  let _subscribers: SetIs[ChatMessageSubscriber val] ref =
    HashSet[ChatMessageSubscriber val, HashIs[ChatMessageSubscriber val]]

  new create(env: Env, name: String) =>
    _env = env
    _name = name

  be new_message(message: ChatMessage) =>
    _env.out.print("Received " + _name + " message - " + message.string())
    for subscriber in _subscribers.values() do
      subscriber.message_received(message)
    end

  be subscribe(subscriber: ChatMessageSubscriber val) =>
    _subscribers.set(subscriber)
    _env.out.print(
      "+1 " + _name + " message subscriber (=" + _subscribers.size().string() + ")"
    )

  be unsubscribe(subscriber: ChatMessageSubscriber val) =>
    _subscribers.unset(subscriber)
    _env.out.print(
      "-1 " + _name + " message subscriber (=" + _subscribers.size().string() + ")"
    )
