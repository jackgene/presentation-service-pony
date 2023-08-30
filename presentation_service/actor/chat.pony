use "collections"

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

interface val ChatMessageListener
  fun val message_received(message: ChatMessage)

interface val ChatMessageListenerActor
  be message_received(message: ChatMessage)

class val _ChatMessageListenerActorAdapter is ChatMessageListener
  let _target: ChatMessageListenerActor tag

  new val create(target: ChatMessageListenerActor tag) =>
    _target = target

  fun val message_received(message: ChatMessage) =>
    _target.message_received(message)

actor ChatMessageBroadcaster
  let _env: Env val
  let _name: String val
  let _listeners: SetIs[ChatMessageListener val] ref =
    HashSet[ChatMessageListener val, HashIs[ChatMessageListener val]]

  new create(env: Env, name: String) =>
    _env = env
    _name = name

  be new_message(message: ChatMessage) =>
    _env.out.print("Received " + _name + " message - " + message.string())
    for listener in _listeners.values() do
      listener.message_received(message)
    end

  be register(listener: ChatMessageListener val) =>
    _listeners.set(listener)
    _env.out.print(
      "+1 " + _name + " message listener (=" + _listeners.size().string() + ")"
    )

  be unregister(listener: ChatMessageListener val) =>
    _listeners.unset(listener)
    _env.out.print(
      "-1 " + _name + " message listener (=" + _listeners.size().string() + ")"
    )
