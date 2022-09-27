use "collections"

class val Transcript
  let text: String

  new val create(text': String) =>
    text = text'

interface val TranscriptionListener
  fun val transcription_received(transcript: Transcript)

actor TranscriptionBroadcaster
  let _env: Env val
  let _listeners: SetIs[TranscriptionListener val] ref =
    HashSet[TranscriptionListener val, HashIs[TranscriptionListener val]]

  new create(env: Env) =>
    _env = env

  be new_transcription_text(text: String) =>
    _env.out.print("Received transcription text - " + text)
    let transcript = Transcript(text)
    for listener in _listeners.values() do
      listener.transcription_received(transcript)
    end

  be register(listener: TranscriptionListener val) =>
    _listeners.set(listener)
    _env.out.print(
      "+1 transcription listener (=" + _listeners.size().string() + ")"
    )

  be unregister(listener: TranscriptionListener val) =>
    _listeners.unset(listener)
    _env.out.print(
      "-1 transcription listener (=" + _listeners.size().string() + ")"
    )
