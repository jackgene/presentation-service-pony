use "collections"

class val Transcript
  let text: String

  new val create(text': String) =>
    text = text'

interface val TranscriptionSubscriber
  fun val transcript_received(transcript: Transcript)

actor TranscriptionBroadcaster
  let _env: Env val
  let _subscribers: SetIs[TranscriptionSubscriber val] ref =
    HashSet[TranscriptionSubscriber val, HashIs[TranscriptionSubscriber val]]

  new create(env: Env) =>
    _env = env

  be new_transcription_text(text: String) =>
    _env.out.print("Received transcription text - " + text)
    let transcript = Transcript(text)
    for subscriber in _subscribers.values() do
      subscriber.transcript_received(transcript)
    end

  be subscribe(subscriber: TranscriptionSubscriber val) =>
    _subscribers.set(subscriber)
    _env.out.print(
      "+1 transcription subscriber (=" + _subscribers.size().string() + ")"
    )

  be unsubscribe(subscriber: TranscriptionSubscriber val) =>
    _subscribers.unset(subscriber)
    _env.out.print(
      "-1 transcription subscriber (=" + _subscribers.size().string() + ")"
    )
