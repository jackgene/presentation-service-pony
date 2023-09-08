use "actor"
use "actor/tokenizing"
use "collections"
use persistent = "collections/persistent"
use "crypto"
use "encode/base64"
use "http_server"

class val BackendHandlerFactory
  let _deck_html: String
  let _chat_messages: ChatMessageBroadcaster
  let _rejected_messages: ChatMessageBroadcaster
  let _language_poll: SendersByTokenCounter
  let _word_cloud: SendersByTokenCounter
  let _questions: ModeratedTextCollector
  let _transcriptions: TranscriptionBroadcaster

  new val create(env: Env, deck_html: String) =>
    _deck_html = deck_html
    _chat_messages = ChatMessageBroadcaster(env, "chat")
    _rejected_messages = ChatMessageBroadcaster(env, "rejected")
    _language_poll = SendersByTokenCounter(
      env, "language-poll"
      where
      extract_tokens = MappedKeywordsTokenizer(
        env where token_by_word = MappedKeywordsTokenizing.languages_by_name()
      ),
      tokens_per_sender = 3,
      chat_messages = _chat_messages,
      rejected_messages = _rejected_messages,
      expected_senders = 200
    )
    _word_cloud = SendersByTokenCounter(
      env, "word-cloud"
      where
      extract_tokens = NormalizedWordsTokenizer(
        env
        where
        min_word_length = 3, max_word_length = 24,
        stop_words = NormalizedWordsTokenizing.stop_words()
      ),
      tokens_per_sender = 7,
      chat_messages = _chat_messages,
      rejected_messages = _rejected_messages,
      expected_senders = 200
    )
    _questions = ModeratedTextCollector(
      env, "question"
      where
      chat_messages = _chat_messages,
      rejected_messages = _rejected_messages,
      expected_messages = 10
    )
    _transcriptions = TranscriptionBroadcaster(env)

  fun apply(session: Session): Handler ref^ =>
    BackendHandler(
      session, _deck_html, _chat_messages, _rejected_messages,
      _language_poll, _word_cloud, _questions, _transcriptions
    )

class BackendHandler is Handler
  let _no_content_response: Response =
    BuildableResponse(where status' = StatusNoContent)
  let _not_found_response: ByteSeqIter = Responses.builder()
    .set_status(StatusNotFound)
    .add_header("Content-Length", "0")
    .finish_headers()
    .build()

  let _session: Session
  let _deck_response: ByteSeqIter
  let _chat_messages: ChatMessageBroadcaster
  let _rejected_messages: ChatMessageBroadcaster
  let _language_poll: SendersByTokenCounter
  let _word_cloud: SendersByTokenCounter
  let _questions: ModeratedTextCollector
  let _transcriptions: TranscriptionBroadcaster

  new create(
    session: Session,
    deck_html: String,
    chat_messages: ChatMessageBroadcaster,
    rejected_messages: ChatMessageBroadcaster,
    language_poll: SendersByTokenCounter,
    word_cloud: SendersByTokenCounter,
    questions: ModeratedTextCollector,
    transcriptions: TranscriptionBroadcaster
  ) =>
    _session = session
    _deck_response = Responses.builder()
      .set_status(StatusOK)
      .add_header("Content-Type", "text/html")
      .add_header("Content-Length", deck_html.size().string())
      .finish_headers()
      .add_chunk(deck_html.array())
      .build()
    _chat_messages = chat_messages
    _rejected_messages = rejected_messages
    _language_poll = language_poll
    _word_cloud = word_cloud
    _questions = questions
    _transcriptions = transcriptions

  fun _bad_request_response(
    body: String = StatusBadRequest.string()
  ): ByteSeqIter =>
    Responses.builder()
      .set_status(StatusBadRequest)
      .add_header("Content-Length", body.size().string())
      .finish_headers()
      .add_chunk(body.array())
      .build()

  fun _route(
    request: Request val, request_id: RequestID
  ): (ByteSeqIter val | None) =>
    match (request.method(), request.uri().path)
    // Deck
    | (GET, let path: String) if path == "/" => _deck_response

    | (GET, let path: String) if path == "/event/language-poll" =>
      let handler_factory: WebSocketHandlerFactory val =
        { (session: WebSocketSession): WebSocketHandler ref^ =>
          let count_subscriber =
            object val is CountsSubscriber
              fun val counts_received(counts: Counts) =>
                session.send_frame(
                  Text(counts.json().string())
                )
            end
          _language_poll.subscribe(count_subscriber)

          object ref is WebSocketHandler
            fun current_session(): WebSocketSession => session

            fun ref closed() =>
              _language_poll.unsubscribe(count_subscriber)
          end
        }
      _session.upgrade_to_websocket(request, request_id, handler_factory)

    | (GET, let path: String) if path == "/event/word-cloud" =>
      let handler_factory: WebSocketHandlerFactory val =
        { (session: WebSocketSession): WebSocketHandler ref^ =>
          let count_subscriber =
            object val is CountsSubscriber
              fun val counts_received(counts: Counts) =>
                session.send_frame(
                  Text(counts.json().string())
                )
            end
          _word_cloud.subscribe(count_subscriber)

          object ref is WebSocketHandler
            fun current_session(): WebSocketSession => session

            fun ref closed() =>
              _word_cloud.unsubscribe(count_subscriber)
          end
        }
      _session.upgrade_to_websocket(request, request_id, handler_factory)

    | (GET, let path: String) if path == "/event/question" =>
      let handler_factory: WebSocketHandlerFactory val =
        { (session: WebSocketSession): WebSocketHandler ref^ =>
          let question_subscriber =
            object val is ModeratedTextSubscriber
              fun val moderated_text_received(moderated_text: ModeratedText) =>
                session.send_frame(
                  Text(moderated_text.json().string())
                )
            end
          _questions.subscribe(question_subscriber)

          object ref is WebSocketHandler
            fun current_session(): WebSocketSession =>
              session

            fun ref closed() =>
              _questions.unsubscribe(question_subscriber)
          end
        }
      _session.upgrade_to_websocket(request, request_id, handler_factory)

    | (GET, let path: String) if path == "/event/transcription" =>
      let handler_factory: WebSocketHandlerFactory val =
        { (session: WebSocketSession): WebSocketHandler ref^ =>
          let transcription_subscriber =
            object val is TranscriptionSubscriber
              fun val transcript_received(transcript: Transcript) =>
                session.send_frame(
                  Text(transcript.json().string())
                )
            end
          _transcriptions.subscribe(transcription_subscriber)

          object ref is WebSocketHandler
            fun current_session(): WebSocketSession => session

            fun ref closed() =>
              _transcriptions.unsubscribe(transcription_subscriber)
          end
        }
      _session.upgrade_to_websocket(request, request_id, handler_factory)

    // Moderation
    | (GET, let path: String) if path == "/moderator" =>
      StaticContent.moderator_response()

    | (GET, let path: String) if path == "/moderator/event" =>
      let handler_factory: WebSocketHandlerFactory val =
        { (session: WebSocketSession): WebSocketHandler ref^ =>
          let message_subscriber =
            object val is ChatMessageSubscriber
              fun val message_received(message: ChatMessage) =>
                session.send_frame(
                  Text(message.json().string())
                )
            end
          _rejected_messages.subscribe(message_subscriber)

          object ref is WebSocketHandler
            fun current_session(): WebSocketSession => session

            fun ref closed() =>
              _rejected_messages.unsubscribe(message_subscriber)
          end
        }
      _session.upgrade_to_websocket(request, request_id, handler_factory)

    | (POST, let path: String) if path == "/chat" =>
      try
        let query_esvs: Array[String] = request.uri().query.split("&")
        var route_or_none: (String | None) = None
        var text_or_none: (String | None) = None

        for query_esv in query_esvs.values() do
          var query_pair: Array[String] = query_esv.split("=", 2)

          match query_pair.shift()?
          | let route_key: String if route_key == "route" =>
            route_or_none = URLEncode.decode(query_pair.shift()?)?
          | let text_key: String if text_key == "text" =>
            text_or_none = URLEncode.decode(query_pair.shift()?)?
          end
        end

        match (route_or_none, text_or_none)
        | (let route: String, let text: String) =>
          let route_parts: Array[String] = route.split_by(" to ")
          let sender: String = route_parts.shift()?
          let recipient_or_none: (String | None) =
            match route_parts.shift()?
            | "Everyone" => "Everyone"
            | "You" => "You"
            | "You (Direct Message)" => "You"
            end
          match recipient_or_none
          | let recipient: String =>
            _chat_messages.new_message(
              ChatMessage(sender, recipient, text)
            )
          | None if sender != "You" =>
            return _bad_request_response("""malformed "route": """ + route)
          end
          _no_content_response
        | (None, _) =>
          _bad_request_response("""missing "route" parameter""")
        else
          _bad_request_response("""missing "text" parameter""")
        end
      else
        _bad_request_response()
      end

    | (GET, let path: String) if path == "/reset" =>
      _language_poll.reset()
      _word_cloud.reset()
      _questions.reset()
      _no_content_response

    // Transcription
    | (GET, let path: String) if path == "/transcriber" =>
      StaticContent.transcriber_response()

    | (POST, let path: String) if path == "/transcription" =>
      try
        let query_esvs: Array[String] = request.uri().query.split("&")
        var text_or_none: (String | None) = None

        for query_esv in query_esvs.values() do
          var query_pair: Array[String] = query_esv.split("=", 2)

          match query_pair.shift()?
          | let text_key: String if text_key == "text" =>
            text_or_none = URLEncode.decode(query_pair.shift()?)?
          end
        end

        match text_or_none
        | let text: String =>
          _transcriptions.new_transcription_text(text)
          _no_content_response
        else
          _bad_request_response("""missing "text" parameter""")
        end
      else
        _bad_request_response()
      end

    else
      _not_found_response
    end

  fun ref apply(request: Request val, request_id: RequestID) =>
    match _route(request, request_id)
    | let response: ByteSeqIter val =>
      _session.send_raw(response, request_id)
      _session.send_finished(request_id)
    end
