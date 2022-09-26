use "actor"
use "collections"
use persistent = "collections/persistent"
use "crypto"
use "debug"
use "encode/base64"
use "http_server"
use "json"
use "model"

class val BackendHandlerFactory
  let _deck_html: String
  let _chat_messages: ChatMessageBroadcaster
  let _rejected_messages: ChatMessageBroadcaster
  let _language_poll: SendersByTokenCounter

  new val create(env: Env, deck_html: String) =>
    _deck_html = deck_html
    _chat_messages = ChatMessageBroadcaster(env, "chat")
    _rejected_messages = ChatMessageBroadcaster(env, "rejected")
    _language_poll = SendersByTokenCounter(
      env, "language-poll", TokenFromFirstWord(Token.languages_by_name())
      where
      chat_messages = _chat_messages,
      rejected_messages = _rejected_messages,
      expected_senders = 200
    )

  fun apply(session: Session): Handler ref^ =>
    BackendHandler(
      session, _deck_html, _chat_messages, _rejected_messages, _language_poll
    )

class BackendHandler is Handler
  let _no_content_response: Response =
    BuildableResponse(where status' = StatusNoContent)
  let _bad_request_response: ByteSeqIter = Responses.builder()
    .set_status(StatusBadRequest)
    .add_header("Content-Length", "0")
    .finish_headers()
    .build()
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

  new ref create(
    session: Session,
    deck_html: String,
    chat_messages: ChatMessageBroadcaster,
    rejected_messages: ChatMessageBroadcaster,
    language_poll: SendersByTokenCounter
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

  fun ref _route(
    request: Request val, request_id: RequestID
  ): (ByteSeqIter val | None) =>
    match (request.method(), request.uri().path)
    // Deck
    | (GET, let path: String) if path == "/" => _deck_response

    | (GET, let path: String) if path == "/event/language-poll" =>
      let listener_factory: WebSocketHandlerFactory val =
        { (session: WebSocketSession): WebSocketHandler ref^ =>
          let count_listener =
            object val is TokensByCountListener
              fun val counts_received(
                counts: persistent.Map[I64, persistent.Vec[String]]
              ) =>
                let count_and_tokens_pairs_json = Array[JsonType]
                for (count', tokens') in counts.pairs() do
                  let tokens_json = Array[JsonType]
                  for token in tokens'.values() do
                    tokens_json.push(token)
                  end

                  let count_and_tokens_pair_json = Array[JsonType]
                  count_and_tokens_pair_json.push(count')
                  count_and_tokens_pair_json.push(
                    JsonArray.from_array(tokens_json)
                  )

                  count_and_tokens_pairs_json.push(
                    JsonArray.from_array(count_and_tokens_pair_json)
                  )
                end
                session.send_frame(
                  Text(
                    JsonArray.from_array(count_and_tokens_pairs_json).string()
                  )
                )
            end
          _language_poll.register(count_listener)

          object ref is WebSocketHandler
            fun ref close_received(status: (CloseStatus | None)) =>
              session.dispose()

            fun ref closed() =>
              _language_poll.unregister(count_listener)
          end
        }
      _session.upgrade_to_websocket(request, request_id, listener_factory)

    | (GET, let path: String) if path == "/event/question" =>
      let listener_factory: WebSocketHandlerFactory val =
        { (session: WebSocketSession): WebSocketHandler ref^ =>
          object ref is WebSocketHandler
          end
        }
      _session.upgrade_to_websocket(request, request_id, listener_factory)

    | (GET, let path: String) if path == "/event/transcription" =>
      let listener_factory: WebSocketHandlerFactory val =
        { (session: WebSocketSession): WebSocketHandler ref^ =>
          object ref is WebSocketHandler
          end
        }
      _session.upgrade_to_websocket(request, request_id, listener_factory)

    // Moderation
    | (GET, let path: String) if path == "/moderator" =>
      StaticContent.moderator_response()

    | (GET, let path: String) if path == "/moderator/event" =>
      let listener_factory: WebSocketHandlerFactory val =
        { (session: WebSocketSession): WebSocketHandler ref^ =>
          let message_listener =
            object val is ChatMessageListener
              fun val message_received(message: ChatMessage) =>
                let message_json: Map[String, JsonType] =
                  HashMap[String, JsonType, HashEq[String]](3)
                message_json("s") = message.sender
                message_json("r") = message.recipient
                message_json("t") = message.text
                session.send_frame(
                  Text(JsonObject.from_map(message_json).string())
                )
            end
          _rejected_messages.register(message_listener)

          object ref is WebSocketHandler
            fun ref close_received(status: (CloseStatus | None)) =>
              session.dispose()

            fun ref closed() =>
              _rejected_messages.unregister(message_listener)
          end
        }
      _session.upgrade_to_websocket(request, request_id, listener_factory)

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
            | "Me" => "Me"
            | "Me (Direct Message)" => "Me"
            end
          match recipient_or_none
          | let recipient: String =>
            _chat_messages.new_message(
              ChatMessage(sender, recipient, text)
            )
          end
        else error end

        _no_content_response
      else
        _bad_request_response // TODO build bad request with body
      end

    | (GET, let path: String) if path == "/reset" =>
      _language_poll.reset()
      _no_content_response

    // Transcription
    | (GET, let path: String) if path == "/transcriber" =>
      StaticContent.transcriber_response()
    else
      _not_found_response
    end

  fun ref apply(request: Request val, request_id: RequestID) =>
    match _route(request, request_id)
    | let response: ByteSeqIter val =>
      _session.send_raw(response, request_id)
      _session.send_finished(request_id)
    end

  fun ref throttled() =>
    Debug("throttled")

  fun ref unthrottled() =>
    Debug("unthrottled")
