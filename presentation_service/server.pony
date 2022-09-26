use "cli"
use "files"
use "http_server"
use "net"

actor Main
  new create(env: Env) =>
    try
      let command = _parse_args(env)?
      let host = "0.0.0.0"
      let port = command.option("port").string()
      let deck_html = _load_deck(env, command.option("html-path").string())?

      // Start the top server control actor.
      let server = Server(
        TCPListenAuth(env.root),
        LoggingServerNotify(env),             // notify for server lifecycle events
        BackendHandlerFactory(env, deck_html) // factory for session-based application backend
        where config = ServerConfig(          // configuration of Server
          where host' = host,
                port' = port,
                max_concurrent_connections' = 10000
        )
      )
    end

  fun _parse_args(env: Env): Command ? =>
    let command_spec =
      try
        CommandSpec.leaf(
          "presentation-service-pony",
          "Presentation Service",
          [
            OptionSpec.string("port", "HTTP server port", 'p', "8973")
            OptionSpec.string("html-path", "Presentation HTML file path")
          ],
          []
        )? .> add_help()?
      else
        env.exitcode(1)
        error
      end
    match CommandParser(command_spec).parse(env.args, env.vars)
    | let c: Command => c
    | let ch: CommandHelp =>
      ch.print_help(env.out)
      env.exitcode(0)
      error
    | let se: SyntaxError =>
      env.err.print(se.string())
      env.exitcode(1)
      error
    end

  fun _load_deck(env: Env, path_spec: String): String ? =>
    let path = FilePath(FileAuth(env.root), path_spec)
    match OpenFile(path)
    | let file: File =>
      var contents = ""
      while file.errno() is FileOK do
        contents = contents.add(file.read_string(1024))
      end
      
      contents
    else
      env.err.print("Error opening file '" + path_spec + "'")
      env.exitcode(1)
      error
    end

class LoggingServerNotify is ServerNotify
  """
  Notification class that is notified about
  important lifecycle events for the Server
  """
  let _env: Env

  new iso create(env: Env) =>
    _env = env

  fun ref listening(server: Server ref) =>
    """
    Called when the Server starts listening on its host:port pair via TCP.
    """
    try
      (let host, let service) = server.local_address().name()?
      _env.err.print("connected: " + host + ":" + service)
    else
      _env.err.print("Couldn't get local address.")
      _env.exitcode(1)
      server.dispose()
    end

  fun ref not_listening(server: Server ref) =>
    """
    Called when the Server was not able to start listening on its host:port pair via TCP.
    """
    _env.err.print("Failed to listen.")
    _env.exitcode(1)

  fun ref closed(server: Server ref) =>
    """
    Called when the Server is closed.
    """
    _env.err.print("Shutdown.")
