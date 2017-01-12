module Server = Cohttp_lwt_unix.Server

type config = int option

let callback _conn req _body =
  let open Cohttp in
  let uri = Request.uri req in
  match Request.meth req, Uri.path uri with
  | `GET, "/metrics" ->
    let data = Prometheus.CollectorRegistry.(collect default) in
    let body = Fmt.to_to_string Prometheus.TextFormat_0_0_4.output data in
    let headers = Header.init_with "Content-Type" "text/plain; version=0.0.4" in
    Server.respond_string ~status:`OK ~headers ~body ()
  | _ -> Server.respond_error ~status:`Bad_request ~body:"Bad request" ()

let serve = function
  | None -> []
  | Some port ->
    let mode = `TCP (`Port port) in
    let thread = Cohttp_lwt_unix.Server.create ~mode (Server.make ~callback ()) in
    [thread]

let listen_prometheus =
  let open Cmdliner in
  let doc =
    Arg.info ~doc:
      "Port on which to provide Prometheus metrics over HTTP, \
       of the form port or host:port"
      ["listen-prometheus"]
  in
  Arg.(value & opt (some int) None doc)

let opts = listen_prometheus
