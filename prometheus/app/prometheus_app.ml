open Prometheus

module Server = Cohttp_lwt_unix.Server

let failf fmt =
  Fmt.kstrf failwith fmt

module TextFormat_0_0_4 = struct
  let re_unquoted_escapes = Str.regexp "[\\\n]"
  let re_quoted_escapes = Str.regexp "[\"\\\n]"

  let quote s =
    match Str.matched_string s with
    | "\\" -> "\\\\"
    | "\n" -> "\\n"
    | "\"" -> "\\\""
    | x -> failf "Unexpected match %S" x

  let output_metric_type f = function
    | Counter   -> Fmt.string f "counter"
    | Gauge     -> Fmt.string f "gauge"
    | Summary   -> Fmt.string f "summary"
  (* | Histogram -> Fmt.string f "histogram" *)

  let output_unquoted f s =
    Fmt.string f @@ Str.global_substitute re_unquoted_escapes quote s

  let output_quoted f s =
    Fmt.string f @@ Str.global_substitute re_quoted_escapes quote s

  let output_value f v =
    match classify_float v with
    | FP_normal | FP_subnormal | FP_zero -> Fmt.float f v
    | FP_infinite when v > 0.0 -> Fmt.string f "+Inf"
    | FP_infinite -> Fmt.string f "-Inf"
    | FP_nan -> Fmt.string f "Nan"

  let output_pairs f (label_names, label_values) =
    let cont = ref false in
    let output_pair name value =
      if !cont then Fmt.string f ", "
      else cont := true;
      Fmt.pf f "%a=\"%a\"" LabelName.pp name output_quoted value
    in
    Array.iter2 output_pair label_names label_values

  let output_labels ~label_names f = function
    | [||] -> ()
    | label_values -> Fmt.pf f "{%a}" output_pairs (label_names, label_values)

  let output_sample ~base ~label_names ~label_values f (ext, sample) =
    Fmt.pf f "%a%s%a %a@."
      MetricName.pp base ext
      (output_labels ~label_names) label_values
      output_value sample

  let output_metric ~name ~label_names f (label_values, samples) =
    List.iter (output_sample ~base:name ~label_names ~label_values f) samples

  let output f =
    MetricMap.iter (fun metric samples ->
        let {MetricInfo.name; metric_type; help; label_names} = metric in
        Fmt.pf f
          "#HELP %a %a@.\
           #TYPE %a %a@.\
           %a"
          MetricName.pp name output_unquoted help
          MetricName.pp name output_metric_type metric_type
          (LabelSetMap.pp ~sep:Fmt.nop (output_metric ~name ~label_names)) samples
      )
end

module Runtime = struct
  let start_time = Unix.gettimeofday ()

  let current = ref (Gc.stat ())
  let update () =
    current := Gc.stat ()

  let simple_metric ~metric_type ~help name fn =
    let info = {
      MetricInfo.
      name = MetricName.v name;
      help;
      metric_type;
      label_names = [| |];
    }
    in
    let collect () =
      LabelSetMap.singleton [| |] ["", fn ()]
    in
    info, collect

  let ocaml_gc_allocated_bytes =
    simple_metric ~metric_type:Counter "ocaml_gc_allocated_bytes" Gc.allocated_bytes
      ~help:"Total number of bytes allocated since the program was started."

  let ocaml_gc_major_words =
    simple_metric ~metric_type:Counter "ocaml_gc_major_words" (fun () -> (!current).Gc.major_words)
      ~help:"Number of words allocated in the major heap since the program was started."

  let ocaml_gc_minor_collections =
    simple_metric ~metric_type:Counter "ocaml_gc_minor_collections" (fun () -> float_of_int (!current).Gc.minor_collections)
      ~help:"Number of minor collection cycles completed since the program was started."

  let ocaml_gc_major_collections =
    simple_metric ~metric_type:Counter "ocaml_gc_major_collections" (fun () -> float_of_int (!current).Gc.major_collections)
      ~help:"Number of major collection cycles completed since the program was started."

  let ocaml_gc_heap_words =
    simple_metric ~metric_type:Gauge "ocaml_gc_heap_words" (fun () -> float_of_int (!current).Gc.heap_words)
      ~help:"Total size of the major heap, in words."

  let ocaml_gc_compactions =
    simple_metric ~metric_type:Counter "ocaml_gc_compactions" (fun () -> float_of_int (!current).Gc.compactions)
      ~help:"Number of heap compactions since the program was started."

  let ocaml_gc_top_heap_words =
    simple_metric ~metric_type:Counter "ocaml_gc_top_heap_words" (fun () -> float_of_int (!current).Gc.top_heap_words)
      ~help:"Maximum size reached by the major heap, in words."

  let process_cpu_seconds_total =
    simple_metric ~metric_type:Counter "process_cpu_seconds_total" Sys.time
      ~help:"Total user and system CPU time spent in seconds."

  let process_start_time_seconds =
    simple_metric ~metric_type:Counter "process_start_time_seconds" (fun () -> start_time)
      ~help:"Start time of the process since unix epoch in seconds."

  let metrics = [
    ocaml_gc_allocated_bytes;
    ocaml_gc_major_words;
    ocaml_gc_minor_collections;
    ocaml_gc_major_collections;
    ocaml_gc_heap_words;
    ocaml_gc_compactions;
    ocaml_gc_top_heap_words;
    process_cpu_seconds_total;
    process_start_time_seconds;
  ]
end

type config = int option

let callback _conn req _body =
  let open Cohttp in
  let uri = Request.uri req in
  match Request.meth req, Uri.path uri with
  | `GET, "/metrics" ->
    let data = Prometheus.CollectorRegistry.(collect default) in
    let body = Fmt.to_to_string TextFormat_0_0_4.output data in
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

let () =
  CollectorRegistry.(register_pre_collect default) Runtime.update;
  let add (info, collector) =
    CollectorRegistry.(register default) info collector in
  List.iter add Runtime.metrics
