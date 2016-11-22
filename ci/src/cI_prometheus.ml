open! Astring
open Asetmap

module type NAME = sig
  val valid : Str.regexp
end

module Name(N : NAME) : sig
  type t = private string
  val v : string -> t
  val pp : t Fmt.t
  val compare : t -> t -> int
end = struct
  type t = string

  let v name =
    if not (Str.string_match N.valid name 0) then
      CI_utils.failf "Invalid name %S" name;
    name

  let compare = String.compare

  let pp = Fmt.string
end

module MetricName = Name(struct let valid = Str.regexp "^[a-zA-Z_:][a-zA-Z0-9_:]*$" end)
module LabelName  = Name(struct let valid = Str.regexp "^[a-zA-Z_][a-zA-Z0-9_]*$" end)

type metric_type =
  | Counter
  | Gauge
  | Summary
(*
  | Histogram
*)

let pp_metric_type f = function
  | Counter   -> Fmt.string f "counter"
  | Gauge     -> Fmt.string f "gauge"
  | Summary   -> Fmt.string f "summary"
(*
  | Histogram -> Fmt.string f "histogram"
*)

module LabelSet = struct
  type t = string array
  let compare (a:t) (b:t) = compare a b
end
module LabelSetMap = Map.Make(LabelSet)

module MetricInfo = struct
  type t = {
    name : MetricName.t;
    metric_type : metric_type;
    help : string;
    label_names : LabelName.t array;
  }

  let pp_opt () = function
    | None -> ""
    | Some v -> v ^ "_"

  let v ~help ?(label_names=[||]) ~metric_type ?namespace ?subsystem name =
    let name = Printf.sprintf "%a%a%s" pp_opt namespace pp_opt subsystem name in
    {
      name = MetricName.v name;
      metric_type;
      help;
      label_names;
    }

  let compare a b = MetricName.compare a.name b.name
end

module MetricMap = Map.Make(MetricInfo)

module CollectorRegistry = struct
  type t = {
    mutable metrics : (unit -> (string * float) list LabelSetMap.t) MetricMap.t;
  }

  type snapshot = (string * float) list LabelSetMap.t MetricMap.t

  let create () = {
    metrics = MetricMap.empty
  }

  let default = create ()

  let register t info collector =
    assert (not (MetricMap.mem info t.metrics));
    t.metrics <- MetricMap.add info collector t.metrics

  let collect t =
    MetricMap.map (fun f -> f ()) t.metrics
end

module TextFormat_0_0_4 = struct
  let re_unquoted_escapes = Str.regexp "[\\\n]"
  let re_quoted_escapes = Str.regexp "[\"\\\n]"

  let quote s =
    match Str.matched_string s with
    | "\\" -> "\\\\"
    | "\n" -> "\\n"
    | "\"" -> "\\\""
    | x -> CI_utils.failf "Unexpected match %S" x

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
      if !cont then Fmt.string f " "
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
          MetricName.pp name pp_metric_type metric_type
          (LabelSetMap.pp ~sep:Fmt.nop (output_metric ~name ~label_names)) samples
      )
end

module type METRIC = sig
  type family
  type t
  val v_labels : label_names:string array -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> family
  val labels : family -> string array -> t
  val v_label : label:string -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> (string -> t)
  val v : ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> t
end

module type CHILD = sig
  type t
  val create : unit -> t
  val values : t -> (string * float) list       (* extension, value *)
  val metric_type : metric_type
end

module Metric(Child : CHILD) : sig
  include METRIC with type t = Child.t
end = struct
  type family = {
    metric : MetricInfo.t;
    mutable children : Child.t LabelSetMap.t;
  }

  type t = Child.t

  let collect t =
    LabelSetMap.map Child.values t.children

  let v_labels ~label_names ?(registry=CollectorRegistry.default) ~help ?namespace ?subsystem name =
    let label_names = Array.map LabelName.v label_names in
    let metric = MetricInfo.v ~metric_type:Child.metric_type ~help ~label_names ?namespace ?subsystem name in
    let t = {
      metric;
      children = LabelSetMap.empty;
    } in
    CollectorRegistry.register registry metric (fun () -> collect t);
    t

  let labels t label_values =
    assert (Array.length t.metric.MetricInfo.label_names = Array.length label_values);
    match LabelSetMap.find label_values t.children with
    | Some child -> child
    | None ->
      let child = Child.create () in
      t.children <- LabelSetMap.add label_values child t.children;
      child

  let v_label ~label ?registry ~help ?namespace ?subsystem name =
    let family = v_labels ~label_names:[|label|] ?registry ~help ?namespace ?subsystem name in
    fun x -> labels family [| x |]

  let v ?registry ~help ?namespace ?subsystem name =
    let family = v_labels ~help ?registry ?namespace ?subsystem name ~label_names:[||] in
    labels family [||]
end

module Counter = struct
  include Metric(struct
      type t = float ref
      let create () = ref 0.0
      let values t = ["", !t]
      let metric_type = Counter
    end)

  let inc_one t =
    t := !t +. 1.0

  let inc t v =
    assert (v >= 0.0);
    t := !t +. v
end

module Gauge = struct
  include Metric(struct
      type t = float ref
      let create () = ref 0.0
      let values t = ["", !t]
      let metric_type = Gauge
    end)

  let inc t v =
    t := !t +. v
  let inc_one t = inc t 1.0

  let dec t x = inc t (-. x)
  let dec_one t = dec t 1.0

  let set t v =
    t := v

  let track_inprogress t fn =
    inc_one t;
    Lwt.finalize fn (fun () -> dec_one t; Lwt.return_unit)

  let time t fn =
    let start = Unix.gettimeofday () in
    Lwt.finalize fn
      (fun () ->
         let finish = Unix.gettimeofday () in
         inc t (finish -. start);
         Lwt.return_unit
      )
end

module Summary = struct
  module Child = struct
    type t = {
      mutable count : float;
      mutable sum : float;
    }
    let create () = { count = 0.0; sum = 0.0 }
    let values t =
      [
        "_sum", t.sum;
        "_count", t.count;
      ]
    let metric_type = Summary
  end
  include Metric(Child)

  let observe t v =
    let open Child in
    t.count <- t.count +. 1.0;
    t.sum <- t.sum +. v

  let time t fn =
    let start = Unix.gettimeofday () in
    Lwt.finalize fn
      (fun () ->
         let finish = Unix.gettimeofday () in
         observe t (finish -. start);
         Lwt.return_unit
      )
end
