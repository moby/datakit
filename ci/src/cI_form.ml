type data = [`String of string | `File of Multipart.file] Multipart.StringMap.t
type 'a or_error = ('a, string) result

module State = struct
  type field = {
    data : string option;
    error : string option;
  }

  type t = field Multipart.StringMap.t ref

  let empty : t = ref Multipart.StringMap.empty

  let empty_field = {
    data = None;
    error = None;
  }

  let pop field t =
    match Multipart.StringMap.find field !t with
    | exception Not_found -> empty_field
    | data ->
      t := Multipart.StringMap.remove field !t;
      data

  let bindings t =
    Multipart.StringMap.bindings !t

  let of_values vs =
    let map =
      List.fold_left (fun acc (key, data) ->
          let field = { data = Some data; error = None } in
          Multipart.StringMap.add key field acc
        ) Multipart.StringMap.empty vs
    in
    ref map
end

module Html = struct
  open! Tyxml.Html

  type field_type =
    [ `Url
    | `Tel
    | `Text
    | `Time
    | `Search
    | `Password
    | `Checkbox
    | `Range
    | `Radio
    | `Submit
    | `Reset
    | `Number
    | `Hidden
    | `Month
    | `Week
    | `File
    | `Email
    | `Image
    | `Datetime_local
    | `Datetime
    | `Date
    | `Color
    | `Button ]

  let field state descr ty name =
    let s = State.pop name state in
    let err =
      match s.State.error with
      | None -> []
      | Some err -> [div ~a:[a_class ["alert"; "alert-danger"]] [txt err]]
    in
    let init = s.State.data |> CI_utils.default "" in
    let id = "field-" ^ name in
    div ~a:[a_class ["form-group"]] ([
        label ~a:[a_label_for id] [txt descr];
        input ~a:[a_class ["form-control"]; a_id id; a_input_type ty; a_name name; a_value init] ()
      ] @ err)

  let form state ~csrf_token ~form_class ~action children =
    let query = [
      "CSRFToken", [csrf_token];
    ] in
    let action = Printf.sprintf "%s?%s" action (Uri.encoded_of_query query) in
    let warnings =
      State.bindings state |> List.map (fun (name, field) ->
          let err = field.State.error |> CI_utils.default "Unexpected field" in
          div ~a:[a_class ["alert"; "alert-danger"]] [txt (Fmt.strf "%s: %s" name err)]
        )
    in
    form ~a:[a_class form_class; a_action action; a_method `Post; a_enctype "multipart/form-data"]
      (children @ warnings)
end

module Validator = struct
  type 'a t = data -> string Multipart.StringMap.t -> ('a, string Multipart.StringMap.t) result

  type 'a reader = string -> 'a or_error

  let maybe v _ acc =
    if Multipart.StringMap.is_empty acc then Ok v
    else Error acc

  let fail field ~msg _ acc =
    Error (Multipart.StringMap.add field msg acc)

  let tagged name acc = function
    | Ok x -> Ok x
    | Error e -> Error (Multipart.StringMap.add name e acc)

  let get name fn parts acc =
    tagged name acc begin
      match Multipart.StringMap.find name parts with
      | `String s -> fn s
      | `File _ -> Error "File upload in form!"
      | exception Not_found -> Error "Missing field"
    end

  let string x = Ok x

  let non_empty = function
    | "" -> Error "Cannot be empty"
    | s -> Ok s

  let uri x =
    try Ok (Uri.of_string x)
    with ex -> Error (Printexc.to_string ex)

  let confirm required actual =
    if required <> actual then Error "Values don't match!"
    else Ok ()

  let optional v = function
    | "" -> Ok None
    | x ->
      match v x with
      | Ok x -> Ok (Some x)
      | Error _ as e -> e

  let ( >>!= ) x f parts acc =
    match x parts acc with
    | Ok x -> f x parts acc
    | Error _ as e -> e

  let ( <*> ) a b parts acc =
    match a parts acc with
    | Ok a ->
      begin match b parts acc with
      | Ok b -> Ok (a, b)
      | Error _ as e -> e
      end
    | Error acc ->
      match b parts acc with
      | Ok _ -> Error acc
      | Error _ as e -> e

  let run (x:'a t) parts : ('a, State.t) result =
    match x parts Multipart.StringMap.empty with
    | Ok y -> Ok y
    | Error errors ->
      let merge _k p e =
        let p =
          match p with
          | None -> None
          | Some (`String p) -> Some p
          | Some (`File _) -> None
        in
        match p, e with
        | None, None -> None
        | data, error -> Some {State.data; error}
      in
      Error (ref (Multipart.StringMap.merge merge parts errors))
end
