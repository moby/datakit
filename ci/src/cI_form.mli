type 'a or_error = ('a, string) result

module State : sig
  type t
  (** The state of a form upload from the user. *)

  type field = {
    data : string option;
    error : string option;
  }

  val empty : t
  (** A state with no fields. *)

  val pop : string -> t -> field
  (** [pop field_name t] returns the current value of the field and removes [field_name] from [t].
      Returns an empty field record if the field isn't known. *)

  val bindings : t -> (string * field) list
  (** [bindings t] is the list of [(name, field)] pairs that haven't been popped. *)

  val of_values : (string * string) list -> t
  (** [of_values vs] is a state initialised with [(field_name, value)] pairs in [vs]. *)
end

module Html : sig
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

  val form : State.t -> csrf_token:string -> form_class:string list -> action:string ->
    [< Html_types.form_content_fun > `Div ] Tyxml.Html.elt list ->
    [> Html_types.form ] Tyxml.Html.elt
  (** [form state ~csrf_token ~form_class ~action controls] is an HTML form
      which posts the values to [action].
      If [state] still contains any fields, they are reported as unknown-field
      errors. *)

  val field : State.t -> string -> field_type -> string -> [> Html_types.div] Tyxml.Html.elt
  (** [field state label type name] is an HTML form control for entering a value of type [type].
      If [state] contains a value for this field, that will be the initial value.
      If [state] contains an error for this field, it will be displayed (and removed from [state]). *)
end

module Validator : sig
  type 'a t
  (** An ['a t] is a validator that parses a form and, on success, returns an ['a]. *)

  type 'a reader = string -> 'a or_error

  val maybe : 'a -> 'a t
  (** [maybe x] is a validator that successfully returns [x] if there were no other validation errors. *)

  val fail : string -> msg:string -> 'a t
  (** [fail field ~msg] is a validation that fails, reporting [msg] against [field]. *)

  val get : string -> (string -> ('a, string) result) -> 'a t
  (** [get field conv] gets the uploaded field named [field] and processes it with [conv].
      If [conv] fails, an error is reported against [field]. *)

  val string : string reader
  (** [string s] always accepts [s]. *)

  val uri : Uri.t reader
  (** [uri s] tries to parse [s] as a Uri. *)

  val non_empty : string reader
  (** [non_empty s] accepts [s] if it is not the empty string. *)

  val confirm : string -> unit reader
  (** [confirm x y] accepts [y] if it is the same as [x] (useful for enter-this-twice confirmation fields). *)

  val optional : 'a reader -> 'a option reader
  (** [optional x] is a reader that returns [None] for the empty string,
      while using [x] to read non-empty strings. *)

  val ( >>!= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** [x >>!= f] is [f y] if the validator [x] successfully produces [y]. *)

  val ( <*> ) : 'a t -> 'b t -> ('a * 'b) t
  (** [x <*> y] validates [x] and [y] and, if both are successful, returns the pair of values.
      If either fails, all errors are reported. *)

  val run : 'a t -> [`String of string | `File of Multipart.file] Multipart.StringMap.t -> ('a, State.t) result
  (** [run v form_data] runs validator [v] on form data uploaded by the user.
      It returns ['a] on success, or a [State.t] if there were any validation errors. *)
end
