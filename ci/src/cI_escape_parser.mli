type colour =
  [ `Black
  | `Blue
  | `Cyan
  | `Green
  | `Magenta
  | `Red
  | `White
  | `Yellow ]

type sgr =
  [ `BgCol of [`Default | colour]
  | `Bold
  | `FgCol of [`Default | colour]
  | `Italic
  | `NoBold
  | `NoItalic
  | `NoReverse
  | `NoUnderline
  | `Reset
  | `Reverse
  | `Underline ]

type escape =
  [ `Reset
  | `Ctrl of [ `SelectGraphicRendition of sgr list] ]

val parse : CI_char_stream.t ->
  [ `Literal of CI_char_stream.t
  | `Escape of escape * CI_char_stream.t
  | `Invalid of CI_char_stream.t
  | `Incomplete ]
(** [parse stream] returns the first token in [stream] and the stream directly after it,
    or [`Incomplete] if more data is required to parse the first token.
    [`Literal s2] indicates that everything between [stream] and [s2] should be output as literal text.
    [`Escape (e, s2)] indicates that the first token was escape sequence [e].
    [`Invalid s2] indicates that the first token was malformed or not understood and processing should continue
    from [s2].
*)
