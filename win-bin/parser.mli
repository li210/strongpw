type token =
  | EOL
  | SPACE
  | ACT1
  | ACT2
  | SPE of (
# 7 "bin/parser.mly"
           char
# 10 "bin/parser.mli"
)
  | DIG of (
# 8 "bin/parser.mly"
           int
# 15 "bin/parser.mli"
)
  | LOW of (
# 9 "bin/parser.mly"
           string
# 20 "bin/parser.mli"
)
  | UPP of (
# 10 "bin/parser.mly"
           string
# 25 "bin/parser.mli"
)
  | PLUS
  | MINUS
  | BEGIN
  | END

val pwdf :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
