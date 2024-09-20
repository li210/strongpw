type token =
  | EOL
  | SPACE
  | ACT1
  | ACT2
  | SPE of (
# 7 "linux-bin/parser.mly"
           char
# 10 "linux-bin/parser.mli"
)
  | DIG of (
# 8 "linux-bin/parser.mly"
           int
# 15 "linux-bin/parser.mli"
)
  | LOW of (
# 9 "linux-bin/parser.mly"
           string
# 20 "linux-bin/parser.mli"
)
  | UPP of (
# 10 "linux-bin/parser.mly"
           string
# 25 "linux-bin/parser.mli"
)
  | PLUS
  | MINUS
  | BEGIN
  | END

val pwdf :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
