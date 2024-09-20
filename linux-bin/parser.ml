type token =
  | EOL
  | SPACE
  | ACT1
  | ACT2
  | SPE of (
# 7 "bin/parser.mly"
           char
# 10 "bin/parser.ml"
)
  | DIG of (
# 8 "bin/parser.mly"
           int
# 15 "bin/parser.ml"
)
  | LOW of (
# 9 "bin/parser.mly"
           string
# 20 "bin/parser.ml"
)
  | UPP of (
# 10 "bin/parser.mly"
           string
# 25 "bin/parser.ml"
)
  | PLUS
  | MINUS
  | BEGIN
  | END

open Parsing
let _ = parse_error;;
# 1 "bin/parser.mly"
   open List
    open Ast 
# 37 "bin/parser.ml"
let yytransl_const = [|
  257 (* EOL *);
  258 (* SPACE *);
  259 (* ACT1 *);
  260 (* ACT2 *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* BEGIN *);
  268 (* END *);
    0|]

let yytransl_block = [|
  261 (* SPE *);
  262 (* DIG *);
  263 (* LOW *);
  264 (* UPP *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\004\000\004\000\004\000\005\000\
\005\000\005\000\005\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\002\000\002\000\007\000\
\007\000\007\000\007\000\007\000\007\000\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000\002\000\001\000\002\000\002\000\003\000\
\003\000\002\000\002\000\001\000\001\000\001\000\001\000\002\000\
\002\000\002\000\002\000\002\000\003\000\003\000\004\000\001\000\
\002\000\002\000\002\000\002\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\030\000\001\000\000\000\000\000\000\000\
\012\000\000\000\014\000\015\000\000\000\000\000\000\000\013\000\
\000\000\000\000\000\000\020\000\000\000\000\000\011\000\000\000\
\019\000\016\000\017\000\018\000\002\000\005\000\000\000\000\000\
\000\000\026\000\022\000\021\000\023\000\000\000\000\000\004\000\
\000\000\007\000\000\000\000\000"

let yydgoto = "\002\000\
\004\000\005\000\031\000\032\000\013\000\014\000\015\000"

let yysindex = "\017\000\
\015\255\000\000\038\255\000\000\000\000\046\255\046\255\066\255\
\000\000\046\255\000\000\000\000\018\255\028\255\001\255\000\000\
\014\255\018\255\014\255\000\000\083\255\005\255\000\000\022\255\
\000\000\000\000\000\000\000\000\000\000\000\000\053\255\060\255\
\046\255\000\000\000\000\000\000\000\000\020\255\040\255\000\000\
\087\255\000\000\087\255\014\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\088\255\000\000\000\000\011\255\000\000\000\000\000\000\
\045\255\013\255\052\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\064\255\000\000\000\000\000\000\000\000\072\255\078\255\000\000\
\007\255\000\000\009\255\070\255"

let yygindex = "\000\000\
\000\000\000\000\026\000\072\000\092\000\253\255\250\255"

let yytablesize = 99
let yytable = "\017\000\
\019\000\033\000\034\000\022\000\021\000\033\000\034\000\008\000\
\008\000\009\000\009\000\024\000\035\000\010\000\033\000\034\000\
\037\000\001\000\008\000\023\000\009\000\040\000\024\000\024\000\
\010\000\003\000\044\000\041\000\043\000\024\000\029\000\030\000\
\025\000\026\000\027\000\028\000\029\000\030\000\006\000\007\000\
\008\000\042\000\009\000\010\000\011\000\012\000\006\000\007\000\
\008\000\038\000\009\000\016\000\011\000\012\000\040\000\008\000\
\027\000\009\000\016\000\011\000\012\000\042\000\008\000\025\000\
\009\000\016\000\011\000\012\000\008\000\020\000\009\000\016\000\
\011\000\012\000\003\000\028\000\003\000\003\000\003\000\003\000\
\006\000\029\000\006\000\006\000\006\000\006\000\036\000\025\000\
\026\000\027\000\028\000\025\000\026\000\027\000\028\000\039\000\
\013\000\013\000\018\000"

let yycheck = "\006\000\
\007\000\001\001\002\001\010\000\008\000\001\001\002\001\001\001\
\002\001\001\001\002\001\001\001\012\001\001\001\001\001\002\001\
\012\001\001\000\012\001\002\001\012\001\002\001\012\001\002\001\
\012\001\011\001\033\000\031\000\032\000\002\001\009\001\010\001\
\005\001\006\001\007\001\008\001\009\001\010\001\001\001\002\001\
\003\001\002\001\005\001\006\001\007\001\008\001\001\001\002\001\
\003\001\024\000\005\001\006\001\007\001\008\001\002\001\003\001\
\012\001\005\001\006\001\007\001\008\001\002\001\003\001\012\001\
\005\001\006\001\007\001\008\001\003\001\004\001\005\001\006\001\
\007\001\008\001\003\001\012\001\005\001\006\001\007\001\008\001\
\003\001\012\001\005\001\006\001\007\001\008\001\004\001\005\001\
\006\001\007\001\008\001\005\001\006\001\007\001\008\001\024\000\
\009\001\010\001\007\000"

let yynames_const = "\
  EOL\000\
  SPACE\000\
  ACT1\000\
  ACT2\000\
  PLUS\000\
  MINUS\000\
  BEGIN\000\
  END\000\
  "

let yynames_block = "\
  SPE\000\
  DIG\000\
  LOW\000\
  UPP\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'funct) in
    Obj.repr(
# 18 "bin/parser.mly"
                ( () )
# 154 "bin/parser.ml"
               : unit))
; (fun __caml_parser_env ->
    Obj.repr(
# 21 "bin/parser.mly"
              ( ( ) )
# 160 "bin/parser.ml"
               : 'plus))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'plus) in
    Obj.repr(
# 22 "bin/parser.mly"
                ( ( ) )
# 167 "bin/parser.ml"
               : 'plus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'plus) in
    Obj.repr(
# 23 "bin/parser.mly"
                ( ( ) )
# 174 "bin/parser.ml"
               : 'plus))
; (fun __caml_parser_env ->
    Obj.repr(
# 25 "bin/parser.mly"
                 ( ( ) )
# 180 "bin/parser.ml"
               : 'minus))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'minus) in
    Obj.repr(
# 26 "bin/parser.mly"
                 ( ( ) )
# 187 "bin/parser.ml"
               : 'minus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'minus) in
    Obj.repr(
# 27 "bin/parser.mly"
                 ( ( ) )
# 194 "bin/parser.ml"
               : 'minus))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pwd) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'plus) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pwd) in
    Obj.repr(
# 29 "bin/parser.mly"
                  ( '+',(  _1 , _3  ))
# 203 "bin/parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pwd) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'minus) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pwd) in
    Obj.repr(
# 30 "bin/parser.mly"
                   ( '-',(  _1 ,  check_pw _3) )
# 212 "bin/parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'operation) in
    Obj.repr(
# 31 "bin/parser.mly"
                     ( _2 )
# 219 "bin/parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'operation) in
    Obj.repr(
# 32 "bin/parser.mly"
                     ( _1 )
# 226 "bin/parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 36 "bin/parser.mly"
            (   [Ast.Special _1] )
# 233 "bin/parser.ml"
               : 'pwd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 38 "bin/parser.mly"
         (  [Ast.Digit _1]   )
# 240 "bin/parser.ml"
               : 'pwd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "bin/parser.mly"
         (  [Ast.Lowercase _1] )
# 247 "bin/parser.ml"
               : 'pwd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 40 "bin/parser.mly"
         (   [Ast.Uppercase _1]  )
# 254 "bin/parser.ml"
               : 'pwd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pwd) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 41 "bin/parser.mly"
             (  (_1 @ [Ast.Digit _2])    )
# 262 "bin/parser.ml"
               : 'pwd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pwd) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "bin/parser.mly"
             (   ( _1 @ [Ast.Lowercase _2])    )
# 270 "bin/parser.ml"
               : 'pwd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pwd) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 43 "bin/parser.mly"
             (  (_1 @ [Ast.Uppercase _2])    )
# 278 "bin/parser.ml"
               : 'pwd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pwd) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 44 "bin/parser.mly"
             (   (_1 @ [Ast.Special _2])   )
# 286 "bin/parser.ml"
               : 'pwd))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "bin/parser.mly"
               (    Ast.Iterator::(!(Ast.active_pw))    )
# 292 "bin/parser.ml"
               : 'pwd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pwd) in
    Obj.repr(
# 46 "bin/parser.mly"
                  (  Ast.Iterator::(if List.is_empty (!(Ast.active_pw)) then _2 else !(Ast.active_pw))  )
# 299 "bin/parser.ml"
               : 'pwd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sequence) in
    Obj.repr(
# 49 "bin/parser.mly"
                        ( Ast.run_sequence _2 None)
# 306 "bin/parser.ml"
               : 'funct))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'sequence) in
    Obj.repr(
# 50 "bin/parser.mly"
                            ( Ast.run_sequence _3 (Some _2) )
# 314 "bin/parser.ml"
               : 'funct))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'operation) in
    Obj.repr(
# 53 "bin/parser.mly"
                  ( [_1] )
# 321 "bin/parser.ml"
               : 'sequence))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sequence) in
    Obj.repr(
# 54 "bin/parser.mly"
                          (  _2  )
# 328 "bin/parser.ml"
               : 'sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'sequence) in
    Obj.repr(
# 55 "bin/parser.mly"
                    (  _1  )
# 335 "bin/parser.ml"
               : 'sequence))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sequence) in
    Obj.repr(
# 56 "bin/parser.mly"
                  (  _2  )
# 342 "bin/parser.ml"
               : 'sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'sequence) in
    Obj.repr(
# 57 "bin/parser.mly"
                  (  _1  )
# 349 "bin/parser.ml"
               : 'sequence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sequence) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sequence) in
    Obj.repr(
# 58 "bin/parser.mly"
                           (  _1  @  _3  )
# 357 "bin/parser.ml"
               : 'sequence))
(* Entry pwdf *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let pwdf (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : unit)
