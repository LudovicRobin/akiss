type token =
  | Sharp
  | Percent
  | EOF
  | VariantUnify
  | Unify
  | GetVariants
  | Reduce
  | Match
  | In
  | Ms
  | Cpu
  | Real
  | Second
  | Unifier
  | Variant
  | Result
  | Solution
  | Maude
  | Line1
  | Identifier of (string)
  | Number of (string)
  | Int of (int)
  | Equals
  | Dot
  | Slash
  | Comma
  | Colon
  | Arrow
  | EqualUnify
  | EqualMatch
  | LeftP
  | RightP
  | Zero
  | Plus
  | NoUnifiers
  | NoUnifier
  | NoMatch
  | NoMoreUnifiers
  | NoMoreVariants
  | Rewritesline
  | Decisiontimeline
  | Bool
  | True
  | False
  | Greater
  | Term
  | Bye

open Parsing;;
let _ = parse_error;;
# 2 "parsemaude.mly"

(****************************************************************************)
(* Akiss                                                                    *)
(* Copyright (C) 2011-2014 Baelde, Ciobaca, Delaune, Kremer                 *)
(*                                                                          *)
(* This program is free software; you can redistribute it and/or modify     *)
(* it under the terms of the GNU General Public License as published by     *)
(* the Free Software Foundation; either version 2 of the License, or        *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU General Public License for more details.                             *)
(*                                                                          *)
(* You should have received a copy of the GNU General Public License along  *)
(* with this program; if not, write to the Free Software Foundation, Inc.,  *)
(* 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.              *)
(****************************************************************************)

open Term


(** [freshen sigma] replaces all variables in the range of the substitution
  * [sigma] by fresh variables. *)
let freshensubst (sigma : subst) =
  let varlist = vars_of_term_list (List.map snd sigma) in
  let fresh_subst =
    List.map (fun x -> (x, Var(Util.fresh_variable ()))) varlist
  in
  List.map (fun (v,t) -> v, (apply_subst t fresh_subst)) sigma


let freshenvariant ((t, sigma) : (term * subst)) =
  let varlist = vars_of_term_list (t::(List.map snd sigma)) in
  let fresh_subst =
    List.map (fun x -> (x, Var(Util.fresh_variable ()))) varlist
  in
  let fs = List.map (fun (v,t) -> v, (apply_subst t fresh_subst)) sigma in
  let ft = apply_subst t fresh_subst in
  (ft, fs)

    
(** Translating symbol names back to Akiss conventions *)

let translate_symbol = function
  | "akisstest" -> "!test!"
  | "akissout" -> "!out!"
  | "akissin" -> "!in!"
  | s when Util.startswith ~prefix:"akissch" s ->
      String.sub s 7 (String.length s - 7)
  | s when Util.startswith ~prefix:"akiss" s ->
      begin try
        Scanf.sscanf s "akissw%d" (fun d -> "w" ^ string_of_int d)
      with _ ->
        try Scanf.sscanf s "akissn%d" (fun d -> "!n!" ^ string_of_int d)
        with _ -> Scanf.sscanf s "akiss%duple" (fun d -> "!tuple!")
      end
  | s -> s

let translate_name x =
  try Scanf.sscanf x "akissn%d" (fun x -> Printf.sprintf "!n!%d" x)
  with Scanf.Scan_failure _ | End_of_file -> x

# 118 "parsemaude.ml"
let yytransl_const = [|
  257 (* Sharp *);
  258 (* Percent *);
    0 (* EOF *);
  259 (* VariantUnify *);
  260 (* Unify *);
  261 (* GetVariants *);
  262 (* Reduce *);
  263 (* Match *);
  264 (* In *);
  265 (* Ms *);
  266 (* Cpu *);
  267 (* Real *);
  268 (* Second *);
  269 (* Unifier *);
  270 (* Variant *);
  271 (* Result *);
  272 (* Solution *);
  273 (* Maude *);
  274 (* Line1 *);
  278 (* Equals *);
  279 (* Dot *);
  280 (* Slash *);
  281 (* Comma *);
  282 (* Colon *);
  283 (* Arrow *);
  284 (* EqualUnify *);
  285 (* EqualMatch *);
  286 (* LeftP *);
  287 (* RightP *);
  288 (* Zero *);
  289 (* Plus *);
  290 (* NoUnifiers *);
  291 (* NoUnifier *);
  292 (* NoMatch *);
  293 (* NoMoreUnifiers *);
  294 (* NoMoreVariants *);
  295 (* Rewritesline *);
  296 (* Decisiontimeline *);
  297 (* Bool *);
  298 (* True *);
  299 (* False *);
  300 (* Greater *);
  301 (* Term *);
  302 (* Bye *);
    0|]

let yytransl_block = [|
  275 (* Identifier *);
  276 (* Number *);
  277 (* Int *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\004\000\005\000\005\000\005\000\006\000\007\000\007\000\
\007\000\018\000\008\000\009\000\009\000\009\000\019\000\010\000\
\011\000\011\000\013\000\012\000\014\000\016\000\016\000\016\000\
\016\000\016\000\020\000\020\000\021\000\021\000\017\000\017\000\
\022\000\015\000\015\000\000\000"

let yylen = "\002\000\
\003\000\001\000\002\000\002\000\002\000\002\000\002\000\004\000\
\006\000\008\000\002\000\002\000\006\000\009\000\001\000\001\000\
\002\000\003\000\009\000\001\000\001\000\002\000\003\000\006\000\
\002\000\007\000\003\000\006\000\009\000\001\000\004\000\004\000\
\004\000\004\000\000\000\001\000\001\000\003\000\000\000\002\000\
\003\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\002\000\044\000\000\000\003\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\004\000\000\000\015\000\005\000\
\000\000\000\000\020\000\006\000\000\000\000\000\000\000\007\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\011\000\012\000\000\000\017\000\000\000\022\000\000\000\025\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\018\000\000\000\023\000\000\000\000\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\024\000\000\000\028\000\
\000\000\000\000\041\000\000\000\027\000\042\000\043\000\009\000\
\000\000\000\000\000\000\000\000\036\000\000\000\000\000\000\000\
\000\000\000\000\013\000\000\000\031\000\032\000\000\000\033\000\
\034\000\010\000\000\000\000\000\000\000\026\000\038\000\014\000\
\029\000\019\000"

let yydgoto = "\002\000\
\005\000\006\000\013\000\014\000\029\000\015\000\032\000\016\000\
\036\000\017\000\040\000\018\000\071\000\019\000\104\000\107\000\
\066\000\033\000\037\000\108\000\109\000\067\000"

let yysindex = "\008\000\
\005\255\000\000\001\255\000\000\000\000\011\255\000\000\023\255\
\039\255\060\255\067\255\068\255\040\255\248\254\016\255\012\255\
\254\254\038\255\041\255\062\255\063\255\064\255\065\255\066\255\
\000\000\078\255\047\255\054\255\000\000\072\255\000\000\000\000\
\016\255\074\255\000\000\000\000\012\255\094\255\057\255\000\000\
\082\255\083\255\073\255\075\255\076\255\077\255\079\255\080\255\
\000\000\000\000\085\255\000\000\085\255\000\000\086\255\000\000\
\069\255\070\255\006\255\006\255\006\255\006\255\006\255\071\255\
\081\255\000\000\085\255\000\000\084\255\087\255\000\000\089\255\
\092\255\096\255\088\255\090\255\091\255\093\255\099\255\049\255\
\095\255\085\255\006\255\000\000\069\255\006\255\031\255\100\255\
\101\255\006\255\006\255\006\255\006\255\000\000\103\255\000\000\
\006\255\248\254\000\000\085\255\000\000\000\000\000\000\000\000\
\097\255\098\255\104\255\102\255\000\000\105\255\107\255\108\255\
\006\255\109\255\000\000\254\254\000\000\000\000\006\255\000\000\
\000\000\000\000\106\255\111\255\110\255\000\000\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\118\255\000\000\000\000\000\000\120\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\043\255\000\000\250\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\053\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\255\000\000\000\000\000\000\000\000\000\000\
\000\000\008\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\113\255\113\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\255\254\000\000\000\000\000\000\000\000\
\000\000\000\000\114\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\009\000\000\000\076\000\000\000\
\080\000\000\000\012\000\000\000\053\000\000\000\000\000\197\255\
\209\255\000\000\000\000\048\000\021\000\000\000"

let yytablesize = 150
let yytable = "\077\000\
\078\000\079\000\080\000\081\000\026\000\068\000\073\000\074\000\
\001\000\039\000\039\000\038\000\039\000\008\000\009\000\010\000\
\011\000\012\000\007\000\084\000\039\000\003\000\004\000\099\000\
\075\000\027\000\101\000\034\000\028\000\039\000\020\000\030\000\
\111\000\112\000\098\000\039\000\039\000\114\000\076\000\030\000\
\030\000\039\000\030\000\030\000\039\000\030\000\021\000\035\000\
\030\000\030\000\031\000\030\000\116\000\124\000\030\000\030\000\
\025\000\030\000\039\000\039\000\030\000\030\000\030\000\030\000\
\030\000\039\000\039\000\022\000\039\000\039\000\095\000\096\000\
\102\000\103\000\023\000\024\000\041\000\039\000\048\000\042\000\
\043\000\044\000\045\000\046\000\047\000\049\000\039\000\039\000\
\039\000\039\000\039\000\051\000\050\000\053\000\055\000\056\000\
\057\000\058\000\059\000\064\000\060\000\061\000\062\000\065\000\
\063\000\069\000\115\000\083\000\052\000\082\000\072\000\088\000\
\086\000\070\000\087\000\089\000\054\000\090\000\092\000\091\000\
\093\000\094\000\085\000\097\000\113\000\105\000\106\000\126\000\
\119\000\122\000\123\000\125\000\120\000\129\000\016\000\121\000\
\021\000\100\000\110\000\127\000\000\000\117\000\118\000\035\000\
\037\000\128\000\000\000\000\000\000\000\130\000"

let yycheck = "\059\000\
\060\000\061\000\062\000\063\000\013\001\053\000\001\001\002\001\
\001\000\016\001\017\001\014\001\014\001\003\001\004\001\005\001\
\006\001\007\001\018\001\067\000\013\001\017\001\018\001\083\000\
\019\001\034\001\086\000\016\001\037\001\036\001\008\001\016\001\
\092\000\093\000\082\000\038\001\038\001\097\000\033\001\013\001\
\014\001\034\001\016\001\017\001\037\001\019\001\008\001\036\001\
\022\001\023\001\035\001\025\001\100\000\113\000\028\001\029\001\
\017\001\031\001\016\001\017\001\034\001\035\001\036\001\037\001\
\038\001\013\001\014\001\008\001\016\001\017\001\022\001\023\001\
\042\001\043\001\008\001\008\001\039\001\035\001\001\001\039\001\
\019\001\019\001\019\001\019\001\019\001\039\001\034\001\035\001\
\036\001\037\001\038\001\020\001\039\001\020\001\001\001\039\001\
\015\001\015\001\026\001\020\001\026\001\026\001\026\001\019\001\
\026\001\020\001\098\000\027\001\033\000\039\001\041\001\020\001\
\026\001\045\001\026\001\020\001\037\000\030\001\028\001\030\001\
\028\001\023\001\039\001\029\001\022\001\026\001\026\001\116\000\
\025\001\023\001\023\001\023\001\031\001\023\001\017\001\031\001\
\017\001\085\000\091\000\119\000\255\255\045\001\045\001\031\001\
\031\001\040\001\255\255\255\255\255\255\040\001"

let yynames_const = "\
  Sharp\000\
  Percent\000\
  EOF\000\
  VariantUnify\000\
  Unify\000\
  GetVariants\000\
  Reduce\000\
  Match\000\
  In\000\
  Ms\000\
  Cpu\000\
  Real\000\
  Second\000\
  Unifier\000\
  Variant\000\
  Result\000\
  Solution\000\
  Maude\000\
  Line1\000\
  Equals\000\
  Dot\000\
  Slash\000\
  Comma\000\
  Colon\000\
  Arrow\000\
  EqualUnify\000\
  EqualMatch\000\
  LeftP\000\
  RightP\000\
  Zero\000\
  Plus\000\
  NoUnifiers\000\
  NoUnifier\000\
  NoMatch\000\
  NoMoreUnifiers\000\
  NoMoreVariants\000\
  Rewritesline\000\
  Decisiontimeline\000\
  Bool\000\
  True\000\
  False\000\
  Greater\000\
  Term\000\
  Bye\000\
  "

let yynames_block = "\
  Identifier\000\
  Number\000\
  Int\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'firstLine) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'result) in
    Obj.repr(
# 101 "parsemaude.mly"
                          ( _2 )
# 359 "parsemaude.ml"
               :  [ `Variants of ( (Term.term * Term.subst) list) | `Unify of (Term.subst list) | `Match of (Term.subst list) | `Norm of Term.term | `Equal of bool ] ))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parsemaude.mly"
         ( )
# 365 "parsemaude.ml"
               : 'firstLine))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parsemaude.mly"
               ( )
# 371 "parsemaude.ml"
               : 'firstLine))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unifierPreamble) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unifierList) in
    Obj.repr(
# 108 "parsemaude.mly"
                               ( `Unify _2 )
# 379 "parsemaude.ml"
               : 'result))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'acunifierPreamble) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'acunifierList) in
    Obj.repr(
# 109 "parsemaude.mly"
                                   ( `Unify _2 )
# 387 "parsemaude.ml"
               : 'result))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'matchPreamble) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'matcherList) in
    Obj.repr(
# 110 "parsemaude.mly"
                             ( `Match _2 )
# 395 "parsemaude.ml"
               : 'result))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'variantPreamble) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'variantList) in
    Obj.repr(
# 111 "parsemaude.mly"
                               ( `Variants _2 )
# 403 "parsemaude.ml"
               : 'result))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'reducePreamble) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'resultTerm) in
    Obj.repr(
# 112 "parsemaude.mly"
                                                 (`Norm _4 )
# 411 "parsemaude.ml"
               : 'result))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'equalsPreamble) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'bool) in
    Obj.repr(
# 113 "parsemaude.mly"
                                                      ( `Equal _6 )
# 419 "parsemaude.ml"
               : 'result))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 116 "parsemaude.mly"
                                                             ()
# 428 "parsemaude.ml"
               : 'unifierPreamble))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parsemaude.mly"
                           ([])
# 434 "parsemaude.ml"
               : 'unifierList))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parsemaude.mly"
                               ([])
# 440 "parsemaude.ml"
               : 'unifierList))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'substitution) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'unifierList) in
    Obj.repr(
# 122 "parsemaude.mly"
     ((freshensubst _5)::_6)
# 449 "parsemaude.ml"
               : 'unifierList))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    Obj.repr(
# 126 "parsemaude.mly"
                 ( )
# 458 "parsemaude.ml"
               : 'acunifierPreamble))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "parsemaude.mly"
             ( [] )
# 464 "parsemaude.ml"
               : 'acunifierList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'acunifier) in
    Obj.repr(
# 130 "parsemaude.mly"
             ([_1])
# 471 "parsemaude.ml"
               : 'acunifierList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'acunifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'acunifierList) in
    Obj.repr(
# 131 "parsemaude.mly"
                           ( _1::_2 )
# 479 "parsemaude.ml"
               : 'acunifierList))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'substitution) in
    Obj.repr(
# 135 "parsemaude.mly"
     (freshensubst _3)
# 487 "parsemaude.ml"
               : 'acunifier))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    Obj.repr(
# 139 "parsemaude.mly"
                 ( )
# 496 "parsemaude.ml"
               : 'matchPreamble))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "parsemaude.mly"
           ( [] )
# 502 "parsemaude.ml"
               : 'matcherList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'matcher) in
    Obj.repr(
# 143 "parsemaude.mly"
           ([_1])
# 509 "parsemaude.ml"
               : 'matcherList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'matcher) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'matcherList) in
    Obj.repr(
# 144 "parsemaude.mly"
                       ( _1::_2 )
# 517 "parsemaude.ml"
               : 'matcherList))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'substitution) in
    Obj.repr(
# 148 "parsemaude.mly"
     (freshensubst _3)
# 525 "parsemaude.ml"
               : 'matcher))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 152 "parsemaude.mly"
                                            ( )
# 533 "parsemaude.ml"
               : 'variantPreamble))
; (fun __caml_parser_env ->
    Obj.repr(
# 155 "parsemaude.mly"
                               ([])
# 539 "parsemaude.ml"
               : 'variantList))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'resultTerm) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'substitution) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'variantList) in
    Obj.repr(
# 157 "parsemaude.mly"
                 ( freshenvariant(_5,_6)::_7 )
# 549 "parsemaude.ml"
               : 'variantList))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 160 "parsemaude.mly"
                   ( _3 )
# 556 "parsemaude.ml"
               : 'resultTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 163 "parsemaude.mly"
                                       ( )
# 564 "parsemaude.ml"
               : 'reducePreamble))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 166 "parsemaude.mly"
                                                          ( )
# 573 "parsemaude.ml"
               : 'equalsPreamble))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 170 "parsemaude.mly"
     (
       let id = translate_symbol _1 in
       if (List.mem id !private_names) || (List.mem id !channels) ||
	 (List.mem (id,0) !fsymbols) || List.mem id ["empty";"!test!"] ||
	 (Str.string_match (Str.regexp "w[0-9]+") id 0) ||
	 (Str.string_match (Str.regexp "!n![0-9]+") id 0)
       then
	 Fun(id,[])
       else
	 Var id
     )
# 590 "parsemaude.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 181 "parsemaude.mly"
                           ( Var ("#"^_2) )
# 597 "parsemaude.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 182 "parsemaude.mly"
                             ( Var ("%"^_2) )
# 604 "parsemaude.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'termlist) in
    Obj.repr(
# 183 "parsemaude.mly"
                                    ( let t = Fun(translate_symbol
						    _1,_3) in t )
# 613 "parsemaude.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'termlist) in
    Obj.repr(
# 185 "parsemaude.mly"
                              ( let l = _3 in
				List.fold_left
				  (fun a b -> Fun ("plus",[a;b]))
				  (List.hd l) (List.tl l))
# 623 "parsemaude.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 191 "parsemaude.mly"
   ( [] )
# 629 "parsemaude.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'netermlist) in
    Obj.repr(
# 192 "parsemaude.mly"
              (	_1 )
# 636 "parsemaude.ml"
               : 'termlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 195 "parsemaude.mly"
        ( [ _1 ] )
# 643 "parsemaude.ml"
               : 'netermlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'netermlist) in
    Obj.repr(
# 196 "parsemaude.mly"
                         ( _1 :: _3 )
# 651 "parsemaude.ml"
               : 'netermlist))
; (fun __caml_parser_env ->
    Obj.repr(
# 199 "parsemaude.mly"
   ( [] )
# 657 "parsemaude.ml"
               : 'substitution))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'assignment) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'substitution) in
    Obj.repr(
# 200 "parsemaude.mly"
                           ( _1 :: _2 )
# 665 "parsemaude.ml"
               : 'substitution))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 203 "parsemaude.mly"
                          ( (_1, _3) )
# 673 "parsemaude.ml"
               : 'assignment))
; (fun __caml_parser_env ->
    Obj.repr(
# 206 "parsemaude.mly"
         (true)
# 679 "parsemaude.ml"
               : 'bool))
; (fun __caml_parser_env ->
    Obj.repr(
# 207 "parsemaude.mly"
         (false)
# 685 "parsemaude.ml"
               : 'bool))
(* Entry main *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  [ `Variants of ( (Term.term * Term.subst) list) | `Unify of (Term.subst list) | `Match of (Term.subst list) | `Norm of Term.term | `Equal of bool ] )
;;
