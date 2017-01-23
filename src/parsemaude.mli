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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  [ `Variants of ( (Term.term * Term.subst) list) | `Unify of (Term.subst list) | `Match of (Term.subst list) | `Norm of Term.term | `Equal of bool ] 
