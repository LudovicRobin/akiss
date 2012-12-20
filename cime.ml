open Term

let output_string ch s = Format.fprintf ch "%s" s

let prefix s s' =
  if String.length s > String.length s' then false else
    try
      for i = 0 to String.length s - 1 do
        if s.[i] <> s'.[i] then failwith "false"
      done ;
      true
    with Failure "false" -> false

let rec pp_list pp sep chan = function
  | [] -> ()
  | [x] -> pp chan x
  | x::tl ->
      pp chan x ;
      output_string chan sep ;
      pp_list pp sep chan tl

(** Parameters w1,w2... are not globally declared, we must discover
  * them as we print terms, and declare them to cime. *)
let add_param,reset_params,list_params =
  let params = ref [] in
  let reset () = params := [] in
  let list_params chan =
    List.iter (fun x -> Format.fprintf chan "akiss_%s:0; " x) !params
  in
  let add x = params := x :: !params in
    add,reset,list_params

(** Print out a term in cime notation, translating symbols as needed
  * and registering every parameter used. *)
let rec print chan = function
  | Fun ("!tuple!",[]) ->
      print chan (Fun ("akiss_0uple",[]))
  | Fun ("!tuple!",[x]) ->
      print chan (Fun ("akiss_1uple",[x]))
  | Fun ("!tuple!",[x;y]) ->
      print chan (Fun ("akiss_2uple",[x;y]))
  | Fun ("!test!",[]) ->
      print chan (Fun ("akiss_test",[]))
  | Fun ("!out!",s) ->
      print chan (Fun ("akiss_out",s))
  | Fun ("!in!",s) ->
      print chan (Fun ("akiss_in",s))
  | Fun ("A",[]) -> output_string chan "akiss_chA"
  | Fun ("B",[]) -> output_string chan "akiss_chB"
  | Fun ("C",[]) -> output_string chan "akiss_chC"
  | Fun ("plus",[x;y]) ->
      Format.fprintf chan "(%a)+(%a)" print x print y
  | Fun (s,[]) | Var s ->
      begin try
        Scanf.sscanf s "w%d"
          (fun _ -> add_param s ; Format.fprintf chan "akiss_%s" s)
      with Scanf.Scan_failure _ ->
        output_string chan s
      end
  | Fun (s,args) ->
      Format.fprintf chan "%s(%a)" s (pp_list print ", ") args

(** Print a term to a string. *)
let sprint t =
  print Format.str_formatter t ;
  Format.flush_str_formatter ()

(** Translating symbol names back to Akiss conventions *)
let translate_atom = function
  | "akiss_test" -> "!test!"
  | "akiss_out" -> "!out!"
  | "akiss_in" -> "!in!"
  | "akiss_0uple" | "akiss_1uple" | "akiss_2uple" -> "!tuple!"
  | "akiss_chA" -> "A"
  | "akiss_chB" -> "B"
  | "akiss_chC" -> "C"
  | s when prefix "akiss_" s ->
      Scanf.sscanf s "akiss_w%d" (fun d -> "w" ^ string_of_int d)
  | s -> s

(** Turn a symbol into an Akiss variable or constant.
  * TODO this is very naive. *)
let atom_of_string s =
  if s.[0] = 'X' || s.[0] = 'x' then Var s else Fun (s,[])

let arity_check s n =
  match s with
    | "world" -> n=2
    | _ -> true

(** Print CiME script for unifying [s] and [t]. *)
let print_script ?(op=`Unification) chan s t =
  let declare name kind f =
    Format.fprintf chan "let %s = %s \"" name kind ;
    f () ;
    Format.fprintf chan "\";\n"
  in
  let () = reset_params () in
  let st_vars = vars_of_term_list [s;t] in
  let s = sprint s in
  let t = sprint t in
    declare "S" "signature"
      (fun () ->
         output_string chan "akiss_0uple:0; akiss_1uple:1; akiss_2uple:2; " ;
         output_string chan "akiss_out:1; akiss_in:2; akiss_test:0; " ;
         output_string chan
           "world:2; empty:0; knows:3; reach:1; identical:3; ridentical:3;\n" ;
         (* TODO don't hardcode, requires moving channels out of Main *)
         output_string chan "akiss_chA:0; akiss_chB:0; akiss_chC:0; " ;
         list_params chan ;
         output_string chan "\n" ;
         pp_list
           (fun chan (f,n) ->
              if f = "plus" then
                Format.fprintf chan "+ : AC"
              else
                Format.fprintf chan "%s:%d" f n)
           "; "
           chan
           !fsymbols ;
         output_string chan "; " ;
         pp_list
           (fun chan v ->
              Format.fprintf chan "%s:constant" v)
           "; "
           chan
           !vars ;
         output_string chan "; " ;
         pp_list
           (fun chan v ->
              Format.fprintf chan "%s:constant" v)
           "; "
           chan
           !private_names) ;
    declare "X" "variables"
      (fun () ->
         pp_list output_string ", " chan st_vars) ;
    Format.fprintf chan "let A = algebra S;\n" ;
    declare "s" "term A" (fun () -> output_string chan s) ;
    declare "t" "term A" (fun () -> output_string chan t) ;
    match op with
      | `Unification ->
          Format.fprintf chan "let csu = unify s t;\n"
      | `Matching ->
          Format.fprintf chan "let csu = matching s t;\n"

(** Tokenizer for CiME output *)
let cime_tokens chan =
  let stream = Stream.of_channel chan in
  let keywords =
    [ "let";"=";"->";"(";")";"{";"}";"[";"]";",";";" ]
  in
  let keywords = "+" :: keywords in
    Genlex.make_lexer keywords stream

(** Parse CiME output to obtain the csu *)
let cime_parse tokens =

  let string_of_token = function
    | (Genlex.Kwd k) -> "K "^k
    | (Genlex.Ident k) -> "I "^k
    | _ -> "?"
  in
  let perror s =
    Format.printf "parse error %s: %s\n%!" s
      (match Stream.peek tokens with
         | None -> "$"
         | Some tok -> string_of_token tok) ;
    failwith "parse error"
  in
  let passert x b =
    if x <> b then
      perror
        (Format.sprintf "%s<>%s" (string_of_token x) (string_of_token b))
  in

  let rec parse_plus () =
    let t = parse_term () in
      if Stream.peek tokens <> Some (Genlex.Kwd "+") then
        t
      else begin
        ignore (Stream.next tokens) ;
        Fun ("plus",[t; parse_plus ()])
      end

  and parse_term () =
    match Stream.next tokens with
      | Genlex.Ident s ->
          let s = translate_atom s in
            if Stream.peek tokens <> Some (Genlex.Kwd "(") then
              atom_of_string s
            else begin
              ignore (Stream.next tokens) ;
              let args = parse_terms () in
                assert (arity_check s (List.length args)) ;
                Fun (s,args)
            end
      | _ -> perror "in term"

  and parse_terms () =
    let t = parse_plus () in
    match Stream.next tokens with
      | Genlex.Kwd "," -> t :: parse_terms ()
      | Genlex.Kwd ")" -> [t]
      | _ -> perror "in terms"
  in

  let rec parse_solution () =
    match Stream.next tokens with
      | Genlex.Kwd "]" -> []
      | Genlex.Ident x ->
          let t =
            let sep = Stream.next tokens in
              passert sep (Genlex.Kwd "->") ;
              parse_plus ()
          in
            (x,t) :: parse_solution ()
      | t -> perror (Format.sprintf "in solution (%s)" (string_of_token t))
  in
  let rec parse_solutions () =
    match Stream.next tokens with
      | Genlex.Kwd "}" -> []
      | Genlex.Kwd "[" ->
          let sol = parse_solution () in
          let more = parse_solutions () in
            sol :: more
      | t -> perror (Format.sprintf "in solutions (at %s)" (string_of_token t))
  in
  let rec strip_junk () =
    (* begin match Stream.peek tokens with
      | None -> Format.printf "EOS\n"
      | Some tok -> Format.printf "junk %s\n" (string_of_token tok)
    end ; *)
    if Stream.next tokens = Genlex.Kwd "let" &&
       Stream.next tokens = Genlex.Ident "csu"
    then
      let eq = Stream.next tokens in
      let op = Stream.next tokens in
        passert eq (Genlex.Kwd "=") ;
        passert op (Genlex.Kwd "{") ;
        parse_solutions ()
    else strip_junk ()
  in
    strip_junk ()

(** Run CiME to perform unification and parse its output.
  * TODO path to executable shouldn't be hardcoded... *)
let run_cime ?op outchan s t =
  let home = Sys.getenv "HOME" in
  let chan_out,chan_in =
    Unix.open_process
      (home ^ "/soft/cime-out/src/main.native \
       -icime /dev/stdin -ocime /dev/stdout")
  in
    print_script ?op (Format.formatter_of_out_channel chan_in) s t ;
    close_out chan_in ;
    let csu =
      try
        (* Make sure the very first line is a let, not an error *)
        assert
          (let l = input_line chan_out in
             (* Format.printf "peek line %S\n" l ; *)
             prefix "let" l) ;
        (* Don't parse precisely the first lines *)
        let rec strip () =
          let l = input_line chan_out in
            (* Format.printf "peek line %S\n" l ; *)
            if prefix "let t =" l then
              let tokens = cime_tokens chan_out in
                cime_parse tokens
            else
              strip ()
        in
          strip ()
      with e ->
        print_script ?op Format.std_formatter s t ;
        raise e
    in
      match Unix.close_process (chan_out,chan_in) with
        | Unix.WEXITED 0 ->
            (* if op = Some `Matching then begin
              print_script ?op Format.std_formatter s t ;
              Printf.printf "%d results\n" (List.length csu) ;
            end ; *)
            csu
        | _ -> Format.printf "CiME exited abnormally!\n" ; exit 1

(** Perform unification with CiME *)
let csu s t = run_cime stdout s t

(** Perform unification with CiME and expect only one mgu *)
let mgu s t =
  (* Format.printf "Cime.mgu %s %s\n" (show_term s) (show_term t) ; *)
  match run_cime stdout s t with
    | [] -> raise Not_unifiable
    | [s] -> s
    | _ -> assert false

(** Perform unification with CiME *)
let mgm s t =
  match run_cime ~op:`Matching stdout s t with
    | [] -> raise Not_matchable
    | [s] -> s
    | _ -> assert false

(** Wrapper on mgu with checks against old version *)
let mgu_checked s t =
  try
    let sigma = Term.mgu s t in
      try
        let theta = mgu s t in
        let s1 = Term.apply_subst s sigma in
        let s2 = Term.apply_subst s theta in
        let t1 = Term.apply_subst t sigma in
        let t2 = Term.apply_subst t theta in
          if not (s1 = s2 && s2 = t1 && t1 = t2) then begin
            Format.printf "s1=%s\ns2=%s\nt1=%s\nt2=%s\n"
              (show_term s1)
              (show_term s2)
              (show_term t1)
              (show_term t2) ;
            assert false
          end ;
          theta
      with
        | Term.Not_unifiable -> assert false
  with
    | Term.Not_unifiable ->
        ignore (mgu s t) ;
        assert false

(** Equality modulo AC
  * TODO this is
  *   quick: this is inefficient
  *   dirty: the substitution may be the identity without being [] *)
let equals s t = csu s t = [[]]