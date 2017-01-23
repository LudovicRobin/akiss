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

open Parser
open Util
open Term
open Horn
open Process

module R = Theory.R

(** {1 Seed statements} *)

let current_parameter oc =
  "w" ^ (string_of_int oc)
;;

let worldadd w t =
  revworld (Fun("world", [t; revworld w]))
;;

let rec worldreplempty w wp =
  match w with
    | Fun("empty", []) -> wp
    | Fun("world", [f; r]) -> Fun("world", [f; worldreplempty r wp])
    | Var(_) -> invalid_arg("worldreplempty for var")
    | _ -> invalid_arg("worldreplempty")
;;

let normalize_msg_atom rules = function
  | Predicate("knows", [w; r; t]) ->
      Predicate("knows", [R.normalize w rules; r; R.normalize t rules])
  | Predicate("knows+", [w; r; t]) ->
      Predicate("knows+", [R.normalize w rules; r; R.normalize t rules])
  | Predicate("reach", [w]) ->
      Predicate("reach", [R.normalize w rules])
  | Predicate("identical", [w; r; rp]) ->
      Predicate("identical", [R.normalize w rules; r; rp])
  | Predicate("ridentical", [w; r; rp]) ->
      Predicate("ridentical", [R.normalize w rules; r; rp])
  | _ -> invalid_arg("normalize_msg_atom")
;;

let normalize_msg_st (head, body) rules =
  (normalize_msg_atom rules head, trmap (fun x -> normalize_msg_atom rules x) body)
;;

let apply_subst_msg_atom sigma = function
  | Predicate("knows", [w; r; t]) ->
      Predicate("knows", [apply_subst w sigma; r; apply_subst t sigma])
  | Predicate("reach", [w]) ->
      Predicate("reach", [apply_subst w sigma])
  | Predicate("identical", [w; r; rp]) ->
      Predicate("identical", [apply_subst w sigma; r; rp])
  | Predicate("ridentical", [w; r; rp]) ->
      Predicate("ridentical", [apply_subst w sigma; r; rp])
  | _ -> invalid_arg("apply_subst_msg_atom")
;;

let apply_subst_msg_st (head, body) sigma =
  (apply_subst_msg_atom sigma head,
   trmap (fun x -> apply_subst_msg_atom sigma x) body)
;;

(** {2 Compute knows statements from a trace} *)

(** Core statements without variant computations *)
let trace_equationalize (head, body) rules sigmas=
  let newatom sigma = function
    | (Predicate(x, [y; z; t])) ->
       Predicate(x, [apply_subst y sigma; z; apply_subst t sigma])
    | _ -> invalid_arg("newatom") in
  let newhead sigma = match head with
    | Predicate("knows", [w; r; t]) ->
       Predicate("knows", [apply_subst w sigma; r; apply_subst t sigma])
    | Predicate("reach", [w]) -> Predicate("reach", [apply_subst w sigma])
    | _ -> invalid_arg("wrong head") in
  let newclause sigma =
    (newhead sigma, trmap (fun x -> newatom sigma x) body) in
  trmap newclause sigmas
;;


let rec trace_statements_h oc tr rules substitutions body world clauses =
  match tr with
    | NullTrace -> List.rev clauses
    | Trace(Output(ch, t), remaining_trace) ->
	let next_world = worldadd world (Fun("!out!", [Fun(ch, [])])) in
	let next_head = Predicate("knows",
	       [worldreplempty next_world (Var(fresh_variable ()));
		Fun(current_parameter oc, []);
		t]) in
	let new_clause = (next_head, body) in
	let new_reach = (Predicate("reach", [next_world]), body) in
	trace_statements_h (oc + 1) remaining_trace rules substitutions body
	 next_world (List.concat [
		(trace_equationalize new_clause rules substitutions);
		(trace_equationalize new_reach rules substitutions);
		clauses])
    | Trace(Input(ch, v), remaining_trace) ->
	let next_world = worldadd world (Fun("!in!", [Fun(ch, []); Var(v)])) in
	let premisse = Predicate("knows", [world;
				     Var(fresh_variable ());
				     Var(v)]) in
	let next_body = (List.append body [premisse]) in
	let new_reach = (Predicate(
			    "reach",
			    [next_world]),
			  next_body)  in
	trace_statements_h oc remaining_trace rules substitutions next_body
	  next_world (List.concat [ trace_equationalize new_reach rules substitutions; clauses])
    | Trace(Test(s, t), remaining_trace) ->
    	let next_world = worldadd world (Fun("!test!", [])) in
    	let next_substitutions = List.concat (List.map 
		(fun sub ->
			List.map (fun sb -> compose sub sb) 
			(R.unifiers (apply_subst s sub) (apply_subst t sub) rules)) substitutions) in
	let new_reach = (Predicate(
			    "reach",
			    [next_world]),
			  body)  in
	trace_statements_h oc remaining_trace rules next_substitutions body
	  next_world ((trace_equationalize new_reach rules next_substitutions) @ clauses)
    | Trace(Guess(g), remaining_trace) ->
        let next_world = worldadd world (Fun("!guess!", [g])) in
        let next_head = Predicate("knows",
                                  [worldreplempty next_world (Var(fresh_variable ()));
                                   Fun(current_parameter oc, []);
                                      g]) in
        let new_clause = (next_head, body) in
        let new_reach = (Predicate("reach", [next_world]), body) in
          trace_statements_h (oc + 1) remaining_trace rules substitutions body
            next_world (List.concat [
              (trace_equationalize new_clause rules substitutions);
              (trace_equationalize new_reach rules substitutions);
              clauses])
    | Trace(Event, remaining_trace) ->
        let next_world = worldadd world (Fun("!event!", [])) in
        let new_reach = (Predicate("reach", [next_world]), body) in
          trace_statements_h (oc + 1) remaining_trace rules substitutions body
            next_world (List.concat [ (trace_equationalize new_reach rules substitutions);
              clauses])

;;


let trace_variantize (head, body) rules =
  match head with
    | Predicate("knows", [world; recipe; t]) ->
	let v = R.variants t rules in
	let new_clause (_, sigma) =
          Horn.new_clause
            (normalize_msg_st (apply_subst_msg_st (head, body) sigma) rules)
	in
	trmap new_clause v
    | Predicate("reach", [w]) ->
	let v = R.variants w rules in
	let newhead sigma = Predicate("reach",
				[R.normalize (apply_subst w sigma) rules]) in
	let newbody sigma = trmap
	  (function
	     | Predicate("knows", [x; y; z]) ->
		 Predicate("knows", [R.normalize (apply_subst x sigma) rules;
				     y;
				     R.normalize (apply_subst z sigma) rules])
	     | _ -> invalid_arg("reach_variantize")) body in
	trmap (fun (_, sigma) -> Horn.new_clause (newhead sigma, newbody sigma)) v
    | _ -> invalid_arg("variantize")
;;



let trace_statements tr rules =
  let kstatements = trace_statements_h 0 tr rules [[]] [] (Fun("empty", [])) [] in
    List.concat
      (List.map
         (fun x -> trace_variantize x rules)
         (kstatements))
;;

let guess_context_statements symbol arity rules =
  let w = Var(fresh_variable ()) in
  let vYs = trmap fresh_variable (create_list () arity) in
  let vZs = trmap fresh_variable (create_list () arity) in
  let add_knows x y = Predicate("knows+", [w; x; y]) in
  let box_vars names = trmap (function x -> Var(x)) names in
  let body sigma = List.map2
    (add_knows)
    (box_vars vYs)
    (trmap (function x -> apply_subst x sigma) (box_vars vZs))
  in
  let t = Fun(symbol, box_vars vZs) in
  let v = R.variants t rules in
    trmap (function (t',sigma) ->
        new_clause
          (Predicate("knows+",
                     [w;
                      Fun(symbol, box_vars vYs);
                      t'
                     ]),
           body sigma)) 
      v

(** Compute the part of seed statements that comes from the theory. *)
let context_statements symbol arity rules =
  let w = Var(fresh_variable ()) in
  let vYs = trmap fresh_variable (create_list () arity) in
  let vZs = trmap fresh_variable (create_list () arity) in
  let add_knows x y = Predicate("knows", [w; x; y]) in
  let box_vars names = trmap (function x -> Var(x)) names in
  let body sigma = List.map2
    (add_knows)
    (box_vars vYs)
    (trmap (function x -> apply_subst x sigma) (box_vars vZs))
  in
  if Theory.xor && symbol = "plus" then
    (* World variable *)
    let w = Var "X" in
    (* Recipe variables, marked or not *)
    let r1 = Var "X1" in
    let r2 = Var "X2" in
    let p1 = Var "Q1" in
    let p2 = Var "Q2" in
    (* Message variables *)
    let x1 = Var "X11" in
    let x2 = Var "X12" in
    let x3 = Var "X13" in
    (* Syntactic sugar *)
    let (+) a b = Fun("plus",[a;b]) in
    let knows r x = Predicate("knows",[w;r;x]) in
    let (<=) h t = Horn.new_clause (h,t) in
    let (<==) h t = Horn.new_clause ~vip:true (h,t) in
      (* Kinit statements for xor *)
      [ knows (r1+r2) (x1+x2)
          <= [ knows r1 x1 ; knows r2 x2 ] ;
        knows (p1+p2) x1
          <== [ knows p1 (x1+x2) ; knows p2 x2 ] ;
        knows (p1+p2) (x1+x2)
          <== [ knows p1 (x1+x3) ; knows p2 (x2+x3) ] ]
  else
  let t = Fun(symbol, box_vars vZs) in
  let v = R.variants t rules in
    trmap
    (function (t',sigma) ->
        new_clause
          (Predicate("knows",
                     [w;
                      Fun(symbol, box_vars vYs);
                      t'
                     ]),
           body sigma))
    v

(** Compute everything *)
let seed_statements trace rew =
  let context_clauses =
    List.concat
      (List.map
         (fun (f,a) ->
            context_statements f a rew)
         (List.sort (fun (_,a) (_,a') -> compare a a') Theory.fsymbols))
  in
  let trace_clauses =
    trace_statements trace rew
  in
    List.concat [context_clauses; trace_clauses]


let guess_seed_statements trace rew =
  let classic_seed = seed_statements trace rew in
  let guess_context_clauses =
    List.concat
      (List.map
         (fun (f,a) ->
            guess_context_statements f a rew)
         (List.sort (fun (_,a) (_,a') -> compare a a') ((Theory.fsymbols))))
  in 
  let guess_weak_names_clauses = 
      List.map (fun x -> Horn.new_clause (Predicate("knows+", [Var(fresh_variable ()); Fun(Printf.sprintf "w%s" (x),[]); Fun(x,[])]), [])) (Theory.weaknames) 
  in
    List.concat [classic_seed; guess_context_clauses; guess_weak_names_clauses]
