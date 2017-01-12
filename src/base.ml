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

module type T = sig

  type t
  type elt

  val create : unit -> t
  val add : elt -> t -> unit

  val next_not_solved : t -> (elt*elt) option
  val next_solved : t -> (elt*elt) option
  val next_ri_solved : t -> (elt*elt list) option

  module S : Set.S with type elt = elt

  val solved : t -> S.t
  val not_solved : t -> S.t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_solved : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_ri_size : int -> (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_r_size : int -> (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val only_solved : t -> elt list

end

module type O = sig
  type t
  val compare : t -> t -> int
  val is_solved : t -> bool
  val is_reach_st : t -> bool
  val is_ridentical_st : t -> bool
  val is_ext_ridentical_st : t -> bool
  val is_world_size_st : t -> int -> bool
  val world_length : t -> int 
  val count_guess_w : t -> int 
  val is_ext_equation_st : t -> bool
  val is_guess_in_term_st : t -> bool 
  val show_statement : t -> string 
end

module Make (M:O) : T with type elt = M.t = struct

  module S = Set.Make(M)

  type elt = M.t

  type t = {
    mutable solved : S.t ;
    mutable not_solved : S.t ;
    s_todo : (elt*elt) Queue.t ;
    ns_todo : (elt*elt) Queue.t ;
    rs_todo : (elt * elt list) Queue.t 
  }

  let create () = {
    solved = S.empty ; not_solved = S.empty ;
    s_todo = Queue.create () ; ns_todo = Queue.create () ; rs_todo = Queue.create()
  }

  let new_pair queue pair = Queue.push pair queue
  let new_ri_clauses queue (ric : (elt * (elt list)))  = Queue.push ric queue

  let next_not_solved kb =
    try Some (Queue.pop kb.ns_todo) with Queue.Empty -> None

  let next_solved kb =
    try Some (Queue.pop kb.s_todo) with Queue.Empty -> None

  let next_ri_solved kb =
    try Some (Queue.pop kb.rs_todo) with Queue.Empty -> None

  let is_max_reach x kb = 
      not(S.exists (fun y -> (M.world_length y > M.world_length x)) kb)

  let get_ext_identicals_step step kb : M.t list =
    let ids = List.filter (fun x -> 
                             if M.is_ext_equation_st x then 
                               (
                                if ((M.count_guess_w x) <= step) then 
                                  (
                                    true
                                  )
                                else false
                               )
                             else false
    ) kb in 
    let idsselected = List.filter (M.is_guess_in_term_st) ids in
        idsselected

  let rec gen_joint_ext_identicals (count_guess : int) (step : int) (kb : M.t
  list) : M.t list list = 
      if step >= count_guess then []
      else
          let ids = get_ext_identicals_step step kb in
              begin
          let rec aux_concat (l : M.t list list) (ll : M.t list list) : M.t list list =
              begin
             match l with 
             | [] -> [] 
             | x::xs -> 
                     if ll = [] then x::aux_concat xs ll else
                     List.append (List.map (fun y -> 
                     List.append x y) ll) (aux_concat xs ll) 
             end 
          in
          aux_concat (List.map (fun x -> [x]) ids) (gen_joint_ext_identicals count_guess (step + 1) kb)
              end

  let rec add x kb =
    if M.is_solved x then begin
      kb.solved <- S.add x kb.solved ;
      S.iter (fun y -> new_pair kb.s_todo (x,y)) kb.solved ;
      S.iter (fun y -> new_pair kb.ns_todo (x,y)) kb.not_solved;

      if M.is_ext_equation_st x then 
        (
        S.iter (fun y -> if M.is_reach_st y then (add y kb) else ()) kb.solved;
        )
      else
      if M.is_reach_st x && (is_max_reach x kb.solved) then
          (
            let ji = (gen_joint_ext_identicals (M.count_guess_w x) 0 (S.elements kb.solved)) in 
              List.iter (fun y -> (if y != [] then new_ri_clauses kb.rs_todo (x,y)))  ji
          )
 
    end else begin
      kb.not_solved <- S.add x kb.not_solved ;
      S.iter (fun y -> new_pair kb.ns_todo (y,x)) kb.solved
    end

  let fold f kb x =
    S.fold f kb.not_solved (S.fold f kb.solved x)

  let fold_solved f kb x =
    S.fold f kb.solved x

  let fold_ri_size trace_size f kb x =
      S.fold f (S.filter (fun y -> (M.is_ext_ridentical_st y) && (M.is_world_size_st y trace_size)) kb.solved) x

  let fold_r_size trace_size f kb x =
      S.fold f (S.filter (fun y -> (M.is_reach_st y) && (M.is_world_size_st y trace_size) ) kb.solved) x

  let only_solved kb = S.elements kb.solved

  let solved kb = kb.solved
  let not_solved kb = kb.not_solved

end
