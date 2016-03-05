(* Lemonade, the sparkling monad library

Copyright © 2016 Michael Grünewald
Copyright © 2014 Gabriel Radanne, Peter Zotov.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, with linking exceptions;
either version 2.1 of the License, or (at your option) any later
version. See COPYING file for details.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA. *)

open Printf
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Ast_convenience

module Maybe =
  Lemonade_Maybe

(* Extend String operations. *)
module String = struct
  include String

  let is_prefix s t =
    length t >= length s && sub t 0 (length s) = s

  let suffix offset t =
    sub t offset (length t - offset)

  let split delim t =
    let rec loop i =
      try
        let j = index_from t i delim in
        sub t i (j - i) :: loop (j + 1)
      with
        Not_found -> [sub t i (length t - i)]
    in
    loop 0
end


let extension_node_name =
  "lemonade"

let extension_node_dot =
  extension_node_name ^ "."

let extension_node_prefix =
  "__ppx_" ^ extension_node_name ^ "__"

let newname i =
  sprintf "%s%d" extension_node_prefix i

let with_loc f { txt ; loc } =
  (f txt) [@metaloc loc]

let is_catchall case =
  let rec is_catchall_pattern p =
    match p.ppat_desc with
    | Ppat_any
    | Ppat_var _ -> true
    | Ppat_alias (p, _) -> is_catchall_pattern p
    | _ -> false
  in
  case.pc_guard = None && is_catchall_pattern case.pc_lhs

let maybe_add_catchall_case cases =
  if not(List.exists is_catchall cases) then
    cases @ [Exp.case [%pat? exn] [%expr raise exn]]
  else
    cases

let lid_from_extension_id name =
  let open Longident in
  let names = String.(split '.' name) in
  List.fold_left
    (fun acc name -> Ldot (acc, name))
    (Lident (List.hd names))
    (List.tl names)

let lemonade_operators ?(loc = Location.none) = function
  | Some("") ->
      raise(Location.Error
              (Location.errorf "Missing lemonade argument for monadic bind"))
  | Some(name) ->
      [%expr [%e Exp.ident (lid (name ^ ".bind"))]],
      [%expr [%e Exp.ident (lid (name ^ ".return"))]]
  | None ->
      [%expr bind],
      [%expr return]

let lemonade_extension ?loc txt =
  Maybe.map (lemonade_operators ?loc)
    (if String.is_prefix extension_node_dot txt then
       Some(Some(String.(suffix (length extension_node_dot) txt)))
     else if String.is_prefix extension_node_name txt then
       Some(None)
     else
       None)

(** [p = x] ≡ [__ppx_lemonade_$i = x] *)
let lemonade_bindings lst =
  let loop i binding =
    { binding with
      pvb_pat = (pvar @@ newname i)
          [@metaloc binding.pvb_expr.pexp_loc] }
  in
  List.mapi loop lst


(** [p = x] and e ≡ [bind __ppx_lwt_$i (fun p -> e)] *)
let lemonade_binds (bind, return) exploc lst exp =
  let rec loop i bindings =
    match bindings with
    | [] -> exp
    | binding :: t ->
      let name =
        (evar @@ newname i)
          [@metaloc binding.pvb_expr.pexp_loc]
      in
      let f =
        [%expr (fun [%p binding.pvb_pat] -> [%e loop (i+1) t])]
          [@metaloc binding.pvb_loc]
      in
      let new_exp =
        [%expr [%e bind] [%e name] [%e f]]
          [@metaloc exploc]
      in
      { new_exp with pexp_attributes = binding.pvb_attributes }
  in
  loop 0 lst

let lemonade_expression mapper ((bind, return) as monad) exp attributes =
  default_loc := exp.pexp_loc;
  let pexp_attributes = attributes @ exp.pexp_attributes in
  match exp.pexp_desc with

  (** [let%lemonade $p$ = $e$ in $e'$] ≡ [bind $e$ (fun $p$ -> $e'$)] *)
  | Pexp_let (Nonrecursive, vbl, expression) ->
      let new_exp =
        Exp.let_
          Nonrecursive
          (lemonade_bindings vbl)
          (lemonade_binds monad exp.pexp_loc vbl expression)
      in
      mapper.expr mapper { new_exp with pexp_attributes }


  (** [match%lemonade $e$ with $c$] ≡ [bind $e$ (function $c$)]
      [match%lemonade $e$ with exception $x$ | $c$] ≡ [try_bind (fun () -> $e$) (function $c$) (function $x$)] *)
  | Pexp_match (e, cases) ->
      let exns, cases =
        List.partition
          begin function
            | { pc_lhs = [%pat? exception [%p? _]] } -> true
            | _ -> false
          end
          cases
    in
    let exns =
      List.map
        begin function
          | { pc_lhs = [%pat? exception [%p? pat]]} as case -> { case with pc_lhs = pat }
          | _ -> assert false
        end
        exns
    in
    let exns = maybe_add_catchall_case exns in
    let new_exp =
      match exns with
      | [] -> [%expr bind [%e e] [%e Exp.function_ cases]]
      | _  -> [%expr
                match [%e e] with
                | exception exn -> [%e Exp.function_ exns] exn
                | m -> [%e bind] m [%e Exp.function_ cases]]
    in
    mapper.expr mapper { new_exp with pexp_attributes }

  (** [while%lemonade $cond$ do $body$ done] ≡
      [let rec __ppx_lwt_loop () =
         if $cond$ then Lwt.bind $body$ __ppx_lwt_loop
         else Lwt.return ()
       in __ppx_lwt_loop]
   *)
  | Pexp_while (cond, body) ->
     let new_exp =
       [%expr
         let rec __ppx_lemonade_loop () =
           if [%e cond] then [%e bind] [%e body] __ppx_lemonade_loop
           else [%e return] ()
         in
         __ppx_lemonade_loop ()]
     in
     mapper.expr mapper { new_exp with pexp_attributes }

   (** [for%lemonade $p$ = $start$ (to|downto) $end$ do $body$ done] ≡
       [let __ppx_lwt_bound = $end$ in
       let rec __ppx_lwt_loop $p$ =
         if $p$ COMP __ppx_lwt_bound then Lwt.return ()
         else Lwt.bind $body$ (fun () -> __ppx_lwt_loop ($p$ OP 1))
       in __ppx_lwt_loop $start$] *)
   | Pexp_for (({ ppat_desc = Ppat_var p_var} as p), start, bound, dir, body) ->
     let comp, binop = match dir with
       | Upto -> evar ">", evar "+"
       | Downto -> evar "<", evar "-"
     in
     let q = with_loc evar p_var in
     let exp_bound = [%expr __ppx_lemonade_bound] [@metaloc bound.pexp_loc] in
     let pat_bound = [%pat? __ppx_lemonade_bound] [@metaloc bound.pexp_loc] in
     let new_exp =
       [%expr
        let [%p pat_bound] : int = [%e bound] in
        let rec __ppx_lemonade_loop [%p p] =
          if [%e comp] [%e q] [%e exp_bound] then
            [%e return] ()
          else
            [%e bind] [%e body] (fun () -> __ppx_lemonade_loop ([%e binop] [%e q] 1))
        in
        __ppx_lemonade_loop [%e start]
       ]
     in
     mapper.expr mapper { new_exp with pexp_attributes }


  (** [try%lemonade $e$ with $c$] ≡
      [catch (fun () -> $e$) (function $c$)]
  *)
   | Pexp_try (expr, cases) ->
     let cases = maybe_add_catchall_case cases in
     let new_exp =
       [%expr try [%e expr] () with exn -> [%e Exp.function_ cases] exn]
     in
     mapper.expr mapper { new_exp with pexp_attributes }

  (** [if%lemonade $c$ then $e1$ else $e2$] ≡
      [match%lemonade $c$ with true -> $e1$ | false -> $e2$]
      [if%lemonade $c$ then $e1$] ≡
      [match%lemonade $c$ with true -> $e1$ | false -> Lwt.return_unit]
  *)
   | Pexp_ifthenelse (cond, e1, e2) ->
     let e2 =
       match e2 with
       | Some e -> e
       | None -> [%expr [%e return] ()]
     in
     let cases = [
       Exp.case [%pat? true] e1 ;
       Exp.case [%pat? false] e2 ;
     ]
     in
     let new_exp = [%expr [%e bind] [%e cond] [%e Exp.function_ cases]] in
     mapper.expr mapper { new_exp with pexp_attributes }

  | _ -> mapper.expr mapper exp

let lemonade_mapper argv =
  let open Ast_mapper in
  let super = default_mapper in
  let expr this e =
    match e with
    | { pexp_desc = Pexp_extension ({ txt = id; loc }, PStr [{ pstr_desc = Pstr_eval (exp, attr) }]) } ->
        (match lemonade_extension ~loc id with
         | Some(monad) -> lemonade_expression this monad exp attr
         | None -> super.expr this e)
    | _ -> super.expr this e
  in
  { default_mapper with expr }

let () = Ast_mapper.run_main lemonade_mapper
