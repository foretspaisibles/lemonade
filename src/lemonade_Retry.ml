(* Lemonade_Retry -- The retry monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

module type RetryType =
sig
  type error
  type tag
  type environment
  val policy : (tag * 'a) list -> environment -> (tag * 'a) list
end

module type S =
sig
  type error
  type tag
  type environment
  type (+'a) outcome =
    | Success of 'a
    | Error of error

  include Mixture_Monad.S

  val throw : error -> 'a t
  val catch : 'a t -> (error -> 'a t) -> 'a t
  val retry : tag -> (environment -> 'a t) -> 'a t -> 'a t
  val run : environment -> 'a t -> 'a outcome
end

module Make(Retry:RetryType) =
struct

  type error =
    Retry.error

  type tag =
    Retry.tag

  type environment =
    Retry.environment

  type (+'a) outcome =
    | Success of 'a
    | Error of error


  module Basis =
  struct
    type (+'a) t =
      | SUCCESS of 'a
      | ERROR of ((tag *(environment -> 'a t)) list) * error

    let return x =
      SUCCESS(x)

    let rec bind m f =
      match m with
      | SUCCESS(x) -> f x
      | ERROR(plan, err) -> ERROR(List.map (bind_strategy f) plan, err)
    and bind_strategy f (tag, strategy) =
      (tag, fun env -> bind (strategy env) f)
  end

  module MethodsMonad =
    Mixture_Monad.Make(Basis)

  include Basis
  include MethodsMonad

  let throw err =
    ERROR([], err)

  let catch m handler =
    match m with
    | SUCCESS(x) -> SUCCESS(x)
    | ERROR(_, err) -> handler err

  let retry tag f = function
    | ERROR(plan, err) -> ERROR((tag, f) :: plan, err)
    | whatever -> whatever

  let rec run env = function
    | SUCCESS(x) -> Success(x)
    | ERROR([], err) -> Error(err)
    | ERROR(plan, err) -> _run env err (Retry.policy plan env)
  and _run env err = function
    | [] -> Error(err)
    | (_,f) :: tl ->
        match run env (f env) with
        | Error(_) -> run env (ERROR(tl, err))
        | whatever -> whatever
end
