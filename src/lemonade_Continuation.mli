(* Lemonade_Continuation -- The continuation monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Continuation monad.

    The Continuation monad represents computations in
    continuation-passing style (CPS). In continuation-passing style
    function result is not returned, but instead is passed to another
    function, received as a parameter (continuation). Computations are
    built up from sequences of nested continuations, terminated by a
    final continuation (often {i id}) which produces the final
    result. Since continuations are functions which represent the
    future of a computation, manipulation of the continuation
    functions can achieve complex manipulations of the future of the
    computation, such as interrupting a computation in the middle,
    aborting a portion of a computation, restarting a computation, and
    interleaving execution of computations.  The Continuation monad
    adapts CPS to the structure of a monad. *)


(** The input signature of the functor [Lemonade_Continuation.Make]. *)
module type FinalType =
sig
  type t
end


(** The output signature of the functor [Lemonade_Continuation.Make]. *)
module type S =
sig
  (** The final type. *)
  type final

  (** The type of continuations. *)
  type 'a t = ('a -> final) -> final

  include Lemonade_Type.S
    with type 'a t := 'a t

  val call_cc : (('a -> 'b t) -> 'a t) -> 'a t
  (** Call with current continuation. *)
end

(** Functor building an implementation of the [Success] monad. *)
module Make(Final:FinalType):
  S with type final = Final.t
