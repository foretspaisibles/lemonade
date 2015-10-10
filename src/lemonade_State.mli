(* Lemonade_State -- The classic state monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** State monad. *)

(** The input signature of the functor [Lemonade_State.Make]. *)
module type StateType =
sig
  type t
  (** The type of states carried by the monad. *)
end


(** The output signature of the functor [Lemonade_State.Make]. *)
module type S =
sig
  type state
  (** The type of states carried by the monad. *)

  type (+'a) t
  (** The type of computations of an ['a], carrying a state of type
      [state]. *)

  include Lemonade_Type.S
    with type 'a t := 'a t

  val state : (state -> (state * 'a)) -> 'a t
  (** Embed a simple state action in the monad. *)

  val read : state t
  (** Return the state from the internals of the monad. *)

  val write : state -> unit t
  (** Replace the state inside the monad. *)

  val modify : (state -> state) -> unit t
  (** Maps an old state to a new state inside a state monad. The old
      state is discarded. *)

  val run : 'a t -> state -> (state * 'a)
  (** Unwrap a computation in the state monad as a function. (The
      converse of state.). *)

  val eval : 'a t -> state -> 'a
  (** Evaluate a state computation with the given initial state and
      return the final value, discarding the final state. *)

  val exec : 'a t -> state -> state
  (** Evaluate a state computation with the given initial state and
      return the final state, discarding the final value. *)

  val maps : (state *'a -> state * 'b) -> 'a t -> 'b t
  (** Map both the return value and final state of a computation using
      the given function.

      {b Note:} The derivation of the notation [maps] is similar to
      the derivation of the notation [mapi] from the standard
      library.*)

  val with_state : (state -> state) -> 'a t -> 'a t
  (** [with_state f m] is the monad executing action [m] on a state
      modified by applying [f]. *)
end

(** Functor building an implementation of the [State] monad. *)
module Make(State:StateType) :
sig
  include S
    with type state = State.t

  (** The state monad transformer. *)
  module T(M:Lemonade_Type.S): sig
    type state = State.t
    include Lemonade_Type.S

    val state : (state -> (state * 'a)) -> 'a t
    (** Embed a simple state action in the monad. *)

    val read : state t
    (** Return the state from the internals of the monad. *)

    val write : state -> unit t
    (** Replace the state inside the monad. *)

    val modify : (state -> state) -> unit t
    (** Maps an old state to a new state inside a state monad. The old
        state is discarded. *)

    val run : 'a t -> state -> (state * 'a) M.t
    (** Unwrap a computation in the state monad as a function. (The
        converse of state.). *)

    val eval : 'a t -> state -> 'a M.t
    (** Evaluate a state computation with the given initial state and
        return the final value, discarding the final state. *)

    val exec : 'a t -> state -> state M.t
    (** Evaluate a state computation with the given initial state and
        return the final state, discarding the final value. *)

    val maps : (state *'a -> state * 'b) -> 'a t -> 'b t
    (** Map both the return value and final state of a computation using
        the given function.

        {b Note:} The derivation of the notation [maps] is similar to
        the derivation of the notation [mapi] from the standard
        library.*)

    val with_state : (state -> state) -> 'a t -> 'a t
    (** [with_state f m] is the monad executing action [m] on a state
        modified by applying [f]. *)

    val lift : 'a M.t -> 'a t
    (** Embed the monad [M] in the associated state monad. *)
  end
end
