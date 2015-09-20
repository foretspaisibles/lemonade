(* Lemonade_State -- The classic state monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

module type StateType =
sig
  type t
end

module type S =
sig
  type state
  type (+'a) t
  include Lemonade_Type.S
    with type 'a t := 'a t
  val state : (state -> (state * 'a)) -> 'a t
  val read : state t
  val write : state -> unit t
  val modify : (state -> state) -> unit t
  val run : 'a t -> state -> (state * 'a)
  val eval : 'a t -> state -> 'a
  val exec : 'a t -> state -> state
  val maps : (state * 'a -> state * 'b) -> 'a t -> 'b t
  val with_state : (state -> state) -> 'a t -> 'a t
end

module Make(State:StateType) =
struct
  type state = State.t
  module Basis =
  struct
    type 'a t =
      state -> (state * 'a)

    let return x =
      fun s  -> (s, x)

    let bind m f =
      fun state -> let state',x = m state in f x state'
  end

  module MethodsMonad =
    Mixture_Monad.Make(Basis)

  include Basis
  include MethodsMonad

  let state m =
    m

  let read =
    fun state -> (state, state)

  let write x =
    fun _ -> (x,())

  let modify f =
    bind read (fun s -> write (f s))

  let run m state =
    m state

  let eval m state =
    snd (m state)

  let exec m state =
    fst (m state)

  let maps f m =
    fun state -> f(m state)

  let with_state f m =
    fun state -> m (f state)
end
