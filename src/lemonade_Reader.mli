(* Lemonade_Reader -- The classic reader monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Reader monad.

    Values in a {i Reader} monad or {i Environment monad} represent a
    computation, which can read values from a shared environment, pass
    values from function to function, and execute sub-computations in
    a modified environment. Using a {i Reader} monad for such
    computations is often clearer and easier than using a {i State}
    monad. *)

(** The input signature of the functor [Lemonade_Reader.Make]. *)
module type EnvironmentType =
sig
  type t
  (** The type of data consumed in a reader monad. *)
end

(** The output signature of the functor [Lemonade_Reader.Make]. *)
module type S =
sig
  type environment
  (** The type of consumed data. *)

  include Lemonade_Type.S

  val read : environment t
  (** Access the current environment. *)

  val run : environment -> 'a t -> 'a
  (** Perform a computation in the given environment errors. *)

  val local : (environment -> environment) -> 'a t -> 'a t
  (** Execute a computation in a modified environment. *)

  val access : (environment -> 'a) -> 'a t
  (** Access to a component of the current environment. *)

end

(** Functor building an implementation of the [Success] monad. *)
module Make(Environment:EnvironmentType):
sig
  include S
    with type environment = Environment.t

  (** The success monad transformer. *)
  module T(M:Lemonade_Type.S): sig
    type environment = Environment.t
    (** The type of consumed data. *)

    include Lemonade_Type.S

    val read : environment t
    (** Access the current environment. *)

    val run : environment -> 'a t -> 'a M.t
    (** Perform a computation in the given environment errors. *)

    val local : (environment -> environment) -> 'a t -> 'a t
    (** Execute a computation in a modified environment. *)

    val access : (environment -> 'a) -> 'a t
    (** Access to a component of the current environment. *)

    val lift : 'a M.t -> 'a t
    (** Add an environment to a monad of type ['a M.t]. *)
  end
end
