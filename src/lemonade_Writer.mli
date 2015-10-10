(* Lemonade_Writer -- The classic writer monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Writer monad.

    Values in a {i Writer} monad represent a computation accumulating
    some output. *)

(** The input signature of the functor [Lemonade_Writer.Make]. *)
module type OutputType =
sig
  type t
  (** The type of output to accumulate. *)

  val empty : unit -> t
  (** The empty output. *)

  val append : t -> t -> t
  (** Catenate two output. *)
end


(** The output signature of the functor [Lemonade_Writer.Make]. *)
module type S =
sig
  type output
  (** The type of output. *)

  include Lemonade_Type.S

  val writer : output * 'a -> 'a t
  (** Embed a simple writer action. *)

  val tell : output -> unit t
  (** [tell w] is an action that produces the output [w]. *)

  val listen : 'a t -> (output * 'a) t
  (** [listen m] is an action that executes the action m and adds its
      output to the value of the computation. *)

  val pass : ((output -> output) * 'a) t -> 'a t
  (** [pass m] is an action that executes the action [m], which
      returns a value and a function, and returns the value, applying
      the function to the output. *)

  val listens : (output -> 'b) -> 'a t -> ('a * 'b) t
  (** [listens f m] is an action that executes the action [m] and adds
      the result of applying [f] to its output to the value of the
      computation. *)

  val censor : (output -> output) -> 'a t -> 'a t
  (** [censor f m] is an action action that executes the computation
      [m] and filters its output with [f], leaving its return values
      unchanged. *)

  val run : 'a t -> output * 'a
  (** Execute a computation and examine its output and return value. *)

  val eval : 'a t -> 'a
  (** Execute a computation and examine its return value
      while discarding its output. *)

  val exec : 'a t -> output
  (** Execute a computation and examine its output while discarding
      its return value. *)
end

(** Functor building an implementation of the [Writer] monad. *)
module Make(Output:OutputType):
sig
  include S
    with type output = Output.t

  (** The writer monad transformer. *)
  module T(M:Lemonade_Type.S): sig
    type output = Output.t
    (** The type of consumed data. *)

    include Lemonade_Type.S

    val writer : output * 'a -> 'a t
    (** Embed a simple writer action. *)

    val tell : output -> unit t
    (** [tell w] is an action that produces the output [w]. *)

    val listen : 'a t -> (output * 'a) t
    (** [listen m] is an action that executes the action m and adds its
        output to the value of the computation. *)

    val pass : ((output -> output) * 'a) t -> 'a t
    (** [pass m] is an action that executes the action [m], which
        returns a value and a function, and returns the value, applying
        the function to the output. *)

    val listens : (output -> 'b) -> 'a t -> ('a * 'b) t
    (** [listens f m] is an action that executes the action [m] and adds
        the result of applying [f] to its output to the value of the
        computation. *)

    val censor : (output -> output) -> 'a t -> 'a t
    (** [censor f m] is an action action that executes the computation
        [m] and filters its output with [f], leaving its return values
        unchanged. *)

    val run : 'a t -> (output * 'a) M.t
    (** Execute a computation and examine its output and return value. *)

    val eval : 'a t -> 'a M.t
    (** Execute a computation and examine its return value
        while discarding its output. *)

    val exec : 'a t -> output M.t
    (** Execute a computation and examine its output while discarding
        its return value. *)

    val lift : 'a M.t -> 'a t
    (** Add an environment to a monad of type ['a M.t]. *)
  end
end
