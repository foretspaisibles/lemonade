(* Lemonade_Success -- The classic success monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Success monad.

    The success monad is a monad in which one can run computations
    throwing errors. This is implemented as a functor parametrised by the
    error type.

    Note that the [throw] and [catch] operations defined below are
    totally independant of exceptions. *)

(** The input signature of the functor [Lemonade_Success.Make]. *)
module type ErrorType =
sig
  type t
  (** The type of error messages. *)
end

(** The output signature of the functor [Lemonade_Success.Make]. *)
module type S =
sig
  type error
  (** The type of error messages. *)

  (** The outcome of computations throwing errors. *)
  type (+'a) outcome =
    | Success of 'a
    | Error of error

  include Lemonade_Type.S

  val error : error -> 'a t
  (** Fail with the given error. *)

  val recover : 'a t -> (error -> 'a t) -> 'a t
  (** [recover m handler] is a monad containing the same value as [m]
      and thrown errors are interepreted by the [handler]. *)

  val run : 'a t -> 'a outcome
  (** Perform a computation with errors. *)
end

(** Functor building an implementation of the [Success] monad. *)
module Make(Error:ErrorType):
sig
  include S
    with type error = Error.t

  (** The success monad transformer. *)
  module T(M:Lemonade_Type.S): sig
    include Lemonade_Type.S
      with type 'a t = 'a t M.t

    val lift : 'a M.t -> 'a t
  end
end
