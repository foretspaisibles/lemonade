(* Lemonade_Retry -- The retry monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Retry monad.

    The retry monad is a monad in which one can run computations
    throwing errors, which can be handled by retry strategies
    according to a policy. This is implemented as a functor
    parametrised by the error type and the policy.

    Note that the [throw] and [catch] operations defined below are
    totally independant of exceptions. *)

(** The input signature of the functor [Lemonade_Retry.Make]. *)
module type RetryType =
sig
  type error
  (** The type of error messages. *)

  type tag
  (** The type of tags identifying retry strategies. *)

  type environment
  (** The type of computation environment. *)

  val policy : (tag * 'a) list -> environment -> (tag * 'a) list
  (** The type of policies, used to select a retry strategy.

      The policy filters applyable retry strategies under a given
      environment. *)
end

(** The output signature of the functor [Lemonade_Retry.Make]. *)
module type S =
sig
  type error
  (** The type of error messages. *)

  type tag
  (** The type of tags identifying retry strategies. *)

  type environment
  (** The type of computation environment. *)

  (** The type of computations throwing errors. *)
  type (+'a) outcome =
    | Success of 'a
    | Error of error

  include Lemonade_Type.S

  val throw : error -> 'a t
  (** Throw the given error. *)

  val catch : 'a t -> (error -> 'a t) -> 'a t
  (** [catch m handler] is a monad containing the same value as [m]
      and thrown errors are interepreted by the [handler]. *)

  val retry : tag -> (environment -> 'a t) -> 'a t -> 'a t
  (** [retry tag strategy m] compute the same value as [m] having the
      chance let the retry policy use [strategy] on errors. *)

  val run : environment -> 'a t -> 'a outcome
  (** Run the given retryable computation. *)
end

(** Functor building an implementation of the [Retry] monad. *)
module Make(Retry:RetryType) : S
  with type error = Retry.error
   and type tag = Retry.tag
   and type environment = Retry.environment
