(* Lemonade_Type -- The classic type monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** The monad definition. *)

(** The general signature of a monad. *)
module type S =
sig
  type (+'a) t
  (** The type of monads. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind m f] bind [f] to the monad [m]. *)

  val return : 'a -> 'a t
  (** [return a] embed the value [a] in the monad. *)

  val apply : ('a -> 'b) t -> 'a t -> 'b t
  (** [apply f] sequence computations and combine their results with [f]. *)

  val join : ('a t) t -> 'a t
  (** [join mm] bind [mm] to the identity, reducing the monad. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f] is the natural tranformation between monads,
      induced by [f]. *)

  val bind2 : 'a t -> 'b t -> ('a -> 'b -> 'c t) -> 'c t
  (** Similar to [bind], but works on two arguments. *)

  val bind3 : 'a t -> 'b t -> 'c t -> ('a -> 'b -> 'c -> 'd t) -> 'd t
  (** Similar to [bind], but works on three arguments. *)

  val bind4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a -> 'b -> 'c -> 'd -> 'e t) -> 'e t
  (** Similar to [bind], but works on four arguments. *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** A version of [map] for binary functions. *)

  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** A version of [map] for ternary functions. *)

  val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  (** A version of [map] for quaternary functions. *)

  val dist : 'a t list -> 'a list t
  (** The applicative distributor for list, that is, the natural
      transformation of a list of computations in the computation of a
      list. *)

  val ignore : 'a t -> unit t
  (** Monadic ignore. *)

  val filter : ('a -> bool t) -> 'a t list -> 'a list t
  (** Filter a list of computations with the given monadic predicate. *)

  val only_if : bool -> unit t -> unit t
  (** [only_if flag m] returns [m] only if [flag] is [true]. *)

  val unless : bool -> unit t -> unit t
  (** [unless flag m] returns [m] only if [flag] is [false]. *)

  module Infix : sig
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
    (** A shorthand for [apply], the sequential application. *)

    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    (** A shorthand for [map]. *)

    val ( <* ) : 'a t -> 'b t -> 'a t
    (** Sequence actions, discarding the value of the first
        argument. *)

    val ( >* ) : 'a t -> 'b t -> 'b t
    (** Sequence actions, discarding the value of the second
        argument. *)

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    (** [ m >>= f] is equivalent to [bind m f]. *)

    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
    (** A composable shorthand for [map]. *)

    val ( >> ) : 'a t -> (unit -> 'b t) -> 'b t
    (** [m >> f] binds [m] to [f], a context function. *)

    val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
    (** [g >=> f] is the (contravariant) monadic composition of [g]
        followed by [f]. *)

    val ( <=< ) : ('b -> 'c t) -> ('a -> 'b t) -> ('a -> 'c t)
    (** [f <=< g] is the monadic composition of [g] followed by [f]. *)
  end
end
