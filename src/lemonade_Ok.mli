(* Lemonade_Ok -- A variant of the success monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** A widely spread variant of the success monad.

It is mainly useful when working with Yojson. *)

type (+'a) t =
  [ `Error of string | `Ok of 'a ]
(** The type of monads computing a value of type ['a] or failing with
    an error message. *)

include Lemonade_Type.S
    with type 'a t := 'a t

val error : string -> 'a t
(** A computation failed with the given error message. *)

val errorf : ('a, unit, string, 'b t) format4 -> 'a
(** A computation failed with the given error message, formatted by sprintf. *)

val run : 'a t -> 'a
(** [run m] return the value computed by [m] if [m] succeeded or throw
    a [Failure] exception with the given message otherwise. *)

val pp_print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** A generic printer for monadic values. *)



(** The maybe monad transformer. *)
module T(M:Lemonade_Type.S) :
sig
  include Lemonade_Type.S
    with type 'a t = [ `Error of string | `Ok of 'a ] M.t

  val lift : 'a M.t -> 'a t
end
