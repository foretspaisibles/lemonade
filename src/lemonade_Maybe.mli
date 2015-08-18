(* Lemonade_Maybe -- The classic maybe monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** The classic maybe monad. *)

type (+'a) t =
  'a option

val pp_print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
(** A generic printer for option values. *)

val is_some : 'a option -> bool
(** Predicate recognising options containing a value. *)

val is_none : 'a option -> bool
(** Predicate recognising empty options. *)

val find : 'a option -> 'a
(** Return the value held by an option or raise [Not_found] if the
    option is empty. *)

val default : 'a -> 'a option -> 'a
(** [default val opt] return the content of [opt] or [val] if it is empty. *)

val of_list : 'a list -> 'a option
(** Return an option containing the first element of the list if any,
    or empty if the list is empty. *)

val to_list : 'a option -> 'a list
(** Return a list containing the element held by the option if any, ot
    the empty list ifthe option is empty. *)

val filter : 'a option list -> 'a list
(** [filter lst] is the list of values held by the options in [lst]. *)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
(** [filter_map f lst] is the list deduced from [lst] by applying [f]
    to element of the list and retaining the values held by the list. *)

include Lemonade_Type.S
    with type 'a t := 'a t

(** The maybe monad transformer. *)
module T(M:Lemonade_Type.S) :
sig
  include Lemonade_Type.S
    with type 'a t = 'a option M.t

  val lift : 'a M.t -> 'a t
end
