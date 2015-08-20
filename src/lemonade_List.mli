(* Lemonade_List -- The classic list monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** List monad. *)

type (+'a) t =
  'a list

include Lemonade_Type.S
    with type 'a t := 'a t

val pp_print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
(** A generic printer for list values. *)

(** The list monad transformer. *)
module T(M:Lemonade_Type.S) :
sig
  include Lemonade_Type.S
    with type 'a t = 'a list M.t

  val lift : 'a M.t -> 'a t
end
