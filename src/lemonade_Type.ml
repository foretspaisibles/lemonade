(* Lemonade_Type -- The classic type monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

module type S =
sig
  type (+'a) t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val join : ('a t) t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val bind2 : 'a t -> 'b t -> ('a -> 'b -> 'c t) -> 'c t
  val bind3 : 'a t -> 'b t -> 'c t -> ('a -> 'b -> 'c -> 'd t) -> 'd t
  val bind4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a -> 'b -> 'c -> 'd -> 'e t) -> 'e t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  val dist : 'a t list -> 'a list t
  val ignore : 'a t -> unit t
  val filter : ('a -> bool t) -> 'a t list -> 'a list t
  val only_if : bool -> unit t -> unit t
  val unless : bool -> unit t -> unit t
  module Infix : sig
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val ( <* ) : 'a t -> 'b t -> 'a t
    val ( >* ) : 'a t -> 'b t -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >> ) : 'a t -> (unit -> 'b t) -> 'b t
    val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
    val ( <=< ) : ('b -> 'c t) -> ('a -> 'b t) -> ('a -> 'c t)
  end
end
