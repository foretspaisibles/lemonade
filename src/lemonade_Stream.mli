(* Lemonade_Stream -- Monadic streams

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2016 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Monadic streams. *)

exception Empty
(** Exception raised when trying to read from an empty stream. *)

(** The output signature of the functor [Lemonade_Stream.Make]. *)
module type S =
sig

  type 'a t
  (** The type of streams, holding values of type ['a]. *)

  type (+ 'a) monad
  (** The type of monadic computations, yielding a value of type ['a]. *)

  val from : (int -> 'a option monad) -> 'a t
  (** [from f] create a stream from the given input function. [f] is
      called each time more input is needed, and the stream ends when [f]
      returns [None]. *)

  val of_list : 'a list -> 'a t
  (** [of_list l] create a stream returning all elements of [l]. *)

  val of_array : 'a array -> 'a t
  (** [of_array a] create a stream returning all elements of [a]. *)

  val of_string : string -> char t
  (** [of_string str] create a stream returning all characters of [str]. *)

  val to_list : 'a t -> 'a list monad
  (** Return the list of elements of the given stream. *)

  val to_string : char t -> string monad
  (** Return the word composed of all characters of the given
      stream. *)

  val peek : 'a t -> 'a option monad
  (** [peek st] return the first element of the stream, if any,
      without removing it. *)

  val npeek : int -> 'a t -> 'a list monad
  (** [npeek n st] return at most the first [n] elements of [st],
      without removing them. *)

  val get : 'a t -> 'a option monad
  (** [get st] remove and return the first element of the stream, if
      any. *)

  val nget : int -> 'a t -> 'a list monad
  (** [nget n st] remove and return at most the first [n] elements of
      [st]. *)

  val get_while : ('a -> bool) -> 'a t -> 'a list monad
  (** [get_while f st] return the longest prefix of [st] where all
      elements satisfy [f]. *)

  val next : 'a t -> 'a monad
  (** [next st] remove and return the next element of the stream, of
      fail with {!Empty} if the stream is empty. *)

  val junk : 'a t -> unit monad
  (** [junk st] remove the first element of [st]. *)

  val njunk : int -> 'a t -> unit monad
  (** [njunk n st] removes at most the first [n] elements of the
      stream. *)

  val junk_while : ('a -> bool) -> 'a t -> unit monad
  (** [junk_while f st] removes all elements at the beginning of the
      streams which satisfy [f]. *)

  val is_empty : 'a t -> bool monad
  (** [is_empty st] return wether the given stream is empty *)


  (** {2 Stream transversal} *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f st] maps the value returned by [st] with [f] *)

  val map_list : ('a -> 'b list) -> 'a t -> 'b t
  (** [map_list f st] applies [f] on each element of [st] and flattens
      the lists returned *)

  val filter : ('a -> bool) -> 'a t -> 'a t
  (** [filter f st] keeps only value [x] such that [f x] is [true] *)

  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  (** [filter_map f st] filter and map [st] at the same time *)

  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b monad
  (** [fold f s x] fold_like function for streams. *)

  val iter : ('a -> unit) -> 'a t -> unit monad
  (** [iter f s] iterates over all elements of the stream *)

  val find : ('a -> bool) -> 'a t -> 'a option monad
  (** [find f s] find an element in a stream. *)

  val find_map : ('a -> 'b option) -> 'a t -> 'b option monad
  (** [find f s] find and map at the same time. *)

  val combine : 'a t -> 'b t -> ('a * 'b) t
  (** [combine s1 s2] combine two streams. The stream will ends when
      the first stream ends. *)

  val append : 'a t -> 'a t -> 'a t
  (** [append s1 s2] return a stream which return all elements of
      [s1], then all elements of [s2] *)

  val concat : 'a t t -> 'a t
  (** [concat st] return the concatenation of all streams of [st]. *)

  val flatten : 'a list t -> 'a t
  (** [flatten st = map_list (fun l -> l) st] *)
end

(** The functor [Lemonade_Stream.Make]. *)
module Make(Monad:Lemonade_Type.S): S
  with type 'a monad = 'a Monad.t
