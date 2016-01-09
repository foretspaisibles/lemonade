(* Lemonade_Ok -- A variant of the success monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Printf

module Basis : sig
  type 'a t = [ `Ok of 'a | `Error of string ]
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end = struct
  type 'a t = [ `Ok of 'a | `Error of string ]

  let bind m (f : 'a -> 'b t) =
    match m with
    |`Ok(x) -> f x
    |`Error(_) as error -> error

  let return x =
    `Ok(x)
end

module Methods =
  Mixture_Monad.Make(Basis)

include Basis
include Methods

let run = function
  |`Ok(whatever) -> whatever
  |`Error(mesg) -> ksprintf failwith "Error: %s" mesg

let error mesg =
  `Error(mesg)

let errorf fmt =
  ksprintf (fun mesg -> `Error mesg) fmt

let pp_print f pp = function
  | `Ok(x) -> Format.fprintf pp "`Ok(%a)" f x
  | `Error(mesg) -> Format.fprintf pp "`Error(%S)" mesg

module T(M:Mixture_Monad.S) =
struct
  module Germ : sig
    type 'a t = [ `Ok of 'a | `Error of string ] M.t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
  end = struct

    type 'a t =
      'a Basis.t M.t

    let bind m f =
      M.bind m
        (function
          | `Ok(x) -> f x
          | `Error(err) -> M.return (`Error(err)))

    let return x =
      M.return(`Ok(x))
  end

  include Mixture_Monad.Transformer.Make(Basis)(M)(Germ)
end
