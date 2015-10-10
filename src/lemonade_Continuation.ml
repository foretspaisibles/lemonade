(* Lemonade_Continuation -- The continuation monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

module type FinalType =
sig
  type t
end

module type S =
sig
  type final
  type 'a t = ('a -> final) -> final
  include Lemonade_Type.S
    with type 'a t := 'a t
  val call_cc : (('a -> 'b t) -> 'a t) -> 'a t
end

module Make(Final:FinalType) =
struct
  type final = Final.t
  module Basis =
  struct
    type 'a t = ('a -> final) -> final

    let return x =
      fun cont -> cont x

    let bind m f =
      fun cont -> m (fun x -> (f x) cont)
  end

  module MethodsMonad =
    Mixture_Monad.Make(Basis)

  include Basis
  include MethodsMonad

  let call_cc kont =
    fun cont -> kont (fun x -> (fun _ -> cont x)) cont
end
