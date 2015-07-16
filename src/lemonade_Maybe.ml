(* Lemonade_Maybe -- The classic maybe monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)


module Basis =
struct

  type (+'a) t =
    'a option

  let bind opt f =
    match opt with
    | Some(x) -> f x
    | None -> None

  let return x =
    Some(x)

end

module MethodsMonad = Mixture_Monad.Make(Basis)

include Basis
include MethodsMonad
