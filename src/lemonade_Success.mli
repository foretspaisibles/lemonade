(* Lemonade_Success -- The classic success monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Success monad.

This is mostly a toy example, but its reporting possibilities could be
extended in something useful. *)

type (+'a) t =
| Success of 'a
| Error of string

include Mixture_Monad.S
    with type 'a t := 'a t
