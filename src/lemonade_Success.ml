(* Lemonade_Success -- The classic success monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)


module type ErrorType =
sig
  type t
end

module type S =
sig
  include Mixture_Monad.S
  type error
  type (+'a) outcome =
    | Success of 'a
    | Error of error
  val error : error -> 'a t
  val recover : 'a t -> (error -> 'a t) -> 'a t
  val run : 'a t -> 'a outcome
end


module Make(Error:ErrorType) =
struct

  type error =
    Error.t

  type (+'a) outcome =
    | Success of 'a
    | Error of error

  module Basis =
  struct

    type 'a t = 'a outcome

    let bind succ f =
      match succ with
      | Success(x) -> f x
      | Error(s) -> Error(s)

    let return x =
      Success(x)
  end

  module MethodsMonad =
    Mixture_Monad.Make(Basis)

  include Basis
  include MethodsMonad

  let error err =
    Error(err)

  let recover m handler =
    match m with
    | Success(x) -> Success(x)
    | Error(err) -> handler err

  let run m =
    m


  module T(M:Mixture_Monad.S) =
  struct
    module Germ =
    struct

      type 'a t =
        'a Basis.t M.t

      let bind m f =
        M.bind m
          (function
            | Success(x) -> f x
            | Error(err) -> M.return (Error(err)))

      let return x =
        M.return(Success(x))
    end

    include Mixture_Monad.Transformer.Make(Basis)(M)(Germ)
  end
end
