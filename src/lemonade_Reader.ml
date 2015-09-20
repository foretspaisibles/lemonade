(* Lemonade_Reader -- The classic reader monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

module type EnvironmentType =
sig
  type t
end


module type S =
sig
  type environment
  include Lemonade_Type.S
  val read : environment t
  val run : environment -> 'a t -> 'a
  val local : (environment -> environment) -> 'a t -> 'a t
  val access : (environment -> 'a) -> 'a t
end


module Make(Environment:EnvironmentType) =
struct
  type environment = Environment.t
  module Basis =
  struct
    type 'a t =
      environment -> 'a

    let return x =
        fun _ -> x

    let bind m f =
      fun env -> f (m env) env
  end

  module MethodsMonad =
    Mixture_Monad.Make(Basis)

  include Basis
  include MethodsMonad

  let read =
    fun env -> env

  let run env m =
    m env

  let local f m =
    fun env -> m (f env)

  let access f =
    fun env -> f env


  module T(M:Lemonade_Type.S) =
  struct
    type environment = Environment.t

    module BasisT =
    struct
      type 'a t =
        environment -> 'a M.t
      let return x =
        fun _ -> M.return x
      let bind m f =
        fun env -> M.bind (m env) (fun x -> f x env)
    end

    module MethodsMonadT =
      Mixture_Monad.Make(BasisT)

    include BasisT
    include MethodsMonadT

    let read =
      fun env -> M.return env

    let run env m =
      m env

    let local f m =
      fun env -> m (f env)

    let access f =
      fun env -> M.return(f env)

    let lift m =
      fun _ -> m
  end
end
