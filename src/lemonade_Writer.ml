(* Lemonade_Writer -- The classic writer monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

module type OutputType =
sig
  type t
  val empty : unit -> t
  val append : t -> t -> t
end


module type S =
sig
  type output
  include Lemonade_Type.S
  val writer : output * 'a -> 'a t
  val tell : output -> unit t
  val listen : 'a t -> (output * 'a) t
  val pass : ((output -> output) * 'a) t -> 'a t
  val listens : (output -> 'b) -> 'a t -> ('a * 'b) t
  val censor : (output -> output) -> 'a t -> 'a t
  val run : 'a t -> output * 'a
  val eval : 'a t -> 'a
  val exec : 'a t -> output
end

module Make(Output:OutputType)=
struct
  type output = Output.t
  module Basis =
  struct
    type 'a t = output * 'a
    let return x =
      (Output.empty(), x)
    let bind (out, x) f =
      let more, y = f x in
      (Output.append out more, y)
  end

  module MethodsMonad =
    Mixture_Monad.Make(Basis)

  include Basis
  include MethodsMonad

  let writer m =
    m

  let tell out =
    (out, ())

  let listen ((out, x) as m) =
    (out, m)

  let pass (out, (filter, x)) =
    (filter out, x)

  let listens f (out, x) =
    (out, (x, f out))

  let censor f (out, x) =
    (f out, x)

  let run m =
    m

  let exec (out, _) =
    out

  let eval (_, x) =
    x

  module T(M:Lemonade_Type.S)=
  struct
    type output = Output.t
    module BasisT =
    struct
      type 'a t =
        'a Basis.t M.t
      let return x =
        M.return(Basis.return x)
      let bind m f =
        M.bind m
          (fun (out, x) ->
             M.bind
               (f x)
               (fun (more, y) -> M.return(Output.append out more, y)))
    end

    module MethodsMonadT =
      Mixture_Monad.Make(BasisT)

    include BasisT
    include MethodsMonadT

    let writer m =
      M.return m

    let tell out =
      M.return (out, ())

    let listen m =
      M.bind m (fun ((out, x) as m) -> M.return (out, m))

    let pass m =
      M.bind m (fun (out, (filter, x)) -> M.return (filter out, x))

    let listens f m =
      M.bind m (fun (out, x) -> M.return (out, (x, f out)))

    let censor f m =
      M.bind m (fun (out, x) -> M.return (f out, x))

    let run m =
      m

    let exec m =
      M.map fst m

    let eval m =
      M.map snd m

    let lift m =
      M.bind m (fun x -> BasisT.return x)
  end
end
