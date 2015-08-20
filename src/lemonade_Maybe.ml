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

let pp_print f pp m =
  let open Format in
  match m with
  | Some(x) -> fprintf pp "Some(%a)" f x
  | None -> fprintf pp "None"

let is_some = function
  | Some(_) -> true
  | None -> false

let is_none = function
  | Some(_) -> false
  | None -> true

let find = function
  | Some(b) -> b
  | None -> raise Not_found

let default a = function
  | Some(b) -> b
  | None -> a

let of_list = function
  | [] -> None
  | a :: _ -> Some(a)

let to_list = function
  | Some(a) -> [a]
  | None -> []

let filter_map f lst =
  List.fold_left
    (fun acc x -> match f x with
       | Some(a) -> a :: acc
       | None -> acc) [] lst
  |> List.rev

let filter lst =
  filter_map (fun x -> x) lst

module MethodsMonad =
  Mixture_Monad.Make(Basis)

include Basis
include MethodsMonad

module T(M:Mixture_Monad.S) =
struct

  module Germ =
  struct

    type 'a t =
      'a option M.t

    let bind m f =
      M.bind m
        (function
          | None -> M.return None
          | Some(x) -> f x)

    let return x =
      M.return(Some(x))

  end


  include Mixture_Monad.Transformer.Make(Basis)(M)(Germ)
end
