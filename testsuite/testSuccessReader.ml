(* TestSuccessReader -- Test natural transformation

   Mixture (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2016 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Format
open Broken

module Error =
struct
  type t = string * string
end

module Success =
  Lemonade_Success.Make(Error)

module Environment =
struct
  type t = string
end

module Reader =
  Lemonade_Reader.Make(Environment)

module Basis =
  Reader.T(Success)

include Basis

type 'a outcome = 'a Success.outcome =
  | Success of 'a
  | Error of Error.t

let error err =
  Basis.lift(Success.error err)


(* Lift operations from the success monad *)

let run env m =
  Success.run(Basis.run env m)

let recover m f =
  let g x =
    Success.return(f x)
  in
  let m' =
    Reader.bind m
      (fun s -> Reader.return(Success.recover (Success.map Basis.return s) g))
  in
  Basis.join m'

(* Pretty printing *)

let pp_print_outcome_list_string pp m =
  let pp_print_list_string pp lst =
    Lemonade_List.pp_print pp_print_string pp lst
  in
  let pp_print_outcome f pp =
    function
    | Success(x) -> fprintf pp "Success(%a)" f x
    | Error(name, mesg) -> fprintf pp "Error(%S, %S)" name mesg
  in
  pp_print_outcome pp_print_list_string pp m

let assert_outcome name env f expected =
  assert_equal ~printer:pp_print_outcome_list_string
    name (fun () -> run env f) () expected

let () =
  register_suite "success_reader"
    "Test the Success Reader natural transformation"
    [
      assert_outcome "prefix"
        "prefix"
        (Basis.access begin fun prefix -> [ prefix ^ "-a"; prefix ^ "-b"] end)
        (Success [ "prefix-a"; "prefix-b"]);

      assert_outcome "join"
        "join"
        (Basis.join (Basis.access begin
             fun prefix -> Reader.return(Success.return [ prefix ^ "-a" ]) end))
        (Success ["join-a"]);
    ]
