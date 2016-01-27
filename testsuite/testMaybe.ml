(* TestMaybe -- Test the maybe monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Broken

module MaybeList =
  Lemonade_Maybe.T(Lemonade_List)

let pp_print_maybelist_int pp m =
  Lemonade_List.pp_print (Lemonade_Maybe.pp_print Format.pp_print_int) pp m

let assert_maybelist_int id ?expected_failure f a b =
  assert_equal
    id
    ?expected_failure
    ~printer:pp_print_maybelist_int
    ~equal:( (=) )
    f a b

let assert_list_int id ?expected_failure f a b =
  assert_equal
    id
    ?expected_failure
    ~printer:(Lemonade_List.pp_print Format.pp_print_int)
    ~equal:( (=) )
    f a b

let sieve p lst =
  Lemonade_Maybe.filter_map
    (fun x -> if x mod p <> 0 then Some(x) else None)
    lst

let ( $ ) f g =
  fun x -> f (g x)

let () =
  register_suite "maybe" "Test the Maybe monad" [

    assert_list_int "sieve"
      ((sieve 2) $ (sieve 3) $ (sieve 5))
      [2; 3; 4; 5; 6; 7; 8; 9; 10 ]
      [ 7 ];
  ]
