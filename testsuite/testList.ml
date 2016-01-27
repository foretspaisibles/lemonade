(* TestList -- Test the list monad

   Lemonade (https://github.com/michipili/lemonade)
   This file is part of Lemonade

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Broken

module ListMaybe =
  Lemonade_List.T(Lemonade_Maybe)

let pp_print_listmaybe_int pp m =
  Lemonade_Maybe.pp_print (Lemonade_List.pp_print Format.pp_print_int) pp m


let assert_listmaybe_int id ?expected_failure f a b =
  assert_equal
    id
    ?expected_failure
    ~printer:pp_print_listmaybe_int
    ~equal:( (=) )
    f a b

let divisors n =
  let rec loop acc k =
    match (k >= n), (n mod k = 0) with
    | true, _ -> acc
    | _, true -> loop (k :: acc) (k+1)
    | _, false -> loop acc (k+1)
  in
  loop [] 2

let safe_divisors n =
  if n > 0 then Some(divisors n) else None

let () =
  register_suite "listtransform" "Test list transformation features" [

    assert_listmaybe_int "1"
      (ListMaybe.bind (Some[ 21; 16; 7; 14 ]))
      safe_divisors
      (Some(Lemonade_List.bind [ 21; 16; 7; 14 ] divisors));

    assert_listmaybe_int "2"
      (ListMaybe.bind (Some[ 21; 16; 7; -1; 14 ]))
      safe_divisors
      None
  ]
