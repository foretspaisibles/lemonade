(* TestPPX -- Test Preprocessor

   Mixture (https://github.com/michipili/mixture)
   This file is part of Mixture

   Copyright © 2013–2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

open Printf
open Broken

module Maybe =
  Lemonade_Maybe

let pp_print_maybe_bool pp m =
  Lemonade_Maybe.pp_print Format.pp_print_bool pp m

let assert_maybe id ?expected_failure f =
  assert_equal
    id
    ?expected_failure
    ~printer:pp_print_maybe_bool
    ~equal:( (=) )
    f () (Some true)

let maybe_assoc lst name =
  try Some(List.assoc name lst)
  with Not_found -> None

let () =
  let open Maybe in
  register_suite "ppx" "Test Lemonade PPX rewriter" [

    assert_maybe "let%lemonade"
      begin function () ->
        let%lemonade a = Some true in
        return a
      end;

    assert_maybe "nested let%lemonade"
      begin function () ->
        let%lemonade a = Some 1 in
        let%lemonade b = Some 1 in
        return (a = b)
      end;

    assert_maybe "and let%lemonade"
      begin function () ->
        let%lemonade a = Some 1
        and b = Some 1 in
        return (a = b)
      end;

    assert_maybe "match%lemonade"
      begin function () ->
        let x = return (Some 0) in
        match%lemonade x with
        | Some x -> return (x + 1 = 1)
        | None -> return false
      end;

    assert_maybe "match-exn"
      begin function () ->
        let x = return (Some 3) in
        let%lemonade a =
          match%lemonade x with
          | exception Not_found -> return false
          | Some x -> return (x = 3)
          | None -> return false
        and b =
          match%lemonade (raise Not_found) with
          | exception Not_found -> return true
          | _ -> return false
        in
        return (a && b)
      end;

    assert_maybe "if%lemonade"
      begin function () ->
        let open Maybe.Infix in
        let x = return true in
        let%lemonade a =
          if%lemonade x then return true else return false
        in
        let%lemonade b =
          if%lemonade x >|= not then return false else return true
        in
        (if%lemonade x >|= not then return ())
        >>= fun () -> return (a && b)
      end;

    assert_maybe "for%lemonade" (* Test for proper sequencing *)
      begin function () ->
        let r = ref [] in
        let f x =
          return (r := x :: !r)
        in
        let%lemonade () =
          for%lemonade x = 3 to 5 do f x done
        in return (!r = [5 ; 4 ; 3])
      end;

    assert_maybe "while%lemonade" (* Test for proper sequencing *)
      begin function () ->
        let r = ref [] in
        let f x =
          return (r := x :: !r)
        in
        let%lemonade () =
          let c = ref 2 in
          while%lemonade !c < 5 do incr c ; f !c done
        in return (!r = [5 ; 4 ; 3])
      end;

(*    assert_maybe "assert%lemonade"
      begin function () ->
        let%lemonade () = assert%lemonade true
        in return true
      end;

    assert_maybe "sequence"
      begin function () ->
        let lst = ref [] in
        (lst := 2 :: !lst; return()) >>
        (lst := 1 :: !lst; return()) >>
        (return (!lst = [1;2]))
      end;

      assert_maybe "structure let"
      begin function () ->
        let module M =
        struct
          let%lemonade result = return true
        end
        in
        return M.result
      end; *)
  ]
