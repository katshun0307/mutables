open OUnit
open Mutables

(* test for mutable dict *)
let dict_test = "MutableDict" >::: [
    "get1" >:: (fun _ -> assert_equal None (
        let dict = MutableDict.init [] in
        MutableDict.get dict "a"));
    "get2" >:: (fun _ -> assert_equal (Some 5) (
        let dict = MutableDict.init [("a", 5)] in
        MutableDict.get dict "a"));
    "get_exn1" >:: (fun _ -> assert_equal 5 (
        let dict = MutableDict.init [("a", 5)] in
        MutableDict.get_exn dict "a"));
    "get_exn2" >:: (fun _ -> assert_raises Not_found (
        fun () ->
          let dict = MutableDict.init [] in
          MutableDict.get_exn dict "a"));
  ]

let _ = run_test_tt_main dict_test
