open OUnit2

let tests = "test suite for sum" >::: [
  "empty" >:: (fun _ -> assert_equal 0 0);
  "singleton" >:: (fun _ -> assert_equal 1 1));
]

let () = print_endline "Hello, world! - test file"

let _ = run_test_tt_main tests