open OUnit2
open Account

let invalid_amount_test (input : string) : test =
  input ^ "is an invalid amount" >:: fun _ ->
  assert_raises (InvalidAmount input) (fun () ->
      make ~balance:input "test" "test")

let invalid_currency_tests =
  [
    ( "AAA is invalid currency" >:: fun _ ->
      assert_raises (InvalidCurrency "AAA") (fun () ->
          make ~balance:"5.00 AAA" "test" "test") )
    (* ( "usd is valid currency" >:: fun _ -> assert_raises (InvalidCurrency
       "usd") (fun () -> make ~balance:"5.00 usd" "test" "test") ); *);
  ]

let invalid_amount_tests =
  List.map invalid_amount_test
    [
      "";
      "   ";
      "asdf";
      "0";
      "2.20";
      "2.2";
      "2.222";
      "2.2222 USD";
      "USD";
      "5.55  CAD";
      "   4 . 4 4 C A D ";
      "4. 44 CAD";
      "3.33 C AD";
    ]

let acc1 = make ~balance:"0.00 USD" "test" "test"

let balance_tests =
  [
    ( "acc1 balance is 0.0 USD" >:: fun _ ->
      assert_equal "0.0 USD" (balance acc1) ~printer:(fun x -> x) );
  ]
(* let deposit_amount_tests = [ ( "100 USD is a valid deposit" >:: fun _ ->
   assert_equal "100 USD" (deposit acc1 "100 USD") ); ]*)

let suite =
  "test suite for final project"
  >::: List.flatten
         [ invalid_currency_tests; invalid_amount_tests; balance_tests ]

let _ = run_test_tt_main suite