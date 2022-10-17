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

let acc2 = deposit acc1 "100.24 USD"
let acc3 = deposit acc2 "21.99 USD"
let acc4 = deposit acc1 "1.04 USD"

let deposit_amount_tests =
  [
    ( "100.24 USD is a valid deposit" >:: fun _ ->
      assert_equal "100.24 USD" (balance acc2) ~printer:(fun x -> x) );
    ( "depositing twice is a valid deposit" >:: fun _ ->
      assert_equal "122.23 USD" (balance acc3) ~printer:(fun x -> x) );
    ( "1.04 USD is a valid deposit" >:: fun _ ->
      assert_equal "1.04 USD" (balance acc4) ~printer:(fun x -> x) );
    ( "depositing different currency is invalid" >:: fun _ ->
      assert_raises (InvalidDeposit "21.41 CAD") (fun () ->
          deposit acc2 "21.41 CAD") );
  ]

let withdraw1 = withdraw acc3 "20.20 USD"

(*let withdraw1 = withdraw acc3 "20.00 USD" (*this works but the above gives an
  error*)*)
let withdraw2 = withdraw withdraw1 "20.99 USD"

let withdraw_amount_tests =
  [
    ( "20.20 USD is a valid withdrawal" >:: fun _ ->
      assert_equal "102.03 USD" (balance withdraw1) ~printer:(fun x -> x) );
    ( "withdrawing twice is a valid deposit" >:: fun _ ->
      assert_equal "81.04 USD" (balance withdraw2) ~printer:(fun x -> x) );
    ( "withdrawing different currency is invalid" >:: fun _ ->
      assert_raises (InvalidWithdrawal "21.41 CAD") (fun () ->
          withdraw acc2 "21.41 CAD") );
  ]

let suite =
  "test suite for final project"
  >::: List.flatten
         [
           invalid_currency_tests;
           invalid_amount_tests;
           balance_tests;
           deposit_amount_tests;
           withdraw_amount_tests;
         ]

let _ = run_test_tt_main suite