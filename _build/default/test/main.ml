open OUnit2
open Venmo.Account
open Venmo.State

(*****************************************************************************
  Creating accounts for testing
  ******************************************************************************)
let acc1 = create 0 "test" "test" ~balance:"0.00 USD" "USD"
let acc2 = deposit acc1 "100.24 USD"
let acc3 = deposit acc2 "21.99 USD"
let acc4 = deposit acc1 "1.04 USD"
(*this test case also doesn't pass because of decimal error as mentioned in
  slack *)

let data_dir_prefix = "data" ^ Filename.dir_sep
let zero = Yojson.Basic.from_file (data_dir_prefix ^ "zero_bal.json")
let pos = Yojson.Basic.from_file (data_dir_prefix ^ "pos_bal.json")

(*****************************************************************************
  Begin tests
  ******************************************************************************)
let invalid_amount_test (input : string) : test =
  input ^ "is an invalid amount" >:: fun _ ->
  assert_raises (InvalidAmount input) (fun () ->
      create 0 "test" "test" ~balance:input "USD")

let invalid_currency_tests =
  [
    ( "AAA is invalid currency" >:: fun _ ->
      assert_raises (InvalidCurrency "AAA") (fun () ->
          create 0 "test" "test" ~balance:"5.00 AAA" "USD") );
    ( "usd is valid currency" >:: fun _ ->
      assert_equal "5.00 USD"
        (balance (create 0 ~balance:"5.00 usd" "test" "test" "USD")) );
  ]

let invalid_amount_tests =
  List.map invalid_amount_test
    [
      "";
      "   ";
      "asdf";
      "USD";
      "5.55  CAD";
      "   4 . 4 4 C A D ";
      "4. 44 CAD";
      "3.33 C AD";
    ]

let balance_tests =
  [
    ( "acc1 balance is 0.00 USD" >:: fun _ ->
      assert_equal "0.00 USD" (balance acc1) ~printer:(fun x -> x) );
    ( "acc2 balance is 100.24 USD" >:: fun _ ->
      assert_equal "100.24 USD" (balance acc2) ~printer:(fun x -> x) );
    ( "acc3 balance is 122.23 USD" >:: fun _ ->
      assert_equal "122.23 USD" (balance acc3) ~printer:(fun x -> x) );
    ( "acc4 balance is 1.04 USD" >:: fun _ ->
      assert_equal "1.04 USD" (balance acc4) ~printer:(fun x -> x) );
  ]
  @ List.map
      (fun b ->
        b ^ "parses to 2.20 USD" >:: fun _ ->
        assert_equal "2.20 USD"
          (balance (create 0 ~balance:b "test" "test" "USD")))
      [ "2.20"; "2.2"; "2.202"; "2.2022 USD" ]

let from_json_tests =
  [
    ( "balance of from_json zero is 0.00 USD" >:: fun _ ->
      assert_equal "0.00 USD" (balance (from_json zero 0)) ~printer:(fun x -> x)
    );
    ( "username of from_json zero is zero balance" >:: fun _ ->
      assert_equal "zero balance"
        (username (from_json zero 0))
        ~printer:(fun x -> x) );
    ( "username of from_json pos is positive balance" >:: fun _ ->
      assert_equal "positive balance"
        (username (from_json zero 0))
        ~printer:(fun x -> x) );
    ( "balance of from_json pos is 3110.00 USD" >:: fun _ ->
      assert_equal "3110.00 USD"
        (balance (from_json pos 0))
        ~printer:(fun x -> x) );
  ]

let deposit_acc_usd = create 0 "test" "test" ~balance:"0.00 USD" "USD"
let deposit_acc_eur = create 0 "test" "test" ~balance:"0.00 EUR" "EUR"
let deposit_acc_krw = create 0 "test" "test" ~balance:"0.00 KRW" "KRW"
let deposit_acc_rmb = create 0 "test" "test" ~balance:"0.00 RMB" "RMB"
let deposit_acc_cad = create 0 "test" "test" ~balance:"0.00 CAD" "CAD"
let deposit_acc_cml = create 0 "test" "test" ~balance:"0.00 CML" "CML"

let deposit_amount_tests =
  (*ADD CURRENCY CONVERSIONS*)
  [
    ( "100.24 USD is a valid deposit" >:: fun _ ->
      assert_equal "100.24 USD" (balance acc2) ~printer:(fun x -> x) );
    ( "depositing twice is a valid deposit" >:: fun _ ->
      assert_equal "122.23 USD" (balance acc3) ~printer:(fun x -> x) );
    ( "1.04 USD is a valid deposit" >:: fun _ ->
      assert_equal "1.04 USD" (balance acc4) ~printer:(fun x -> x) );
    ( "USD to USD conversion" >:: fun _ ->
      assert_equal "111.11 USD"
        (balance (deposit deposit_acc_usd "111.11 USD"))
        ~printer:(fun x -> x) );
    ( "EUR to USD conversion" >:: fun _ ->
      assert_equal "111.11 USD"
        (balance (deposit deposit_acc_usd "111.11 EUR"))
        ~printer:(fun x -> x) );
    ( "KRW to USD conversion" >:: fun _ ->
      assert_equal "0.07 USD"
        (balance (deposit deposit_acc_usd "100.00 KRW"))
        ~printer:(fun x -> x) );
    ( "RMB to USD conversion" >:: fun _ ->
      assert_equal "14.00 USD"
        (balance (deposit deposit_acc_usd "100.00 RMB"))
        ~printer:(fun x -> x) );
    ( "CAD to USD conversion" >:: fun _ ->
      assert_equal "73.00 USD"
        (balance (deposit deposit_acc_usd "100.00 CAD"))
        ~printer:(fun x -> x) );
    ( "USD to EUR conversion" >:: fun _ ->
      assert_equal "100.00 EUR"
        (balance (deposit deposit_acc_eur "100.00 USD"))
        ~printer:(fun x -> x) );
    ( "EUR to EUR conversion" >:: fun _ ->
      assert_equal "100.00 EUR"
        (balance (deposit deposit_acc_eur "100.00 EUR"))
        ~printer:(fun x -> x) );
    ( "KRW to EUR conversion" >:: fun _ ->
      assert_equal "0.71 EUR"
        (balance (deposit deposit_acc_eur "1000.00 KRW"))
        ~printer:(fun x -> x) );
    ( "RMB to EUR conversion" >:: fun _ ->
      assert_equal "14.00 EUR"
        (balance (deposit deposit_acc_eur "100.00 RMB"))
        ~printer:(fun x -> x) );
    ( "CAD to EUR conversion" >:: fun _ ->
      assert_equal "74.00 EUR"
        (balance (deposit deposit_acc_eur "100.00 CAD"))
        ~printer:(fun x -> x) );
    ( "CML to EUR conversion" >:: fun _ ->
      assert_equal "1.00 EUR"
        (balance (deposit deposit_acc_eur "3110.00 CML"))
        ~printer:(fun x -> x) );
    ( "USD to KRW conversion" >:: fun _ ->
      assert_equal "1422.00 KRW"
        (balance (deposit deposit_acc_krw "1.00 USD"))
        ~printer:(fun x -> x) );
    ( "EUR to KRW conversion" >:: fun _ ->
      assert_equal "1417.09 KRW"
        (balance (deposit deposit_acc_krw "1.00 EUR"))
        ~printer:(fun x -> x) );
    ( "KRW to KRW conversion" >:: fun _ ->
      assert_equal "100.00 KRW"
        (balance (deposit deposit_acc_krw "100.00 KRW"))
        ~printer:(fun x -> x) );
    ( "RMB to KRW conversion" >:: fun _ ->
      assert_equal "196.07 KRW"
        (balance (deposit deposit_acc_krw "1.00 RMB"))
        ~printer:(fun x -> x) );
    ( "CAD to KRW conversion" >:: fun _ ->
      assert_equal "1041.84 KRW"
        (balance (deposit deposit_acc_krw "1.00 CAD"))
        ~printer:(fun x -> x) );
    ( "CML to KRW conversion" >:: fun _ ->
      assert_equal "1422.00 KRW"
        (balance (deposit deposit_acc_krw "3110.00 CML"))
        ~printer:(fun x -> x) );
    ( "USD to RMB conversion" >:: fun _ ->
      assert_equal "7.25 RMB"
        (balance (deposit deposit_acc_rmb "1.00 USD"))
        ~printer:(fun x -> x) );
    ( "EUR to RMB conversion" >:: fun _ ->
      assert_equal "7.23 RMB"
        (balance (deposit deposit_acc_rmb "1.00 EUR"))
        ~printer:(fun x -> x) );
    ( "KRW to RMB conversion" >:: fun _ ->
      assert_equal "5.10 RMB"
        (balance (deposit deposit_acc_rmb "1000.00 KRW"))
        ~printer:(fun x -> x) );
    ( "RMB to RMB conversion" >:: fun _ ->
      assert_equal "1.00 RMB"
        (balance (deposit deposit_acc_rmb "1.00 RMB"))
        ~printer:(fun x -> x) );
    ( "CAD to RMB conversion" >:: fun _ ->
      assert_equal "5.31 RMB"
        (balance (deposit deposit_acc_rmb "1.00 CAD"))
        ~printer:(fun x -> x) );
    ( "CML to RMB conversion" >:: fun _ ->
      assert_equal "7.25 RMB"
        (balance (deposit deposit_acc_rmb "3110.00 CML"))
        ~printer:(fun x -> x) );
    ( "USD to CAD conversion" >:: fun _ ->
      assert_equal "1.36 CAD"
        (balance (deposit deposit_acc_cad "1.00 USD"))
        ~printer:(fun x -> x) );
    ( "EUR to CAD conversion" >:: fun _ ->
      assert_equal "1.36 CAD"
        (balance (deposit deposit_acc_cad "1.00 EUR"))
        ~printer:(fun x -> x) );
    ( "KRW to CAD conversion" >:: fun _ ->
      assert_equal "0.96 CAD"
        (balance (deposit deposit_acc_cad "1000.00 KRW"))
        ~printer:(fun x -> x) );
    ( "RMB to CAD conversion" >:: fun _ ->
      assert_equal "0.19 CAD"
        (balance (deposit deposit_acc_cad "1.00 RMB"))
        ~printer:(fun x -> x) );
    ( "CAD to CAD conversion" >:: fun _ ->
      assert_equal "1.00 CAD"
        (balance (deposit deposit_acc_cad "1.00 CAD"))
        ~printer:(fun x -> x) );
    ( "CML to CAD conversion" >:: fun _ ->
      assert_equal "1.36 CAD"
        (balance (deposit deposit_acc_cad "3110.00 CML"))
        ~printer:(fun x -> x) );
    ( "USD to CML conversion" >:: fun _ ->
      assert_equal "3110.00 CML"
        (balance (deposit deposit_acc_cml "1.00 USD"))
        ~printer:(fun x -> x) );
    ( "EUR to CML conversion" >:: fun _ ->
      assert_equal "3110.00 CML"
        (balance (deposit deposit_acc_cml "1.00 EUR"))
        ~printer:(fun x -> x) );
    ( "KRW to CML conversion" >:: fun _ ->
      assert_equal "2.18 CML"
        (balance (deposit deposit_acc_cml "1.00 KRW"))
        ~printer:(fun x -> x) );
    ( "RMB to CML conversion" >:: fun _ ->
      assert_equal "435.40 CML"
        (balance (deposit deposit_acc_cml "1.00 RMB"))
        ~printer:(fun x -> x) );
    ( "CAD to CML conversion" >:: fun _ ->
      assert_equal "2270.30 CML"
        (balance (deposit deposit_acc_cml "1.00 CAD"))
        ~printer:(fun x -> x) );
    ( "CML to CML conversion" >:: fun _ ->
      assert_equal "1.00 CML"
        (balance (deposit deposit_acc_cml "1.00 CML"))
        ~printer:(fun x -> x) );
  ]

let withdraw1 = withdraw acc3 "20.20 USD"
let withdraw2 = withdraw withdraw1 "20.99 USD"

let withdraw_amount_tests =
  [
    ( "20.20 USD is a valid withdrawal" >:: fun _ ->
      assert_equal "102.03 USD" (balance withdraw1) ~printer:(fun x -> x) );
    ( "withdrawing twice is a valid deposit" >:: fun _ ->
      assert_equal "81.04 USD" (balance withdraw2) ~printer:(fun x -> x) );
  ]

let test_state = init_state
let _ = add_account test_state acc1

let state_account_tests =
  [
    ( "current account of initial state is None" >:: fun _ ->
      assert_equal None (current_account init_state) );
    ( "username already exists raises InvalidUsername" >:: fun _ ->
      assert_raises (InvalidUsername "test") (fun () ->
          check_username test_state "test") );
    ( "username doesn't exist is valid" >:: fun _ ->
      assert_equal () (check_username test_state "not test") );
  ]

let _ = add_account test_state acc2
let pay_acc1 = make_payment test_state 0 1 "50.12 USD"
let after_pay_acc1 = (accounts test_state).(0)

let state_pay_tests =
  [
    ( "pay in home currency: paid account" >:: fun _ ->
      assert_equal "50.12 USD" (balance after_pay_acc1) ~printer:(fun x -> x) );
    (let _ = make_payment test_state 1 0 "3110 CML" in
     "pay in CML to USD account" >:: fun _ ->
     assert_equal "51.12 USD"
       (balance (accounts test_state).(0))
       ~printer:(fun x -> x));
  ]

let state_request_tests = []

let transaction_tests =
  [
    (let transaction_test_acc =
       create 0 "test" "test" ~balance:"0.00 USD" "USD"
     in
     "original transaction list is empty" >:: fun _ ->
     assert_equal "\nTransaction History\n\n"
       (display_history transaction_test_acc) ~printer:(fun x -> x));
  ]

let state_to_file_tests =
  [ ("test_state to file" >:: fun _ -> assert_equal () (to_file test_state)) ]

let suite =
  "test suite for final project"
  >::: List.flatten
         [
           invalid_currency_tests;
           invalid_amount_tests;
           balance_tests;
           deposit_amount_tests;
           withdraw_amount_tests;
           state_account_tests;
           state_pay_tests;
           state_request_tests;
           state_to_file_tests;
           transaction_tests;
         ]

let _ = run_test_tt_main suite