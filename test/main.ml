open OUnit2
(** Test Plan

    We used unit tests to test:

    - Our currency system, including testing conversions between strings and our
      custom type for monetary amounts, currency conversions, etc.
    - Account-level changes, such as active status, balance after deposit, etc.
    - State-level changes, such as changed balance after payments and requests
      between users, adding and removing from users' friend lists, changed
      current account after logging in, username validation, etc.

    We developed OUnit tests using both glass-box and black-box testing.

    - We used black-box testing to test the correctness of the preconditions and
      postconditions and expected behavior outlined in the specifications of a
      subset of our functions, including our (hidden internal) function to parse
      an amount from a string, our functions that induce state-level changes
      (payments, requests, deposits, withdrawals, log in/log out, etc.).
    - We used glass-box testing (with bisect) to increase our confidence that
      our unit tests covered all branches of our system that were possible to
      reach with OUnit tests. Because a large portion of our implementation was
      hidden from the client and not easily tested except in the command line
      during the running of the system or in the data file before and after
      making some changes, we rely on those manual testing methods to test the
      branches we could not reach.

    We used manual testing to test the remaining features of our system,
    including:

    - user prompts and interaction with the system (including what happens in
      the case of an invalid input)
    - notification inbox, friends list, messages list
    - conversions between transactions and strings (since this was encapsulated
      such that it was impossible to test with OUnit, and was only used for
      conversion to and from the data file)

    Our testing approach demonstrates the correctness of our system because we
    adequately covered all the modules in our system with either automatic or
    manual testing. In both, we made sure to include tests for basic
    functionalities (to make sure our functions and features do what they say
    they do), corner cases or cases that we thought of as most likely to contain
    errors, and cases that raise exceptions (such as invalid inputs). *)

open Venmo.Account
open Venmo.State

(*****************************************************************************
  Creating accounts for testing
  ******************************************************************************)
let acc1 = create 0 "test" "test" ~balance:"0.00 USD" "USD"
let acc2 = deposit acc1 "100.24 USD"
let acc3 = deposit acc2 "21.99 USD"
let acc4 = deposit acc1 "1.04 USD"
let acc5 = create 0 "test" "test" ~balance:"0.00 USD" "USD"
let data_dir_prefix = "data" ^ Filename.dir_sep
let zero = Yojson.Basic.from_file (data_dir_prefix ^ "zero_bal.json")
let pos = Yojson.Basic.from_file (data_dir_prefix ^ "pos_bal.json")

(*****************************************************************************
  Begin tests
  ******************************************************************************)

let acc_tests =
  [
    ("active account" >:: fun _ -> assert_equal true (is_active acc1));
    ( "deactivate account" >:: fun _ ->
      assert_equal false (is_active (deactivate acc5)) );
  ]

let invalid_amount_test (input : string) : test =
  input ^ "is an invalid amount" >:: fun _ ->
  assert_raises (InvalidAmount input) (fun () ->
      create 0 "test" "test" ~balance:input "USD")

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

let invalid_currency_tests =
  [
    ( "AAA is invalid currency" >:: fun _ ->
      assert_raises (InvalidCurrency "AAA") (fun () ->
          create 0 "test" "test" ~balance:"5.00 AAA" "USD") );
    ( "usd is valid currency" >:: fun _ ->
      assert_equal "5.00 USD"
        (balance (create 0 ~balance:"5.00 usd" "test" "test" "USD")) );
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

let state_pay_tests =
  let state_acc1 = create 0 "state1" "state" ~balance:"0.00 USD" "USD" in
  let state_acc2 = create 1 "state2" "state" ~balance:"100.00 USD" "USD" in
  let state_acc3 = create 2 "state3" "state" ~balance:"3110.00 CML" "CML" in
  let state_acc4 = create 3 "state4" "state" ~balance:"100.00 USD" "USD" in
  let unchanged_acc = create 4 "state5" "state" ~balance:"100.00 USD" "USD" in
  let state_pay_state = init_state in
  let _ = add_account state_pay_state unchanged_acc in
  let _ = add_account state_pay_state state_acc4 in
  let _ = add_account state_pay_state state_acc3 in
  let _ = add_account state_pay_state state_acc2 in
  let _ = add_account state_pay_state state_acc1 in
  let _ = make_payment state_pay_state "state2" "state1" "25.00 USD" in
  let _ = make_payment state_pay_state "state3" "state4" "3110.00 CML" in
  make_deposit state_pay_state "state3" "1.00 CML";
  [
    ( "state pay in home currency" >:: fun _ ->
      assert_equal "25.00 USD"
        (balance (accounts state_pay_state).(0))
        ~printer:(fun x -> x) );
    (* ( "no initial current account" >:: fun _ -> assert_equal None
       (current_account init_state) ); *)
    ( "pay in CML to USD account" >:: fun _ ->
      assert_equal "101.00 USD"
        (balance (accounts state_pay_state).(3))
        ~printer:(fun x -> x) );
    ( "make valid CML deposit" >:: fun _ ->
      assert_equal "1.00 CML"
        (balance (accounts state_pay_state).(2))
        ~printer:(fun x -> x) );
    ( "username already exists raises InvalidUsername" >:: fun _ ->
      assert_raises (InvalidUsername "state1") (fun () ->
          check_username state_pay_state "state1") );
    ( "username doesn't exist is valid" >:: fun _ ->
      assert_equal () (check_username state_pay_state "not state") );
    ( "adding accounts increases account length correctly" >:: fun _ ->
      assert_equal 5
        (Array.length (accounts state_pay_state))
        ~printer:string_of_int );
    (* ( "initial state has no current account" >:: fun _ -> assert_equal None
       (current_account state_pay_state) ); *)
    ( "login has correct current account" >:: fun _ ->
      assert_equal unchanged_acc
        (let _ = login_system state_pay_state "state5" "state" in
         current (current_account state_pay_state))
        ~printer:display );
    ( "logout has correct current account" >:: fun _ ->
      assert_equal None
        (let _ = logout state_pay_state in
         current_account state_pay_state) );
    ( "add then remove friend" >:: fun _ ->
      assert_equal 0
        (List.length
           (let _ = login_system state_pay_state "state2" "state" in
            let _ = add_friend_state state_pay_state "state3" in
            let _ = remove_friend_state state_pay_state "state3" in
            friend_list unchanged_acc))
        ~printer:string_of_int );
  ]

let notification_tests = []

let invalid_transaction_test (invalid_tran : string) : test =
  let j : Yojson.Basic.t =
    `Assoc
      [
        ("username", `String "user");
        ("password", `String "pw");
        ("balance", `String "0.00 USD");
        ("home currency", `String "USD");
        ("history", `List [ `String invalid_tran ]);
        ("active", `String "true");
        ("notification inbox", `List []);
        ("friend list", `List []);
        ("message inbox", `List []);
      ]
  in
  let file = data_dir_prefix ^ "invalid_transaction.json" in
  let oc = open_out file in
  Yojson.Basic.to_channel oc j;
  close_out oc;
  invalid_tran ^ "is an invalid transaction" >:: fun _ ->
  assert_raises InvalidTransaction (fun () ->
      from_json
        (Yojson.Basic.from_file (data_dir_prefix ^ "invalid_transaction.json"))
        0)

let invalid_transaction_tests =
  List.map invalid_transaction_test
    [
      "";
      "   ";
      "asdf";
      "USD";
      "5.55  CAD";
      "   4 . 4 4 C A D ";
      "4. 44 CAD";
      "3.33 C AD";
      "Deposited USD";
      "Withdrew 50.. CML";
      "Paid 3.00 CAD";
      "Requested from user2";
      "Requested 50.00 CML to user1";
      "Paid 5.00 USD from user";
      "Paid 5 CML to";
      "Requested 5 from user";
      "Received 5USD from user";
    ]

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
  [ ("init_state to file" >:: fun _ -> assert_equal () (to_file init_state)) ]

let state_from_file_tests =
  [
    ( "from file logged into state3" >:: fun _ ->
      assert_equal "state3"
        (fst (from_file ()) |> current_account |> current |> username) );
    ( "from file running id" >:: fun _ ->
      assert_equal 5 (snd (from_file ())) ~printer:string_of_int );
  ]

let suite =
  "test suite for final project"
  >::: List.flatten
         [
           acc_tests;
           invalid_currency_tests;
           invalid_amount_tests;
           balance_tests;
           deposit_amount_tests;
           withdraw_amount_tests;
           state_pay_tests;
           notification_tests;
           state_to_file_tests;
           invalid_transaction_tests;
           transaction_tests;
         ]

let _ = run_test_tt_main suite