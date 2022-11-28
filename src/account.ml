open Yojson.Basic.Util

(** [currency] is the type that represents a currency. *)
type currency =
  | USD
  | EUR
  | KRW
  | RMB
  | CAD
  | CML

type amount = {
  number : float;
  currency : currency;
}
(** [amount] is the type that represents an amount of money. The [number] field
    is float, rounded to the nearest hundredth, which represents the units of
    currency. The [currency] field represents the currency that the amount of
    money is in. *)

type transaction =
  | Pay of {
      payer : string; (* username of the payer *)
      payee : string; (* username of the payee *)
      amount : string;
    }
  | Request of {
      payer : string;
      payee : string;
      amount : string;
      accepted : bool;
    }
  | Deposit of {
      (* could also model this as a payment to oneself instead of defining a new
         constructor *)
      account : string;
      amount : string;
    }
  | Withdraw of {
      account : string;
      amount : string;
    }

type notifications =
  | FriendRequest
  | PaymentRequest

type notification_status =
  | NotAccepted
  | Accepted

type notification = PaymentRequest of transaction

(*let add_notification acc not = acc.notification_inbox < - Array.append [|
  not|] acc.notification_inbox let notif_clear acc = acc.notification_inbox <-
  []

  let string_of_notif notif = match notif with | PaymentRequest Request {payer;
  payee; amount} -> payee ^ " requested " ^ payer ^ " " ^ amount |
  PaymentRequest n -> raises (Failure "invalid transaction for notification") *)

type t = {
  id : int;
  username : string;
  password : string;
  balance : amount;
  home_currency : currency;
  mutable history : transaction list;
  active : bool;
  mutable notification_inbox : notification list;
}

exception InvalidAmount of string
exception InvalidCurrency of string
exception InvalidTransaction
exception InvalidDeposit of string
exception InvalidWithdrawal of string
exception InvalidConversion

(** [currency_of_string s] converts [s] to a [currency], and raises
    [InvalidCurrency s] if [s] is not the name of a constructor of [currency].
    Examples:

    - [currency_of_string "USD"] is [USD].
    - [currency_of_string "EUR"] is [EUR].
    - [currency_of_string "AAA"] raises [InvalidCurrency "AAA"].
    - [currency_of_string ""] raises [InvalidCurrency ""]. *)

let currency_of_string s =
  match String.uppercase_ascii s with
  | "USD" -> USD
  | "EUR" -> EUR
  | "KRW" -> KRW
  | "RMB" -> RMB
  | "CAD" -> CAD
  | "CML" -> CML
  | _ -> raise (InvalidCurrency s)

(** [string_of_currency c] converts [c] to a [string]. Examples:

    - [string_of_currency USD] is ["USD"].
    - [string_of_currency EUR] is ["EUR"].*)

let string_of_currency c =
  match c with
  | USD -> "USD"
  | EUR -> "EUR"
  | KRW -> "KRW"
  | RMB -> "RMB"
  | CAD -> "CAD"
  | CML -> "CML"

(*The following functions allow for currency conversion based on the conversion
  rates as of 10/29/2022 *)

(*[to_usd n c] converts number [n] of currency [c] to an USD amount*)
let to_usd n c =
  if c = USD then { number = n; currency = USD }
  else if c = EUR then { number = n; currency = USD }
  else if c = KRW then { number = n *. 0.0007; currency = USD }
  else if c = RMB then { number = n *. 0.14; currency = USD }
  else if c = CAD then { number = n *. 0.73; currency = USD }
  else if c = CML then { number = n /. 3110.; currency = USD }
  else raise InvalidConversion

(*[to_eur n c] converts number [n] of currency [c] to an EUR amount*)
let to_eur n c =
  if c = EUR then { number = n; currency = EUR }
  else if c = USD then { number = n; currency = EUR }
  else if c = KRW then { number = n *. 0.00071; currency = EUR }
  else if c = RMB then { number = n *. 0.14; currency = EUR }
  else if c = CAD then { number = n *. 0.74; currency = EUR }
  else if c = CML then { number = n /. 3110.; currency = EUR }
  else raise InvalidConversion

(*[to_krw n c] converts number [n] of currency [c] to an KRW amount*)
let to_krw n c =
  if c = KRW then { number = n; currency = KRW }
  else if c = USD then { number = n *. 1422.; currency = KRW }
  else if c = EUR then { number = n *. 1417.09; currency = KRW }
  else if c = RMB then { number = n *. 196.07; currency = KRW }
  else if c = CAD then { number = n *. 1041.84; currency = KRW }
  else if c = CML then { number = n *. 1422. /. 3110.; currency = KRW }
  else raise InvalidConversion

(*[to_rmb n c] converts number [n] of currency [c] to an RMB amount*)
let to_rmb n c =
  if c = RMB then { number = n; currency = RMB }
  else if c = USD then { number = n *. 7.25; currency = RMB }
  else if c = EUR then { number = n *. 7.23; currency = RMB }
  else if c = KRW then { number = n *. 0.0051; currency = RMB }
  else if c = CAD then { number = n *. 5.31; currency = RMB }
  else if c = CML then { number = n *. 7.25 /. 3110.; currency = RMB }
  else raise InvalidConversion

(*[to_cad n c] converts number [n] of currency [c] to an CAD amount*)
let to_cad n c =
  if c = CAD then { number = n; currency = CAD }
  else if c = USD then { number = n *. 1.36; currency = CAD }
  else if c = EUR then { number = n *. 1.36; currency = CAD }
  else if c = KRW then { number = n *. 0.00096; currency = CAD }
  else if c = RMB then { number = n *. 0.19; currency = CAD }
  else if c = CML then { number = n *. 1.36 /. 3110.; currency = CAD }
  else raise InvalidConversion

let to_cml n c =
  if c = CML then { number = n; currency = CML }
  else if c = USD then { number = n *. 3110.; currency = CML }
  else if c = EUR then { number = n *. 3110.; currency = CML }
  else if c = KRW then { number = n *. 0.0007 *. 3110.; currency = CML }
  else if c = CAD then { number = n *. 0.73 *. 3110.; currency = CML }
  else if c = RMB then { number = n *. 0.14 *. 3110.; currency = CML }
  else raise InvalidConversion

(** [parse_amount s] parses a player's input into an [amount], as follows. The
    sequence of numbers before the space in [s] is converted to a float and 
    rounded to the hundredth of a unit, and is the contents of the [number] 
    field. The sequence of letters after the space is translated into a 
    [currency] type and becomes the [currency] field. 

    Requires: [s] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [InvalidAmount] if [s] does not represent a valid amount. A string
    representing a valid amount must contain a consecutive sequence of numeric 
    characters and at most one period. Optionally, it may be followed by a 
    3-character string that represents a valid currency (i.e., is the name of a 
    constructor of the [currency] type), provided it is separated from the 
    numeric sequence by at least one space and no non-spaces. 
    
    Examples: 
      - [parse_amount "5.00 USD"] is {number = 0.; currency = USD}
      - [parse_amount "5 USD"] is {number = 5.; currency = USD}
      - [parse_amount "5"] is {number = 5.; currency = USD}
      - [parse_amount "10.99 CAD"] is {number = 10.99; currency = CAD}
      - [parse_amount "0.00   CAD"] is {number = 0.; currency = CAD}
      - [parse_amount "0.00"] is {number = 0.; currency = USD}
      - [parse_amount "USD"] raises InvalidAmount "USD"
      - [parse_amount " "] raises InvalidAmount " "
      - [parse_amount "0   .   00 USD"] raises InvalidAmount "0   .   00 USD"
  *)
let parse_amount (s : string) : amount =
  let split = String.split_on_char ' ' (String.trim s) in
  try
    let amt =
      match split with
      | [] -> raise (InvalidAmount s)
      | [ n ] -> { number = float_of_string (String.trim n); currency = USD }
      | [ n; c ] ->
          {
            number = float_of_string (String.trim n);
            currency = currency_of_string c;
          }
      | _ -> raise (InvalidAmount s)
    in
    if amt.number < 0. then raise (InvalidAmount s) else amt
  with Failure f -> raise (InvalidAmount s)

(** [unparse_amount amt] returns a string representing an amount, rounded to the
    nearest hundredth.

    NOTE: [unparse_amount] does not work with very big numbers (~> 10^23) *)

let unparse_amount (a : amount) : string =
  Printf.sprintf "%.2f" (Float.round (a.number *. 100.) /. 100.)
  ^ " "
  ^ string_of_currency a.currency

let string_of_transaction (t : transaction) : string =
  match t with
  | Deposit { amount } -> "Deposited " ^ amount
  | Withdraw { amount } -> "Withdrew " ^ amount
  | Pay { payee; amount } -> "Paid " ^ amount ^ " to " ^ payee
  | Request { payer; amount } -> "Requested " ^ amount ^ " from " ^ payer

(** [transaction_of_string s un] parses a string that describes a transaction
    into a [transaction] involving the account with username [un] and
    potentially another account. Input: A string that represents a valid
    transaction. A string representing a valid transaction must contain the
    following words (and no more), separated by spaces, in the following order:

    - a word describing the transaction type (one of "Deposited", "Withdrew",
      "Paid", or "Requested")
    - 2 words representing a valid amount
    - and 0 or 2 words representing the other party in a transaction:
    - ... if the first word was "Paid", then the word "to" followed by a valid
      username representing the payee
    - ... if the first word was "Requested", then the word "from" followed by a
      valid username representing the payer (requestee)
    - ... if the first word was "Deposited" or "Withdrew", then 0 words

    Raises: [InvalidTransaction] if [s] does not represent a valid transaction.

    Examples:

    - [transaction_of_string "Deposited 5.00 USD" un] is
      [Deposit {account = un; amount = "5.00 USD"}]
    - [transaction_of_string "Deposited 5.00USD" un] raises [InvalidTransaction]
    - [transaction_of_string "Withdrew 311.0 CML" un] is
      [Withdraw {account = un; amount = "311.00 CML"}]
    - [transaction_of_string "Paid 30.00 CAD to user1" un] is
      [Pay {payer = un; payee = "user1"; amount = "30.00 CAD"}]
    - [transaction_of_string "Paid 30.00 CAD user1" un] raises
      [InvalidTransaction]
    - [transaction_of_string "Requested 30.00 CAD from user1" un] is
      [Request {payer = "user1"; payee = un; amount = "30.00 CAD"; accepted = false}]
    - [transaction_of_string "Requested 30.00 CAD to user1" un] raises
      [InvalidTransaction]
    - [transaction_of_string "Deposited 5.00 USD to user1" un] raises
      [InvalidTransaction] *)
let transaction_of_string (s : string) (un : string) : transaction =
  let split = String.split_on_char ' ' (String.trim s) in
  match split with
  | [] | [ _ ] | [ _; _ ] -> raise InvalidTransaction
  | action :: number :: curr :: tail -> (
      let amt = String.trim (String.trim number ^ " " ^ String.trim curr) in
      match String.trim action with
      | "Deposited" -> Deposit { account = un; amount = amt }
      | "Withdrew" -> Withdraw { account = un; amount = amt }
      | "Paid" -> (
          match tail with
          | [ _to; payee ] ->
              if _to <> "to" then raise InvalidTransaction
              else Pay { payer = un; payee; amount = amt }
          | _ -> raise InvalidTransaction)
      | "Requested" -> (
          match tail with
          | [ _from; payer ] ->
              if _from <> "from" then raise InvalidTransaction
              else Request { payer; payee = un; amount = amt; accepted = false }
          | _ -> raise InvalidTransaction)
      | _ -> raise InvalidTransaction)

(** [notif_of_string s un] parses a string that describes a notification to
    [notification]. Input: A string that represents a valid notification. A
    string representing a valid transaction must contain the following words
    (and no more), separated by spaces, in the following order:

    - a word describing the payer will be the first word
    - second word will always be "requested"
    - a word describing the payee will be the third word
    - last two words will be the amount -accepted will always be false since it
      is in the notification list

    Raises: [InvalidTransaction] if [s] does not represent a valid request
    transaction.

    Examples:

    - [transaction_of_string "user1 requested user2 3.00 USD"] is
      [PaymentRequest (Request {payer = user1; payee = user2; amount = "3.00 USD"; accepted = false })]
      \-anything else will raise an error *)

let notif_of_string str =
  let split = String.split_on_char ' ' (String.trim str) in
  match split with
  | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] | [ _; _; _; _ ] ->
      failwith "Invalid Notification"
  | payee :: req :: payer :: amount :: curr :: tail ->
      let amt = String.trim (String.trim amount ^ " " ^ String.trim curr) in
      PaymentRequest (Request { payer; payee; amount = amt; accepted = false })

let string_of_notif notif =
  match notif with
  | PaymentRequest (Request { payer; payee; amount }) ->
      payee ^ " requested " ^ payer ^ " " ^ amount
  | PaymentRequest n -> raise (Failure "invalid transaction for notification")

(*let notif_of_string *)
let from_json j id =
  let bal =
    if j |> member "balance" = `Null then { number = 0.00; currency = USD }
    else j |> member "balance" |> to_string |> parse_amount
  in
  let un = j |> member "username" |> to_string in
  {
    id;
    username = un;
    password = j |> member "password" |> to_string;
    balance = bal;
    home_currency =
      j |> member "home currency" |> to_string |> currency_of_string;
    history =
      j |> member "history" |> to_list |> List.map to_string
      |> List.map (fun s -> transaction_of_string s un);
    active = true;
    notification_inbox =
      j
      |> member "notification inbox"
      |> to_list |> List.map to_string
      |> List.map (fun s -> notif_of_string s);
  }

let to_json acc : Yojson.Basic.t =
  match acc with
  | {
   id;
   username;
   password;
   balance;
   home_currency;
   history;
   active;
   notification_inbox;
  } ->
      `Assoc
        [
          ("username", `String username);
          ("password", `String password);
          ("balance", `String (unparse_amount balance));
          ("home currency", `String (string_of_currency home_currency));
          ( "history",
            `List
              (List.map (fun t -> `String (string_of_transaction t)) history) );
          ("active", `String (string_of_bool active));
          ( "notification inbox",
            `List
              (List.map
                 (fun not -> `String (string_of_notif not))
                 notification_inbox) );
        ]

let create (id : int) (username : string) (password : string)
    ?(balance = "0.00 USD") (home_curr : string) : t =
  {
    id;
    username;
    password;
    balance = parse_amount balance;
    home_currency = currency_of_string home_curr;
    history = [];
    active = true;
    notification_inbox = [];
  }

let username acc = acc.username
let password acc = acc.password
let check_password str acc = str = password acc
let balance acc = unparse_amount acc.balance
let is_active acc = acc.active
let deactivate acc = { acc with active = false }
let history acc = acc.history
let print_info name info = name ^ ": " ^ info

let rec print_list = function
  | [] -> ()
  | h :: t ->
      print_endline h;
      print_list t

let display acc =
  print_newline ();
  print_endline "Account Information";
  print_endline (print_info "Account Username" (username acc));
  print_endline (print_info "Balance" (balance acc));
  print_newline ()

let display_history acc =
  print_newline ();
  print_endline "Transaction History";
  print_list (List.map string_of_transaction (history acc));
  print_newline ()

let to_homecurr acc parsed_amt =
  match acc.home_currency with
  | USD -> to_usd parsed_amt.number parsed_amt.currency
  | EUR -> to_eur parsed_amt.number parsed_amt.currency
  | KRW -> to_krw parsed_amt.number parsed_amt.currency
  | RMB -> to_rmb parsed_amt.number parsed_amt.currency
  | CAD -> to_cad parsed_amt.number parsed_amt.currency
  | CML -> to_cml parsed_amt.number parsed_amt.currency

let deposit acc amt =
  print_endline "Deposited!";
  (*this line prints out but doesn't update*)
  let a = to_homecurr acc (parse_amount amt) in
  let acc_num = acc.balance.number in
  let amt_num = a.number in
  {
    acc with
    balance = { number = acc_num +. amt_num; currency = acc.home_currency };
  }

let withdraw acc amt =
  let a = to_homecurr acc (parse_amount amt) in
  let acc_num = acc.balance.number in
  let amt_num = a.number in
  {
    acc with
    balance = { number = acc_num -. amt_num; currency = acc.home_currency };
  }

let withdraw_transaction acc amt =
  Withdraw { account = acc.username; amount = amt }

let deposit_transaction acc amt =
  Deposit { account = acc.username; amount = amt }

let pay_transaction payer payee amount = Pay { payer; payee; amount }

let make_request t payer amount =
  PaymentRequest
    (Request { payer = username t; payee = payer; amount; accepted = false })

let add_notification acc not =
  acc.notification_inbox <- not :: acc.notification_inbox

let notif_clear acc = acc.notification_inbox <- []
