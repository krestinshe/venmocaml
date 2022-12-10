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
  | Received of {
      payer : string;
      amount : string;

  }

type notifications =
  | FriendRequest
  | PaymentRequest

type notification = PaymentRequest of transaction | FriendRequest of (string * bool )

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
  mutable friend_list : string list;
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
  match c with
  | USD -> { number = n; currency = USD }
  | EUR -> { number = n; currency = USD }
  | KRW -> { number = n *. 0.0007; currency = USD }
  | RMB -> { number = n *. 0.14; currency = USD }
  | CAD -> { number = n *. 0.73; currency = USD }
  | CML -> { number = n /. 3110.; currency = USD }

(*[to_eur n c] converts number [n] of currency [c] to an EUR amount*)
let to_eur n c =
  match c with
  | USD -> { number = n; currency = EUR }
  | EUR -> { number = n; currency = EUR }
  | KRW -> { number = n *. 0.00071; currency = EUR }
  | RMB -> { number = n *. 0.14; currency = EUR }
  | CAD -> { number = n *. 0.74; currency = EUR }
  | CML -> { number = n /. 3110.; currency = EUR }

(*[to_krw n c] converts number [n] of currency [c] to an KRW amount*)
let to_krw n c =
  match c with
  | USD -> { number = n *. 1422.; currency = KRW }
  | EUR -> { number = n *. 1417.09; currency = KRW }
  | KRW -> { number = n; currency = KRW }
  | RMB -> { number = n *. 196.07; currency = KRW }
  | CAD -> { number = n *. 1041.84; currency = KRW }
  | CML -> { number = n *. 1422. /. 3110.; currency = KRW }

(*[to_rmb n c] converts number [n] of currency [c] to an RMB amount*)
let to_rmb n c =
  match c with
  | USD -> { number = n *. 7.25; currency = RMB }
  | EUR -> { number = n *. 7.23; currency = RMB }
  | KRW -> { number = n *. 0.0051; currency = RMB }
  | RMB -> { number = n; currency = RMB }
  | CAD -> { number = n *. 5.31; currency = RMB }
  | CML -> { number = n *. 7.25 /. 3110.; currency = RMB }

(*[to_cad n c] converts number [n] of currency [c] to an CAD amount*)
let to_cad n c =
  match c with
  | USD -> { number = n *. 1.36; currency = CAD }
  | EUR -> { number = n *. 1.36; currency = CAD }
  | KRW -> { number = n *. 0.00096; currency = CAD }
  | RMB -> { number = n *. 0.19; currency = CAD }
  | CAD -> { number = n; currency = CAD }
  | CML -> { number = n *. 1.36 /. 3110.; currency = CAD }

let to_cml n c =
  match c with
  | USD -> { number = n *. 3110.; currency = CML }
  | EUR -> { number = n *. 3110.; currency = CML }
  | KRW -> { number = n *. 0.0007 *. 3110.; currency = CML }
  | RMB -> { number = n *. 0.14 *. 3110.; currency = CML }
  | CAD -> { number = n *. 0.73 *. 3110.; currency = CML }
  | CML -> { number = n; currency = CML }

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
  | Received {payer; amount} -> "Received " ^ amount ^ " from " ^ payer

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
      | "Received" -> (
        match tail with
        | [ _from; payer ] ->
            if _from <> "from" then raise InvalidTransaction
            else Received { payer= un; amount = amt; }
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
  | friend :: req :: "following:":: status :: not :: tail -> FriendRequest (friend, if (not = "accepted") then true else false)
  | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] | [ _; _; _; _ ] | [ _; _; _; _ ; _]| [ _; _; _; _ ; _; _] | [ _; _; _; _ ; _; _; _]->
      failwith "Invalid Notification"
  | payee :: req :: payer :: amount :: curr :: and_ :: transaction_ :: not :: tail->

      let amt = String.trim (String.trim amount ^ " " ^ String.trim curr) in
      PaymentRequest (Request { payer; payee; amount = amt; accepted = if (not = "accepted") then true else false})


let string_of_notif notif =
  match notif with
  | PaymentRequest (Request { payer; payee; amount; accepted }) ->
      payee ^ " requested " ^ payer ^ " " ^ amount ^ " and transaction" ^ (if accepted then " accepted" else " not accepted")
  | FriendRequest (friend, accept) -> friend ^ " requested following: status" ^ if (accept) then " accepted" else " not accepted"
  | _-> raise (Failure "invalid transaction for notification")

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
      friend_list = j |> member "friend list" |> to_list |> List.map to_string;
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
    friend_list = [];
  }

let username acc = acc.username
let password acc = acc.password
let check_password str acc = str = password acc
let balance acc = unparse_amount acc.balance
let is_active acc = acc.active
let deactivate acc = { acc with active = false }
let history acc = acc.history
let string_of_info name info = name ^ ": " ^ info

(*let rec print_list = function | [] -> () | h :: t -> print_endline h;
  print_list t

  let display acc = print_newline (); print_endline "Account Information";
  print_endline (string_of_info "Account Username" (username acc));
  print_endline (string_of_info "Balance" (balance acc)); print_newline ()

  let display_history acc = print_newline (); print_endline "Transaction
  History"; print_list (List.map string_of_transaction (history acc));
  print_newline ()*)

let rec list_to_string str = function
  | [] -> str
  | h :: t -> list_to_string (str ^ h ^ "\n") t

let display acc =
  "\n" ^ "Account Information\n"
  ^ string_of_info "Account Username" (username acc)
  ^ "\n"
  ^ string_of_info "Balance" (balance acc)
  ^ "\n"

let display_history acc =
  "\n" ^ "Transaction History\n"
  ^ list_to_string "" (List.map string_of_transaction (history acc))
  ^ "\n"
let display_notif acc = "\n" ^ "Notification Inbox\n"
^ list_to_string "" (List.map string_of_notif (acc.notification_inbox)) 
^ "\n"

let display_friends acc = "\n" ^ "Friends List\n"
^ list_to_string "" (acc.friend_list)
^ "\n"


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

let pay_transaction acc payee amount = Pay { payer = acc.username; payee = payee; amount = amount }

let received_transaction payer amount = Received { payer = payer; amount = amount }

let make_request t payer amount =
  PaymentRequest
    (Request { payer = payer; payee = username t; amount; accepted = false })

let add_notification acc not =
  acc.notification_inbox <- not :: acc.notification_inbox

let add_transaction acc tran = acc.history <- tran :: acc.history
let notif_clear acc = acc.notification_inbox <- []

let length_notif acc = List.length acc.notification_inbox

let notif_inbox acc = acc.notification_inbox 

 

(*let rec go_over_notif acc st = let i = 0 in 
while (i < length_notif acc) do
  if (notif_accepted (List.nth (notif_inbox acc) i)) then begin
  print_endline (string_of_notif (List.nth (notif_inbox acc) i)); 
  print_endline "Will you accept the payment request? [yes/no]";
  print_string ">";
  let answer = read_line () in 
  let notif = (List.nth (notif_inbox acc) i) in
  if (answer = "yes") then begin (State.make_payment st (notif_payer notif) (notif_payee notif) (notif_amount notif)); end
  else if ( answer = "no") then begin (print_endline "You can accpet the payment request unless you clear the inbox."); end 
  else failwith "Invalid command"
end
done*)

let notif_payer notif = match notif with
| PaymentRequest
(Request { payer; payee; amount; accepted }) -> payer
| FriendRequest (friend, accept) -> friend
| _ -> failwith "Invalid notif"

let notif_payee notif = match notif with
| PaymentRequest
(Request { payer; payee; amount; accepted }) -> payee
| _ -> failwith "Invalid notif"

let notif_amount notif = match notif with
| PaymentRequest
(Request { payer; payee; amount; accepted }) -> amount
| _ -> failwith "Invalid notif"

let notif_accepted notif = match notif with
| PaymentRequest
(Request { payer; payee; amount; accepted }) -> accepted
| FriendRequest (friend, accept) -> accept
| _ -> failwith "Invalid notif"

let acc_new_inbox acc inbox = acc.notification_inbox <- inbox

let make_notif payer payee amount accepted = PaymentRequest (Request {payer = payer; payee = payee; amount = amount; accepted = accepted})
let make_notif_friend friend accepted = FriendRequest (friend, accepted)
let add_friend acc friend = acc.friend_list <- friend :: acc.friend_list
let find_friend friend x = (x != friend)
let remove_friend acc friend = acc.friend_list <- List.filter (find_friend friend) acc.friend_list

let friend_list acc = acc.friend_list

let notif_type notif = match notif with 
| PaymentRequest n -> true
| FriendRequest n -> false