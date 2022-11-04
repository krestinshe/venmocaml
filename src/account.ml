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

type t = {
  id : int;
  username : string;
  password : string;
  balance : amount;
  home_currency : currency;
  history : string list;
}

exception InvalidAmount of string
exception InvalidCurrency of string
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
  if c = EUR then { number = n; currency = USD }
  else if c = KRW then { number = n *. 0.0007; currency = USD }
  else if c = RMB then { number = n *. 0.14; currency = USD }
  else if c = CAD then { number = n *. 0.73; currency = USD }
  else if c = CML then { number = n /. 3110.; currency = USD }
  else raise InvalidConversion

(*[to_eur n c] converts number [n] of currency [c] to an EUR amount*)
let to_eur n c =
  if c = USD then { number = n; currency = EUR }
  else if c = KRW then { number = n *. 0.00071; currency = EUR }
  else if c = RMB then { number = n *. 0.14; currency = EUR }
  else if c = CAD then { number = n *. 0.74; currency = EUR }
  else if c = CML then { number = n /. 3110.; currency = EUR }
  else raise InvalidConversion

(*[to_krw n c] converts number [n] of currency [c] to an KRW amount*)
let to_krw n c =
  if c = USD then { number = n *. 1422.; currency = KRW }
  else if c = EUR then { number = n *. 1417.09; currency = KRW }
  else if c = RMB then { number = n *. 196.07; currency = KRW }
  else if c = CAD then { number = n *. 1041.84; currency = KRW }
  else if c = CML then { number = n *. 1422. /. 3110.; currency = KRW }
  else raise InvalidConversion

(*[to_rmb n c] converts number [n] of currency [c] to an RMB amount*)
let to_rmb n c =
  if c = USD then { number = n *. 7.25; currency = RMB }
  else if c = EUR then { number = n *. 7.23; currency = RMB }
  else if c = KRW then { number = n *. 0.0051; currency = RMB }
  else if c = CAD then { number = n *. 5.31; currency = RMB }
  else if c = CML then { number = n *. 7.25 /. 3110.; currency = RMB }
  else raise InvalidConversion

(*[to_cad n c] converts number [n] of currency [c] to an CAD amount*)
let to_cad n c =
  if c = USD then { number = n *. 1.36; currency = CAD }
  else if c = EUR then { number = n *. 1.36; currency = CAD }
  else if c = KRW then { number = n *. 0.00096; currency = CAD }
  else if c = RMB then { number = n *. 0.19; currency = CAD }
  else if c = CML then { number = n *. 1.36 /. 3110.; currency = CAD }
  else raise InvalidConversion

let to_cml n c =
  if c = USD then { number = n *. 3110.; currency = CML }
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
      - [parse_amount "0.00 USD"] is {number = 0.; currency = USD}
      - [parse_amount "10.99 CAD"] is {number = 10.99; currency = CAD}
      - [parse_amount "0.00   CAD"] is {number = 0.; currency = CAD}
      - [parse_amount "0.00"] is {number = 0.; currency = USD}
      - [parse_amount "USD"] raises InvalidAmount "USD"
      - [parse_amount " "] raises InvalidAmount " "
      - [parse_amount "0   .   00 USD"] raises InvalidAmount "0   .   00 USD"
  *)

(*[round_num n] rounds [n] to the hundredths place and is used in displaying the
  balance amount*)
let round_num (n : float) : float = Float.round (n *. 100.) /. 100.

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

(* let first_split = String.split_on_char ' ' (String.trim s) in let
   second_split = match first_split with | [] -> raise (InvalidAmount s) | h ::
   t -> String.split_on_char '.' h @ t in match second_split with | [] | [ _ ] |
   [ _; _ ] | _ :: _ :: _ :: _ :: _ -> raise (InvalidAmount s) | [ d; c; curr ]
   -> ( let d' = int_of_string d in let c' = int_of_string c in let amt = {
   number = (d', c'); currency = currency_of_string curr } in match amt with |
   amt -> if d' < 0 || c' < 0 || c' > 99 then raise (InvalidAmount s) else amt |
   exception Failure s -> raise (InvalidAmount s))*)

let unparse_amount (a : amount) : string =
  string_of_float a.number ^ " " ^ string_of_currency a.currency

let from_json j id =
  let bal =
    if j |> member "balance" = `Null then { number = 0.00; currency = USD }
    else j |> member "balance" |> to_string |> parse_amount
  in
  {
    id;
    username = j |> member "username" |> to_string;
    password = j |> member "password" |> to_string;
    balance = bal;
    home_currency =
      j |> member "home currency" |> to_string |> currency_of_string;
    history = j |> member "history" |> to_list |> List.map to_string;
  }

let create ?(balance = "0.00 USD") (home_curr : string) (id : int)
    (username : string) (password : string) : t =
  {
    id;
    username;
    password;
    balance = parse_amount balance;
    home_currency = currency_of_string home_curr;
    history = [ "Transaction History"; "Initial Value :" ^ "0.00 USD" ];
  }

let username acc = acc.username
let password acc = acc.password
let balance acc = unparse_amount acc.balance
let print_info name info = name ^ ": " ^ info

let rec print_list = function
  | [] -> ()
  | h :: t ->
      print_endline h;
      print_list t

let history acc = acc.history

let display acc =
  print_string "Account Information";
  print_newline ();
  print_string (print_info "Account username" (username acc));
  print_newline ();
  print_string (print_info "Balance" (balance acc));
  print_newline ();
  print_newline ();
  print_list (history acc)

let deposit acc amt =
  let a = parse_amount amt in
  if a.currency = acc.balance.currency then
    let acc_num = acc.balance.number in
    let amt_num = a.number in
    {
      acc with
      balance = { number = acc_num +. amt_num; currency = a.currency };
    } (*history = history acc @ [ "Made Deposit: " ^ amt ]*)
  else raise (InvalidDeposit amt)

let withdraw acc amt =
  let a = parse_amount amt in
  if a.currency = acc.balance.currency then
    let acc_num = acc.balance.number in
    let amt_num = a.number in
    {
      acc with
      balance = { number = acc_num -. amt_num; currency = a.currency };
    } (*history = history acc @ [ "Withdrawal: " ^ amt ];*)
  else raise (InvalidWithdrawal amt)
