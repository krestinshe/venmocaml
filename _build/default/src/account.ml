(* for some reason OCaml kept raising errors for unused variables/values/fields
   so I couldn't "dune build" or test; this is to temporarily avoid that
   problem *)
[@@@warnerror "-unused-value-declaration"]
[@@@warnerror "-unused-field"]
[@@@warnerror "-unused-var-strict"]

(** [currency] is the type that represents a currency. *)
type currency =
  | USD
  | EUR
  | KRW
  | RMB
  | CAD

type amount = {
  number : int * int;
  currency : currency;
}
(** [amount] is the type that represents an amount of money.

    The [number] field is a pair (d, c), in which [d] is the number of whole
    units of currency (before the decimal point), and [c] is the number of
    "cents", i.e., units of currency / 100 (after the decimal point). The value
    represented by [number] is d + c / 100.

    The [currency] field represents the currency that the amount of money is in. *)

type t = {
  username : string;
  password : string;
  balance : amount;
}

exception InvalidUsername of string
exception IncorrectPassword
exception InvalidAmount of string
exception InvalidCurrency of string
exception InvalidDeposit of string
exception InvalidWithdrawal of string

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

(** [parse_amount s] parses a player's input into an [amount], as follows. The
    sequence of numbers before the "." in [s] becomes the first element of the
    [number] field. The sequence of numbers after the "." become the second
    element in the [number] field. The word after the space is translated into a
    [currency] type and becomes the [currency] field. Examples:

    - [parse_amount "0.00 USD"] is {number = (0, 0); currency = USD}
    - [parse_amount "10.99 CAN"] is {number = (10, 99); currency = CAN}.

    Requires: [s] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [InvalidAmount] if [s] does not represent a valid amount. A string
    representing a valid amount must contain the components, with nothing between:
      - a consecutive sequence of numeric characters, 
      - followed by one period, 
      - followed by up to 2 consecutive numeric characters, 
      - followed by one space, 
      - followed by a 3-character string that represents a valid currency 
        (i.e., is the name of a constructor of the [currency] type) 
    
    Examples: 
      - [parse_amount "0.00"] raises InvalidAmount "0.00"
      - [parse_amount "USD"] raises InvalidAmount "USD"
      - [parse_amount " "] raises InvalidAmount " "
      - [parse_amount "0   .   00 USD"] raises InvalidAmount "0   .   00 USD"
      - [parse_amount "0.00   CAD"] raises InvalidAmount "0.00   CAD"
      *)
let parse_amount (s : string) : amount =
  let first_split = String.split_on_char ' ' (String.trim s) in
  let second_split =
    match first_split with
    | [] -> raise (InvalidAmount s)
    | h :: t -> String.split_on_char '.' h @ t
  in
  match second_split with
  | [] | [ _ ] | [ _; _ ] | _ :: _ :: _ :: _ :: _ -> raise (InvalidAmount s)
  | [ d; c; curr ] -> (
      let d' = int_of_string d in
      let c' = int_of_string c in
      let amt = { number = (d', c'); currency = currency_of_string curr } in
      match amt with
      | amt ->
          if d' < 0 || c' < 0 || c' > 99 then raise (InvalidAmount s) else amt
      | exception Failure s -> raise (InvalidAmount s))

let unparse_amount (a : amount) : string =
  let number =
    match a.number with
    | a, b ->
        if b < 10 then string_of_int a ^ ".0" ^ string_of_int b ^ " "
        else string_of_int a ^ "." ^ string_of_int b ^ " "
  in
  number ^ string_of_currency a.currency

let make ?(balance = "0.00 USD") (username : string) (password : string) : t =
  { username; password; balance = parse_amount balance }

let username acc = acc.username
let balance acc = unparse_amount acc.balance
let display = raise (Failure "Unimplemented: Account.display")
let transaction = raise (Failure "Unimplemented: Account.transaction")
(* let print_info name info = name ^ ": " ^ info

   let rec print_list = function | [] -> () | h :: t -> print_endline h;
   print_list t

   let transaction acc = ["Transaction History" ; "Initial Value :" ^ (balance
   acc)] let display acc = print_endline "Account Information" in let _ =
   print_endline print_info "Account username" (username acc) in let _ =
   print_endline print_info "Balance" (balance acc) in print_endline print_list
   transaction acc*)

let deposit acc amt =
  let a = parse_amount amt in
  if a.currency = acc.balance.currency then
    let acc_num = acc.balance.number in
    let amt_num = a.number in
    if snd acc_num + snd amt_num >= 100 then
      {
        acc with
        balance =
          {
            number =
              (fst amt_num + fst acc_num + 1, snd amt_num + snd acc_num - 100);
            currency = a.currency;
          };
      }
    else
      {
        acc with
        balance =
          {
            number = (fst amt_num + fst acc_num, snd amt_num + snd acc_num);
            currency = a.currency;
          };
      }
  else raise (InvalidDeposit amt)

let withdraw acc amt =
  let a = parse_amount amt in
  if a.currency = acc.balance.currency then
    let acc_num = acc.balance.number in
    let amt_num = a.number in
    if snd acc_num - snd amt_num < 0 then
      {
        acc with
        balance =
          {
            number =
              (fst acc_num - fst amt_num - 1, snd acc_num - snd amt_num + 100);
            currency = a.currency;
          };
      }
    else
      {
        acc with
        balance =
          {
            number = (fst acc_num - fst amt_num, snd acc_num - snd amt_num);
            currency = a.currency;
          };
      }
  else raise (InvalidWithdrawal amt)
