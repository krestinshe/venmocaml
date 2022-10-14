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
  raise (Failure "Unimplemented: Account.unparse_amount")

let make ?(balance = "0.00 USD") (username : string) (password : string) : t =
  { username; password; balance = parse_amount balance }

let username acc = acc.username
let balance acc = raise (Failure "Unimplemented: Account.balance")
let display acc = raise (Failure "Unimplemented: Account.display")