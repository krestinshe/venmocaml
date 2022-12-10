open Account

exception InvalidUsername of string
exception IncorrectPassword
exception InvalidTransaction

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
      account : string;
      amount : string;
    }
  | Withdraw of {
      account : string;
      amount : string;
    }

let string_of_transaction (t : transaction) : string =
  match t with
  | Deposit { account; amount } -> "Deposited " ^ amount
  | Withdraw { account; amount } -> "Withdrew " ^ amount
  | Pay { payer; payee; amount } -> "Paid " ^ amount ^ " to " ^ payee
  | Request { payer; payee; amount } -> "Requested " ^ amount ^ " from " ^ payer

(** [transaction_of_string s] parses a string that describes a transaction into
    a [transaction]. Input: A string that represents a valid transaction. A
    string representing a valid transaction must contain the following words
    (and no more), separated by spaces, in the following order:

    - the name of the acting account (paying, requesting, depositing,
      withdrawing)
    - a word describing the transaction type (one of "deposited", "withdrew",
      "paid", or "requested")
    - 2 words representing a valid amount
    - and 0 or 2 words representing the other party in a transaction:
    - ... if the second word was "paid", then the word "to" followed by a valid
      username representing the payee
    - ... if the second word was "requested", then the word "from" followed by a
      valid username representing the payer (requestee)
    - ... if the second word was "deposited" or "withdrew", then 0 words

    Raises: [InvalidTransaction] if [s] does not represent a valid transaction.

    Examples:

    - [transaction_of_string "user deposited 5.00 USD"] is
      [Deposit {account = "user"; amount = "5.00 USD"}]
    - [transaction_of_string "user deposited 5.00USD"] raises
      [InvalidTransaction]
    - [transaction_of_string "user withdrew 311.0 CML"] is
      [Withdraw {account = "user"; amount = "311.00 CML"}]
    - [transaction_of_string "user paid 30.00 CAD to user1"] is
      [Pay {payer = "user"; payee = "user1"; amount = "30.00 CAD"}]
    - [transaction_of_string "user paid 30.00 CAD user1"] raises
      [InvalidTransaction]
    - [transaction_of_string "user requested 30.00 CAD from user1"] is
      [Request {payer = "user1"; payee = "user"; amount = "30.00 CAD"; accepted = false}]
    - [transaction_of_string "user requested 30.00 CAD to user1"] raises
      [InvalidTransaction]
    - [transaction_of_string "user deposited 5.00 USD to user1"] raises
      [InvalidTransaction]
    - [transaction_of_string "deposited 5.00 USD"] raises [InvalidTransaction]*)
let transaction_of_string (s : string) : transaction =
  let split = String.split_on_char ' ' (String.trim s) in
  match split with
  | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] -> raise InvalidTransaction
  | un :: action :: number :: curr :: tail -> (
      let amt = String.trim (String.trim number ^ " " ^ String.trim curr) in
      let un = String.trim un in
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

type t = {
  mutable current_account : Account.t option;
  mutable accounts : Account.t array;
  mutable transactions : transaction list;
} 

let init_state = { current_account = None; accounts = [||]; transactions = [] }
let current_account st = st.current_account
let accounts st = st.accounts
let transactions st = st.transactions
let add_account st acc = st.accounts <- Array.append [| acc |] st.accounts

let delete_account st id =
  st.accounts.(id) <- Account.deactivate st.accounts.(id)

let check_username st un =
  let username_list =
    Array.map (fun acc -> Account.username acc) (accounts st)
  in
  if Array.mem un username_list then raise (InvalidUsername un)

 let rec array_find a p n =
      if n > Array.length a - 1 then failwith "Not found"
      else if p a.(n) then n
      else array_find a p (n + 1)
    
let find_un st un = array_find st.accounts (fun a -> username a = un) 0

let find_account st username = st.accounts.(find_un st username)

let make_payment st paying_acc_id paid_acc_id p =
  st.accounts.(find_un st paying_acc_id) <- withdraw st.accounts.(find_un st paying_acc_id) p;
  st.accounts.(find_un st paid_acc_id) <- deposit st.accounts.(find_un st paid_acc_id) p;
  st.current_account <- Some st.accounts.(find_un st paying_acc_id)



let make_deposit st un p =
  st.accounts.(find_un st un) <- deposit st.accounts.(find_un st un) p;
  st.current_account <- Some (st.accounts.(find_un st un)) 

let to_json st : Yojson.Basic.t =
  `Assoc
    [
      ( "current account",
        `String
          (match current_account st with
          | None -> "none"
          | Some acc -> username acc) );
      ("accounts", `List Array.(to_list (map Account.to_json (accounts st))));
      ( "transactions",
        `List
          (List.map
             (fun t -> `String (string_of_transaction t))
             (transactions st)) );
    ]

let to_file st =
  let file = "data/data.json" in
  let oc = open_out file in
  Yojson.Basic.to_channel oc (to_json st);
  close_out oc

let execute_transaction st t =
  (match t with
  | Pay { payer; payee; amount } ->
      (try check_username st payer
       with InvalidUsername payer -> failwith "Invalid payer username");
      (try check_username st payee
       with InvalidUsername payee -> failwith "Invalid payee username");
      make_payment st (payer) (payee) amount;
      st.transactions <- t :: st.transactions
  | Request { payer; payee; amount; accepted } ->
      (try check_username st payer
       with InvalidUsername payer -> failwith "Invalid payer username");
      (try check_username st payee
       with InvalidUsername payee -> failwith "Invalid payee username");
      if accepted then (
        make_payment st (payer) (payee) amount;
        st.transactions <- t :: st.transactions)
      else ()
  | Deposit { account; amount } ->
      (try check_username st account
       with InvalidUsername payer -> failwith "Invalid username");
      make_deposit st account amount;
      st.transactions <- t :: st.transactions
  | Withdraw { account; amount } ->
      (try check_username st account
       with InvalidUsername payer -> failwith "Invalid username");
      st.accounts.(find_un st account) <-
        withdraw st.accounts.(find_un st account) amount;
      st.transactions <- t :: st.transactions);
  to_file st

let logout st = st.current_account <- None
let acc st un = (accounts st).(find_un st un)
let login_state st un pass = Account.check_password pass (acc st un)

let login_system st un pass =
  if login_state st un pass then st.current_account <- Some (acc st un)

let add_notif_inbox st payer notif =
  Account.add_notification st.accounts.(find_un st payer) notif

 let current acc = match acc with 
 | Some s -> s
 | None -> failwith "current_account doesn't exist"

let add_friend_state st friend = add_friend (current (current_account st)) (username (find_account st friend)) 

let remove_friend_state st friend = remove_friend (current (current_account st)) (username (find_account st friend))