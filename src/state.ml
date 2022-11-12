open Account

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

type t = {
  mutable current_account : Account.t option;
  mutable accounts : Account.t array;
  mutable transactions : transaction list;
}

exception InvalidUsername of string
exception IncorrectPassword

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

let make_payment st paying_acc_id paid_acc_id p =
  st.accounts.(paying_acc_id) <- withdraw st.accounts.(paying_acc_id) p;
  st.accounts.(paid_acc_id) <- deposit st.accounts.(paid_acc_id) p

let rec array_find a p n =
  if n > Array.length a - 1 then failwith "Not found"
  else if p a.(n) then n
  else array_find a p (n + 1)

let find_un st un = array_find st.accounts (fun a -> username a = un) 0

let execute_transaction st t =
  match t with
  | Pay { payer; payee; amount } ->
      (try check_username st payer
       with InvalidUsername payer -> failwith "Invalid payer username");
      (try check_username st payee
       with InvalidUsername payee -> failwith "Invalid payee username");
      make_payment st (find_un st payer) (find_un st payee) amount
  | Request { payer; payee; amount; accepted } ->
      (try check_username st payer
       with InvalidUsername payer -> failwith "Invalid payer username");
      (try check_username st payee
       with InvalidUsername payee -> failwith "Invalid payee username");
      if accepted then
        make_payment st (find_un st payer) (find_un st payee) amount
      else ()
