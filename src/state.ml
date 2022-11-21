open Account

type t = {
  mutable current_account : Account.t option;
  mutable accounts : Account.t array;
      (* mutable transactions : Account.transaction list; *)
}

exception InvalidUsername of string
exception IncorrectPassword

let init_state =
  { current_account = None; accounts = [||] (*transactions = []*) }

let current_account st = st.current_account
let accounts st = st.accounts

(* let transactions st = st.transactions *)
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

let make_deposit st un p =
  st.accounts.(find_un st un) <- deposit st.accounts.(find_un st un) p

(* let execute_transaction st t = match t with | Pay { payer; payee; amount } ->
   (try check_username st payer with InvalidUsername payer -> failwith "Invalid
   payer username"); (try check_username st payee with InvalidUsername payee ->
   failwith "Invalid payee username"); make_payment st (find_un st payer)
   (find_un st payee) amount | Request { payer; payee; amount; accepted } ->
   (try check_username st payer with InvalidUsername payer -> failwith "Invalid
   payer username"); (try check_username st payee with InvalidUsername payee ->
   failwith "Invalid payee username"); if accepted then make_payment st (find_un
   st payer) (find_un st payee) amount else () *)

let logout st = st.current_account <- None
let acc st un = (accounts st).(find_un st un)
let login_state st un pass = Account.check_password pass (acc st un)

let login_system st un pass =
  if login_state st un pass then st.current_account <- Some (acc st un)

(********************************************************************** Writing
  to json
  **********************************************************************)
let to_json st : Yojson.Basic.t =
  `Assoc
    [
      ( "current account",
        `String
          (match current_account st with
          | None -> "none"
          | Some acc -> username acc) );
      ("accounts", `List Array.(to_list (map Account.to_json (accounts st))))
      (* ("transactions", ) *);
    ]

let test = true

let to_file st =
  let file = if test then "data/test_data.json" else "data/data.json" in
  let oc = open_out file in
  Yojson.Basic.to_channel oc (to_json st);
  close_out oc

let add_notif_inbox st payer notif =
  Account.add_notification st.accounts.(find_un st payer) notif

(** notification inbox : notification inbox exist in account.ml t type.
    Whenever, the account makes a request, it returns a notification and this
    notification will be added to the payer's notification_inbox. For now, the
    user can only clear all of the requests in the inbox *)

(**
    notification inbox :
     notification inbox exist in account.ml t type. Whenever, the account makes a request, 
     it returns a notification and this notification will be added to the payer's notification_inbox. 
     For now, the user can only clear all of the requests in the inbox

*)