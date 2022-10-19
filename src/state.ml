type t = {
  mutable current_account : Account.t option;
  mutable accounts : Account.t list;
  mutable transactions : string list;
      (* represent transactions with a different type? *)
}

exception InvalidUsername of string
exception IncorrectPassword

let init_state = { current_account = None; accounts = []; transactions = [] }
let current_account st = st.current_account
let accounts st = st.accounts
let add_account st acc = st.accounts <- acc :: st.accounts

let check_username st un =
  let username_list =
    List.map (fun acc -> Account.username acc) (accounts st)
  in
  if List.mem un username_list then raise (InvalidUsername un)
