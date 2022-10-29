(* type transaction = *)

type t = {
  mutable current_account : Account.t option;
  mutable accounts : Account.t array;
  mutable transactions : string list;
      (* represent transactions with a different type? *)
}

exception InvalidUsername of string
exception IncorrectPassword

let init_state = { current_account = None; accounts = [||]; transactions = [] }
let current_account st = st.current_account
let accounts st = st.accounts
let add_account st acc = st.accounts <- Array.append [| acc |] st.accounts

let check_username st un =
  let username_list =
    Array.map (fun acc -> Account.username acc) (accounts st)
  in
  if Array.mem un username_list then raise (InvalidUsername un)
