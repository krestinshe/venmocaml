open Venmo
open Account
open State

let data_dir_prefix = "data" ^ Filename.dir_sep
let running_id = ref 0

(* let check_esc = match read_line () with *)

(** [instruction] prints a menu of commands users can input to interact with the
    Venmo system and prompts the user to enter a command.

    As of now, requires user to input a valid command.

    Eventual goal: create src/command.ml and Command.parse_command to parse the
    commands *)
let start_instruction () =
  print_endline "Enter a command:";
  print_endline "Create a new account [create]";
  print_endline "Log into an account [log in]";
  print_endline "End session [end]";
  print_string "> ";
  read_line ()

let transaction_instruction () =
  print_endline "Enter a command:";
  print_endline "View balance [balance]";
  print_endline "View transaction history [hist]";
  print_endline "Make a deposit [deposit]";
  print_endline "Pay an account [pay]";
  print_endline "Request money from an account [request]";
  print_endline "Logout of an account [log out]";
  print_endline "End session [end]";
  print_string "> ";
  read_line ()

let full_instruction () =
  print_endline "Enter a command:";
  print_endline "Create a new account [create]";
  print_endline "Log into an account [log in]";
  print_endline "View balance [balance]";
  print_endline "View transaction history [hist]";
  print_endline "Make a deposit [deposit]";
  print_endline "Pay an account [pay]";
  print_endline "Request money from an account [request]";
  print_endline "Logout of an account [log out]";
  print_endline "End session [end]";
  print_string "> ";
  read_line ()

exception InvalidCommand of string
exception PasswordMismatch
exception InvalidPassword

let go_menu s = s = "go menu"

let verify_password pw =
  if
    (String.contains pw '!' || String.contains pw '@' || String.contains pw '#'
   || String.contains pw '&' || String.contains pw '*' || String.contains pw ' '
    )
    && pw <> String.lowercase_ascii pw
    && pw <> String.uppercase_ascii pw
    && (String.contains pw '1' || String.contains pw '2'
      || String.contains pw '3' || String.contains pw '4'
      || String.contains pw '5' || String.contains pw '6'
      || String.contains pw '7' || String.contains pw '1'
      || String.contains pw '8' || String.contains pw '9'
      || String.contains pw '0')
    && String.length pw >= 8
  then ()
  else raise InvalidPassword

let confirm_password pw pw_attempt =
  if pw_attempt = pw then () else raise PasswordMismatch

(** [create st] prompts the user to create an account from a file or by manually
    enter the username and password. If the username is associated with an
    existing account, an InvalidUsername exception is thrown. Otherwise, it
    prompts the user to create the password. It then displays the newly created
    account and adds it to the existing accounts. *)

let rec acc_menu (st : State.t) : unit =
  match String.trim (start_instruction ()) with
  | "create" -> create st
  | "log in" -> login st
  | "end" ->
      print_endline "🐫 Goodbye! 🐫";
      exit 0
  | invalid_command -> raise (InvalidCommand invalid_command)

and transaction_menu (st : State.t) : unit =
  match String.trim (transaction_instruction ()) with
  | "balance" -> balance st
  | "hist" -> display_hist st
  | "deposit" -> deposit st
  | "pay" -> pay st
  | "request" -> request st
  | "log out" -> logout st
  | "end" ->
      print_endline "🐫 Goodbye! 🐫";
      exit 0
  | invalid_command -> raise (InvalidCommand invalid_command)

and full_menu (st : State.t) : unit =
  match String.trim (full_instruction ()) with
  | "create" -> create st
  | "log in" -> login st
  | "balance" -> balance st
  | "hist" -> display_hist st
  | "deposit" -> deposit st
  | "pay" -> pay st
  | "request" -> request st
  | "log out" -> logout st
  | "end" ->
      print_endline "🐫 Goodbye! 🐫";
      exit 0
  | invalid_command -> raise (InvalidCommand invalid_command)

and create (st : State.t) =
  print_endline "To return to menu, type [go menu]";
  print_endline
    "Would you like to create an account from a file or manually? [file/manual]";
  print_string "> ";
  let line = read_line () in
  match line with
  | "go menu" -> acc_menu st
  | exception End_of_file -> ()
  | "file" -> (
      print_endline "Enter a file name:";
      print_string "> ";
      let line = read_line () in
      match line with
      | "go menu" -> acc_menu st
      | exception End_of_file -> ()
      | file_name ->
          (let acc =
             Venmo.Account.from_json
               (Yojson.Basic.from_file (data_dir_prefix ^ file_name ^ ".json"))
               !running_id
           in
           incr running_id;
           check_username st (username acc);
           print_endline "Account successfully created!";
           display acc;
           add_account st acc);
          full_menu st)
  | "manual" ->
      print_endline "Choose a username:";
      print_string "> ";
      let un = read_line () in
      if go_menu un then acc_menu st;
      check_username st un;
      print_endline
        "Set a password \n\
         (Must have no spaces, contain at least 8 characters, and include at \
         least 1 capital letter, 1 lower case letter, 1 number, and 1 special \
         character of !, @,\
        \ #, &, or *):";
      print_string "> ";
      let pw = read_line () in
      if go_menu pw then acc_menu st;
      verify_password pw;
      print_endline "Confirm password";
      print_string "> ";
      let pw' = read_line () in
      if go_menu pw' then acc_menu st;
      confirm_password pw pw';
      print_endline "Select a currency (USD, EUR, KRW, RMB, CAD, CML):";
      print_string ">";
      let home_curr = read_line () in
      if go_menu home_curr then acc_menu st;
      print_endline "Would you like to make an initial deposit? [yes/no]";
      print_string "> ";
      let init_deposit = read_line () in
      if go_menu init_deposit then acc_menu st;
      if init_deposit = "yes" then begin
        print_endline "Enter deposit amount of USD, EUR, KRW, RMB, CAD, CML:";
        print_string "> ";
        let init_bal = read_line () in
        if go_menu init_bal then acc_menu st;
        let acc =
          Account.create !running_id un pw ~balance:init_bal home_curr
        in
        incr running_id;
        print_endline "Account successfully created!";
        display acc;
        add_account st acc;
        full_menu st
      end
      else
        (* create a security question *)
        let acc = Account.create !running_id un pw home_curr in
        incr running_id;
        print_endline "Account successfully created!";
        display acc;
        add_account st acc;
        full_menu st
  | _ ->
      print_endline "Please enter a valid command [file/manual/back]:";
      print_string "> "

and balance st =
  print_string "Current balance: ";
  match current_account st with
  | None -> raise (InvalidCommand "Not logged in")
  | Some acc ->
      print_string (Account.balance acc);
      print_newline ();
      print_newline ();
      transaction_menu st

and deposit st =
  match current_account st with
  | None -> raise (InvalidCommand "Not logged in")
  | Some acc ->
      print_endline "To return to menu, type [go menu]";
      print_endline "Enter deposit amount of USD, EUR, KRW, RMB, CAD, CML:";
      print_string "> ";
      let amt = read_line () in
      if go_menu amt then acc_menu st;
      make_deposit st (username acc) amt;
      print_endline ("Amount " ^ amt ^ " has been deposited");
      print_newline ();
      balance st;
      print_newline ();
      transaction_menu st

and init_deposit st un : unit =
  print_endline "Enter deposit amount of USD, EUR, KRW, RMB, CAD, CML:";
  print_string "> ";
  let amt = read_line () in
  if go_menu amt then acc_menu st;
  make_deposit st un amt;
  print_endline ("Amount" ^ amt ^ "has been deposited");
  ()

and login st =
  print_endline "To return to menu, type [go menu]";
  print_endline "Enter username:";
  print_string "> ";
  let un = read_line () in
  if go_menu un then acc_menu st;
  print_endline "Enter password:";
  print_string "> ";
  let pw = read_line () in
  if go_menu pw then acc_menu st;
  login_system st un pw;
  print_endline ("Successfully logged in! Welcome back " ^ un);
  print_newline ();
  transaction_menu st

and logout st =
  State.logout st;
  print_endline "Successfully logged out!";
  print_newline ();
  acc_menu st

(*next ()*)
and display_hist st =
  match current_account st with
  | None -> raise (InvalidCommand "Not logged in")
  | Some acc ->
      display_history acc;
      transaction_menu st

and pay st =
  match current_account st with
  | None -> raise (InvalidCommand "Not logged in")
  | Some acc -> failwith "Unimplemented: Main.pay"

and request st =
  match current_account st with
  | None -> raise (InvalidCommand "Not logged in")
  | Some acc -> failwith "Unimplemented: Main.request"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_newline ();
  let _ = print_endline "🐫 Welcome to the VenmOCaml demo! 🐫" in
  let curr_st = ref Venmo.State.init_state in
  acc_menu !curr_st;
  print_endline ""

(* Execute the game engine. *)
let () = main ()