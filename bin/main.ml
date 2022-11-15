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
let instruction () =
  print_endline "Enter a command:";
  print_endline "Create a new account [create]";
  print_endline "Log into an account [log in]";
  print_endline "View balance [balance]";
  print_endline "Make a deposit [deposit]";
  print_endline "Pay an account [pay]";
  print_endline "Request money from an account [request]";
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
let menu (st : State.t) : unit =
  match String.trim (instruction ()) with
  | "create" -> failwith "Unimplemented create" (*create st*)
  | "log in" -> failwith "Unimplemented: Main.main" (*login*)
  | "balance" -> failwith "Unimplemented: Main.main" (*balance*)
  | "deposit" -> failwith "Unimplemented: Main.main" (*deposit*)
  | "pay" -> failwith "Unimplemented: Main.main"
  | "request" -> failwith "Unimplemented: Main.main"
  | "end" -> print_endline "Goodbye!"
  | invalid_command -> raise (InvalidCommand invalid_command)

let balance st =
  print_string "Current balance: ";
  match current_account st with
  | None -> raise (InvalidCommand "Not logged in")
  | Some acc -> print_string (balance acc)

let deposit st =
  print_endline "Enter username to proceed: ";
  print_string "> ";
  let un = read_line () in
  balance st;
  print_endline "Enter deposit amount of USD, EUR, KRW, RMB, CAD, CML:";
  print_string "> ";
  let amt = read_line () in
  make_deposit st un amt;
  print_endline ("Amount" ^ amt ^ "has been deposited");
  balance st
(*next ()*)

let init_deposit st un : unit =
  print_endline "Enter deposit amount of USD, EUR, KRW, RMB, CAD, CML:";
  print_string "> ";
  let amt = read_line () in
  make_deposit st un amt;
  print_endline ("Amount" ^ amt ^ "has been deposited");
  ()

let create (st : State.t) =
  print_endline "To return to menu, type [go menu]";
  (* insert go_menu when working*)
  print_endline
    "Would you like to create an account from a file or manually? [file/manual]";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "file" -> (
      print_endline "Enter a file name:";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | file_name ->
          let acc =
            Venmo.Account.from_json
              (Yojson.Basic.from_file (data_dir_prefix ^ file_name ^ ".json"))
              !running_id
          in
          incr running_id;
          check_username st (username acc);
          print_endline "Account successfully created!";
          display acc;
          add_account st acc)
  | "manual" ->
      print_endline "Choose a username:";
      print_string "> ";
      let un = read_line () in
      if go_menu un then menu st;
      check_username st un;
      print_endline
        "Set a password \n\
         (Must have no spaces, contain at least 8 characters, and include at \
         least 1 capital letter, 1 lower case letter, 1 number, and 1 special \
         character of !, @,\
        \ #, &, or *):";
      print_string "> ";
      let pw = read_line () in
      verify_password pw;
      print_endline "Confirm password";
      print_string "> ";
      let pw' = read_line () in
      confirm_password pw pw';
      print_endline "Select a currency (USD, EUR, KRW, RMB, CAD, CML):";
      print_string ">";
      let home_curr = read_line () in
      print_endline "Would you like to make an initial deposit? [yes/no]";
      print_string "> ";
      let init_deposit = read_line () in
      if init_deposit = "yes" then begin
        print_endline "Enter deposit amount of USD, EUR, KRW, RMB, CAD, CML:";
        print_string "> ";
        let init_bal = read_line () in
        let acc = create !running_id un pw ~balance:init_bal home_curr in
        incr running_id;
        print_endline "Account successfully created!";
        display acc;
        add_account st acc
      end
      else
        (* create a security question *)
        let acc = create !running_id un pw home_curr in
        incr running_id;
        print_endline "Account successfully created!";
        display acc;
        add_account st acc
  | _ ->
      print_endline "Please enter a valid command [file/manual/back]:";
      print_string "> "

(*next ()*)

let login st =
  print_endline "To return to menu, type [go menu]";
  print_endline "Enter username:";
  print_string "> ";
  let un = read_line () in
  print_endline "Enter password:";
  print_string "> ";
  let pw = read_line () in
  login_system st un pw;
  print_endline ("Successfully logged in! Welcome back " ^ un)
(*next ()*)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let _ = print_endline "Welcome to the VenmOCaml demo!" in
  let curr_st = ref Venmo.State.init_state in
  begin
    match String.trim (instruction ()) with
    | "create" -> create !curr_st
    | "log in" -> login !curr_st
    | "balance" -> balance !curr_st
    | "deposit" -> deposit !curr_st
    | "pay" -> failwith "Unimplemented: Main.main"
    | "request" -> failwith "Unimplemented: Main.main"
    | "end" -> print_endline "Goodbye!"
    | invalid_command -> raise (InvalidCommand invalid_command)
  end;
  print_endline ""
(* game keeps going *)

(*failwith "Unimplemented: Main.main"*)

(* Execute the game engine. *)
let () = main ()
