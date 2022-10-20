open Venmo
open Account
open State

let data_dir_prefix = "data" ^ Filename.dir_sep

(** [instruction] prints a menu of commands users can input to interact with the
    Venmo system and prompts the user to enter a command.

    As of now, requires user to input a valid command.

    Eventual goal: create src/command.ml and Command.parse_command to parse the
    commands *)
let instruction () =
  print_endline "Enter a command:";
  print_endline "Create a new account: create";
  print_endline "Log into an account: log in";
  print_endline "Make a deposit: deposit";
  print_endline "Pay an account: pay";
  print_endline "Request money from an account: request";
  print_string "> ";
  read_line ()

exception InvalidCommand of string

(** [create st] prompts the user to create an account from a file or by manually
    enter the username and password. If the username is associated with an
    existing account, an InvalidUsername exception is thrown. Otherwise, it
    prompts the user to create the password. It then displays the newly created
    account and adds it to the existing accounts. *)
let create st =
  print_endline
    "Would you like to create an account from a file or manually? [file/manual]";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "manual" ->
      print_endline "Choose a username:";
      print_string "> ";
      let un = read_line () in
      check_username st un;
      print_endline "Set a password:";
      print_string "> ";
      let pw = read_line () in
      (* print_endline "Confirm password"; print_string "> "; if read_line () =
         pw then *)
      let acc = create un pw in
      print_endline "Account successfully created!";
      display acc;
      add_account st acc
  | "file" -> (
      print_endline "Enter a file name:";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | file_name ->
          let acc =
            Venmo.Account.from_json
              (Yojson.Basic.from_file (data_dir_prefix ^ file_name ^ ".json"))
          in
          check_username st (username acc);
          print_endline "Account successfully created!";
          display acc;
          add_account st acc)
  | _ ->
      print_endline "Please enter a valid command (file/manual/back):";
      print_string "> "

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let _ = print_endline "Welcome to the VenmOCaml demo!" in
  let curr_st = ref Venmo.State.init_state in
  begin
    match String.trim (instruction ()) with
    | "create" -> create !curr_st
    | "log in" -> failwith "Unimplemented: Main.main"
    | "deposit" -> failwith "Unimplemented: Main.main"
    | "pay" -> failwith "Unimplemented: Main.main"
    | "request" -> failwith "Unimplemented: Main.main"
    | invalid_command -> raise (InvalidCommand invalid_command)
  end;
  failwith "Unimplemented: Main.main" (* game keeps going *)

(* Execute the game engine. *)
let () = main ()
