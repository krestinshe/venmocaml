open Venmo
open Account
open State

let data_dir_prefix = "data" ^ Filename.dir_sep

let instruction =
  print_endline "Enter a command:";
  print_endline "Create a new account: create";
  print_endline "Log into an account: log in";
  print_endline "Make a deposit: deposit";
  print_endline "Pay an account: pay";
  print_endline "Request money from an account: request";
  print_string "> ";
  read_line

let create st =
  print_endline
    "Would you like to create an account from a file or manually? [file/manual]";
  match read_line () with
  | exception End_of_file -> ()
  | "manual" ->
      print_endline "Choose a username:";
      print_string "> ";
      let un = read_line () in
      print_endline "Set a password:";
      print_string "> ";
      let pw = read_line () in
      let acc = make un pw in
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
          Venmo.Account.display acc;
          Venmo.State.add_account st acc)
  | _ ->
      print_endline "Please enter a valid command (file/manual/back):";
      print_string "> "

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to\n   the VenmOCaml demo!\n";
  let curr_st = ref Venmo.State.init_state in
  match String.trim (instruction ()) with
  | "create" -> create !curr_st
  | _ -> failwith "Unimplemented: Main.main"

(* Execute the game engine. *)
let () = main ()
