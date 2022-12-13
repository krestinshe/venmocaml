open Venmo
open Account
open State

let data_dir_prefix = "data" ^ Filename.dir_sep
let running_id = ref 0

(** [start_instruction ()] prints a menu of commands users can input to interact
    with the Venmo system and prompts the user to enter a command.*)
let start_instruction () =
  print_endline "\nEnter a command:";
  print_endline "Create a new account [create]";
  print_endline "Log into an account [log in]";
  print_endline "End session [end]";
  print_string "> ";
  read_line ()

let transaction_instruction st =
  print_endline
    ("\nCurrently logged in as " ^ username (current (current_account st)));
  print_endline "\nEnter a command:";
  print_endline "View balance [balance]";
  print_endline "View transaction history [hist]";
  print_endline "Make a deposit [deposit]";
  print_endline "Pay an account [pay]";
  print_endline "Request money from an account [request]";
  print_endline "View notification inbox [notification inbox]";
  print_endline "Send follow request [search friend]";
  print_endline "Friend activities [friend]";
  print_endline "Receive/send messages [message]";
  print_endline "Log out [log out]";
  print_endline "End session [end]";
  print_string "> ";
  read_line ()

let full_instruction () =
  print_endline "\nEnter a command:";
  print_endline "Create a new account [create]";
  print_endline "Log into an account [log in]";
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

let rec acc_menu (st : State.t) : unit =
  match String.trim (start_instruction ()) with
  | "create" -> create st
  | "log in" -> login st
  | "end" ->
      print_endline "🐫 Goodbye! 🐫";
      exit 0
  | invalid_command ->
      print_endline "Invalid command!";
      acc_menu st

and transaction_menu (st : State.t) : unit =
  match String.trim (transaction_instruction st) with
  | "balance" -> balance st
  | "hist" -> display_hist st
  | "deposit" -> deposit st
  | "pay" -> pay st
  | "request" -> request st
  | "notification inbox" -> notification_inbox st
  | "search friend" -> search_friend st
  | "friend" -> friend_list_acc st
  | "message" -> message st
  | "log out" -> logout st
  | "end" ->
      print_endline "🐫 Goodbye! 🐫";
      exit 0
  | invalid_command ->
      print_endline "Invalid command!";
      transaction_menu st

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
      to_file st;
      exit 0
  | invalid_command ->
      print_endline "Invalid command!";
      acc_menu st

and set_password (st : State.t) =
  try
    print_endline
      "\n\
       Set a password \n\
       (Must have no spaces, contain at least 8 characters, and include at \
       least 1 capital letter, 1 lower case letter, 1 number, and 1 special \
       character of !, @,\
      \ #, &, or *):";
    print_string "> ";
    let pw = read_line () in
    if go_menu pw then acc_menu st;
    verify_password pw;
    print_endline "\nConfirm password: ";
    print_string "> ";
    let pw' = read_line () in
    if go_menu pw' then acc_menu st;
    confirm_password pw pw';
    pw
  with
  | InvalidPassword ->
      print_endline "\nYour password does not meet the security requirements!";
      set_password st
  | PasswordMismatch ->
      print_endline "\nYour passwords do not match!";
      set_password st
  | _ ->
      print_endline "Error!";
      set_password st

and set_init_bal st =
  try
    print_endline
      "\n\
       Enter a deposit amount in USD, EUR, KRW, RMB, CAD, CML (e.g., 10 USD, \
       31.10 CML):";
    print_string "> ";
    let init_bal = read_line () in
    if go_menu init_bal then acc_menu st;
    init_bal
  with
  | InvalidAmount s ->
      print_endline (s ^ "is an invalid amount!");
      set_init_bal st
  | InvalidCurrency s ->
      print_endline (s ^ "is an invalid currency!");
      set_init_bal st
  | _ ->
      print_endline "Invalid amount!";
      set_init_bal st

and set_home_curr st =
  print_endline "\nSelect a home currency (USD, EUR, KRW, RMB, CAD, CML):";
  print_string "> ";
  let home_curr = read_line () in
  if go_menu home_curr then acc_menu st;
  try
    match String.uppercase_ascii home_curr with
    | "USD" | "EUR" | "KRW" | "RMB" | "CAD" | "CML" -> home_curr
    | _ -> raise (InvalidCurrency home_curr)
  with
  | InvalidCurrency s ->
      print_endline (s ^ "is an invalid currency!");
      set_home_curr st
  | _ ->
      print_endline "Error!";
      set_home_curr st

and create (st : State.t) =
  try
    print_endline "To return to menu, type [go menu]";
    print_endline
      "Would you like to create an account from a file or manually? \
       [file/manual]";
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
                 (Yojson.Basic.from_file
                    (data_dir_prefix ^ file_name ^ ".json"))
                 !running_id
             in
             incr running_id;
             check_username st (username acc);
             print_endline "Account successfully created!";
             print_endline (display acc);
             add_account st acc);
            acc_menu st)
    | "manual" ->
        print_endline "Choose a username:";
        print_string "> ";
        let un = read_line () in
        if go_menu un then acc_menu st;
        check_username st un;

        let pw = set_password st in

        let home_curr = set_home_curr st in

        print_endline "\nWould you like to make an initial deposit? [yes/no]";
        print_string "> ";
        let init_deposit = read_line () in
        if go_menu init_deposit then acc_menu st;
        if init_deposit = "yes" then begin
          let init_bal = set_init_bal st in
          let acc =
            Account.create !running_id un pw ~balance:init_bal home_curr
          in
          incr running_id;
          print_endline "Account successfully created!";
          print_endline (display acc);
          add_account st acc;
          add_transaction acc (deposit_transaction acc init_bal);
          acc_menu st
        end
        else
          let acc = Account.create !running_id un pw home_curr in
          incr running_id;
          print_endline "Account successfully created!";
          print_endline (display acc);
          add_account st acc;
          acc_menu st
    | _ ->
        print_endline "Invalid command!";
        create st
  with
  | InvalidAmount s ->
      print_endline (s ^ " is an invalid amount!");
      create st
  | InvalidCurrency s ->
      print_endline (s ^ " is an invalid currency!");
      create st

and balance st =
  try
    print_string "Current balance: ";
    match current_account st with
    | None -> not_logged_in st
    | Some acc ->
        print_string (Account.balance acc);
        print_newline ();
        print_newline ();
        transaction_menu st
  with _ ->
    print_endline "Error!";
    balance st

and deposit st =
  try
    match current_account st with
    | None -> not_logged_in st
    | Some acc ->
        print_endline "\nTo return to menu, type [go menu]";
        print_endline
          "Enter a deposit amount in USD, EUR, KRW, RMB, CAD, CML (e.g., 10 \
           USD, 31.10 CML):";
        print_string "> ";
        let amt = read_line () in
        if go_menu amt then transaction_menu st;
        make_deposit st (username acc) amt;
        add_transaction
          (current (current_account st))
          (deposit_transaction acc amt);
        print_endline ("Amount " ^ amt ^ " has been deposited");
        print_newline ();
        balance st;
        print_newline ();
        transaction_menu st
  with
  | InvalidAmount s ->
      print_endline (s ^ "is an invalid amount!");
      deposit st
  | InvalidCurrency s ->
      print_endline (s ^ "is an invalid currency!");
      deposit st
  | _ ->
      print_endline "Error!";
      deposit st

and init_deposit st un : unit =
  print_endline
    "\n\
     Enter a deposit amount in USD, EUR, KRW, RMB, CAD, CML (e.g., 10 USD, \
     31.10 CML):";
  print_string "> ";
  let amt = read_line () in
  if go_menu amt then acc_menu st;
  make_deposit st un amt;
  print_endline ("Amount" ^ amt ^ "has been deposited");
  ()

and login st =
  try
    print_endline "\nTo return to menu, type [go menu]";
    print_endline "\nEnter username:";
    print_string "> ";
    let un = read_line () in
    if go_menu un then acc_menu st;
    print_endline "\nEnter password:";
    print_string "> ";
    let pw = read_line () in
    if go_menu pw then acc_menu st;
    login_system st un pw;
    print_endline ("Successfully logged in! Welcome back " ^ un);
    print_newline ();
    transaction_menu st
  with
  | IncorrectPassword ->
      print_endline "Incorrect password!";
      login st
  | Failure _ ->
      print_endline "Account does not exist!";
      login st
  | _ ->
      print_endline "Error!";
      login st

and logout st =
  State.logout st;
  print_endline "Successfully logged out!";
  print_newline ();
  acc_menu st

and display_hist st =
  match current_account st with
  | None -> not_logged_in st
  | Some acc ->
      print_endline (display_history (current (current_account st)));
      transaction_menu st

and friend_list_acc st =
  match current_account st with
  | None -> not_logged_in st
  | Some acc ->
      print_endline (display_friends (current (current_account st)));
      print_newline ();
      print_endline
        "\n\
         Would you like to remove a friend or see your friends' activity? \
         [remove/activity]";
      let select = read_line () in
      if select = "activity" then begin
        print_endline "\nType the username of your friend";
        let username = read_line () in
        if List.mem username (friend_list acc) = false then begin
          print_endline (username ^ " doesn't exist in your friend list");
          transaction_menu st
        end
        else begin
          print_newline ();
          print_endline (display_history (find_account st username));
          print_newline ();
          transaction_menu st
        end
      end
      else if select = "remove" then begin
        print_endline "\nType the username of your friend";
        let friend = read_line () in
        if List.mem friend (friend_list acc) = false then begin
          print_endline (friend ^ " doesn't exist in your friend list");
          transaction_menu st
        end
        else begin
          print_newline ();
          remove_friend_state st friend;
          remove_friend (find_account st friend)
            (username (current (current_account st)));
          print_endline ("You removed " ^ friend ^ " as a friend");
          print_newline ();
          transaction_menu st
        end
      end
      else print_endline "Invalid command";
      transaction_menu st

and pay st =
  try
    match current_account st with
    | None -> not_logged_in st
    | Some acc ->
        print_endline "\nTo return to menu, type [go menu]";
        print_endline
          "Enter the username of the user that you want to pay money";
        print_string "> ";
        let payee = read_line () in
        if go_menu payee then transaction_menu st;
        print_endline
          "\n\
           Enter a paying amount in USD, EUR, KRW, RMB, CAD, CML (e.g., 10 \
           USD, 31.10 CML):";
        print_string "> ";
        let amt = read_line () in
        if go_menu amt then transaction_menu st;
        make_payment st (username acc) payee amt;
        add_transaction
          (current (current_account st))
          (pay_transaction acc payee amt);
        add_transaction (find_account st payee)
          (received_transaction (username acc) amt);
        print_endline ("Amount " ^ amt ^ " has been paid to " ^ payee);
        print_newline ();
        balance st;
        print_newline ();
        transaction_menu st
  with
  | InvalidAmount _ | InvalidCurrency _ ->
      print_endline "Invalid amount!";
      pay st
  | Failure _ ->
      print_endline "User not found!";
      pay st
  | InsufficientBalance ->
      print_endline "Insufficient balance!";
      pay st

and request st =
  try
    match current_account st with
    | None -> not_logged_in st
    | Some acc ->
        print_endline "\nTo return to menu, type [go menu]";
        print_endline
          "Enter the username of the user from whom you want to request money: ";
        print_string "> ";
        let payer = read_line () in
        print_endline
          "\n\
           Enter an amount in USD, EUR, KRW, RMB, CAD, CML (e.g., 10 USD, \
           31.10 CML):";
        print_string "> ";
        let amt = read_line () in
        if go_menu amt then acc_menu st;
        add_notif_inbox st payer (make_request acc payer amt);
        print_endline ("You sent a request to username [" ^ payer ^ "]");
        transaction_menu st
  with
  | InvalidAmount _ ->
      print_endline "Invalid amount!";
      request st
  | Failure _ ->
      print_endline "User not found!";
      request st

and notification_inbox st =
  match current_account st with
  | None -> not_logged_in st
  | Some acc ->
      print_endline
        "\n\
         Would you like to display or go over and respond to your \
         notifications? [display/go over]";
      print_string "> ";
      let command = read_line () in
      if command = "display" then begin
        if command = "display" then begin
          print_endline (display_notif acc);
          print_endline "\nWould you like to clear your inbox? [yes/no]";
          print_string "> ";
          let answer = read_line () in
          if answer = "yes" then begin
            notif_clear acc;
            transaction_menu st
          end
          else if answer != "no" then transaction_menu st
          else print_endline "Invalid answer!";
          transaction_menu st
        end
      end
      else if command = "go over" then begin
        let i = ref 0 in
        let new_inbox = ref [] in
        while !i < length_notif acc do
          if notif_accepted (List.nth (notif_inbox acc) !i) = false then begin
            print_endline (string_of_notif (List.nth (notif_inbox acc) !i));
            print_newline ();
            print_endline
              "\nWould you like to accept the payment/friend request? [yes/no]";
            print_string "> ";
            let answer = read_line () in
            let notif = List.nth (notif_inbox acc) !i in
            if answer = "yes" then
              if List.mem (notif_payer notif) (friend_list acc) then begin
                print_endline
                  ("You are already following user " ^ notif_payer notif);
                transaction_menu st
              end
              else if notif_type notif then begin
                State.make_payment st (notif_payer notif) (notif_payee notif)
                  (notif_amount notif);
                add_transaction
                  (current (current_account st))
                  (pay_transaction acc (notif_payee notif) (notif_amount notif));
                add_transaction
                  (find_account st (notif_payee notif))
                  (received_transaction (username acc) (notif_amount notif));
                new_inbox :=
                  make_notif (notif_payer notif) (notif_payee notif)
                    (notif_amount notif) true
                  :: !new_inbox;
                i := !i + 1
              end
              else begin
                add_friend_state st (notif_payer notif);
                add_friend
                  (find_account st (notif_payer notif))
                  (username (current (current_account st)));
                new_inbox :=
                  make_notif_friend
                    (username (current (current_account st)))
                    true
                  :: !new_inbox;
                add_notif_inbox st (notif_payer notif)
                  (make_notif_friend
                     (username (current (current_account st)))
                     true);
                i := !i + 1
              end
            else if answer = "no" then begin
              print_endline
                "You can accept the request later unless you clear the inbox.";
              if notif_type notif then begin
                new_inbox :=
                  make_notif (notif_payer notif) (notif_payee notif)
                    (notif_amount notif) false
                  :: !new_inbox;
                i := !i + 1
              end
              else begin
                new_inbox :=
                  make_notif_friend (notif_payer notif) false :: !new_inbox;
                i := !i + 1
              end
            end
            else begin
              print_endline "Invalid Command";
              transaction_menu st
            end
          end
          else begin
            new_inbox := List.nth (notif_inbox acc) !i :: !new_inbox;
            i := !i + 1
          end
        done;
        acc_new_inbox (current (current_account st)) !new_inbox;
        transaction_menu st
      end
      else print_endline "Invalid Command";
      transaction_menu st

and search_friend st =
  match current_account st with
  | None -> not_logged_in st
  | Some acc -> (
      print_endline "\nSearch Friends";
      print_endline "Type a username: ";
      print_string "> ";
      let friend = read_line () in
      match find_account st friend with
      | exception Failure s ->
          print_endline "User doesn't exist!";
          transaction_menu st
      | acc_friend ->
          print_endline
            "\nWould you like to send a follow request to this user? [yes/no]";
          print_string "> ";

          let answer = read_line () in
          if answer = "yes" then begin
            if List.mem friend (friend_list acc) then begin
              print_endline ("You are already following user " ^ friend);
              transaction_menu st
            end
            else
              add_notif_inbox st friend (make_notif_friend (username acc) false);
            print_newline ();

            print_endline ("Requested to follow user " ^ friend);
            print_newline ();
            transaction_menu st
          end
          else if answer = "no" then transaction_menu st
          else print_endline "Invalid command!";
          transaction_menu st)

and message st =
  match current_account st with
  | None -> not_logged_in st
  | Some acc ->
      print_newline ();
      print_endline "\nWelcome to your message inbox!";
      print_endline
        "\nWould you like to view your inbox or send a message? [view/send]";
      print_string "> ";
      let answer = read_line () in
      if answer = "view" then begin
        print_endline (display_message acc);
        print_newline ();
        print_endline "\nWould you like to clear your message inbox? [yes/no]";
        print_string "> ";
        let clear = read_line () in
        if clear = "yes" then begin
          message_clear acc;
          transaction_menu st
        end
        else if clear = "no" then transaction_menu st
        else print_endline "Invalid command!";
        transaction_menu st
      end
      else if answer = "send" then begin
        print_newline ();
        print_endline
          "\nType the user's username to whom you would like to send a message:";
        print_string "> ";
        let user = read_line () in
        match find_account st user with
        | exception Failure s ->
            print_endline "User doesn't exist!";
            transaction_menu st
        | acc_friend ->
            print_newline ();
            print_endline "\nType a message that you want to send: ";
            print_string "> ";
            let mess = read_line () in
            add_message (find_account st user) (username acc ^ ": " ^ mess);
            transaction_menu st
      end
      else begin
        print_endline "Invalid command!";
        transaction_menu st
      end

and not_logged_in st =
  print_endline "Not logged in!";
  acc_menu st

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_newline ();
  let _ =
    print_endline "🐫 Welcome to VenmOCaml! 🐫";
    print_endline
      "Follow the instructions below. To return to the menu at any time, type \
       [go menu]."
  in
  let init_state, last_id = from_file () in
  acc_menu init_state;
  running_id := !running_id + last_id + 1;
  print_endline ""

(* Execute the game engine. *)
let () = main ()
