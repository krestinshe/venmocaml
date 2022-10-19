open OUnit2
open Account

let instruction = print_string "welcome to demo";
                  print_newline ();
                  print_string "if you want to make a new account, type account_instantiation;;";
                  print_newline ();


let _  =
    print_string "welcome to the bank. If you want to instantiate an account, type the username and password"; 
    let username = read_line () in 
       let password = read_line () in 
         display (make username password);
     
                           

 
