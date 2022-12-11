(** Representation of dynamic Venmo state.

    This module represents the state of a VenmOCaml system since its start,
    including the accounts that have been created, a history of transactions,
    and functions that cause the state to change. *)

type transaction
(** The abstract type of values representing a VenmOCaml transaction. *)

type t
(** The abstract type of values representing the VenmOCaml state. *)

exception InvalidUsername of string
(** Raised when the user attempts to create an account with a username that is
    already associated with an existing account. *)

exception IncorrectPassword
(** Raised when the user enters the incorrect password for an account. *)

val init_state : t
(** [init_state] is the initial state of the Venmo system. The user is not
    logged into any account and there have been no accounts created or
    transactions made. *)

val check_username : t -> string -> unit
(** [check_username st un] raises [InvalidUsername un] if [un] is already
    associated with an account in the current state, and returns [()] otherwise. *)

val current_account : t -> Account.t option
(** [current_account st] is the account that the user is currently logged into
    in state [st]. *)

val accounts : t -> Account.t array
(** [accounts st] is a set-like array of the accounts that have been created in
    state [st]. *)

val transactions : t -> transaction list
(** [transactions st] is a list of the transactions that have been made in state
    [st], ordered from least recent to most recent. *)

val add_account : t -> Account.t -> unit
(** [add_account st acc] adds [acc] to the list of accounts in [st] and returns
    [()]. *)

val delete_account : t -> int -> unit
(** [delete_account st acc] makes the account identified by [id] inactive in
    [st] and returns [()]. *)

val make_deposit : t -> string -> string -> unit
(** [make_dposit st un p] increases the balance of the account with username
    [un] by [p]*)

val make_payment : t -> string -> string -> string -> unit
(**[make_payment st
  paying_acc_un paid_acc_un p] adds [p] to the balance of
   account with the username [paid_acc_un] and removes [p] from the balance of the
   account identified with username [paying_acc_un]*)

val login_system : t -> string -> string -> unit
(** [login_system st un pass] changes current account of the state if the
    username and password input matches one of the accounts in the state*)

val logout : t -> unit
(** [logout st] removes the current account of the state*)

val to_file : t -> unit
(** [to_file st] converts the state [st] to a JSON object and stores it in
    "data/data.json". *)
val add_notif_inbox : t -> string -> Account.notification -> unit
(** [add_notif_inbox st payer notif] adds the notification to the payer's notification inbox*)

val find_account : t -> string -> Account.t
(** [find_account t str] finds account from its username in state t*)

val current : 'a option -> 'a
(** [current some] returns the original value of 'a option*)

val add_friend_state : t -> string -> unit
(** [add_friend_state t friend] adds friend to t's friend list *)

val remove_friend_state : t -> string -> unit
(** [remove_friend_state t friend] removed friend in t's friend list *)