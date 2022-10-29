(** Representation of dynamic Venmo state.

    This module represents the state of a VenmOCaml system since its start,
    including the accounts that have been created, a history of transactions,
    and functions that cause the state to change. *)

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
(** [current_room_id st] is the account that the user is currently logged into
    in state [st]. *)

val accounts : t -> Account.t array
(** [accounts st] is a set-like array of the accounts that have been created in
    state [st]. The adventurer has visited a room [rm] if their current room
    location is or has ever been [rm]. *)

val add_account : t -> Account.t -> unit
(** [add_account st acc] adds [acc] to the list of accounts in [st] and returns
    [()]. *)

(* val make_payment : t -> int -> int -> string -> unit (**[make_payment st
   paying_acc paid_acc p] adds [p] to account [paid_acc] by removing value [p]
   from [paying_acc]*) *)
