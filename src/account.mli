(** Representation of VenmOCaml account.

    This module represents a VenmOCaml account, including its username,
    password, balance, home currency, transaction history, active status,
    notification inbox, friends list, and message inbox. It contains functions
    that cause the account to change.*)

type amount
(** The abstract type of values representing amounts. *)

type transaction
(** The abstract type of values representing VenmOCaml transations. *)

type notification
(**The abstract type of values representing VenmOCaml notifications. *)

type t
(** The abstract type of values representing accounts. *)

exception InvalidAmount of string
(** Raised when a user attempts to perform an action with an invalid amount. *)

exception InvalidCurrency of string
(** Raised when a user attempts to define an amount with an invalid currency. *)

exception InvalidTransaction
(** Raised when a user attempts to make a transaction with an invalid amount. *)

exception InvalidConversion
(** Raised when a user attempts to complete an invalid conversion. *)

val from_json : Yojson.Basic.t -> int -> t
(** [from_json j] is the account that [j] represents.

    Requires: [j] is a valid JSON account representation. *)

val to_json : t -> Yojson.Basic.t
(** [to_json acc] is the Yojson that represents [acc]. *)

val create : int -> string -> string -> ?balance:string -> string -> t
(** [create id username password ~balance:balance home_curr] is the account with
    unique identifier [id], username [username], password [password], balance
    [balance] (or 0. by default), home currency [home_curr], and is active. *)

val username : t -> string
(** [username acc] is the username of account [acc]. *)

val check_password : string -> t -> bool
(**[check_password str acc] returns true if [str] equals [acc]'s password*)

val balance : t -> string
(** [balance acc] is the current balance of account [acc]. (A user can have a
    negative balance.) *)

val is_active : t -> bool
(** [is_active acc] returns true if account [acc] is active; otherwise, it
    returns false. *)

val deactivate : t -> t
(** [deactivate acc] returns a copy of [acc] that is inactive. *)

val display : t -> string
(** [display acc] returns a string of the account, with its username, and
    balance.*)

val display_history : t -> string
(** [display acc] returns a string representing the account [acc]'s transaction
    history.*)

val display_notif : t -> string
(** [display_notif acc] returns a string representing the account [acc]'s
    notification inbox. *)

val display_friends : t -> string
(** [display_friends acc] returns a string representing the account [acc]'s
    friends list. *)

val display_message : t -> string
(** [display_message acc] returns a string representing the account [acc]'s
    message inbox. *)

val deposit : t -> string -> t
(** [deposit acc amt] adds [amt] to the balance of the account [acc].

    Precondition: [amt] > 0. *)

val withdraw : t -> string -> t
(** [withdraw acc amt] removes [amt] from the balance of the account [acc].

    Precondition: [amt] > 0. *)

exception InsufficientBalance
(** Raised when a user tries to make a withdrawal or payment that would reuslt
    in a negative balance.*)

val deposit_transaction : t -> string -> transaction
(** [deposit_transaction acc tran ]*)

val pay_transaction : t -> string -> string -> transaction
(** [pay_transaction acc payee amount] returns a transaction of paying with the
    information of the payee, payer, and amount. *)

val received_transaction : string -> string -> transaction
(** [received_transaction payer amount] returns a transaction of received with
    the information of the payer and amount*)

val add_transaction : t -> transaction -> unit
(** [add_transaction acc tran] adds transaction [tran] to the transaction
    history of account [acc]. *)

val make_request : t -> string -> string -> notification
(** [make_request t payer amount] requests [payer] [amount] value of money and
    returns a notification. *)

val add_notification : t -> notification -> unit
(** [add_notification acc not] adds notification [not] into the notification
    inbox of the account [acc]. *)

val notif_clear : t -> unit
(** [notif_clear] clears the notification inbox of [acc]. *)

val length_notif : t -> int
(** [length_notif acc] returns the length of [acc]'s notification inbox. *)

val notif_inbox : t -> notification list
(** [notif_inbox acc] returns [acc]'s notification inbox. *)

val notif_payer : notification -> string
(** [notif_payer notif] returns [notif]'s payer information or friend's friend
    username*)

val notif_payee : notification -> string
(** [notif_payee notif] returns [notif]'s payee information*)

val notif_amount : notification -> string
(** [notif_amount notif] returns [notif]'s amount information *)

val notif_accepted : notification -> bool
(** [notif_accepted notif] returns true if [notif] is accepted, and false
    otherwise. *)

val string_of_notif : notification -> string
(** [string_of_notif notif] returns the string form of notification [notif]. *)

val acc_new_inbox : t -> notification list -> unit
(** [acc_new_inbox acc new_notif] sets [acc]'s notification inbox to
    [new_notif]. *)

val make_notif : string -> string -> string -> bool -> notification
(** [make_notif payer payee amount accepted] returns a notification of a payment
    request of amount [amount] between payer [payer] and payee [payee], that has
    been accepted if [accepted] is true and not accepted otherwise. *)

val make_notif_friend : string -> bool -> notification
(** [make_notif_friend friend accepted] returns a notification of a friend
    request from user [friend] that has been accepted if [accepted] is true and
    not accepted otherwise. *)

val add_friend : t -> string -> unit
(** *)

val remove_friend : t -> string -> unit
(** *)

val friend_list : t -> string list
(** *)

val notif_type : notification -> bool
(** *)

val add_message : t -> string -> unit
(** *)

val message_clear : t -> unit
(** *)