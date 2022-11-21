(**Representation of Venmo account*)

type amount
(** The abstract type of values representing amounts. *)

type transaction
(** The abstract type of values representing VenmOCaml transations. *)

type notification
(**The abstract type of values representing VenmOCaml notifications*)

type t
(** The abstract type of values representing accounts. *)

exception InvalidAmount of string
(** Raised when a user attempts to perform an action with an invalid amount. *)

exception InvalidCurrency of string
(** Raised when a user attempts to define an amount with an invalid currency. *)

exception InvalidDeposit of string
(** Placeholder until exchange rates are established. Raised when a user
    attempts to deposit a currency that is different from its balance. *)

exception InvalidWithdrawal of string
(** Placeholder until exchange rates are established. Raised when a user
    attempts to withdraw a currency that is different from its balance. *)

exception InvalidConversion
(** Raised when a user attempts to complete an invalid conversion*)

val from_json : Yojson.Basic.t -> int -> t
(** [from_json j] is the account that [j] represents. Requires: [j] is a valid
    JSON account representation. *)

val to_json : t -> Yojson.Basic.t
(** [to_json acc] is the Yojson that represents [acc]. *)

val create : int -> string -> string -> ?balance:string -> string -> t
(** [create id username password ~balance:balance home_curr] is the account with
    unique identifier [id], username [username], password [password], balance
    [balance] (or 0. by default), home currency [home_curr], and is active. *)

val username : t -> string
(** [username acc] is the username of account [acc]. *)

(*val password : t -> string *)
(** [password acc] is the password of account [acc]. *)

val check_password : string -> t -> bool
(**[check_password str] returns true if the str equals account's password*)

val balance : t -> string
(** [balance acc] is the current balance of account [acc] with the understanding
    that a user can have a negative balance. *)

val is_active : t -> bool
(** [is_active acc] returns true if the account is active; otherwise, it returns
    false. *)

val deactivate : t -> t
(** [deactivate acc] returns a copy of [acc] that is inactive if [acc] is
    active, and does nothing if [acc] is already inactive. *)

val display : t -> unit
(** [display acc] prints the account, with its username, and balance.*)

val display_history : t -> unit
(** [display acc] prints the account's transaction history.*)

val deposit : t -> string -> t
(** [deposit acc amt] adds [amt] to the balance of the account [acc] with the
    precondition that amt > 0 *)

val withdraw : t -> string -> t
(** [withdraw acc amt] removes [amt] from the balance of the account [acc] with
    the precondition that amt > 0. *)

val add_notification : t -> notification -> unit
(** [add_notification acc not] adds notification not into notification_inbox in
    t*)

val notif_clear : t -> unit
(** [notif_clear] clears the notification inbox of acc*)

val make_request : t -> string -> string -> notification
(** [make_request t payer amount] requests payer [amount] value of money and
    returns a notification*)
