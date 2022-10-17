(**Representation of Venmo account*)

type t
(** The abstract type of values representing accounts. *)

exception InvalidUsername of string
(** Raised when a user attempts to create an account with a username that is
    already associated with an existing account. *)

exception IncorrectPassword
(** Raised when a user attempts to perform an action and enters the incorrect
    password when prompted. *)

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

val make : ?balance:string -> string -> string -> t
(** [make ~balance:balance username password] is the account with username
    [username], password [password], and balance set to the amount represented
    by [balance] (or 0. by default). *)

val username : t -> string
(** [username acc] is the username of account [acc]. *)

(* val password : t -> string *)
(** [password acc] is the password of account [acc]. *)

val balance : t -> string
(** [balance acc] is the current balance of account [acc] with the understanding
    that a user can have a negative balance. *)

val display : t -> unit
(** [display acc] prints the account, with its username, balance, and
    transaction history.*)

val deposit : t -> string -> t
(** [deposit acc amt] adds [amt] to the balance of the account [acc] with the
    precondition that amt > 0 *)

val withdraw : t -> string -> t
(** [withdraw acc amt] removes [amt] from the balance of the account [acc] with
    the precondition that amt > 0. *)

(*val transaction : t -> string list (** [transaction acc] returns a string list
  of the history of withdrawals and deposits*)*)
