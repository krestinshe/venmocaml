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

val make : ?balance:string -> string -> string -> t
(** [make ~balance:balance username password] is the account with username
    [username], password [password], and balance set to the amount represented
    by [balance] (or 0. by default). *)

val username : t -> string
(** [username acc] is the username of account [acc]. *)

val balance : t -> string
(** [balance acc] is the current balance of account [acc]. *)

val display : t -> unit
(** [display acc] prints the account, with its username, balance, and
    transaction history. *)
