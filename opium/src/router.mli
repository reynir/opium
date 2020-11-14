module Route : sig
  type t

  val of_string : string -> (t, string) result
  val of_string_exn : string -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

module Params : sig
  type t

  val named : t -> string -> string
  val unnamed : t -> string list
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

type 'a t

val empty : 'a t
val add : 'a t -> Route.t -> 'a -> 'a t
val match_url : 'a t -> string -> ('a * Params.t) option
val sexp_of_t : ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t
