module Route : sig
  type t

  val of_string : string -> (t, string) result
  val of_string_exn : string -> t
end

module Params : sig
  type t

  val named : t -> string -> string
  val unnamed : t -> string list
  val pp : Format.formatter -> t -> unit
end

type 'a t

val empty : 'a t
val add : 'a t -> Route.t -> 'a -> 'a t
val match_url : 'a t -> string -> ('a * Params.t) option
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
