(** Regular expression combinators *)

(** The type of regular expressions *)
type t

(** Sets of characters *)
type charset = Set.Make(Char).t

(** [empty] rejects every string *)
val empty : t

(** [eps] accepts only the empty string *)
val eps : t

(** [any] accepts any single character *)
val any : t

(** [oneof cs] accepts any character in cs *)
val oneof : charset -> t

(** [range l h] accepts any character in the range [l]..[h] *)
val range : char -> char -> t

(** [chr c] accepts exactly the character [c] *)
val chr : char -> t

(** [seq x y] accepts strings [rs] where [x] accepts [r] and [y]
    accepts [s] *)
val seq : t -> t -> t

(** [alt x y] accepts any string accepted by either [x] or [y] *)
val alt : t -> t -> t

(** [opt r] accepts any string accepted by [r], and the empty string *)
val opt : t -> t

(** [star r] accepts any string consisting of zero or more copies of
    a string accepted by [r] *)
val star : t -> t

(** [star r] accepts any string consisting of one or more copies of
    a string accepted by [r] *)
val plus : t -> t

(** Parse a regular expression using the following grammar:

      r ::= (r)          (parenthesized regex)
            .            (match any character)
            rr           (sequencing)
            r|r          (alternation)
            r?           (zero or one)
            r*           (zero or more)
            r+           (one or more)
            c            (literal character)
            [b]          (POSIX bracket expression)

   Raises [Parse_error] on parse error
*)
val parse : string -> t

(** [unparse_charset cs] is a string denoting a regular expression
    that accepts any character in [cs], and nothing else *)
val unparse_charset : charset -> string

(** [compile r] translates [r] to an NFA that succeeds on exactly
    those strings matched by [r] *)
val compile : t -> Nfa.nfa

(** Raised when [parse] is given an invalid regex *)
exception Parse_error of string
