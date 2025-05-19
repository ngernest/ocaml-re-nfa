(** Deterministic finite automata *)

type dfa = {
  start : Nfa.state;  (** the start state *)
  finals : Nfa.StateSet.t;  (** the final (or "accept") states *)
  next : Nfa.state -> Nfa.state Nfa.CharMap.t;
    (** the transition function, that maps a state and a character to the
      next state *)
}

(** [minimize dfa] is a minimized dfa equivalent to the dfa [dfa],
    obtained via Brzozowski's algorithm *)
val minimize : dfa -> dfa

(** [accept dfa l] is [true] iff the dfa [dfa] accepts the
    character sequence [l] *)
val accept : dfa -> char list -> bool

(** [determinize nfa] is a deterministic finite automaton that
    accepts the same language as [nfa].

    NB: at present, [determinize] assumes that [nfa] has no ε
    transitions, which is the case for automata built by {!Regex.compile}.
 *)
val determinize : Nfa.nfa -> dfa

(** [inject dfa] is the deterministic NFA corresponding to [dfa] *)
val inject : dfa -> Nfa.nfa
