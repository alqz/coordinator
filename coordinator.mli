(* [Arg] is the type of modules which can be coordinated. *)
module type Arg = sig

  (* Define the type of the state we update. *)
  type state

  (* Define the type of state updates. *)
  type delta

  (* The type of observation functions. *)
  type observer = unit -> delta Async.Deferred.t

  (* The type of functions that update the state based on observed deltas. *)
  type updater = delta -> state -> state

  (* A way to generate an initial state. *)
  val initial_state: state

  (* Pairs of observers and updaters that modify the state on observation. *)
  val coordinated_units: (observer * updater) list

  (* An optional function that, if not [None], will cause termination. *)
  val terminate: (state -> bool) option

end

module Make: functor (A: Arg) -> sig

  (* Start the coordinator. *)
  val go: unit -> A.state Async.Deferred.t

end
