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

(* Contains utility functions. *)
module Util: sig

  (* Tail-recursion version of [List.map]. *)
  val map: ('a -> 'b) -> 'a list -> 'b list

  (* Tail-recursive version of [List.mapi]. *)
  val mapi: (int -> 'a -> 'b) -> 'a list -> 'b list

  (* Tail-recursive version of [List.split]. *)
  val split: ('a * 'b) list -> 'a list * 'b list

  (* [try_fill iv v] fills [iv] with [v] iff [Ivar.is_empty iv]. *)
  (* val try_fill: 'a Async.Ivar.t -> 'a -> unit *)

  (* [any ds] is determined with [(n, v)] where [v] is the value of the
   * first element of [ds] to become determined and [n] is its index. *)
  val any: 'a Async.Deferred.t list -> (int * 'a) Async.Deferred.t

end = struct

  let map f l = List.rev_map f l |> List.rev

  let split l = map fst l, map snd l

  let mapi f l = l
    |> List.fold_left (fun (i, l) x -> i + 1, f i x :: l) (0, [])
    |> snd |> List.rev

  let try_fill iv v =
    if Async.Ivar.is_empty iv then Async.Ivar.fill iv v else ()

  let (>>>) = Async.upon

  let any ds =
    let iv = Async.Ivar.create () in
    let _ = mapi (fun i d -> d >>> fun v -> try_fill iv (i, v)) ds in
    Async.Ivar.read iv

end

module Make (A: Arg): sig

  (* Start the coordinator. *)
  val go: unit -> A.state Async.Deferred.t

end = struct

  (* There is no need for this type. However, this library can be
   * implemented without functors, and this record is a functor-free
   * way to pass in arguments. *)
  type args = {

    (* A way to generate an initial state. *)
    initial_state: A.state;

    (* Pairs of observers and updaters that update the state on observation. *)
    process_pairs: (A.observer * A.updater) list;

    (* An optional function that, if not [None], will cause termination. *)
    terminate: (A.state -> bool) option;

  }

  let (>>=) = Async.(>>=)

  let rec read_loop st terminate observers updaters futures =
    (* Wait for any to become determined. *)
    Util.any futures >>= fun (i, v) ->
    (* Use the correct updater to generate the new state. *)
    let new_st = (List.nth updaters i) v st in
    if terminate new_st then Async.return st else
    (* Re-read the observer corresponding to the future that was determined. *)
    let new_futures = Util.mapi begin fun j f ->
        if j = i then (List.nth observers i) () else f
      end futures in
    read_loop new_st terminate observers updaters new_futures

  let coordinate args =
    let st = args.initial_state in
    let terminate = match args.terminate with
      | None -> (fun _ -> false) | Some f -> f in
    let (observers, updaters) = Util.split args.process_pairs in
    let futures = Util.map (fun observer -> observer ()) observers in
    read_loop st terminate observers updaters futures

  let go () = coordinate {
    initial_state = A.initial_state;
    process_pairs = A.coordinated_units;
    terminate = A.terminate;
  }

end
