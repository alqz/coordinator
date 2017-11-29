open Async

(* Here is a utility function that returns an int Deferred.t that is
 * determined after some number of seconds using that number. *)
let wait n = after (Core.sec n) >>= fun () -> return n

(* First, we wish to define a state and a means of updating it.
 * Here is the general module type for a state. *)
module type Stateful = sig
  type t
  type delta
  val init: t
  val update: delta -> t -> t
  val to_string: t -> string
end

(* And here is a very basic corresponding implementation of a state. *)
module State: Stateful with type delta = string = struct
  type t = {last_line: string option} (* We just keep the last delta *)
  type delta = string
  let init = {last_line = None}
  let update delta _st = {last_line = Some delta}
  let to_string st = match st.last_line with
    | None -> "empty state" | Some line -> line
end

(* Here is a module type that defines a general endpoint which
 * can only be read asynchronously. *)
module type Async_input = sig
  type source
  type output
  val setup: string -> source (* Give it a name *)
  val read: source -> output Deferred.t
end

(* And here is a corresponding implementation. Every read is
 * a fake ping to a server that becomes determined with
 * result every 3 to 7 seconds. *)
module Server: Async_input with type output = string = struct
  type source = string (* Just the name of the server; some may use a queue *)
  type output = string
  let setup name = "This is server " ^ name ^ ": "
  let ping prefix ping_delay = wait (float_of_int ping_delay) >>= fun n ->
    return (prefix ^ (string_of_float n))
  let read prefix = ping prefix (Random.int 5 + 3)
end

(* Here is another asynchronous input module that deals with
 * keyboard input from the user's computer. *)
module Keyboard: Async_input with type output = string = struct
  type source = unit
  type output = string
  let setup _ = () (* No need for special setup *)
  let read () = Reader.(read_line (Lazy.force stdin)) >>= fun r ->
    return (match r with `Eof -> "" | `Ok a -> a)
end

(* Given all of the above modules that we wish to coordinate, we can
 * construct a coordination argument. *)
module My_arg: Coordinator.Arg = struct

  type state = State.t
  type delta = string (* string *)
  type observer = unit -> delta Deferred.t
  type updater = delta -> state -> state

  (* First, let's create two servers using our own "library". *)
  let server_a = Server.setup "A"
  let server_b = Server.setup "B"
  let server_observe server () = Server.read server
  let server_updater = State.update

  (* Next, let's also try to read from the keyboard using our "library". *)
  let keyboard = Keyboard.setup ""
  let keyboard_observe () = Keyboard.read keyboard
  let keyboard_updater d st = State.update ("Keyboard: " ^ d) st

  (* Lastly, this is just a printer that reminds itself
   * to print every one second, and then prints the state. *)
  let print_reminder _ = wait 0.5 >>= fun _ -> return ""
  let print_state _ st =
    print_string "print_state: ";
    print_endline (State.to_string st);
    st

  (* Coordination argument, a list of observers and updaters *)
  let coordinated_units = [
    (server_observe server_a, server_updater);
    (server_observe server_b, server_updater);
    (keyboard_observe, keyboard_updater);
    (print_reminder, print_state);
  ]

  (* Initial state is just our initial state. *)
  let initial_state = State.init

  (* We define our termination condition to be when the state
   * as a string is "exit" or "quit". *)
  let terminate st =
    let s = State.to_string st in
    s = "exit" || s = "quit"

  let terminate = Some terminate

end

(* Create the coordinator. *)
module C = Coordinator.Make (My_arg)

(* Coordinator will create the right bindings. *)
let _ = C.go ()

(* Allow the functional observer to be scheduled. *)
let _ = Scheduler.go ()
