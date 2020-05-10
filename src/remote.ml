open Lwt.Infix
open Ppx_lwt

type client_command =
  | Rotate of [`CCW | `CW]
  | Move of [`Left | `Right]
  | Hold
  | HardDrop

type server_packet =
  | Packet of State.Local.t * State.Local.t list
  | Quit

module Client = struct
  module S = State.Local

  type t = unit

  exception Gameover of t

  let pauseable = false

  let init _ _ _ =
    failwith "Cannot init client state"

  let score _ =
    failwith "unimplemented"

  let level _ =
    failwith "unimplemented"

  let lines _ =
    failwith "unimplemented"

  let field_width _ =
    failwith "unimplemented"

  let field_height _ =
    failwith "unimplemented"

  let value _ _ _ =
    failwith "unimplemented"

  let queue _ =
    failwith "unimplemented"

  let held _ =
    failwith "unimplemented"

  let update _ _ _ =
    failwith "unimplemented"

  let rotate _ _ =
    failwith "unimplemented"

  let move _ _ =
    failwith "unimplemented"

  let hold _ =
    failwith "unimplemented"

  let hard_drop _ =
    failwith "unimplemented"

  let handle_events _ _ =
    failwith "unimplemented"
end

module Server = struct
  module S = State.Local

  type io_pair = Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel
  type accepted = (Lwt_unix.sockaddr * io_pair) list

  type connecting_status =
    | Pending of unit Lwt.t * accepted ref
    | Done

  type command_buffer = (Lwt_unix.sockaddr * client_command) list

  type t = {
    status : connecting_status;
    sock : Lwt_unix.file_descr;
    buffer : command_buffer ref;
    handlers : unit Lwt.t list;
    server_state : S.t;
    client_states : (Lwt_unix.sockaddr * S.t) list;
  }

  exception Gameover of t

  let start_client
      (server : t)
      (client : Lwt_unix.sockaddr)
      (ic : Lwt_io.input Lwt_io.channel) : t =
    let buffer = server.buffer in
    let rec handle () =
      let%lwt value = Lwt_io.read_value ic in
      buffer := (client, value) :: !buffer;
      handle ()
    in { server with handlers = handle () :: server.handlers }

  let create_sock (addr : Lwt_unix.sockaddr) : Lwt_unix.file_descr =
    let open Lwt_unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    bind sock addr |> ignore;
    listen sock 32;
    sock

  let register_client
      (clients : accepted ref)
      (fd, addr : Lwt_unix.file_descr * Lwt_unix.sockaddr) : unit Lwt.t =
    let ic = Lwt_io.of_fd Lwt_io.Input fd in
    let oc = Lwt_io.of_fd Lwt_io.Output fd in
    clients := (addr, (ic, oc)) :: !clients;
    Lwt.return ()

  let rec accept_clients
      (clients : accepted ref)
      (sock : Lwt_unix.file_descr) () : unit Lwt.t =
    Lwt_unix.accept sock
    >>= register_client clients
    >>= if List.length !clients < 3
    then accept_clients clients sock
    else Lwt.return

  let create_server (addr : Lwt_unix.sockaddr) : t =
    let clients : accepted ref = ref [] in
    let sock = create_sock addr in
    let acceptor = accept_clients clients sock () in
    {
      status = Pending (acceptor, clients);
      sock = sock;
      buffer = ref [];
      handlers = [];
      server_state = S.init 10 20 1;
      client_states = [];
    }

  (** [start server] is [server] with the game started and with it not accepting
      more clients.
      Requires: [server] is not already started. *)
  let start (server : t) : t =
    match server.status with
    | Pending (acceptor, clients) ->
      Lwt.cancel acceptor;
      let f s (addr, (ic, oc)) = start_client s addr ic in
      let server' = List.fold_left f server !clients in
      { server' with status = Done }
    | Done ->
      failwith "Cannot start already started server"

  let pauseable = false

  let init _ _ _ =
    failwith "Cannot init client state"

  let update (server : t) (delta : int) (soft_drop : bool) : t Lwt.t =
    let%lwt () = Lwt_main.yield () in
    let update_fun state = S.update state delta false in
    let all_states = server.server_state :: List.map snd server.client_states in
    let server = match List.map update_fun all_states with
      | server_state :: client_states -> server (* TODO *)
      | _ -> failwith "Unexpected number of states in server update"
    in failwith "unimplemented"

  let wrap (f : S.t -> S.t) (server : t) : t =
    { server with server_state = f server.server_state }

  let score (server : t) : int = S.score server.server_state
  let level (server : t) : int = S.level server.server_state
  let lines (server : t) : int = S.lines server.server_state
  let field_width (server : t) : int  = S.field_width server.server_state
  let field_height (server : t) : int = S.field_height server.server_state
  let value (server : t) : int -> int -> State.v = S.value server.server_state
  let queue (server : t) : Tetromino.t list = S.queue server.server_state
  let held (server : t) : Tetromino.t option = S.held server.server_state
  let rotate  (rot : [`CCW | `CW]) (server : t) : t = wrap (S.rotate rot) server
  let move (dir : [`Left | `Right]) (server : t) : t = wrap (S.move dir) server
  let hold : t -> t = wrap S.hold
  let hard_drop : t -> t = wrap S.hard_drop
  let handle_events (f : State.event -> unit) = wrap (S.handle_events f)
end

let create_client (addr : Lwt_unix.sockaddr) : Client.t =
  ()

let create_server : Lwt_unix.sockaddr -> Server.t = Server.create_server
