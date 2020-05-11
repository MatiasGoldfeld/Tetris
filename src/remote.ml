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

  type t = {
    local : S.t ref;
    remote : S.t list ref;
    receiver : unit Lwt.t;
    out_channel : Lwt_io.output Lwt_io.channel;
  }

  exception Gameover of t

  (** [new_state ()] is a new game state. *)
  let new_state () = State.create_state 10 20 1

  (** [create_sock ())] is the client sock. *)
  let create_sock () : Lwt_unix.file_descr =
    let open Lwt_unix in socket PF_INET SOCK_STREAM 0

  (** [create_receiver local remote ic] spawns a thread that checks for input
      from [ic], and writes that to [local] and [remote]. *)
  let create_receiver
      (local : S.t ref) (remote : S.t list ref)
      (ic : Lwt_io.input Lwt_io.channel) : unit Lwt.t =
    let rec receive () =
      let%lwt packet = Lwt_io.read_value ic in
      match packet with
      | Packet (l, r) ->
        local := l;
        remote := r;
        receive ()
      | Quit -> Lwt.return ()
    in receive ()

  (** [create_client] is a client named [name] which attempts to connect to
      server [server]. *)
  let create_client (name : string) (server : Lwt_unix.sockaddr) : t =
    let sock = create_sock () in
    Lwt.async (fun _ -> Lwt_unix.connect sock server);
    let ic = Lwt_io.of_fd Lwt_io.Input sock in
    let oc = Lwt_io.of_fd Lwt_io.Output sock in
    let local = ref (new_state ()) in
    let remote = ref [] in
    {
      local = local;
      remote = remote;
      receiver = create_receiver local remote ic;
      out_channel = oc;
    }

  let send_command (client : t) (command : client_command) : unit Lwt.t =
    Lwt_io.write_value client.out_channel command

  let pauseable = false

  let init _ _ _ =
    failwith "Cannot init client state"

  let update (client : t) (delta : int) (soft_drop : bool) : t Lwt.t =
    let%lwt () = Lwt_main.yield () in
    Lwt.return client

  (** [return f client] is [client]'s local state applied to [f]. *)
  let return (f : S.t -> 'a) (client : t) : 'a = f !(client.local)

  (** [wrap command client] is [client] with the side effect of sending
      [command] to the server. *)
  let wrap (command : client_command) (client : t)  : t =
    Lwt.async (fun _ -> send_command client command);
    client

  let score : t -> int = return S.score
  let level : t -> int = return S.level
  let lines : t -> int = return S.lines
  let field_width : t -> int = return S.field_width
  let field_height : t -> int = return S.field_height
  let value : t -> int -> int -> State.v = return S.value
  let queue : t -> Tetromino.t list = return S.queue
  let held : t -> Tetromino.t option = return S.held
  let rotate (rot : [`CCW | `CW]) : t -> t = wrap (Rotate rot)
  let move (dir : [`Left | `Right]) : t -> t = wrap (Move dir)
  let hold : t -> t = wrap Hold
  let hard_drop : t -> t = wrap HardDrop
  let handle_events (f : State.event -> unit) (client : t) : t  =
    client.local := (S.handle_events f !(client.local));
    client
end

module Server = struct
  module S = State.Local

  type connecting_status =
    | Creating
    | Pending of unit Lwt.t
    | Done

  type command_buffer = (Lwt_unix.sockaddr * client_command) list

  type t = {
    status : connecting_status ref;
    sock : Lwt_unix.file_descr;
    buffer : command_buffer ref;
    handlers : unit Lwt.t list ref;
    local : S.t;
    player_states : (Lwt_unix.sockaddr, S.t Lwt.t) Hashtbl.t;
    out_channels : (Lwt_unix.sockaddr, Lwt_io.output Lwt_io.channel) Hashtbl.t;
  }

  exception Gameover of t

  (** [new_state ()] is a new game state. *)
  let new_state () = State.create_state 10 20 1

  (** [register_client server client] registers [client] in [server], spawning
      a handler to handle incoming commands from the client. *)
  let register_client
      (server : t)
      (fd, addr : Lwt_unix.file_descr * Lwt_unix.sockaddr) : unit =
    let ic = Lwt_io.of_fd Lwt_io.Input fd in
    let oc = Lwt_io.of_fd Lwt_io.Output fd in
    Hashtbl.add server.player_states addr (Lwt.return @@ new_state ());
    Hashtbl.add server.out_channels addr oc;
    let rec handle () =
      let%lwt value = Lwt_io.read_value ic in
      server.buffer := (addr, value) :: !(server.buffer);
      handle ()
    in server.handlers := handle () :: !(server.handlers)

  (** [accept_clients server sock] is a thread that accepts incoming connectios
      on [sock] and registers them in [server]. *)
  let rec accept_clients
      (server : t)
      (sock : Lwt_unix.file_descr) () : unit Lwt.t =
    let%lwt client = Lwt_unix.accept sock in
    register_client server client;
    if Hashtbl.length server.player_states < 4
    then accept_clients server sock ()
    else Lwt.return ()

  (** [create_sock addr] is the hosting sock on [addr]. *)
  let create_sock (addr : Lwt_unix.sockaddr) : Lwt_unix.file_descr =
    let open Lwt_unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    Lwt.async (fun _ -> bind sock addr);
    listen sock 32;
    sock

  (** [create_server name addr] is a server named [name] hosting on [addr]. *)
  let create_server (name : string) (addr : Lwt_unix.sockaddr) : t =
    let sock = create_sock addr in
    let server = {
      status = ref Creating;
      sock = sock;
      buffer = ref [];
      handlers = ref [];
      local = new_state ();
      player_states = Hashtbl.create 3;
      out_channels = Hashtbl.create 3;
    } in
    server.status := Pending (accept_clients server sock ());
    server

  (** [start server] is [server] with the game started and with it not accepting
      more clients.
      Requires: [server] is not already started. *)
  let start (server : t) : t =
    match !(server.status) with
    | Pending acceptor ->
      Lwt.cancel acceptor;
      server.status := Done;
      server
    | Creating | Done ->
      failwith "Cannot start non-pending server"

  let pauseable = false

  let init _ _ _ =
    failwith "Cannot init client state"

  (** [handle_commands server commands] handles all buffered incoming client
      [commands] in [server] *)
  let rec handle_commands (server : t) (commands : command_buffer) : unit =
    match commands with
    | [] -> ()
    | (addr, command) :: t ->
      let state_prom =
        let%lwt state = Hashtbl.find server.player_states addr in
        Lwt.return @@ match command with
        | Rotate rot -> S.rotate rot state
        | Move dir -> S.move dir state
        | Hold -> S.hold state
        | HardDrop -> S.hard_drop state
      in Hashtbl.replace server.player_states addr state_prom;
      handle_commands server t

  (** [send_states server addr] sends all the states in [server] to [addr]. *)
  let send_states (server : t) (addr : Lwt_unix.sockaddr) : unit Lwt.t =
    let%lwt client_state = Hashtbl.find server.player_states addr in
    let seq_f (client, state) = if client = addr then None else Some state in
    let%lwt other_states = Hashtbl.to_seq server.player_states
                           |> Seq.filter_map seq_f
                           |> List.of_seq
                           |> Lwt.all
    in
    let oc = Hashtbl.find server.out_channels addr in
    let packet = Packet (client_state, server.local :: other_states) in
    Lwt_io.write_value oc packet

  let update (server : t) (delta : int) (soft_drop : bool) : t Lwt.t =
    let%lwt () = Lwt_main.yield () in
    let commands = List.rev !(server.buffer) in
    server.buffer := [];
    if !(server.status) == Done
    then handle_commands server commands
    else ();
    let update_fun _ state_prom = Some begin
        let%lwt state = state_prom in
        S.update state delta false
      end in
    Hashtbl.filter_map_inplace update_fun server.player_states;
    let%lwt local = S.update server.local delta false in
    let server = { server with local = local } in
    let%lwt () = Hashtbl.to_seq server.player_states
                 |> Seq.map (fun (addr, _) -> send_states server addr)
                 |> List.of_seq
                 |> Lwt.join
    in Lwt.return server

  (** [return f server] is [server]'s local state applied to [f]. *)
  let return (f : S.t -> 'a) (server : t) : 'a = f server.local

  (** [wrap f server] is [server] with [f] applied to the local player state. *)
  let wrap (f : S.t -> S.t) (server : t) : t =
    if !(server.status) <> Done
    then server
    else { server with local = f server.local }

  (** [start_trigger f server] is [server] with the game started if it wasn't.
      If it was, it is [wrap f server]. *)
  let start_trigger (f : S.t -> S.t) (server : t) : t =
    if !(server.status) <> Done
    then start server
    else wrap f server

  let score : t -> int = return S.score
  let level : t -> int = return S.level
  let lines : t -> int = return S.lines
  let field_width : t -> int  = return S.field_width
  let field_height : t -> int = return S.field_height
  let value : t -> int -> int -> State.v = return S.value
  let queue : t -> Tetromino.t list = return S.queue
  let held : t -> Tetromino.t option = return S.held
  let rotate  (rot : [`CCW | `CW]) : t -> t = wrap (S.rotate rot)
  let move (dir : [`Left | `Right]) : t -> t = wrap (S.move dir)
  let hold : t -> t = wrap S.hold
  let hard_drop : t -> t = start_trigger S.hard_drop
  let handle_events (f : State.event -> unit) : t -> t =
    wrap (S.handle_events f)
end

let create_client : string -> Lwt_unix.sockaddr -> Client.t =
  Client.create_client

let create_server : string -> Lwt_unix.sockaddr -> Server.t =
  Server.create_server
