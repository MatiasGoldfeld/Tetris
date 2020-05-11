(** [Client] is a [State.S] with functionality for a client multiplayer game
    connected to a server *)
module Client : State.S

(** [Server] is a [State.S] with functionality for a hosted multiplayer game
    with several clients. *)
module Server : State.S

(** [create_client] is a client named [name] which attempts to connect to
    server [server]. *)
val create_client : string -> Lwt_unix.sockaddr -> Client.t

(** [create_server name addr] is a server named [name] hosting on [addr]. *)
val create_server : string -> Lwt_unix.sockaddr -> Server.t
