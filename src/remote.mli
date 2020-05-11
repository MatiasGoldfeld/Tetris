module Client : State.S
module Server : State.S

val create_client : string -> Lwt_unix.sockaddr -> Client.t
val create_server : string -> Lwt_unix.sockaddr -> Server.t