module Client : State.S
module Server : State.S

val create_client : Lwt_unix.sockaddr -> Client.t
val create_server : Lwt_unix.sockaddr -> Server.t