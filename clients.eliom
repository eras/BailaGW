open Types

{shared{
type t = {
  id      : string;
  stream  : Types.processed_message Lwt_stream.t;
  send    : Types.processed_message option -> unit;
  channel : Types.processed_message Eliom_comet.Channel.t;
}
}}

{server{
let clients = ref []

let current_client : t Eliom_state.volatile_table = Eliom_state.create_volatile_table ~scope:Eliom_common.default_session_scope ()

let add_client id =
  let (stream, send) = Lwt_stream.create () in
  let client = {
    id;
    stream;
    send;
    channel = Eliom_comet.Channel.create ~name:("messages" ^ id) stream;
  } in
  clients := client::!clients;
  client

let broadcast msg =
  List.iter
    (fun client ->
       try
         Printf.printf "Sending to %s\n%!" client.id;
         client.send (Some msg);
         Printf.printf "Sent to %s\n%!" client.id;
       with exn ->
         Printf.printf "Exception %s while sending to client %s\n%!"
           (Printexc.to_string exn)
           client.id
    )
    !clients;
  Lwt.return ()
}}
