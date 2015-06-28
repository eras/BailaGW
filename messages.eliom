open Common
module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Lwt)
module S = Sqlexpr

{shared{
type timestamp = string deriving (Json)

type message = {
  timestamp : timestamp;
  src       : string;
  dst       : string;
  text      : string;
} deriving (Json)

type messages = message list deriving (Json)

type range = (int * int) deriving (Json)

type fragment =
  | Url of string
deriving (Json)

type processed_message = {
  pm_meta    : (range * fragment) list;
  pm_message : message;
} deriving (Json)

}}

let bus = Eliom_bus.create ~name:"messages" Json.t<processed_message>

{server{
   let message_db =
     let db = S.open_db "bailagw.sqlite3" in
     Lwt_unix.run (
       S.execute db
         sqlinit"CREATE TABLE IF NOT EXISTS message(
              message INTEGER PRIMARY KEY,
              timestamp TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
              src TEXT NOT NULL,
              dst TEXT NOT NULL,
              str TEXT NOT NULL
            );" >>= fun () ->
       S.execute db
         sqlinit"CREATE TABLE IF NOT EXISTS config(
              key TEXT NOT NULL,
              value TEXT NOT NULL
            );" >>= fun () ->
       S.execute db
         sqlinit"CREATE TABLE IF NOT EXISTS image(
              image TEXT NOT NULL,
              scale INTEGER NOT NULL DEFAULT 0, -- scale = 0 = original
              src TEXT NOT NULL,
              dst TEXT NOT NULL,
              timestamp TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
              content_type TEXT NOT NULL
            );"
     );
     db

  let db_add_message message =
    S.execute message_db sql"INSERT INTO message(src, dst, str) VALUES (%s, %s, %s)" message.src message.dst message.text >>= fun () ->
    S.select_one message_db sql"SELECT @s{datetime(timestamp, 'localtime')} FROM message WHERE message = last_insert_rowid()" >>= fun timestamp ->
    Lwt.return { message with timestamp = timestamp }

  let process_message message =
    let urls, tags, text = Urls.urls_tags_of_string message.text in
    {
      pm_meta    = List.map (fun (range, text) -> (range, Url text)) urls;
      pm_message = { message with text };
    }

  let message_to_clients (message : message) =
    let _ = Eliom_bus.write bus (process_message message) in
    Lwt.return ()
}}

let all_messages_query = sqlc"SELECT @s{datetime(timestamp, 'localtime')}, @s{src}, @s{dst}, @s{str} FROM message ORDER BY timestamp"

let of_sql_message (timestamp, src, dst, text) = { timestamp; src; dst; text }

let iter_all f =
  S.iter message_db
    (of_sql_message @. f)
    all_messages_query

let get_all () =
  S.select message_db all_messages_query >>= fun messages ->
  List.map of_sql_message messages |> Lwt.return

let add_image src dst uuid content_type =
  S.execute message_db sql"INSERT INTO image(image, src, dst, content_type) VALUES (%s, %s, %s, %s)" src dst uuid content_type

let () =
  Lwt.async (
    fun () ->
      iter_all message_to_clients
  )
