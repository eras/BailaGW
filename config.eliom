type config = {
  c_nick       : string;
  c_username   : string;
  c_realname   : string;
  c_channel    : string;
  c_irc_server : string;
  c_irc_port   : int;
}

{server{
open Common
module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Lwt)
module S = Sqlexpr

let config db =
  let select_one_opt key = 
    S.select db sql"SELECT @s{value} FROM config WHERE key = %s" key >>= function
      | value::[] -> Lwt.return value
      | _ -> Lwt.fail (Invalid_argument ("Missing configuration key " ^ key))
  in
  Lwt_unix.run (
    select_one_opt "nick"       >>= fun c_nick ->
    select_one_opt "username"   >>= fun c_username ->
    select_one_opt "realname"   >>= fun c_realname ->
    select_one_opt "channel"    >>= fun c_channel ->
    select_one_opt "irc_server" >>= fun c_irc_server ->
    select_one_opt "irc_port"   >>= fun c_irc_port ->
    Lwt.return { c_nick; c_username; c_realname;
                 c_channel;
                 c_irc_server; c_irc_port = int_of_string c_irc_port }
  )
}}
