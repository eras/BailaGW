open Eliom_lib
open Eliom_content
open Html5.D

let login_elt, login_input_elt =
  let login_input_elt = input ~input_type:`Text () in
  let login_elt =
    div ~a:[a_id "login"; a_style "display: none"]
      [p [pcdata "millä nimellä kuljet?"]; login_input_elt]
  in
  login_elt, login_input_elt
    
let message_area_elt =
  div ~a:[a_id "message_area"; a_style "display: none"] [pcdata "Baila baila"]

let input_area_elt =
  Eliom_content.Html5.D.Raw.(textarea ~a:[a_id "input_area"; a_maxlength 1000; a_style "display: none"] (pcdata ""))

{client{
   open Common
   open Eliom_content.Html5.D (* provides functions to create HTML nodes *)

   let message_with_meta processed =
     let text = processed.Messages.pm_message.Messages.text in
     let rec scan offset meta =
       match meta with
       | [] ->
         [pcdata (String.sub text offset (String.length text - offset))]
       | ((ofs, len), Messages.Url url)::rest when ofs = offset ->
         Raw.a ~a:[a_href url; a_target "_new"] [pcdata url]::scan (ofs + len) rest
       | (((ofs, _ofs1), _)::_) as meta ->
         assert (ofs > offset);
         pcdata (String.sub text offset (ofs - offset))::scan ofs meta
     in
     scan 0 processed.Messages.pm_meta

   let add_processed_message (processed : Messages.processed_message) =
     (* let area = Eliom_content.Html5.To_dom.of_p %message_area_elt in *)
     (* let () = area##innerHTML##appendData (Js.string message) in *)
     (* let () = area##innerHTML <- area##innerHTML##concat ((Js.string message)##concat (Js.string " moi ")) in *)
     let message = processed.Messages.pm_message in
     let element =
       div ~a:[a_class ["message"]]
         [span ~a:[a_class ["src"]] [pcdata message.Messages.src];
          span ~a:[a_class ["timestamp"]] [pcdata message.Messages.timestamp];
          span ~a:[a_class ["text"]] (message_with_meta processed)] in
     Eliom_content.Html5.Manip.appendChild %message_area_elt element;
     Eliom_content.Html5.Manip.scrollIntoView ~bottom:true %input_area_elt;
     ()

   let start_backlog backlog_service send_add_message channel nick =
     let input_area = Eliom_content.Html5.To_dom.of_textarea %input_area_elt in
     input_area##style##display <- Js.string "block";
     input_area##onkeydown <- Dom_html.handler (
         fun ev ->
           if ev##keyCode = 13 then
             ( let value = Js.to_string input_area##value in
               Lwt.async (fun () -> send_add_message Messages.{ timestamp = "now"; src = "BailaGW/" ^ nick; dst = channel; text = value });
               input_area##value <- Js.string "";
               Js._false )
           else
             Js._true
       );
     Eliom_client.call_ocaml_service ~service:backlog_service () () >>= fun response ->
     (Eliom_content.Html5.To_dom.of_div %message_area_elt)##style##display <- Js.string "block";
     List.iter add_processed_message response;
     Lwt.async (fun () -> Lwt_stream.iter add_processed_message (Eliom_bus.stream %Messages.bus));
     Lwt.return ()

    let query_nick continue =
      let nick_box = (Eliom_content.Html5.To_dom.of_div %login_elt) in
      let nick_input = (Eliom_content.Html5.To_dom.of_input %login_input_elt) in
      nick_box##style##display <- Js.string "block";
      nick_input##onkeydown <- Dom_html.handler (
          fun ev ->
            if ev##keyCode = 13 then
              ( let value = Js.to_string nick_input##value in
                nick_box##style##display <- Js.string "none";
                Lwt.async (fun () -> continue value);
                Js._false )
            else
              Js._true
        );
      Lwt.return ()
        
   let init_client backlog_service send_add_message channel =
     Lwt.async (
       fun () ->
         query_nick (start_backlog backlog_service send_add_message channel)
     )
}}
