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

let input_field_elt, input_area_elt, upload_image_elt =
  let input_field_elt = Eliom_content.Html5.D.Raw.(textarea ~a:[a_maxlength 1000] (pcdata "")) in
  let upload_image_elt = input ~input_type:`File ~a:[a_accept ["image/*"]] () in
  let input_area_elt = 
    div ~a:[a_id "input_area"; a_style "display: none"] [
      input_field_elt;
      upload_image_elt;
    ]
  in
  input_field_elt, input_area_elt, upload_image_elt

    {shared{
type ('nick) context = {
  image_upload_service :
    (unit,
     Eliom_lib.file_info * (string * string),
     [ Eliom_service.service_method ],
     [ Eliom_service.attached ],
     [ `AttachedCoservice | `Service ],
     [ `WithoutSuffix ],
     unit,
     [ `One of Eliom_lib.file_info ]
       Eliom_parameter.param_name *
     ([ `One of string ] Eliom_parameter.param_name *
      [ `One of string ] Eliom_parameter.param_name),
     [ Eliom_service.registrable ],
     [ Eliom_service.http_service ])
      Eliom_service.service;
  backlog_service :
    (unit, unit,
     [ Eliom_service.service_method ],
     [ Eliom_service.attached ],
     [ Eliom_service.service_kind ],
     [ `WithoutSuffix ],
     unit,
     unit,
     [ Eliom_service.registrable ],
     Messages.processed_message list Eliom_service.ocaml_service)
      Eliom_service.service;
  names_service :
    (unit, unit,
     [ Eliom_service.service_method ],
     [ Eliom_service.attached ],
     [ Eliom_service.service_kind ],
     [ `WithoutSuffix ],
     unit,
     unit,
     [ Eliom_service.registrable ],
     string list Eliom_service.ocaml_service)
      Eliom_service.service;
  send_add_message : Messages.message -> unit Lwt.t;
  channel          : string;
  nick             : 'nick;
}
}}

{client{
open Common
open Eliom_content.Html5.D (* provides functions to create HTML nodes *)
module To_dom = Eliom_content.Html5.To_dom

let message_with_meta processed =
  let open Messages in
  match processed.pm_message.contents with
  | Text text ->
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
  | Image id ->
    let uri_orig = Eliom_uri.make_string_uri ~absolute:true ~service:%ImageDownload.service (id, 0) in
    let uri_small = Eliom_uri.make_string_uri ~absolute:true ~service:%ImageDownload.service (id, 1) in
    let img_elt = img ~src:uri_small ~alt:id () in
    let img = To_dom.of_img img_elt in
    img##onload <- Dom_html.handler (
        fun ev ->
          Eliom_content.Html5.Manip.scrollIntoView ~bottom:true %input_area_elt;
          Js._true
      );
    [Raw.a ~a:[a_href uri_orig; a_target "_new"] [img_elt]]

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

let println str =
  let element =
    div ~a:[a_class ["message"]]
      [span ~a:[a_class ["text"]] [pcdata str]] in
  Eliom_content.Html5.Manip.appendChild %message_area_elt element;
  Eliom_content.Html5.Manip.scrollIntoView ~bottom:true %input_area_elt;
  ()

let printelm elements =
  let element = div ~a:[a_class ["message"]] elements in
  Eliom_content.Html5.Manip.appendChild %message_area_elt element;
  Eliom_content.Html5.Manip.scrollIntoView ~bottom:true %input_area_elt;
  ()

type operation = string context -> unit Lwt.t

type command = {
  command     : string;
  description : string;
  operation   : operation;
}

let cmd_help commands context =
  commands
  |> List.map (fun command -> tr [td ~a:[a_class ["command"]] [pcdata command.command];
                                  td ~a:[a_class ["description"]] [pcdata command.description]])
  |> (fun tbl -> printelm [table tbl])
  |> Lwt.return

let cmd_names context =
  Eliom_client.call_ocaml_service ~service:context.names_service () () >>= fun names ->
  println ("Peole here: " ^ String.concat " " names);
  Lwt.return ()

let rec commands = [
  { command = "help";
    description = "Lists the available commands";
    operation = fun context -> cmd_help commands context };
  { command = "names";
    description = "Lists the people who are on the channel";
    operation = cmd_names }
]

let is_command name cmd = cmd.command = name

let process_command context channel nick line =
  match Re.split (Re_pcre.regexp "[\t ]+") line with
  | command::args when List.exists (is_command command) commands ->
    (List.find (is_command command) commands).operation context
  | _ ->
    println "No such command :(";
    Lwt.return ()

let process_line context channel nick line =
  if String.length line > 1 && String.sub line 0 1 = "/" then
    Lwt.async (fun () -> process_command context channel nick (String.sub line 1 (String.length line - 1)))
  else
    Lwt.async (fun () -> context.send_add_message Messages.{ timestamp = "now"; src = nick; dst = channel; contents = Text line })

let start_backlog ({ image_upload_service; backlog_service; send_add_message; channel; nick } as context) =
  let input_field = To_dom.of_textarea %input_field_elt in
  let input_area = To_dom.of_div %input_area_elt in
  let upload_image = To_dom.of_input %upload_image_elt in
  input_area##style##display <- Js.string "block";
  input_field##onkeydown <- Dom_html.handler (
      fun ev ->
        if ev##keyCode = 13 then
          ( let value = Js.to_string input_field##value in
            input_field##value <- Js.string "";
            process_line context channel nick value;
            Js._false )
        else
          Js._true
    );
  upload_image##onchange <- Dom_html.handler (
      fun event ->
        let files = upload_image##files in
        begin Js.Optdef.iter upload_image##files @@ fun files ->
          Js.Opt.iter files##item(0) @@ fun file ->
          Firebug.console##debug (Printf.ksprintf Js.string "size of file: %d" file##size);

          let xmlhttp = XmlHttpRequest.create () in
          let onreadystatechange () =
            match xmlhttp##readyState, xmlhttp##status with
            | XmlHttpRequest.DONE, 200 ->
              Firebug.console##debug (Printf.ksprintf Js.string "file uploaded: %s" (Js.to_string xmlhttp##responseText))
            (* (Dom_html.getElementById "myDiv")##innerHTML <- xmlhttp##responseText *)
            | _ -> ()
          in
          xmlhttp##onreadystatechange <- Js.wrap_callback onreadystatechange;
          let form = jsnew Form.formData () in
          form##append_blob (Js.string "image", Js.Unsafe.coerce file);
          form##append (Js.string "src", Js.string nick);
          form##append (Js.string "dst", Js.string channel);
          xmlhttp##_open (Js.string "POST", Js.string "image_upload", Js._true);
          xmlhttp##send_formData (form)
        end;
        Js._false
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

let init_client {names_service; image_upload_service; backlog_service; send_add_message; channel} =
  Lwt.async (
    fun () ->
      query_nick (fun nick -> start_backlog {names_service; image_upload_service; backlog_service; send_add_message; channel; nick})
  )
}}
