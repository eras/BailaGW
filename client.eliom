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

(*
(function () {
    var takePicture = document.querySelector("#take-picture"),
        showPicture = document.querySelector("#show-picture");

    if (takePicture && showPicture) {
        // Set events
        takePicture.onchange = function (event) {
            // Get a reference to the taken picture or chosen file
            var files = event.target.files,
                file;
            if (files && files.length > 0) {
                file = files[0];
                try {
                    // Create ObjectURL
                    var imgURL = window.URL.createObjectURL(file);

                    // Set img src to ObjectURL
                    showPicture.src = imgURL;

                    // Revoke ObjectURL
                    URL.revokeObjectURL(imgURL);
                }
                catch (e) {
                    try {
                        // Fallback if createObjectURL is not supported
                        var fileReader = new FileReader();
                        fileReader.onload = function (event) {
                            showPicture.src = event.target.result;
                        };
                        fileReader.readAsDataURL(file);
                    }
                    catch (e) {
                        //
                        var error = document.querySelector("#error");
                        if (error) {
                            error.innerHTML = "Neither createObjectURL or FileReader are supported";
                        }
                    }
                }
            }
        };
    }
})();
*)

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

   let start_backlog image_upload_service backlog_service send_add_message channel nick =
     let input_field = To_dom.of_textarea %input_field_elt in
     let input_area = To_dom.of_div %input_area_elt in
     let upload_image = To_dom.of_input %upload_image_elt in
     input_area##style##display <- Js.string "block";
     input_field##onkeydown <- Dom_html.handler (
         fun ev ->
           if ev##keyCode = 13 then
             ( let value = Js.to_string input_field##value in
               Lwt.async (fun () -> send_add_message Messages.{ timestamp = "now"; src = nick; dst = channel; contents = Text value });
               input_field##value <- Js.string "";
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
        
   let init_client image_upload_service backlog_service send_add_message channel =
     Lwt.async (
       fun () ->
         query_nick (start_backlog image_upload_service backlog_service send_add_message channel)
     )
}}
