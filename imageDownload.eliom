open Eliom_lib
open Eliom_content
open Html5.D

let service =
  Eliom_registration.Any.register_service
    ~path:["BailaGW"; "image"]
    ~get_params:Eliom_parameter.(string "id" ** int "scale")
    (fun (id, scale) () ->
       Messages.find_image id scale >>= function
       | Some (src, dst, uuid, content_type, _scale) ->
         Eliom_registration.File.send
           ~content_type
           (Printf.sprintf "images/%s.%d" uuid scale)
       | None when scale = 1 ->
         begin Messages.find_image id 0 >>= function
           | Some (src, dst, uuid, content_type, _scale) ->
             begin
               match Pcre.extract ~pat:"/([a-zA-Z0-9]*)" ~full_match:false content_type with
               | [|type_|] ->
                 let args =
                   [|"convert"; "-strip"; "-interlace"; "Plane";
                     Printf.sprintf "%s:images/%s.0" type_ id;
                     "-scale"; "600x600";
                     Printf.sprintf "jpeg:images/%s.1" id|]
                 in
                 begin Lwt_process.exec ~timeout:10.0 ("/usr/bin/convert", args) >>= function
                   | Unix.WEXITED 0 ->
                     Messages.add_image src dst id "image/jpeg" 1 >>= fun () ->
                     Eliom_registration.File.send ~content_type:"image/jpeg" (Printf.sprintf "images/%s.1" id)
                   | _ -> Eliom_registration.File.send "/dev/null"
                 end
               | exception Not_found -> Eliom_registration.File.send "/dev/null"
               | _ -> Eliom_registration.File.send "/dev/null"
             end
           | None -> Eliom_registration.File.send "/dev/null"
         end
       | None ->
         Eliom_registration.File.send "/dev/null"
    )
