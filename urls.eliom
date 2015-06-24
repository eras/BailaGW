let flip f a b = f b a

let url_re = Pcre.regexp ~flags:[`UTF8] "(?<!!)\\b((?:[a-z]{2,})://(?:[^ .,:;>]|[,.:;>](?! ))*)"

let tag_re = Pcre.regexp ~flags:[`UTF8] " *\\[((?:[\\w\\d _!-]+)(?:,[\\w\\d _!-]+)*)\\] *"

module IntSet = Set.Make(struct type t = int let compare = compare end)

let rec (--<) a b =
  if a < b then a::(a + 1) --< b
  else []

let inter_overlapping ms1 ms2 =
  let used2 = 
    List.fold_left
      (fun set substrings ->
         let (ofs0, ofs1) = Pcre.get_substring_ofs substrings 0 in
         List.fold_left (fun set x -> IntSet.add x set) set (ofs0 --< ofs1)
      )
      IntSet.empty
      ms2
  in
  List.filter
    (fun substrings ->
       let (ofs0, ofs1) = Pcre.get_substring_ofs substrings 0 in
       let (_, found, right) = IntSet.split ofs0 used2 in
       not found || IntSet.min_elt right >= ofs1
    )
    ms1

let adjust_pair ofs (a, b) = (a + ofs, b + ofs)

let trim s =
  let s = Pcre.replace ~pat:"^ *" ~templ:"" s in
  let s = Pcre.replace ~pat:" *$" ~templ:"" s in
  s

let concat_map f l = List.concat (List.map f l)

let urls_tags_of_string string = 
  try
    let url_matches = Array.to_list (Pcre.exec_all ~rex:url_re string) in
    let tag_matches = try Array.to_list (Pcre.exec_all ~rex:tag_re string) with Not_found -> [] in
    let tag_matches = inter_overlapping tag_matches url_matches in
    let tags = 
      try List.map trim @@ concat_map (Pcre.split ~pat:",") @@ List.map (flip Pcre.get_substring 1) tag_matches
      with Not_found -> [] in
    let url_matches = List.map (flip Pcre.get_substring_ofs 0) url_matches in
    let url_matches, string = 
      let ofs, url_matches, strings, _ =
        List.fold_left
          (fun (prev_ofs, url_matches, strings, cur_delta) tag_match ->
             (* replace tags with a space and adjust url match offsets properly *)
             let (ofs0, ofs1) = Pcre.get_substring_ofs tag_match 0 in
             let len = ofs0 - prev_ofs in
             let delta = ofs0 - ofs1 in
             let strings, delta =
               if len > 0 then (" ", `Filler)::(String.sub string prev_ofs len, `Content)::strings, delta + 1
               else strings, delta
             in
             (ofs1, 
              List.map (fun x -> 
                  if fst x >= ofs0 + cur_delta then
                    adjust_pair delta x
                  else
                    x)
                url_matches, 
              strings,
              cur_delta + delta)
          )
          (0, url_matches, [], 0)
          tag_matches
      in
      let strings = 
        if ofs <> String.length string then
          (String.sub string ofs (String.length string - ofs), `Content)::strings 
        else
          strings 
      in
      let strings = match strings with
        | (_, `Content)::_ -> strings
        | (_, `Filler)::rest -> rest
        | [] -> []
      in
      (url_matches, String.concat "" (List.rev (List.map fst strings)))
    in
    let (urls, prev_ofs, buf) = 
      List.fold_left 
        (fun (urls, prev_ofs, buf) (ofs0, ofs1) -> 
           let len = ofs1 - ofs0 in
           (((ofs0, len), String.sub string ofs0 len)::urls, ofs1, buf)
        )
        ([], 0, Buffer.create (String.length string))
        url_matches
    in
    (List.rev urls, tags, string)
  with Not_found -> ([], [], string)
