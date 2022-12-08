type fs =
  | File of {name: string; size: int}
  | Dir of {name: string; entries: (fs ref) list}

type state = 
  | State of {curdir: fs ref; stack: (fs ref) list}

let calc_size _ = 0

let safe_pcre rex str = 
  try Some(Pcre.extract ~rex str) with _ -> None

let mk_tree lines = 
  let cmd_ls_rex = Pcre.regexp "\\$ ls" in
  let cmd_cd_rex = Pcre.regexp "\\$ cd (.*)" in
  let dir_line_rex = Pcre.regexp "dir (.*)" in
  let file_line_rex = Pcre.regexp "(\\d+) (.*)" in

  let rec handle_ls lines state: state =
    let rec itr_entries lines entries = 
      match lines with
      | [] -> (entries, [])
      | entry :: tl ->
        let dir_line_ext = safe_pcre dir_line_rex entry in
        let file_line_ext = safe_pcre file_line_rex entry in

        match (dir_line_ext, file_line_ext) with
        | (Some(dir_line), None) ->
          let newdir = ref (Dir({name = dir_line.(1); entries = []})) in
          itr_entries tl (newdir :: entries)

        | (None, Some(file_line)) ->
          let newfile = ref (File({name = file_line.(2); size = file_line.(1) |> int_of_string})) in
          itr_entries tl (newfile :: entries)

        | _ -> (entries, lines)
    in

    let State({curdir; stack}) = state in
    let name = match !curdir with | Dir({name; _}) -> name | File({name; _}) -> name in
    let (entries, remaining) = itr_entries lines [] in

    curdir := (Dir({name; entries}));
    
    parse_action remaining (State({curdir; stack}))

  and handle_cd target lines state =
    let State({curdir; stack}) = state in
    if String.equal target ".." then
      parse_action lines (State({curdir = List.hd stack; stack = List.tl stack}))
    else
      let entries =
        match !curdir with
        | Dir({entries; _}) -> entries
        | _ -> failwith "must be a dir" in
      let newdir = List.find (fun entry -> 
        match !entry with
        | Dir({name; _}) -> String.equal name target
        | _ -> false
      ) entries in
      parse_action lines (State({curdir = newdir; stack = curdir :: stack}))

  and parse_action lines state = 
    match lines with
    | [] -> state
    | cmd_str :: tail ->

      let cmd_ls_extr = safe_pcre cmd_ls_rex cmd_str in
      let cmd_cd_extr = safe_pcre cmd_cd_rex cmd_str in

      match (cmd_ls_extr, cmd_cd_extr) with
      | (Some(_), None) -> handle_ls tail state
      | (None, Some(cd_extr)) -> handle_cd (cd_extr.(1)) tail state
      | _ when (String.equal cmd_str "") -> state
      | _ -> failwith (Printf.sprintf "invalid state: %s" cmd_str)
  in 

  match lines with
  | [] -> None
  | _ :: tail -> 
    let State({curdir = _; stack}) = parse_action tail (State({curdir = ref (Dir({name = "/"; entries = []})); stack = []})) in
    match stack with
    | [] -> None
    | _ ->
      Some(List.rev stack |> List.hd)

let rec dump_tree tree prefix = 
  match !tree with
  | File({name; _}) -> Printf.printf "%s- file: %s\n" prefix name
  | Dir({name; entries}) ->
    Printf.printf "%s- dir: %s\n" prefix name;
    List.iter (fun entry -> dump_tree entry ("    " ^ prefix)) entries

let pt1 () =
  let rec sum_dirs tree sum =
    match !tree with
    | File({name = _; size}) -> (size, 0)
    | Dir({name = _; entries}) ->
      let (dirsize, totalsum) = List.fold_left (
        fun (dirsize, totalsum) next ->
          let (size, sum) = (sum_dirs next sum) in
          (dirsize + size, totalsum + sum)
      ) (0, sum) entries in
      if dirsize <= 100000 then (dirsize, totalsum + dirsize)
      else (dirsize, totalsum)
  in

  match mk_tree (Advent.input_data()) with
  | Some(tree) ->
    let (_, sum) = sum_dirs tree 0 in
    sum
  | None ->
    print_endline "No tree produced!";
    0


let pt2 () =
  let rec total_sum tree = 
    match !tree with
    | File({name = _; size}) -> size
    | Dir({name = _; entries}) ->
      List.fold_left (
        fun cur next -> cur + (total_sum next)
      ) 0 entries
  in
  let rec sum_dirs tree total =
    match !tree with
    | File({name = _; size}) -> (size, max_int)
    | Dir({name = _; entries}) ->

      let (dirsize, minsize) = List.fold_left (
        fun (dirsize, minsize) next ->
          let (size, localmin) = (sum_dirs next total) in
          (dirsize + size, (Int.min localmin minsize))
      ) (0, max_int) entries in

      if (70000000 - total + dirsize) >= 30000000 then (dirsize, (Int.min minsize dirsize))
      else (dirsize, minsize)
  in

  match mk_tree (Advent.input_data()) with
  | None ->
    print_endline "No tree produced!";
    0

  | Some(tree) ->
    let total_sum = total_sum tree in
    let (_, min_dirsize) = sum_dirs tree total_sum in
    min_dirsize

let () = 
  let pt1_out = pt1 () in

  Printf.printf "pt1: %d\n" pt1_out;

  let pt2_out = pt2 () in 

  Printf.printf "pt2: %d\n" pt2_out ;; 
