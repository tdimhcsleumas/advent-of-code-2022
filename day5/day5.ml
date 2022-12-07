let calc_stack_count width = (width + 1) / 4

let gen_ints n =
  let rec gen_ints_rec n list = 
    if n == 0 then list
    else gen_ints_rec (n - 1) (n :: list)
  in
  gen_ints_rec n []

let rec stack_extractor exit_str lines stacks =
  match lines with 
  | [] -> ([], stacks)
  | line :: tl when String.equal line exit_str -> (tl, stacks)
  | line :: tl ->
    let new_stacks = Array.copy stacks in

    let apply_crate n crate =
      let idx = n / 4 in
      if crate != ' ' then new_stacks.(idx) <- (crate :: new_stacks.(idx))
      else ()
    in

    let rec parse_line = (function
      | [] -> ()
      | _ :: (n, crate) :: _ :: [] -> apply_crate n crate; parse_line []
      | _ :: (n, crate) :: _ :: _ :: tl -> apply_crate n crate; parse_line tl
      | _ -> ()
    ) in

    String.to_seqi line |> List.of_seq |> parse_line;

    stack_extractor exit_str tl new_stacks

let create_stacks lines = 
  match (lines) with
  | [] -> ([], Array.make 0 [])
  | head :: _ ->
    let stack_count = String.length head |> calc_stack_count in
    let stacks = Array.make stack_count [] in

    let exit_str = gen_ints stack_count
    |> List.map(fun n -> Printf.sprintf " %d " n)
    |> String.concat " " in

    Printf.printf "exit_str:\n%s\n" exit_str;

    stack_extractor exit_str lines stacks
    |> function | (lines, stacks) -> (lines, Array.map (List.rev) stacks)

let pt1 lines stacks =
  match lines with
  | [] -> ()
  | _ :: operations ->
    let rex = Pcre.regexp "move (\\w+) from (\\w+) to (\\w+)" in
    List.iter (fun opr ->
      let extracted = Pcre.extract ~rex opr in
      let amt = extracted.(1) |> int_of_string in
      let src = (extracted.(2) |> int_of_string) - 1 in
      let dest = (extracted.(3) |> int_of_string) - 1 in

      for _ = 0 to (amt - 1) do
        match stacks.(src) with
        | [] -> ()
        | head :: tail ->
          stacks.(dest) <- (head :: stacks.(dest));
          stacks.(src) <- tail
      done
    ) operations;

    Array.iter (fun list -> Printf.printf "%c" (List.hd list)) stacks;
    print_endline ""

let pt2 lines stacks =
  match lines with
  | [] -> ()
  | _ :: operations ->
    let rex = Pcre.regexp "move (\\w+) from (\\w+) to (\\w+)" in
    List.iter (fun opr ->
      print_endline opr;
      let extracted = Pcre.extract ~rex opr in
      let amt = extracted.(1) |> int_of_string in
      let src = (extracted.(2) |> int_of_string) - 1 in
      let dest = (extracted.(3) |> int_of_string) - 1 in
      let temp_stack = ref [] in

      for _ = 0 to (amt - 1) do
        match stacks.(src) with
        | [] -> ()
        | head :: tail ->
          temp_stack := (head :: !temp_stack);
          stacks.(src) <- tail
      done;

      stacks.(dest) <- (List.rev_append !temp_stack stacks.(dest));

    ) operations;

    Array.iter (fun list -> Printf.printf "%c" (List.hd list)) stacks;
    print_endline ""

let () = 
  let filename = Sys.argv.(1) in
  let pair = Iter.IO.lines_of filename |> Iter.to_list |> create_stacks in
  let (lines, stacks) = pair in

  pt1 lines (Array.copy stacks);
  pt2 lines (Array.copy stacks) ;;
