
let pt1 lines =
  let sums = (Hashtbl.create 400) in
  let rec simulate lines start itr x =
    if itr > 250 then sums
    else
      match lines with
      | [] -> simulate start start itr x
      | "" :: _ -> simulate start start itr x
      | hd :: tl ->

        match String.split_on_char ' ' hd with
        | "noop" :: [] ->
          Hashtbl.replace sums itr x;
          simulate tl start (itr + 1) x

        | "addx" :: amt_str :: _ ->
          let amt = int_of_string amt_str in
          Hashtbl.replace sums itr x;
          Hashtbl.replace sums (itr + 1) x;
          simulate tl start (itr + 2) (x + amt)

        | _ -> failwith (Printf.sprintf "Invalid string: %s" hd)
  in
  let sums = simulate lines lines 1 1 in
  let sum = ref 0 in
  for i = 0 to 5 do
    let cycle = 20 + (i * 40) in
    sum := (Hashtbl.find sums cycle) * cycle + !sum
  done;
  (!sum, sums)

let pt2 sums =  
  let rec draw_screen itr col = 
    match itr, col with
    | itr, _ when itr > 240 -> print_endline "";
    | _, 40 -> print_endline ""; draw_screen itr 0
    | itr, col ->
      let pos = (Hashtbl.find sums itr) in
      if pos = col || pos + 1 = col || pos - 1 = col
        then print_char '#'
        else print_char '.';
      draw_screen (itr + 1) (col + 1)
  in
  draw_screen 1 0

let () = 
  let test_lines = Advent.test_data() in
  let input_lines = Advent.input_data() in

  let (pt1_test, pt1_test_sums) = pt1 test_lines in
  let (pt1_input, pt1_input_sums) = pt1 input_lines in

  Printf.printf "pt1 test: %d\n" pt1_test;
  Printf.printf "pt1 input: %d\n" pt1_input;

  print_endline "pt2 test";
  pt2 pt1_test_sums;

  print_endline "pt2 input";
  pt2 pt1_input_sums;

