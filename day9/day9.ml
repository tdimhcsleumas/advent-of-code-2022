type action =
  | Up
  | Down
  | Left
  | Right

let parse_action action_str = 
  let (action_str, amt) = match String.split_on_char ' ' action_str with
  | action :: amt :: _ -> (action, int_of_string amt)
  | _ -> failwith "invalid string"
  in
  let action = match action_str with
  | "U" -> Up
  | "D" -> Down
  | "L" -> Left
  | "R" -> Right
  | _ -> failwith "invalid string"
  in (action, amt)

let adjust_tpos (xh, yh) (xt, yt) = 
  let xdiff = xh - xt in
  let ydiff = yh - yt in
  let adjust_x = if xdiff = 0 then 0
    else if xdiff > 0 then 1
    else -1
  in
  let adjust_y = if ydiff = 0 then 0
    else if ydiff > 0 then 1
    else -1
  in
  (xt + adjust_x, yt + adjust_y)

let is_adjacent (xh, yh) (xt, yt) = 
  let xdiff = if xh > xt then xh - xt else xt - xh in
  let ydiff = if yh > yt then yh - yt else yt - yh in
  xdiff <= 1 && ydiff <= 1

let pt1 lines = 
  let rec apply_action (action, amt) ((xh, yh), (xt, yt), visited) = 
    if amt == 0 then ((xh, yh), (xt, yt))
    else 
      let new_hpos = match action with
      | Up -> (xh, yh + 1)
      | Down -> (xh, yh - 1)
      | Left -> (xh - 1, yh)
      | Right -> (xh + 1, yh)
      in

      let new_tpos =
        if is_adjacent new_hpos (xt, yt) then (xt, yt)
        else adjust_tpos new_hpos (xt, yt)
      in

      Hashtbl.replace visited new_tpos 1;

      apply_action (action, amt - 1) (new_hpos, new_tpos, visited)
  in
  let rec simulate lines (hpos, tpos, visited) = 
    match lines with
    | [] -> visited
    | head :: _ when String.equal head "" -> visited
    | head :: tail ->
      let action = parse_action head in
      let (new_hpos, new_tpos) = apply_action action (hpos, tpos, visited) in
      simulate tail (new_hpos, new_tpos, visited)
  in
  let visited = simulate lines ((0, 0), (0, 0), Hashtbl.create 10000) in
  Hashtbl.length visited

let pt2 lines = 
  let rec apply_action (action, amt) ((xh, yh), rope, visited) = 
    if amt == 0 then ((xh, yh), rope)
    else 
        let new_hpos = match action with
        | Up -> (xh, yh + 1)
        | Down -> (xh, yh - 1)
        | Left -> (xh - 1, yh)
        | Right -> (xh + 1, yh)
        in

        for i = 0 to 8 do
          let prev =
            if i = 0 then new_hpos
            else rope.(i - 1)
          in

          let new_tpos =
            if is_adjacent prev rope.(i) then rope.(i)
            else adjust_tpos prev rope.(i)
          in

          rope.(i) <- new_tpos;
        done;

        Hashtbl.replace visited rope.(8) 1;

        apply_action (action, amt - 1) (new_hpos, rope, visited)
  in
  let rec simulate lines (hpos, rope, visited) = 
    match lines with
    | [] -> visited
    | head :: _ when String.equal head "" -> visited
    | head :: tail ->
      let action = parse_action head in
      let (new_hpos, new_rope) = apply_action action (hpos, rope, visited) in
      simulate tail (new_hpos, new_rope, visited)
  in
  let visited = simulate lines ((0, 0), Array.make 9 (0, 0), Hashtbl.create 20000) in
  Hashtbl.length visited

let () = 
  let test_lines = Advent.test_data() in
  let input_lines = Advent.input_data() in

  let pt1_test = pt1 test_lines in
  let pt1_input = pt1 input_lines in

  Printf.printf "pt1 test: %d\n" pt1_test;
  Printf.printf "pt1 input: %d\n" pt1_input;

  let pt2_test = pt2 test_lines in
  let pt2_input = pt2 input_lines in

  Printf.printf "pt2 test: %d\n" pt2_test;
  Printf.printf "pt2 input: %d\n" pt2_input;;
