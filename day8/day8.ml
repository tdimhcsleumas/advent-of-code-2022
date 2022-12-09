let build_mat lines = 
  let rec build_mat_rec lines mat =
    match lines with
    | [] -> mat
    | line :: _ when String.equal line "" -> mat
    | line :: rem ->
      let row = String.to_seq line
      |> Seq.map (fun c -> (Char.code c) - 0x30)
      |> Array.of_seq in
      let mat = (Array.append mat (Array.make 1 row)) in
      build_mat_rec rem mat
  in
  build_mat_rec lines (Array.make 0 (Array.make 0 0))

let fold_forward mat func state = 
  let rec fold_forward_rec mat coords func state =
    match coords with
    | i, _ when i == (Array.length mat) -> state
    | i, j when j == (Array.length mat.(i)) -> fold_forward_rec mat (i + 1, 0) func state
    | i, j ->
      let new_state = (func mat (i, j) state) in
      fold_forward_rec mat (i, j + 1) func new_state
  in
  fold_forward_rec mat (0, 0) func state

let fold_backward mat func state = 
  let rec fold_backward_rec mat coords func state =
    match coords with
    | i, _ when i == -1 -> state
    | i, j when j == -1 -> fold_backward_rec mat (i - 1, (Array.length mat.(i)) - 1) func state
    | i, j ->
      let new_state = (func mat (i, j) state) in
      fold_backward_rec mat (i, j - 1) func new_state
  in
  fold_backward_rec mat (
    (Array.length mat) - 1,
    (Array.length mat.(0)) - 1
  ) func state

let pt1 mat =
  let fold_cb mat (i, j) (max_rows, max_cols, visibles) =
    let entry = mat.(i).(j) in
    if (entry > max_rows.(i) || entry > max_cols.(j)) then
      begin
        max_rows.(i) <- Int.max entry max_rows.(i);
        max_cols.(j) <- Int.max entry max_cols.(j);
        (max_rows, max_cols, (i, j) :: visibles)
      end
    else 
      (max_rows, max_cols, visibles)
  in
  let itr_forward mat = 
    let (_, _, visibles) = fold_forward mat fold_cb (
      Array.make (Array.length mat) (-1),
      Array.make (Array.length mat.(0)) (-1),
      []
    ) in
    visibles
  in
  let itr_backward mat = 
    let (_, _, visibles) = fold_backward mat fold_cb (
      Array.make (Array.length mat) (-1),
      Array.make (Array.length mat.(0)) (-1),
      []
    ) in
    visibles
  in

  let visibles = itr_forward mat in
  let rev_visibles = itr_backward mat in
  let all_visibles = (List.rev_append visibles rev_visibles) in
  (List.sort_uniq (fun (i1, j1) (i2, j2) -> 
    if i1 == i2 && j1 == j2 then 0
    else if i1 < i2 || (i1 == i2 && j1 < j2) then -1
    else 1
  ) all_visibles)
  |> (List.length)


let dist x1 x2 = 
  if x1 > x2 then x1 - x2
  else x2 - x1

let closest_opt arr target = 
  Array.fold_left(fun cur next ->
    match (cur, next) with
    | None, None -> None
    | None, Some(i) -> Some(i)
    | Some(i), None -> Some(i)
    | Some(i1), Some(i2) ->
      let diff1 = dist i1 target in
      let diff2 = dist i2 target in
      if diff1 < diff2 then Some(i1) else Some(i2)
  ) None arr

  (*
    Consider the value for the previously considered entry
    * if it's smaller than the current entry then score = prev + 1   
  *)
let pt2 mat =
  let starting_scores = Hashtbl.create 10000 in
  let fold_cb mat (i, j) (is_forward, last_i_pos, last_j_pos, scores) =
    let entry = mat.(i).(j) in

    let i_sub = Array.sub last_i_pos.(j) entry (10 - entry)  in
    let i_max_opt = closest_opt i_sub i in

    let j_sub = Array.sub last_j_pos.(i) entry (10 - entry) in
    let j_max_opt = closest_opt j_sub j in

    let row_score =
      if i == 0 || i == (Array.length mat) - 1 then 0
      else 
        match i_max_opt with
        | None -> if (is_forward) then i else ((Array.length mat) - 1) - i
        | Some(i_max) -> dist i i_max
    in

    let col_score =
      if j == 0 || j == (Array.length mat.(0)) - 1 then 0
      else
        match j_max_opt with
        | None -> if (is_forward) then j else ((Array.length mat.(0)) - 1) - j
        | Some(j_max) -> dist j j_max
    in
    
    last_i_pos.(j).(entry) <- Some(i);
    last_j_pos.(i).(entry) <- Some(j);

    let curscore = match Hashtbl.find_opt scores (i, j) with
    | Some(score) -> score
    | None -> 1
    in

    Hashtbl.replace scores (i, j) (row_score * col_score * curscore);

    (is_forward, last_i_pos, last_j_pos, scores)
  in
  let itr_forward mat starting_scores = 
    let (_, _, _, scores) = fold_forward mat fold_cb (
      true,
      Array.make_matrix (Array.length mat) 10 None,
      Array.make_matrix (Array.length mat) 10 None,
      starting_scores
    ) in
    scores
  in
  let itr_backward mat starting_scores = 
    let (_, _, _, scores) = fold_backward mat fold_cb (
      false,
      Array.make_matrix (Array.length mat) 10 None,
      Array.make_matrix (Array.length mat) 10 None,
      starting_scores
    ) in
    scores
  in

  let for_scores = itr_forward mat starting_scores in
  let all_scores = itr_backward mat for_scores in
  Hashtbl.fold (fun _ v cur -> Int.max v cur) all_scores 0

let () = print_endline "Hello, World!";
  let test_mat = build_mat (Advent.test_data()) in
  let input_mat = build_mat (Advent.input_data()) in

  let pt1_test = pt1 test_mat in
  let pt1_input = pt1 input_mat in

  Printf.printf "pt1 test: %d\n" pt1_test;
  Printf.printf "pt1 input: %d\n" pt1_input;

  let pt2_test = pt2 test_mat in
  let pt2_input = pt2 input_mat in

  Printf.printf "pt2 test: %d\n" pt2_test;
  Printf.printf "pt2 input: %d\n" pt2_input;;
