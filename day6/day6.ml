module CharS = Set.Make(Char)

let count_unique str = 
  CharS.add_seq (String.to_seq str) CharS.empty
  |> CharS.elements |> List.length

let first_n_unique n line =
  let rec itr line idx = 
    if (String.length line - idx) <= (n - 1) then failwith("not found!")
    else if String.sub line idx n |> count_unique == n then idx + n 
    else itr line (idx + 1)
  in
  itr line 0

let pt1 = first_n_unique 4

let pt2 = first_n_unique 14

let () = 
  let filename = Sys.argv.(1) in
  let line = Iter.IO.lines_of filename |> Iter.to_list |> List.hd in
  Printf.printf "pt1: %d\n" (pt1 line);
  Printf.printf "pt2: %d\n" (pt2 line);
