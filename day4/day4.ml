type range =
 | Range of int * int

type assignment = 
 | Assignment of range * range

let encloses = function
  | Assignment(Range(l1, r1), Range(l2, r2)) ->
    l1 <= l2 && r1 >= r2 || l2 <= l1 && r2 >= r1

let intersects = function
  | Assignment(Range(l1, r1), Range(l2, _)) when l1 <= l2 -> r1 >= l2
  | Assignment(Range(l1, _), Range(_, r2)) -> r2 >= l1

let get_assignments filename =
  Iter.IO.lines_of filename
  |> Iter.map (
    fun line ->
      String.split_on_char ',' line
      |> List.map (
        fun range ->
          String.split_on_char '-' range
          |> List.map (int_of_string)
          |> function | l :: r :: _ -> Range(l, r)
            | _ -> failwith "Invalid format"
        )
      |> function
          | l :: r :: _ -> Assignment(l, r)
          | _ -> failwith "Invalid Count"
    )

let pt1 filename =
  get_assignments filename
  |> Iter.fold (fun cur -> function
    | pair when encloses(pair) -> cur + 1
    | _ -> cur
  ) 0

let pt2 filename = 
  get_assignments filename
  |> Iter.fold (fun cur -> function
    | pair when intersects(pair) -> cur + 1
    | _ -> cur
  ) 0

let () = 
  let filename = Sys.argv.(1) in
  let pt1_result = (pt1 filename) in
  let pt2_result = (pt2 filename) in

  Printf.printf "pt1_result: %d, pt2_result: %d\n" pt1_result pt2_result ;;
