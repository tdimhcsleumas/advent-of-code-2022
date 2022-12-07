module CharS = Set.Make(Char)

let lines = 
  let fileName = Sys.argv.(1) in
  let contents = In_channel.with_open_bin fileName In_channel.input_all in
  String.split_on_char '\n' contents

let makeSet s = CharS.add_seq (String.to_seq s) CharS.empty

let scoreFromSet set = 
  let vals = Seq.map (
    fun c ->
      if c >= 'a' && c <= 'z' then Char.code c - 97 + 1
      else if c >= 'A' && c <= 'Z' then Char.code c - 65 + 27
      else 0
  ) (CharS.to_seq set) in
  Seq.fold_left ( + ) 0 vals

let pt1 =
  let scores = List.map (
    fun line ->
      let len = String.length line in
      let mid = len / 2 in
      let first = makeSet (String.sub line 0 mid) in
      let second = makeSet (String.sub line mid mid) in
      scoreFromSet (CharS.inter first second)
  ) lines in

  let score = List.fold_left ( + ) 0 scores in

  Printf.printf "pt1 score: %d\n" score ;;

let pt2 = 
  let (_, _, groups) = List.fold_left (
    fun (count, list, lists) line ->
      if count == 2 then (0, [], (line :: list) :: lists)
      else (count + 1, line :: list, lists)
  ) (0, [], []) lines in

  let sets = List.map (
    fun list -> 
      let head = List.hd list in
      let tail = List.tl list in 
      List.fold_left (
        fun set next ->
          CharS.inter (makeSet next) set
      ) (makeSet head) tail
  ) groups in

  let score = List.fold_left ( + ) 0 (List.map scoreFromSet sets) in
  Printf.printf "pt2 score: %d\n" score ;;
    



