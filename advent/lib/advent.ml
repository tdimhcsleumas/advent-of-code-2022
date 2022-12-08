let lines filename = 
  let contents = In_channel.with_open_bin filename In_channel.input_all in
  String.split_on_char '\n' contents
  
let test_data () =
  lines "./test.txt"

let input_data () =
  lines "./input.txt"
