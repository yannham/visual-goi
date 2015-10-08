open Lambda
open Translate

let filename = Sys.argv.(1)

let rec stringify = function
  | Untyped.Var(v) -> Printf.sprintf "%c" v
  | Untyped.Const(i) -> Printf.sprintf "%d" i
  | Untyped.Abs(v,t) -> Printf.sprintf "\\%c.(%s)" v (stringify t)
  | Untyped.App(t1,t2) -> Printf.sprintf "(%s) (%s)" (stringify t1) (stringify t2)

let main =
  let in_buff = open_in filename in
  let line_buff = Lexing.from_channel in_buff in

  try
    let term = Untyped_parser.main Untyped_lexer.main line_buff in
    Printf.printf "Accepted term : %s\n" (stringify term);
    Printf.printf "Saving in term.dot...\n";
    (
      try
        let oc = open_out "images/net.dot" in
        let output s = Printf.fprintf oc "%s" s in
        let net = UntypedLinearToMELLYS.net_of_term term in
        Viewer.MELLYSToDot.generate_net_dot output net;
        flush oc;
        close_out oc
      with _ -> Printf.fprintf stderr "Error when writing the net in net.dot\n"
    );
    Printf.printf "Done.\n";
    Printf.printf "Converting to net.svg...\n"; 
    (
      try (
        match (Unix.system "dot -Tsvg images/net.dot -o images/net.svg") with
          | Unix.WEXITED(0) -> Printf.printf "Done.\n"
          | Unix.WEXITED(c) -> Printf.printf "Error : dot exited with error code (%d)\n" c
          | _ -> Printf.printf "Error : dot process has been killed or suspended\n"
      )
      with _ -> failwith "Unknown error during the conversion from dot to svg\n"
    )
  with
      | Untyped_lexer.Error msg -> Printf.fprintf stderr "%s%!\n" msg
      | Untyped_parser.Error -> Printf.fprintf stderr "Syntax error in source file at position %d\n%!"
                        (Lexing.lexeme_start line_buff)
