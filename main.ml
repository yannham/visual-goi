open Lambda
open Translate

let filename = Sys.argv.(1)

let rec stringify = function
  | Untyped.Var(v) -> Printf.sprintf "%c" v
  | Untyped.Const(i) -> Printf.sprintf "%d" i
  | Untyped.Abs(v,t) -> Printf.sprintf "\\%c.(%s)" v (stringify t)
  | Untyped.App(t1,t2) -> Printf.sprintf "(%s) (%s)" (stringify t1) (stringify t2)

let main =
  (* let in_buff = open_in filename in
  let line_buff = Lexing.from_channel in_buff in *)

  try
    (* let term = Untyped_parser.main Untyped_lexer.main line_buff in
    Printf.printf "Terme accepté : %s\n" (stringify term); *)
    let t1 = SimplyTyped.Arrow(SimplyTyped.Unit, SimplyTyped.Unit) in
    let id = SimplyTyped.TAbs('x', SimplyTyped.Unit, TVar('x', SimplyTyped.Unit)) in
    let n = SimplyTyped.TApp(SimplyTyped.TVar('x', t1), SimplyTyped.TConst(1)) in
    let m = SimplyTyped.TAbs('f', t1, SimplyTyped.TApp(
      SimplyTyped.TVar('f',t1), SimplyTyped.TConst(1))) in
    let term = SimplyTyped.TApp(m, id) in
    Printf.printf "Ecriture dans term.dot...\n";
    let oc = open_out "images/net.dot" in
    let output s = Printf.fprintf oc "%s" s in
    (* let net = UntypedLinearToMELLYS.net_of_term term in *)
    let net = SimplyTypedToMELLYS_CBN.net_of_term term in
    (* let net,ax_v,ax_neg,ax_pos = Net.MELLYS.axiom (Logic.MELL.Atom("x")) in
    let der_v,der_e = Net.MELLYS.der net ax_neg in
    let r,bang_v,bang_e,bang_assoc = Net.MELLYS.bang net ax_pos in *)
    Viewer.MELLYSToDot.generate_net_dot output net;
    (* with _ -> failwith "Erreur lors de l'écriture du fichier dot\n" *)
    flush oc;
    close_out oc;
    Printf.printf "Effectué.\n";
    Printf.printf "Conversion en net.svg...\n"; 
    (
      try (
        match (Unix.system "dot -Tsvg images/net.dot -o images/net.svg") with
          | Unix.WEXITED(0) -> Printf.printf "Effectué.\n"
          | Unix.WEXITED(c) -> Printf.printf "Erreur : dot exited (%d)\n" c
          | _ -> Printf.printf "Erreur : le processus dot a été tué ou suspendu\n"
      )
      with _ -> failwith "Erreur lors de la conversion dot->svg\n"
    )
  with
    (* | Untyped_lexer.Error msg -> Printf.fprintf stderr "%s%!\n" msg
      | Untyped_parser.Error -> Printf.fprintf stderr "Erreur de syntaxe position %d\n%!"
                        (Lexing.lexeme_start line_buff)*)
    | _ -> ()
