(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

{
  open Untyped_parser 
  exception Error of string
}

rule main = parse
| '\n' 
    { main lexbuf }
| ['0'-'9']+ as i
    { CONST (int_of_string i) }
| '\\'' '*(['a'-'z'] as v)' '*'.'
    { LAMBDA (v) }
| ['a'-'z'] as v 
    { VAR (v) }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| [' ' '\t']
    { SPACE }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
