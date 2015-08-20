/**************************************************************************/
/*                                                                        */
/*  Menhir                                                                */
/*                                                                        */
/*  François Pottier, INRIA Rocquencourt                                  */
/*  Yann Régis-Gianas, PPS, Université Paris Diderot                      */
/*                                                                        */
/*  Copyright 2005-2008 Institut National de Recherche en Informatique    */
/*  et en Automatique. All rights reserved. This file is distributed      */
/*  under the terms of the Q Public License version 1.0, with the change  */
/*  described in file LICENSE.                                            */
/*                                                                        */
/**************************************************************************/

%token <int> CONST 
%token <char> VAR
%token <char> LAMBDA 
%token LPAREN RPAREN
%token SPACE
%token EOF
%left SPACE

%start <Lambda.Untyped.term> main

%%

main:
  | t = term EOF { t }

term:
  | c = CONST { Lambda.Untyped.Const(c) }
  | v = VAR { Lambda.Untyped.Var(v) }
  | v = LAMBDA t = term { Lambda.Untyped.Abs(v,t) }
  | t1 = term SPACE t2 = term { Lambda.Untyped.App(t1,t2) }
  | LPAREN e = term RPAREN { e }
