{
open Parse (* Assumes the parser file is "rtcalc.mly". *)
type token = 
| ADD of string
| SUBT of string
| MULT of string
| DIV of string
| COUNT of string
| ROWCOUNT of string
| COLCOUNT of string
| SUM of string
| ROWSUM of string
| COLSUM of string
| AVG of string
| ROWAVG of string
| COLAVG of string
| MIN of string
| ROWMIN of string
| COLMIN of string
| MAX of string
| ROWMAX of string
| COLMAX of string
| ASSIGNMENT of string
| SEMICOLON of char
| OPENSQUAREBRACKETS of char
| CLOSESQUAREBRACKETS of char
| OPENCURVEBRACKETS of char
| CLOSECURVEBRACKETS of char
| COMMA of char
| COLON of char

}
let digit = ['0'-'9']
let natural = 	['1'-'9']
let id = ['a'-'z' 'A'-'Z']['a'-'z' '0'-'9']*
let fl = '-' ? ('0' | natural digit* | natural digit* '.' digit* natural | '0' '.' digit* natural)
let assignment = ":="
rule token = parse
| '\n'{token lexbuf}
| fl as num 
{  NUM (float_of_string num)}
| [' ' '\t'] { token lexbuf }
| '\n' { NEWLINE }
| digit+
| "." digit+
| "ADD"  { ADD}
| "SUBT" {SUBT}
| "MULT" {MULT}
| "DIV" {DIV }
| "COUNT" {COUNT}
| "ROWCOUNT" {ROWCOUNT }
| "COLCOUNT" {COLCOUNT }
| "SUM" {SUM}
| "ROWSUM" {ROWSUM }
| "COLSUM" {COLSUM}
| "AVG" {AVG}
| "ROWAVG" {ROWAVG }
| "COLAVG" {COLAVG }
| "MIN" {MIN }
| "ROWMIN" {ROWMIN }
| "COLMIN" {COLMIN }
| "MAX" {MAX}
| "ROWMAX" {ROWMAX }
| "COLMAX" {COLMAX}
| assignment {ASSIGNMENT}
| '[' {OPENSQUAREBRACKETS}
| ']' {CLOSESQUAREBRACKETS}
| ')' {CLOSECURVEBRACKETS}
| ';' {SEMICOLON }
| ':' {COLON }
| '('  {OPENCURVEBRACKETS}
| ',' { COMMA }
| _{ token lexbuf }
| eof { raise End_of_file }
