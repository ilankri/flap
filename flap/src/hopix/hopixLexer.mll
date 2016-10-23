{
  open Lexing
  open Error
  open Position
  open HopixParser

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)


}

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']


rule token = parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }
  | eof             { EOF       }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }

