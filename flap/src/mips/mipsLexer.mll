{
  open Error
  open Position
  open Lexing
  open MipsParser

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)

}

rule token = parse
  | eof             { EOF }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }
