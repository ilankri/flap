{
  open Lexing
  open Error
  open Position
  open JavixParser

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)

}

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']

let lowercase_alpha = ['a'-'z' '_']

let uppercase_alpha = ['A'-'Z' '_']

let alpha = lowercase_alpha | uppercase_alpha

let alphanum = alpha | digit | '/'

let identifier = alpha alphanum*

rule token = parse
  (** Layout *)
  | newline              { next_line_and token lexbuf }
  | blank+               { token lexbuf }
  | ";;" ([^'\n']* as c) { COMMENT c }

  (** Keywords *)
  | "box"                  { BOX }
  | "unbox"                { UNBOX }
  | "iadd"                 { ADD }
  | "imul"                 { MUL }
  | "idiv"                 { DIV }
  | "isub"                 { SUB }
  | "irem"                 { REM }
  | "if_icmpeq"            { IFEQ }
  | "if_icmpne"            { IFNE }
  | "if_icmplt"            { IFLT }
  | "if_icmple"            { IFLE }
  | "if_icmpgt"            { IFGT }
  | "if_icmpge"            { IFGE }
  | "bipush"               { PUSH }
  | "pop"                  { POP }
  | "swap"                 { SWAP }
  | "dup"                  { DUP }
  | "astore"               { ASTORE }
  | "aastore"              { AASTORE }
  | "aload"                { ALOAD }
  | "aaload"               { AALOAD }
  | "goto"                 { JUMP }
  | "anewarray"            { ANEWARRAY }
  | "ireturn"              { IRETURN }
  | "tableswitch"          { TABLESWITCH }
  | "default"              { DEFAULT }
  | identifier as i        { ID i }

  (** Literals *)
  | digit+ as d     { INT (int_of_string d) }

  (** Punctuation *)
  | ":"             { COLON }
  | eof             { EOF }

  (** String *)
  | '"'             { read_string (Buffer.create 20) lexbuf }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }

and read_string buf = parse
  | '"'                   { STRING (Buffer.contents buf) }
  | '\\' '\\'             { Buffer.add_char buf '\\';
                            read_string buf lexbuf }
  | '\\' 'n'              { Buffer.add_char buf '\n';
                            read_string buf lexbuf }
  | '\\' 't'              { Buffer.add_char buf '\t';
                            read_string buf lexbuf }
  | '\\' '"'              { Buffer.add_char buf '"';
                            read_string buf lexbuf }
  | ([^ '"' '\\']+  as c) { Buffer.add_string buf c;
                            read_string buf lexbuf }
  | eof                   { error lexbuf "String is not terminated." }
  | _                     { error lexbuf ("Unexpected character in the string:" ^
                                          Lexing.lexeme lexbuf) }
