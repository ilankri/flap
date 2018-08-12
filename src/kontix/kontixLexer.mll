{
  open Lexing
  open Error
  open Position
  open KontixParser

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

let alphanum = alpha | digit | '_'

let identifier = alpha alphanum*

rule token = parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }
  | "/*"            { comment 1 lexbuf           }

  (** Keywords *)
  | "in"            { IN   }
  | "def"           { DEF  }
  | "let"           { LET  }
  | "if"            { IF   }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "eval"          { EVAL }
  | "new"           { NEW }
  | "K"             { K }
  | "E"             { E }

  (** Literals *)
  | digit+ as d     { INT (int_of_string d) }

  (** Identifiers *)
  | identifier as i { ID i }

  (** Infix operators *)
  | "="             { EQUAL   }
  | "+"             { PLUS    }
  | "*"             { STAR    }
  | "/"             { SLASH   }
  | "-"             { MINUS   }
  | "%"             { PERCENT }
  | ">"             { GT      }
  | ">="            { GTE     }
  | "<"             { LT      }
  | "<="            { LTE     }
  | ":="            { ASSIGNS }
  | "&"             { UPPERSAND }
  | "?"             { QMARK }

  (** Punctuation *)
  | ","             { COMMA     }
  | ";"             { SEMICOLON }
  | "("             { LPAREN    }
  | ")"             { RPAREN    }
  | "["             { LBRACKET  }
  | "]"             { RBRACKET  }
  | eof             { EOF       }

  (** String *)
  | '"'             { read_string (Buffer.create 20) lexbuf }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }

and comment level = parse
  | "*/" {
    if level = 1 then
      token lexbuf
    else
      comment (pred level) lexbuf
  }
  | "/*" {
    comment (succ level) lexbuf
  }
  | eof {
    error lexbuf "unterminated comment."
  }
  | newline {
    next_line_and (comment level) lexbuf
  }
  | _ {
    comment level lexbuf
  }

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
