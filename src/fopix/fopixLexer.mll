{
  open Lexing
  open Error
  open Position
  open FopixParser

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
  | "val"           { VAL  }
  | "in"            { IN   }
  | "def"           { DEF  }
  | "end"           { END  }
  | "if"            { IF   }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "eval"          { EVAL }
  | "external"      { EXTERNAL }
  | "switch"	    { SWITCH }
  | "orelse"	    { ORELSE }

  (** Literals *)
  | digit+ as d     { INT (Int32.of_string d) }

  (** Identifiers *)
  | identifier as i { ID i }

  (** Infix operators *)
  | "="             { EQUAL     }
  | "+"             { PLUS      }
  | "*"             { STAR      }
  | "/"             { SLASH     }
  | "-"             { MINUS     }
  | ">"             { GT        }
  | ">="            { GTE       }
  | "<"             { LT        }
  | "<="            { LTE       }
  | ":="            { ASSIGNS   }
  | "&&"            { LAND      }
  | "&"             { UPPERSAND }
  | "||"            { LOR       }
  | "|"		    { PIPE      }

  (** Punctuation *)
  | ","             { COMMA     }
  | ";"             { SEMICOLON }
  | "("             { LPAREN    }
  | ")"             { RPAREN    }
  | "["             { LBRACKET  }
  | "]"             { RBRACKET  }
  | eof             { EOF       }

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
