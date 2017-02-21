{
  open Lexing
  open Error
  open Position
  open HobixParser

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)

  let string_buffer =
    Buffer.create 13

}

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let symbol = [ '+' '-' '*' '/' '<' '=' '>' ]

let digit = ['0'-'9']

let lowercase_alpha = ['a'-'z']

let uppercase_alpha = ['A'-'Z']

let alpha = lowercase_alpha | uppercase_alpha

let alphanum = alpha | digit | '_'

let basic_identifier = lowercase_alpha alphanum*

let prefix_alien_identifier = "`" (alpha | symbol | digit)+

let infix_alien_identifier = "`" (alpha | symbol | digit)+ "`"

let identifier = basic_identifier | prefix_alien_identifier

let uidentifier = uppercase_alpha alphanum*
let hexa   = [ '0'-'9' 'a'-'f' 'A'-'F']

rule token = parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }
  | "{*"            { comment 1 lexbuf           }
  | "**"            { commentline lexbuf         }

  (** Keywords *)
  | "val"           { VAL  }
  | "if"	    { IF }
  | "fi"	    { FI }
  | "while"	    { WHILE }
  | "then"	    { THEN }
  | "else"	    { ELSE }
  | "and"	    { AND }
  | "extern"        { EXTERN }
  | "newblock"      { NEWBLOCK }

  (** Identifiers *)
  | identifier as i  { ID i  }
  | infix_alien_identifier as i { INFIXID i }

  (** Literals *)
  | digit+ as d     { INT (Int32.of_string d) }
  | ("0x" |"0X") hexa+                    { INT (Int32.of_string (lexeme lexbuf)) }
  | ("0b" |"0B") ['0'-'1']+               { INT (Int32.of_string (lexeme lexbuf)) }
  | '"'                                   { string lexbuf }
  | "'\\n'"                               { LCHAR '\n' }
  | "'\\t'"                               { LCHAR '\t' }
  | "'\\b'"                               { LCHAR '\b' }
  | "'\\r'"                               { LCHAR '\r' }
  | "'\\\\'"                              { LCHAR '\\' }
  | "'\\''"                               { LCHAR '\'' }
  | '\'' ([^ '\\' '\''] as c) '\''        {
    if (Char.code c < 32) then
      error lexbuf (
	Printf.sprintf
	  "The ASCII character %d is not printable." (Char.code c)
      );
    LCHAR c
  }
  | "'\\" (digit digit digit as i) "'" {
    let c = int_of_string i in
    if c < 0 || c > 255 then error lexbuf "";
    LCHAR (char_of_int c)
  }
  | "'\\0" ("x" | "X") (hexa hexa as i) "'" {
    let c = int_of_string ("0x" ^ i) in
    if c < 0 || c > 255 then error lexbuf "";
    LCHAR (char_of_int c)
  }
  | "'\\0" ("b" | "B") (['0'-'1']+ as i) "'" {
    let c = int_of_string ("0b" ^ i) in
    if c < 0 || c > 255 then error lexbuf "";
    LCHAR (char_of_int c)
  }
  (* </corrige> *)

  (** Infix operators *)
  | "="             { EQUAL       }
  | ">"             { GT          }
  | ">="            { GTE         }
  | "<"             { LT          }
  | "<="            { LTE         }
  | "=>"            { DRARROW     }
  | "&&"            { LAND        }
  | "||"            { LOR         }
  | "-"             { MINUS       }
  | "+"             { PLUS        }
  | "*"             { STAR        }
  | "/"             { SLASH       }

  (** Punctuation *)
  | ":="	    { DEQUAL    }
  | ";"             { SEMICOLON }
  | "("             { LPAREN    }
  | ")"             { RPAREN    }
  | "["		    { LBRACKET  }
  | "]"		    { RBRACKET  }
  | "{"		    { LBRACE    }
  | "}"		    { RBRACE    }
  | ","             { COMMA     }
  | "\\"            { BACKSLASH }
  | eof             { EOF       }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }

and comment level = parse
  | "*}" {
    if level = 1 then
      token lexbuf
    else
      comment (pred level) lexbuf
  }
  | "{*" {
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

and commentline = parse
  | newline { next_line_and token lexbuf }
  | eof { EOF }
  | _ { commentline lexbuf }

and string = parse
| "\\n"                                 { Buffer.add_char string_buffer '\n'; string lexbuf }
| "\\t"                                 { Buffer.add_char string_buffer '\t'; string lexbuf }
| "\\b"                                 { Buffer.add_char string_buffer '\b'; string lexbuf }
| "\\r"                                 { Buffer.add_char string_buffer '\r'; string lexbuf }
| '\\' '\''                              { Buffer.add_char string_buffer '\''; string lexbuf }
| '\\' '"'                              { Buffer.add_char string_buffer '"'; string lexbuf }
| "\\\\"                                { Buffer.add_char string_buffer '\\'; string lexbuf }

| '\\' (_ as c)                 { error lexbuf
                                    (Printf.sprintf "Bad escape sequence in string '\\%c'" c)
                                }
| "\\" (digit digit digit as i) {
   let c = int_of_string i in
   if c < 0 || c > 255 then error lexbuf "";
   Buffer.add_char string_buffer (char_of_int c); string lexbuf
}
| "\\0" ("x" | "X") (hexa hexa as i) {
   let c = int_of_string ("0x" ^ i) in
   if c < 0 || c > 255 then error lexbuf "";
   Buffer.add_char string_buffer (char_of_int c); string lexbuf
}
| "\\0" ("b" | "B") (['0'-'1']+ as i) {
   let c = int_of_string ("0b" ^ i) in
   if c < 0 || c > 255 then error lexbuf "";
   Buffer.add_char string_buffer (char_of_int c); string lexbuf
}
| '"'                                   {
  let s = Buffer.contents string_buffer in
  Buffer.clear string_buffer;
  LSTRING s
}
| _ as c                                {
  Buffer.add_char string_buffer c;
  string lexbuf
}
| eof                                   {
  error lexbuf "Unterminated string."
}
