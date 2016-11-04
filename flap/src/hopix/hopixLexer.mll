{
open Lexing
open Error
open Position
open HopixParser

exception Invalid_char_literal

let next_line_and f lexbuf  =
  Lexing.new_line lexbuf;
  f lexbuf

let error lexbuf =
  error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)

let char_of_char_atom = function
  | "\\\\" -> '\\'
  | "\\'" -> '\''
  | "\\\"" -> '"'
  | "\\n" -> '\n'
  | "\\t" -> '\t'
  | "\\b" -> '\b'
  | "\\r" -> '\r'
  | s ->
    begin match s.[0] with
      | '\\' ->
        let s = String.sub s 1 (String.length s - 1) in
        begin try char_of_int (int_of_string s) with
          | Failure _ -> raise Invalid_char_literal
        end
      | c -> c
    end
}

let newline = '\n' | '\r' | "\r\n"

let blank = [' ' '\t' '\012']

let digit = ['0'-'9']

let lowercase_letter = ['a'-'z']

let uppercase_letter = ['A'-'Z']

let math_symb = ['+' '-' '*' '/' '<' '=' '>']

let letter = uppercase_letter | lowercase_letter

let alphanum = letter | digit | '_'

let basic_id = lowercase_letter alphanum*

let alien_id = (alphanum | math_symb)+

let alien_prefix_id = '`' alien_id

let alien_infix_id = alien_prefix_id '`'

let constr_id = (uppercase_letter | '_') alphanum*

let type_variable = '\'' basic_id

let hexa_prefix = '0' ['x' 'X']

let bin_prefix = '0' ['b' 'B']

let octal_prefix = '0' ['o' 'O']

let hexa_digit = digit | ['a'-'f' 'A'-'F']

let bin_digit = ['0' '1']

let octal_digit = ['0'-'7']

let deci_int = digit+

let hexa_int = hexa_prefix hexa_digit+

let bin_int = bin_prefix bin_digit+

let octal_int = octal_prefix octal_digit+

let int = deci_int | hexa_int | octal_int | bin_int

let printable = [' '-'~']

let esc_char = ['\\' '\'' '"' 'n' 't' 'b' 'r']

let esc_seq = '\\' (deci_int | hexa_int | octal_int | bin_int | esc_char)

let char_atom = esc_seq | printable # '\''

let string_atom = esc_seq | printable # '"'

rule token = parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }
  | eof             { EOF       }

  (** Keywords *)
  | "type" {TYPE}
  | "extern" {EXTERN}
  | "val" {VAL}
  | "fun" {FUN}
  | "and" {AND}
  | "if" {IF}
  | "then" {THEN}
  | "elif" {ELIF}
  | "else" {ELSE}
  | "ref" {REF}
  | "while" {WHILE}
  | "false" {FALSE}
  | "true" {TRUE}

  (** Operators *)
  | '+' {PLUS}
  | '-' {MINUS}
  | '*' {TIMES}
  | '/' {DIV}
  | "&&" {LAND}
  | "||" {LOR}
  | "=" {EQUAL}
  | "<=" {LEQ}
  | ">=" {GEQ}
  | '<' {LT}
  | '>' {GT}

  (** Punctuation *)
  | '(' {LPAREN}
  | ')' {RPAREN}
  | '[' {LBRACKET}
  | ']' {RBRACKET}
  | '{' {LBRACE}
  | '}' {RBRACE}
  | ':' {COLON}
  | ',' {COMMA}
  | '\\' {BACKSLASH}
  | '?' {QMARK}
  | '!' {EMARK}
  | '|' {PIPE}
  | '&' {AMPERSAND}
  | '_' {UNDERSCORE}
  | "->" {ARROW}
  | "=>" {IMPL}
  | ":=" {COLONEQ}

  (** Literals *)
  | int as s
    {
      try INT (Int32.of_string s) with
      | Failure _ -> error lexbuf "Invalid integer literal."
    }
  | '\'' (char_atom as a) '\''
    {
      try CHAR (char_of_char_atom a) with
      | Invalid_char_literal -> error lexbuf "Invalid character literal."
    }
  | '"' (string_atom* as s) '"'
    {
      try STRING (Scanf.unescaped s) with
      | Scanf.Scan_failure _ -> error lexbuf "Invalid string literal."
    }

  (** Identifiers *)
  | basic_id as id {BASIC_ID id}
  | alien_prefix_id as id {PREFIX_ID id}
  | alien_infix_id as id {INFIX_ID id}
  | constr_id as id {KID id}
  | type_variable as id {TID id}

  (** Comments *)
  | "--" {line_comment lexbuf}
  | "{-" {block_comment 0 lexbuf}

  (** Lexing error. *)
  | _               { error lexbuf "Unexpected character." }

and line_comment = parse
  | newline {next_line_and token lexbuf}
  | eof {EOF}
  | _ {line_comment lexbuf}

and block_comment depth = parse
  | "{-" {block_comment (succ depth) lexbuf}
  | "-}"
    {
      if depth = 0 then token lexbuf else block_comment (pred depth) lexbuf
    }
  | eof {error lexbuf "Unterminated comment."}
  | _ {block_comment depth lexbuf}
