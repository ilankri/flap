(** This module offers a pretty-printer for Javix programs. *)

open PPrintCombinators
open PPrintEngine

open JavixAST

let header p =
  ".class public " ^ p.classname ^ "\n"
  ^ ".super java/lang/Object\n\
     \n\
     .method public static main([Ljava/lang/String;)V\n\
     .limit stack 2\n\
     ; push System.out onto the stack\n\
     getstatic java/lang/System/out Ljava/io/PrintStream;\n\
     ; launch our code and push the int result onto the stack\n\
     invokestatic " ^ p.classname ^ "/code()I\n"
  ^ "; call the PrintStream.println() method.\n\
     invokevirtual java/io/PrintStream/println(I)V\n\
     ; done\n\
     return\n\
     .end method\n\
     \n\
     ;;; box : int --> Integer\n\
     \n\
     .method public static box(I)Ljava/lang/Object;\n\
     .limit locals 1\n\
     .limit stack 3\n\
     new java/lang/Integer\n\
     dup\n\
     iload 0\n\
     invokespecial java/lang/Integer/<init>(I)V\n\
     areturn\n\
     .end method\n\
     \n\
     ;;; unbox : Integer --> int\n\
     \n\
     .method public static unbox(Ljava/lang/Object;)I\n\
     .limit locals 1\n\
     .limit stack 1\n\
     aload 0\n\
     checkcast java/lang/Integer\n\
     invokevirtual java/lang/Integer/intValue()I\n\
     ireturn\n\
     .end method\n\
     \n\
     ;;; the compiled code\n\
     \n\
     .method public static code()I\n\
     .limit locals " ^ string_of_int p.varsize ^ "\n"
  ^ ".limit stack " ^ string_of_int p.stacksize ^ "\n"

let rec program p =
  string (header p) ^^ code p p.code ^^ hardline ^^
  string ".end method" ^^ hardline

and code p c =
  separate_map hardline (labelled_instruction p) c

and labelled_instruction p (l, i) =
  label l ^^ string (instruction p i)

and label lab =
  string (match lab with None -> "" | Some (Label l) -> l^":\n") ^^
  string "\t"

and instruction p = function
  | Box -> "invokestatic "^p.classname^"/box(I)Ljava/lang/Object;"
  | Unbox -> "invokestatic "^p.classname^"/unbox(Ljava/lang/Object;)I"
  | Bipush c -> push c
  | Pop -> "pop"
  | Swap -> "swap"
  | Dup -> "dup"
  | Binop op -> binop op
  | Astore v -> "astore " ^ var v
  | Aload v -> "aload " ^ var v
  | Goto (Label lab) -> "goto " ^ lab
  | If_icmp (op,Label lab) -> "if_icmp"^cmpop op^" "^lab
  | Anewarray -> "anewarray java/lang/Object"
  | AAstore -> "aastore"
  | AAload -> "aaload"
  | Ireturn -> "ireturn"
  | Comment s -> ";; " ^ s
  | Tableswitch (i,labs,Label dft) ->
      let labs = List.map (function Label l -> l) labs in
      "tableswitch "^string_of_int i^"\n\t"^
      (String.concat "\n\t" labs)^
      "\n\tdefault: "^dft
  | Checkarray -> "checkcast [Ljava/lang/Object;"
  | Print s ->
      "getstatic java/lang/System/out Ljava/io/PrintStream;\n\t" ^
      "ldc \""^String.escaped s^"\"\n\t" ^
      "invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n\t" ^
      "iconst_0"

and var (Var v) = string_of_int v

and binop = function
  | Add -> "iadd"
  | Sub -> "isub"
  | Mul -> "imul"
  | Div -> "idiv"
  | Rem -> "irem"

and cmpop = function
  | Eq  -> "eq"
  | Ne  -> "ne"
  | Lt  -> "lt"
  | Le  -> "le"
  | Gt  -> "gt"
  | Ge  -> "ge"

and push n =
  let s = string_of_int n in
  let maxbyte = (1 lsl 7) - 1 and minbyte = - (1 lsl 7)
  and maxshort = (1 lsl 15) - 1 and minshort = - (1 lsl 15) in
  if 0 <= n && n <= 5 then "iconst_"^s
  else if minbyte <= n && n <= maxbyte then "bipush "^s
  else if minshort <= n && n <= maxshort then "sipush "^s
  else "ldc "^s

let to_string f x =
  let b = Buffer.create 13 in
  ToBuffer.pretty 0.5 80 b (f x);
  Buffer.contents b
