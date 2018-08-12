(** The abstract syntax tree for javix programs. *)

type t =
  { classname : string; (* The name of the program, cf .class in Jasmin *)
    code : labelled_instruction list;
    varsize : int; (* The number of variables, cf .limit locals in Jasmin *)
    stacksize : int } (* Maximal stack size, cf .limit stack in Jasmin *)

and labelled_instruction =
    label option * instruction

and instruction =
  | Comment of string (* In Jasmin, comments starts with ; *)
  (* Pseudo instructions, provided via ad-hoc methods (see javixPrettyPrint): *)
  | Box (* conversion from int to Integer, works on top of stack *)
  | Unbox (* conversion from Integer to int, works on top of stack *)
  (* Real jvm instructions, see the jvm specification and/or the Jasmin doc: *)
  | Bipush of int (* push an int on the stack *)
  | Pop
  | Swap
  | Dup
  | Binop of binop (* instructions iadd, imul, idiv, isub, irem *)
  | Aload of var (* load from a variable *)
  | Astore of var (* store in some variable *)
  | Goto of label
  | If_icmp of cmpop * label (* if_icmpeq, if_icmpne, ... *)
  | Anewarray (* builds an array of java Objects, size is in stack top *)
  | AAload (* array access, stack: ...,array,index ---> ...,array[index] *)
  | AAstore (* array modification, stack: ...,array,index,value ---> ... *)
  | Ireturn (* exits and returns the int on top of stack *)
  | Tableswitch of int * label list * label
    (* Tableswitch (n,lablist,labdefault) reads an int k on top of the stack,
       and jumps to the label number (k-n) in lablist if 0<=k-n<length(lablist),
       or jumps to labdefault otherwise *)
  | Checkarray (* checks that the top of stack is an array of java Objects *)
  | Print of string

and var = Var of int

and binop = Add | Sub | Mul | Div | Rem

and cmpop = Eq | Ne | Lt | Le | Gt | Ge

and label = Label of string
