(** This module offers a pretty-printer for Stackix programs. *)

open PPrint
open PPrintCombinators
open PPrintEngine

open MipsAST

let ( ++ ) x y =
  x ^^ break 1 ^^ y

let located f x = f (Position.value x)

let max_label_length =
  List.fold_left (fun m  { label = Label l } ->
      max (String.length l) m
  ) 0

let vcat = separate_map hardline (fun x -> x)

let rec program p =
  vcat (
    (if Options.get_gcc () then [
      string "#include <sys/regdef.h>";
      string ".globl main";
    ] else [])
    @ globals p.globals
    @ (
      directive ".text"
      :: code p.code
    )
  )

and globals gs =
  List.(flatten (map global gs))

and global (Label x) =
  [
    directive ".data";
    string (x ^ ":");
    string (".word 1");
  ]

and directive d = string d

and code instructions =
  let shift = max_label_length instructions in
  List.(flatten (map (labelled shift instruction) instructions))

and labelled shift f x =
  let spaces = String.make (shift + 2) ' ' in
  let label =
    let Label l = x.label in
    let spaces = String.(make (shift - length l) ' ') in
    l ^ ": " ^ spaces
  in
  let spaces = string spaces in
  if x.value = [] then (
    [group (string label)]
  ) else (
    group (string label ^^ group (f spaces (List.hd x.value)))
    :: List.map (fun x -> group (spaces ^^ f spaces x)) (List.tl x.value)
  )

and instruction spaces = function
  | Comment s ->
    string ("# " ^ s)

  | Syscall ->
    string "syscall"

  | Li (r, imm) ->
    string "li" ++ register r ^^ comma ++ immediate imm

  | Lui (r, imm) ->
    string "lui" ++ register r ^^ comma ++ immediate imm

  | Jal l ->
    string "jal" ++ label l

  | J l ->
    string "j" ++ label l

  | Jr r ->
    string "jr" ++ register r

  | Jalr r ->
    string "jalr" ++ register r

  | La (r, a) ->
    string "la" ++ register r ^^ comma ++ address a

  | Sw (r, a) ->
    string "sw" ++ register r ^^ comma ++ address a

  | Lw (r, a) ->
    string "lw" ++ register r ^^ comma ++ address a

  | Mul (r1, r2, r3) ->
    string "mul" ++ register r1 ^^ comma ++ register r2 ^^ comma ++ register r3

  | Div (r1, r2, r3) ->
    string "div" ++ register r1 ^^ comma ++ register r2 ^^ comma ++ register r3

  | Sub (r1, r2, r3) ->
    string "sub" ++ register r1 ^^ comma ++ register r2 ^^ comma ++ register r3

  | Add (r1, r2, r3) ->
    string "add" ++ register r1 ^^ comma ++ register r2 ^^ comma ++ register r3

  | Addiu (r1, r2, imm) ->
    string "addiu" ++ register r1 ^^ comma ++ register r2 ^^ comma ++ immediate imm

  | Move (r1, r2) ->
    string "move" ++ register r1 ^^ comma ++ register r2

  | Beqz (r, l) ->
    string "beqz" ++ register r ^^ comma ++ label l

  | Bnez (r, l) ->
    string "bnez" ++ register r ^^ comma ++ label l

  | Bge (r1, r2, l) ->
    string "bge" ++ register r1 ^^ comma ++ register r2 ^^ comma ++ label l

  | Bgt (r1, r2, l) ->
    string "bgt" ++ register r1 ^^ comma ++ register r2 ^^ comma ++ label l

  | Slt (r1, r2, r3) ->
    string "slt" ++ register r1 ^^ comma ++ register r2 ^^ comma ++ register r3

  | Sgt (r1, r2, r3) ->
    string "sgt" ++ register r1 ^^ comma ++ register r2 ^^ comma ++ register r3

  | Sge (r1, r2, r3) ->
    string "sge" ++ register r1 ^^ comma ++ register r2 ^^ comma ++ register r3

  | Sle (r1, r2, r3) ->
    string "sle" ++ register r1 ^^ comma ++ register r2 ^^ comma ++ register r3

  | Seq (r1, r2, r3) ->
    string "seq" ++ register r1 ^^ comma ++ register r2 ^^ comma ++ register r3

and address = function
  | RegisterAddress r ->
    string "(" ^^ register r ^^ string ")"
  | RegisterOffsetAddress (r, imm) ->
    immediate imm ^^ string "(" ^^ register r ^^ string ")"
  | LabelAddress l ->
    label l


and label (Label l) =
  string l

and immediate = function
  | Literal i ->
    string (Int16.to_string i)
  | LabelAddressHi l ->
    string "%hi(" ^^ label l ^^ string ")"
  | LabelAddressLow l ->
    string "%lo(" ^^ label l ^^ string ")"

and register r =
  string (if Options.get_gcc () then MipsArch.string_of_register' r else MipsArch.string_of_register r)

let to_string f x =
  let b = Buffer.create 13 in
  ToBuffer.pretty 0.9 200 b (f x);
  Buffer.contents b
