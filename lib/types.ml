open! Containers

type spanElement =
  | Stext of string
  | Scode of string
  | Semphasis of spanElement list
  | Sstrong of spanElement list
  | Sdelete of spanElement list
  | Slink of spanElement list * string
  | Simage of string * string

and blockElement =
  | Bnewline
  | Bline
  | Bcode of string option * string
  | Bheading of int * spanElement list
  | Bparagraph of spanElement list
  | Bseq of spanElement list
  | Bquote of blockElement list
  | Blist of blockElement list list

type md_ast = blockElement list
