(** Given a run-length code list generated as specified in the previous problem, construct its uncompressed version.

    #  decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
    - : string list =
      ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let rec unpack_encoding encoding =
  match encoding with
  | One s -> [ s ]
  | Many (i, s) -> if i = 0 then [] else s :: unpack_encoding (Many (i - 1, s))
;;

let rec decode_inner encodings =
  match encodings with
  | [] -> []
  | [ x ] -> unpack_encoding x
  | hd :: tl -> List.append (unpack_encoding hd) (decode_inner tl)
;;

let decode (encodings : 'a rle list) =
  if List.is_empty encodings then [] else decode_inner encodings
;;
