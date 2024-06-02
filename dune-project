(lang dune 3.13)

(name ocaml_problems)

(generate_opam_files true)

(source
 (github KFoxder/ocaml_problems))

(package
 (name ocaml_problems)
 (synopsis "Practice problems for OCaml")
 (depends ocaml dune ounit2 ppx_expect ppx_sexp_conv sexplib)
 (tags
  (ocaml problems learning)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
