(lang dune 2.8)

(name mine)

(version 1.0.6)
(authors "H11 <nirisix@gmail.com>")
(maintainers "H11 <nirisix@gmail.com>")
(source
 (github dhcmrlchtdj/mine))
(license Unlicense)

(package
 (name mine)
 (synopsis "A markdown renderer in OCaml")
 (depends
  (containers           (and (>= 3.4)))
  (re                   (and (>= 1.9.0)))
  (bisect_ppx           (and :build (>= 2.6.0)))
  (dune                 (and :build (>= 2.8.5)))
  (ppx_blob             (and :build (>= 0.7.2)))
  (ppx_deriving         (and :build (>= 5.2.1)))
  (js_of_ocaml          (and :dev (>= 3.9.0)))
  (js_of_ocaml-compiler (and :dev (>= 3.9.0)))
  (js_of_ocaml-ppx      (and :dev (>= 3.9.0)))
  (ocamlformat          (and :dev (>= 0.18.0)))
 ))

(generate_opam_files true)
