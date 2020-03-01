(lang dune 2.0)

(name mine)

(version 1.0.3)
(authors "H11 <nirisix@gmail.com>")
(maintainers "H11 <nirisix@gmail.com>")
(source
 (github dhcmrlchtdj/mine))
(license Unlicense)

(package
 (name mine)
 (synopsis "A markdown renderer in OCaml")
 (depends
  (containers           (and (>= 2.8.1)))
  (re                   (and (>= 1.9.0)))
  (ppx_deriving         (and :build (>= 4.4.1)))
  (dune                 (and :build (>= 2.3.1)))
  (bisect_ppx           (and :build (>= 1.4.2)))
  (js_of_ocaml          (and :dev (>= 3.5.2)))
  (js_of_ocaml-compiler (and :dev (>= 3.5.2)))
  (js_of_ocaml-ppx      (and :dev (>= 3.5.2)))
  ))

(generate_opam_files true)
