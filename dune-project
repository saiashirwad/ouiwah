(lang dune 3.17)

(using menhir 3.0)

(name ouiwah)

(generate_opam_files true)

(source
 (github saiashirwad/ouiwah))

(authors "Sai Ashirwad <saiashirwad@example.com>")

(maintainers "Sai Ashirwad <saiashirwad@example.com>")

(license MIT)

(documentation https://github.com/saiashirwad/ouiwah)

(package
 (name ouiwah)
 (synopsis "Stack-based array programming language")
 (description
  "Ouiwah is a minimal stack-based array programming language with support for n-dimensional arrays and function definitions")
 (depends
  (ocaml
   (>= 4.14.0))
  (dune
   (>= 3.17))
  (menhir
   (>= 3.0))))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
