(lang dune 3.16)

(name convoLib)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name convoLib)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune core core_unix agrid ppx_deriving_yojson flex-array refl ppx_let ppx_typerep_conv utop notty ppx_disable_unused_warnings ppx_deriving_variant_string ppx_deriving ppx_import ppx_variants_conv fmt atd yojson atdgen atdgen-runtime )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
