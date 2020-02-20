(lang dune 2.0)
(name tjr_btree)

;; for opam
(authors              "Tom Ridge <tom.j.ridge@googlemail.com>")
(maintainers          "Tom Ridge <tom.j.ridge@googlemail.com>")
(source               (github tomjridge/tjr_btree))
(homepage             https://github.com/tomjridge/tjr_btree)
(documentation        https://tomjridge.github.io/tjr_btree)
(generate_opam_files  true)


(package 
 (name tjr_btree)
 (synopsis "B-tree library")
 (depends 
   isa_btree
   bin_prot
   ppx_bin_prot))

(package 
 (name tjr_btree_examples)
 (synopsis "B-tree library (examples)")
 (depends
   tjr_btree
   lru
   core
   ))



