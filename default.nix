with import <nixpkgs> {};

pkgs.ocamlPackages_latest.callPackage ./.nix/tjr_btree { }


# 
# # run: nix-shell
# # then: ocamlfind list
# { }:
# let 
#   pkgs = import <nixpkgs> {};
#   stdenv = pkgs.stdenv;
#   op = pkgs.ocamlPackages_latest;
#   ocaml=op.ocaml; 
#   findlib=op.findlib;
#   tjr_btree = op.callPackage ./.nix/tjr_btree { };
# in 
# stdenv.mkDerivation {
#   name = "env";
#   buildInputs = [ ocaml findlib tjr_btree ];
# }
