{ }:
let 

pkgs = import <nixpkgs> {};
stdenv = pkgs.stdenv;
fetchgit = pkgs.fetchgit;
op = pkgs.ocamlPackages_latest;
ocaml=op.ocaml; 
findlib=op.findlib;
opam=pkgs.opam;
ppx_deriving_yojson=op.callPackage ../ppx_deriving_yojson { };
extunix=op.callPackage ../extunix { };
in 
stdenv.mkDerivation {

name = "tjr_btree";
    
src=../..;

buildInputs = [ ocaml findlib opam op.yojson op.ocaml_batteries op.ppx_deriving ppx_deriving_yojson extunix ];

configurePhase = ''true''; 	 # Skip configure

createFindlibDestdir = true;

postInstall=''
mkdir -p $out/build
cp -R . $out/build
'';

}
