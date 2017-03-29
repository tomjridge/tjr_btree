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
extlib=op.ocaml_extlib_maximal;
merlin=op.merlin; # for emacs
emacs=pkgs.emacs; # for mini programming environment
tuareg=pkgs.emacsPackages.tuaregMode; 
in 
stdenv.mkDerivation {

name = "tjr_btree";
    
src=../../src;

# make these available in env
emacs=emacs;
merlin=merlin;
tuareg=tuareg;

buildInputs = [ ocaml findlib opam op.yojson op.ocaml_batteries op.ppx_deriving ppx_deriving_yojson extunix extlib merlin emacs ];

configurePhase = ''true''; 	 # Skip configure

createFindlibDestdir = true;

postInstall=''
mkdir -p $out/build
cp -R . $out/build
'';

}
