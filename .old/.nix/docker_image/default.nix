# Examples of using the docker tools to build packages.
#
# This file defines several docker images. In order to use an image,
# build its derivation with `nix-build`, and then load the result with
# `docker load`. For example:
#
#  $ nix-build -A editors
#  $ docker load < result
#  sudo docker run --privileged -t -i editors /bin/bash

{ }:
let 
pkgs = import <nixpkgs> {};
buildImage = pkgs.dockerTools.buildImage;
in
{
  # 1. basic example
  bash = buildImage {
    name = "bash";
    contents = pkgs.bashInteractive;
  };


  env = 
    # copied from ../../default.nix
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
    buildImage {
    name = "tjr_btree_docker_build_env";
    contents = [
ocaml findlib opam op.yojson op.ocaml_batteries op.ppx_deriving ppx_deriving_yojson extunix
    ];
  };


  image = 
    let 
    pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    fetchgit = pkgs.fetchgit;
    op = pkgs.ocamlPackages_latest;
    in
    buildImage {
    name = "tjr_btree";
    contents = op.callPackage ../tjr_btree { };
  };

  # 5. example of multiple contents, emacs and vi happily coexisting
  editors = buildImage {
    name = "editors";
    contents = [
      pkgs.coreutils
      pkgs.bash
      pkgs.emacs
      pkgs.vim
      pkgs.nano
    ];
  };
}
