{stdenv, fetchFromGitHub, buildOcaml, ocaml, opam,
 camlp4, ounit, ocamlbuild }:

# base-bigarray, base-unix,


buildOcaml rec {
  name = "extunix";

  version = "0.1.4";

  minimumSupportedOcamlVersion = "4.02";

  src = fetchFromGitHub {
    owner = "ygrek";
    repo = "${name}";
    rev = "v${version}";
    sha256 = "020zc0n980znsz974gsi3lakdqbxvycrjw67pmj0d10yq8c3dmck";
  };

  buildInputs = [ opam camlp4 ounit ocamlbuild ];

  doCheck = false; # ? FIXME
  checkTarget = "test";

  meta = with stdenv.lib; {
    description = "A collection of thin bindings to various low-level system API.";
    license = licenses.mit;
  };
}
