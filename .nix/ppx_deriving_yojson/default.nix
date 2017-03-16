{stdenv, fetchFromGitHub, buildOcaml, ocaml, opam,
 yojson, result, ppx_deriving, cppo, ounit, ppx_import }:

# cppo, ppx_deriving, ounit, ppx_import, yojson, result }:

buildOcaml rec {
  name = "ppx_deriving_yojson";

  version = "3.0";

  minimumSupportedOcamlVersion = "4.02";

  src = fetchFromGitHub {
    owner = "whitequark";
    repo = "${name}";
    rev = "v${version}";
    sha256 = "1id1a29qq0ax9qp98b5hv6p2q2r0vp4fbkkwzm1bxdhnasw97msk";
  };

  buildInputs = [ opam  yojson result ppx_deriving cppo ounit ppx_import ];

  doCheck = true;
  checkTarget = "test";

  installPhase = ''
    opam-installer --script --prefix=$out ${name}.install | sh
    ln -s $out/lib/${name} $out/lib/ocaml/${ocaml.version}/site-lib
  '';

  meta = with stdenv.lib; {
    description = "A Yojson codec generator for OCaml >= 4.02.";
    license = licenses.mit;
  };
}
