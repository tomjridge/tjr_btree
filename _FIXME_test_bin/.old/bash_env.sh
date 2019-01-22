set -a # export all vars

required_packages="tjr_btree"


# generic ---------------------------------------------------------------

PKGS="-package $required_packages"

FLGS="-g -thread -bin-annot" 
FOR_PACK=""
INLINE="-inline 0" # inline 0 for debugging native
WARN="-w -8-11"

    # these include syntax, so should work on all files; may be
    # overridden in ocamlc.sh; FIXME don't need -bin-annot twice
  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   -bin-annot         $FLGS $FOR_PACK $WARN $PKGS $SYNTAX"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt -bin-annot $INLINE $FLGS $FOR_PACK $WARN $PKGS $SYNTAX"


# clean ----------------------------------------------------------------

function clean() {
	rm -f *.{cmi,cmo,cmx,o,cmt} a.out *.cma *.cmxa *.a *.byte *.native
}


