(* -*- org -*- *)

(** Main documentation entry point. *)

(**

This file contains general documentation and an overview of the
code. Follow the links to individual modules for more information.


{2 Introduction}

This library implements a B-tree in OCaml. The source is actually
exported from Isabelle, and then OCaml code wraps the
Isabelle-generated code. This is the documentation for the OCaml
wrapper. The documentation for the core B-tree routines can be found
in the [isa_btree] repository.

This document gives an overview of the (sub)packages and a guide to
the structure of the code. Detailed
comments are included in the individual modules. 


{2 Ancestor projects}

See the [README.org] in the project root directory for a list of
dependencies.


{2 Directory structure}

The code is organized into subdirectories (subpackages), and the
subdirectories are in a linear order (which corresponds to the
dependencies between modules). For example, the first subdirectory is
[a_test] which is discussed in the section "Test" below.


{2 Naming conventions}

To understand the interfaces, we need to introduce the following:

- Keys, represented by type variable ['k] 
- Values, type var ['v] 
- Page/block references, ['r] 
- Global state, ['t]


{2 Monads!}

The entire development is parameterized by some notion of
monad. Before reading the rest of the documentation, please look at
the module {!Tjr_monad} from ancestor project [tjr_monad]. Essentially
we use a type [('a,'t) m], where ['t] is some phantom type var that is
used to identify a particular monad instance. There is an associated
type ['t monad_ops] which provides the expected return and bind. In
order to get parametricity over the monad, we are forced to pass
[monad_ops] as parameters to many functions.


{2 Exported code from Isabelle}

Exported code is in the [isa_btree] opam/ocamlfind package. 


{2 Test}

Some simple testing utility functions.


{2 Base types}

This package contains basic B-tree related types:

- {!Base_types} includes most of the other modules in this package
  the other modules
- {!Blk_allocator_ops_type} 
- {!Block} for blocks on disk
- {!Config}
- {!Frame} which imports the Isabelle frame type
- {!Internal_base_types_pervasives} which includes utility functions assumed by
- {!Ls_state} for the leaf stream state
- {!Marshalling_ops_type}
- {!Ord} for orderings (typically over keys)
- {!Page_ref_int} utility module to fix page ref as an integer
- {!R2f} and {!R2t} for internal types used in testing (convert a
  block ref to a frame/tree)
- {!Rstk} for the type of framestacks
- {!String_} for various string utility functions
- {!Tree} for the tree type imported from Isabelle

The {!Tree} module describes a B-tree as an algebraic datatype. The
on-disk B-tree uses references between blocks i.e. a graph-like
structure with pointers rather than a datatype. Indeed, the tree
datatype is used only for testing purposes.


{2 Api}

This package gives the main interface types, including interfaces for
disk, store and map.

- {!Leaf_stream_ops} and {!Leaf_stream_util} for leaf streams
- {!Pre_map_ops} like {!Map_ops} but with explicit state passing
- {!Store_ops} for a layer just above {!Disk_ops}

In addition, the leaf stream interface allows to iterate over the
leaves in a B-tree e.g. to find all the bindings in the map. This
module also documents the type variable naming conventions (see
{!Map_ops}).


{2 Store to map}

This package, particularly the {!Store_to_map} module, wraps the
Isabelle routines to implement a map interface on top of a
store. The key function is {!Store_to_map.store_ops_to_map_ops}
which takes a [store_ops] and returns a [map_ops].

Also included here are {!Big_step} (to iterate the small step
operations provided by Isabelle) and {!Iter_leaf_stream} (to wrap
small-step leaf stream operations).


{2 Disk to store}

{!Disk_to_store} includes a function that naively transforms a disk
to a store. 


{2 Examples}

Various examples. 

{!Bin_prot_marshalling} provides on-disk marshalling courtesy of
[binprot]. You can, of course, provide your own marshalling code,
which may be useful if your backend doesn't support binprot (js_of_ocaml?)

{!Bin_prot_util} is a very small module which defines eg
[bp_size_int].

{!Examples} contains the main constructions for various types of maps.

{!Map_on_fd_util}, utilities for a map backed by a file descriptor.

{!Store_in_mem} implements a simple in-memory store.


{2 Testing}

Various modules to support testing. Some of this looks a bit
suspicious. The main module is {!Test_exhaustive_in_mem}.


{2 Doc}

Contains this overview documentation in module {!Tjr_btree_doc}.


*)

let dummy = 1 (* FIXME needed? *)



