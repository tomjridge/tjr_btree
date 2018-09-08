(* -*- org -*- *)

(** Main documentation entry point. *)

(**

This file contains general documentation and an overview of the
code. Follow the links to individual modules for more information.


{1 Introduction}

This library implements a B-tree in OCaml. The source is actually
exported from Isabelle, and then OCaml code wraps the
Isabelle-generated code. This is the documentation for the OCaml
wrapper. The documentation for the core B-tree routines can be found
in the [isa_btree] repository.

This document gives an overview of the (sub)packages and a guide to
the structure of the code. Detailed
comments are included in the individual modules. 


{1 Ancestor projects}

See the [README.org] in the project root directory for a list of
dependencies.


{1 Directory structure}

The code is organized into subdirectories (subpackages), and the
subdirectories are in a linear order (which corresponds to the
dependencies between modules). For example, the first subdirectory
(ignoring [aa_from_isa] since it is not used) is [ac_test] which is
discussed in the section "Test" below.


{1 Naming conventions}

To understand the interfaces, we need to introduce the following:

- Keys, represented by type variable ['k] 
- Values, type var ['v] 
- Page/block references, ['r] 
- Global state, ['t]


{1 Monads!}

The entire development is parameterized by some notion of
monad. Before reading the rest of the documentation, please look at
the module {!Tjr_monad} from ancestor project [tjr_monad]. Essentially
we use a type [('a,'t) m], where ['t] is some phantom type var that is
used to identify a particular monad instance. There is an associated
type ['t monad_ops] which provides the expected return and bind. In
order to get parametricity over the monad, we are forced to pass
[monad_ops] as parameters to many functions.


{1 Exported code from Isabelle}

Exported code is in the [isa_btree] opam/ocamlfind package. The
[from_isa] package here is not used. FIXME


{1 Test}

Some simple testing utility functions.


{1 Base types}

This package contains basic B-tree related types:

- {!Base_types} includes most of the other modules in this package
- {!Base_types_pervasives} which includes utility functions assumed by
  the other modules
- {!Block} for blocks on disk
- {!Constants} which give size constraints on B-tree nodes FIXME now
  in isa_btree
- {!Frame} which imports the Isabelle frame type
- {!Ls_state} for the leaf stream state
- {!Ord} for orderings (typically over keys)
- {!Params} for various projection functions from a "parameters" object
- {!R2f} and {!R2t} for internal types used in testing (convert a
  block ref to a frame/tree)
- {!Rstk} for the type of framestacks
- {!String_} for various string utility functions
- {!Tree} for the tree type imported from Isabelle

The {!Tree} module describes a B-tree as an algebraic datatype. The
on-disk B-tree uses references between blocks i.e. a graph-like
structure with pointers rather than a datatype. Indeed, the tree
datatype is used only for testing purposes.


{1 Api}

This package gives the main interface types, including interfaces for
disk, store and map.

- {!Disk_ops} for the low-level block interface
- {!Leaf_stream_ops} and {!Leaf_stream_util} for leaf streams
- {!Map_ops} for map operations, find etc.
- {!Page_ref_int} utility module to fix page ref as an integer
- {!Pre_map_ops} like {!Map_ops} but with explicit state passing
- {!Store_ops} for a layer just above {!Disk_ops}

In addition, the leaf stream interface allows to iterate over the
leaves in a B-tree e.g. to find all the bindings in the map. This
module also documents the type variable naming conventions (see
{!Map_ops}).

{b Important note on code style:} Interfaces are essentially groups of
polymorphic functions, collected together and named using a record
type. For example, map operations are something of the form
[('k,'v,'t) map_ops]. To get a handle on the components of such a
thing we provide functions such as {!Map_ops.dest_map_ops} which takes
a set of map operations and a "continuation" function and calls the
function with the components of the record. Example code should make
this clearer:


{v
dest_imperative_map_ops map_ops @@ fun ~find ~insert ~delete ->
  (* write values *)
  for x=1 to max do
    insert (k x) (v x);
  done;
v}




{1 Store to map}

This package, particularly the {!Store_to_map} module, wraps the
Isabelle routines to implement a map interface on top of a
store. The key function is {!Store_to_map.store_ops_to_map_ops}
which takes a [store_ops] and returns a [map_ops].

Also included here are {!Big_step} (to iterate the small step
operations provided by Isabelle) and {!Iter_leaf_stream} (to wrap
small-step leaf stream operations).


{1 Disks}

{!Disk_on_fd} is a persistent block device on top of a "normal" file. 

{!Disk_to_store} includes a function that naively transforms a disk
to a store. 


{1 Stores}

{!Mem_store} is an in-memory store. 

{!Recycling_store} is a store that optimizes page alloc and free to
avoid too many unnecessary writes. Currently commented out. FIXME?


{1 Binprot marshalling}

{!Binprot_marshalling} provides on-disk marshalling courtesy of
[binprot]. You can, of course, provide your own marshalling code,
which may be useful if your backend doesn't support binprot (js_of_ocaml?)


{1 Cache}

A generic LRU {!Cache} on top of a map. This is rather more
sophisticated than usual because we have to do various things when
cache entries (eg uncommitted disk operations) are flushed.


{1 Examples}

Various examples. 

{!Bin_prot_util} is a very small module which defines eg
[bp_size_int].

{!Bytestore} provides a mechanism to store arbitrary length byte
buffers on top of a store. Currently commented out. FIXME?

{!Digest_} provides a digest/hash of a string, for situations where a
string is used as a key, but the exact contents of the string is not needed.

{!Examples_common} 

{!Map_int_blk} provides a map from blk index to blk

{!Map_int_blkx} provides a map from blk index to (partial) blk (i.e.,
only the first part of the blk is used).

{!Map_int_int} provides a map from int to int, using [binprot]
marshalling.

{!Map_on_fd} is a generic map backed by a file descriptor.

{!Mem_map} is a generic map on top of the {!Mem_store}.

{!Small_string} provides a string with a max size of 256 bytes
(using arbitrary length strings as keys requires another approach).

{!Ss_ss_map_on_fd} implements an on-disk map from small string to
small string.


{1 Testing}

Various modules to support testing. Some of this looks a bit
suspicious. FIXME

{1 Doc}

Contains this overview documentation in module {!Tjr_btree_doc}.


*)

let dummy = 1 (* FIXME needed? *)



