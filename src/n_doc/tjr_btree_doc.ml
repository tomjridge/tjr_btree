(** tjr_btree, a COW B-tree in OCaml (documentation) *)

(**

General documentation and overview

{1 Introduction}

This library implements a B-tree in OCaml. The source is actually
   exported from Isabelle, and then OCaml code wraps the
   Isabelle-generated code.

The code is organized into subdirectories (subpackages), and the
   subdirectories are in a linear order (which corresponds to the
   dependencies between modules).

This document gives an overview of the (sub)packages. Detailed
   comments are included in the individual modules.

{1 Guide to the code}

{2 Naming conventions}

To understand the interfaces, we need to introduce the following:

- Keys, represented by type variable ['k] - Values, by type var ['v] -
   Page/block references, ['r] - Global state, ['t]


{2 Exported code from Isabelle}

Exported code is in the [from_isa] package. There is some patching of
   the Isabelle-exported code before it is copied to [base_types] in
   module {!Isa_export}. Hopefully this code is relatively stable. It
   certainly isn't readable.


{2 Pickle}

The marshalling and pickling routines are in the [pickle] package. See
   module {!Pickle}. We also include a utility library for strings
   {!Tjr_string}, and a few functions to deal with logging and testing
   in {!Test}.


{2 Base types}

This package contains basic B-tree related types. They are collected
   in the module {!Base_types}. The {!Base_types.Monad} is a
   state-passing monad with error; this monad shows up in most of the
   interfaces. {!Store_ops} is another important module which
   describes a disk-like interface on top of which we implement the
   B-tree. The {!Base_types.Tree} module describes a B-tree as an
   algebraic datatype. The on-disk B-tree uses references between
   blocks i.e. a graph-like structure with pointers rather than a
   datatype. Indeed, the tree datatype is used only for testing
   purposes.



{2 Prelude}

Miscellaneous definitions. {!Prelude.Small_step} wraps the Isabelle
   small-step definitions.


{2 Api}

{!Btree_api} gives the main interface types, including interfaces for
   disk, store and map (via record types such as [map_ops]). In
   addition, the leaf stream interface allows to iterate over the
   leaves in a B-tree e.g. to find all the bindings in the map. This
   module also documents the type variable naming conventions.

{!Default} is a simple collection of defaults eg default block size.


{2 Store to map}

This package, particularly the {!Store_to_map} module, wraps the
   Isabelle routines to implement a map interface on top of a
   store. The key function is {!Store_to_map.make_map_ops} which takes
   a [store_ops] and returns a [map_ops].



{2 Disks}

{!Disk_on_fd} is a persistent disk on top of a file. {!Disk_to_store}
   includes a function that naively transforms a disk to a store. This
   involves converting a {!Frame} to a block, using
   {!Btree_with_pickle} TODO rename.


{2 Stores}

{!Mem_store} is an in-memory store. {!Recycling_store} is a store that
   optimizes page alloc and free to avoid too many unnecessary writes.


{2 Pre examples}

This package contains various modules needed by the examples, such as
   {!Small_string} which implements strings upto 256 bytes (used for
   keys in the B-tree).


{2 Cache}

A generic LRU {!Cache} on top of a map.


{2 Examples}

Various examples. {!Ss_ss_map_on_fd} implements an on-disk map from
   (small) string to string. {!Int_int_map_on_fd} and
   {!Ss_int_map_on_fd} are similar. {!Map_on_fd} is the generic
   version and {!Mem_map} is a map on top of the
   {!Mem_store}. {!Bytestore} is TODO and should implement arbitrary
   long values.


{2 Testing}

Various tests.


{2 Main}

These [xxx.ml] files get turned into executables
   [xxx.native]. [main.native] is used by the [kv_main.sh] example;
   [test_main.native] is used for testing; [simple_example.native] is
   the simple example.

*)
let dummy = 1
