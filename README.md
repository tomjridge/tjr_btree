# tjr_btree, a B-tree library for OCaml

## Description

tjr_btree is a B-tree library written for OCaml, and based on code extracted from an Isabelle formalization.

## Quick links

* The **Isabelle code** can be found [here](https://github.com/tomjridge/isa_btree).
* Online **ocamldoc** documentation can be found [here](https://tomjridge.github.io/tjr_btree/tjr_btree/Tjr_btree/index.html). The main entry point (for documentation, and using the library) is the module [Tjr_btree.Export](https://tomjridge.github.io/tjr_btree/tjr_btree/Tjr_btree/Export/index.html).
* Introductory **examples**, including a simple on-disk key-value store, can be found in the examples directory e.g. [here](./examples/int_int_map_example_functionality.ml).
* A simple **install script**, together with a Docker build script,  is in the repository:
  <https://github.com/tomjridge/tjr_imp_build_script>. I hope to make the library available in opam soon.




## Introduction

This code implements a B-tree. The implementation can be copy-on-write, or mutate-in-place. 

The B-tree supports the usual operations: `find, insert, delete`.
In addition, there is an `insert_many` operation for inserting
multiple key-value pairs at once. This operation is more efficient
than repeatedly inserting. It is typically used when higher-level
operations are cached and then flushed all at once.

The routines are fully parameterized (e.g., by key and value type, key
order, store type etc). It is easy to have many B-trees using the same
store, even operating concurrently (although individual B-trees must
be updated sequentially at the moment).

The core is implemented in small-step operational style, with each
disk access corresponding to a separate step. Thus, it should be
feasible to introduce very fine-grained concurrency by interleaving
individual steps of each operation (where multiple operations execute
on different B-trees).

The core code is written in Isabelle/HOL and exported to OCaml
(although it could easily be implemented in any language because the
routines are very "concrete"). This repository contains the OCaml
wrapper round the core OCaml code extracted from Isabelle.


## Dependencies (for the library)

From opam, there are no dependencies except what is required by the following libraries.

| Dependency    | Comment                                  |
| ------------- | ---------------------------------------- |
| tjr_fs_shared | Shared definitions for filesystem work   |
| isa_btree     | Base OCaml defns extracted from Isabelle |



## Dependencies (for examples and testing)

| Dependency   | Comment                               |
| ------------ | ------------------------------------- |
| ppx_bin_prot | marshalling via bin_prot, for testing |
| extunix?     | fsync                                 |
|              |                                       |



## Miscellaneous notes

### A note concerning on-disk persistence

All sync-ed states are persistent on disk (imagine that blocks are written once and then never mutated), so operations such as "snapshot" are almost trivial (assuming copy-on-write). FIXME clarify

### Relation to ImpFS

ImpFS is a project to build an advanced high-performance filesystem.
tjr_btree is one of the core libraries used by ImpFS.

### Performance

FIXME to do 
