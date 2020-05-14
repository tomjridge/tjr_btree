# tjr_btree, a B-tree library for OCaml



[TOC]

## Description

tjr_btree is a B-tree library written for OCaml, and based on code extracted from an Isabelle formalization.

Library structure: 

<img src="https://docs.google.com/drawings/d/e/2PACX-1vSqzipIxfOtcWhtSEqcBUpEKPVp1ALtHYyVVBldz7WNP3idcaQTY0iHoLBMf9n4vNMUjDvoIi_gr2gE/pub?w=550&amp;h=336">


System structure:

<img src="https://docs.google.com/drawings/d/e/2PACX-1vSbPmP9hfqwpYdJefrAYVY_7nSf6Mf5kzAXHYEaaAbw6cLwkWJH9GImYG_4KwKRDLOOjDGMvePbodwt/pub?w=1137&amp;h=766">


Type dependencies:

<img src="https://docs.google.com/drawings/d/e/2PACX-1vRpuIadu1YMpruLuyKu5ActBCj9IOfQFnUs6wL91uDpMAh_UG085rhdv5ePmu8IP4xC-TISFa3Tso9D/pub?w=940&amp;h=601">


## Quick links

* The **Isabelle code** can be found [here](https://github.com/tomjridge/isa_btree).
* Online **ocamldoc** documentation can be found [here](https://tomjridge.github.io/ocamldocs/). 
* Introductory **examples**, including a simple on-disk key-value store, can be found in the examples directory.
* To **install**, try the Dockerfile (`docker build .`) and then to build locally, strip the "RUN" prefix from the relevant Dockerfile lines and use opam pin.


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

## Install

The easiest way to install is via opam, pinning the github repos. See the `Dockerfile`.


## Usage

In your code, use:

~~~
open Tjr_btree
open Tjr_btree.Btree_intf
open Tjr_btree_examples
~~~

## Examples

After installing, you can run the examples by typing:

~~~bash
make run
~~~

The output should look something like:

![Imgur](https://i.imgur.com/Cvumjbe.png)



## Dependencies (for the library)

From opam, there are no dependencies except what is required by the following libraries.

| Dependency | Comment                                  |
| ---------- | ---------------------------------------- |
| isa_btree  | Base OCaml defns extracted from Isabelle |



## Dependencies (for examples and testing)

| Dependency             | Comment                               |
| ---------------------- | ------------------------------------- |
| bin_prot, ppx_bin_prot | marshalling via bin_prot, for testing |
| extunix?               | fsync                                 |
|                        |                                       |



## Miscellaneous notes

### A note concerning on-disk persistence

All sync-ed states are persistent on disk (imagine that blocks are written once and then never mutated), so operations such as "snapshot" are almost trivial (assuming copy-on-write). FIXME clarify

### Relation to ImpFS

ImpFS is a project to build an advanced high-performance filesystem.
tjr_btree is one of the core libraries used by ImpFS.

### Performance

FIXME to do 
