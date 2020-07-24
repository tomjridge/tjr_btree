(** [Make_6] main interfaces:

{[
$(INCLUDE("GEN*"))
]}


{2 Note about crash correctness}

Essentially we have to assume that the B-tree writes hit disk in order (ie there is a barrier operation after each write).

In order to make the B-tree fast, we have to use a persistent cache in front.

Fortunately, providing we have the persistent cache, I believe/hope (but have certainly not proved) that the combination (pcache+btree) is crash safe. The reason is that if we crash in the middle of modifying the B-tree, we can replay the operations from the pcache, and this has the effect of fixing up the B-tree.

The persistent cache is on github, as "tjr_pcache".

*)
