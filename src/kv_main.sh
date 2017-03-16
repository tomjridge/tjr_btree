#!/bin/bash

set +x

# execute various commands on a store

FN=./btree.store
MAIN=main.native

rm -f /tmp/store
./$MAIN kv init $FN
echo
./$MAIN kv insert $FN k1 v1
echo
./$MAIN kv list $FN
echo
./$MAIN kv insert $FN k2 v2 
echo
./$MAIN kv insert $FN k3 v3 
echo
./$MAIN kv list $FN


for i in `seq 4 1000`; do
    ./$MAIN kv insert $FN k$i v$i
done

# for 1000 inserts, with syncing after each insert
# real	0m11.880s
# user	0m6.004s
# sys	0m3.892s
# 

# after fiddling with something... fn position? FIXME performance regression
# real	0m25.313s
# user	0m5.984s
# sys	0m17.108s


# with no syncs; but probably this is meaningless
# real	0m3.575s
# user	0m0.144s
# sys	0m0.944s



# listing 1000 kvs; almost all in sys
# real	0m0.021s
# user	0m0.008s
# sys	0m0.012s
