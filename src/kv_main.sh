#!/bin/bash

set +x

# execute various commands on a store

STORE=./btree.store
MAIN=main.native

rm -f $STORE
./$MAIN kv init $STORE
echo
./$MAIN kv insert $STORE k1 v1
echo
./$MAIN kv list $STORE
echo
./$MAIN kv insert $STORE k2 v2 
echo
./$MAIN kv insert $STORE k3 v3 
echo
./$MAIN kv list $STORE


for i in `seq 4 1000`; do
    ./$MAIN kv insert $STORE k$i v$i
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
