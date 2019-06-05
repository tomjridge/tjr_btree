#!/bin/bash

echo "exercise_ss_map -------------------------------------------------"

set +x

# execute various commands on a store

STORE=./btree.store
MAIN=string_string_map_main
echo

echo Executing various simple operations -------------------------------
rm -f $STORE
$MAIN init $STORE
echo
$MAIN insert $STORE k1 v1
echo
$MAIN list $STORE
echo
$MAIN insert $STORE k2 v2 
echo
$MAIN insert $STORE k3 v3 
echo
$MAIN list $STORE
echo

echo Start inserting many entries... -----------------------------------
echo NOTE this would be faster with insert_many

# echo Start inserting many entries
# for i in `seq 4 1000`; do
#     $MAIN insert $STORE k$i v$i
# done
# echo Finish


time $MAIN insert_range $STORE 4 10000
echo Finish 
echo NOTE should take about .8s for 10k entries, with some of that as startup overhead
echo

# $MAIN list $STORE
