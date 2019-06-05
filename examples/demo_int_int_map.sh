#!/bin/bash

set +x

# execute various commands on a store

STORE=./btree.store
MAIN=btree_main
command -v $MAIN || { echo "I require $MAIN but it's not installed.  Aborting."; exit 1; }
echo

echo Executing various simple operations
echo -----------------------------------
echo
rm -f $STORE
$MAIN init $STORE
echo
$MAIN insert $STORE 1 1
echo
$MAIN list $STORE
echo
$MAIN insert $STORE 2 2 
echo
$MAIN insert $STORE 3 3 
echo
$MAIN list $STORE
echo


echo Inserting many entries
echo ----------------------
echo
time $MAIN insert_range $STORE 4 10000
echo NOTE should take about .02s for 10k entries, with some of that as startup overhead
echo


# $MAIN list $STORE
