#!/bin/sh

PROG=./ei
EXPECT_DIR=test/expect
ACTUAL_DIR=test/actual

FILE=$1
SRC=test/src/$FILE
EXPECT=$EXPECT_DIR/$FILE
ACTUAL=$ACTUAL_DIR/$FILE

if ! [ -e $SRC ]; then
    echo "error: $SRC not found"
    exit 1
fi

if ! [ -e $EXPECT ]; then
    echo "error: $EXPECT not found"
    exit 1
fi

mkdir -p $ACTUAL_DIR
$PROG fmt $SRC > $ACTUAL
DIFF=`diff $EXPECT $ACTUAL`

if [ "$DIFF" = "" ]; then
    exit 0
else
    cat < $DIFF
    exit 1
fi
