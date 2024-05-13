#!/bin/sh
#
# Shell wrapper for MiniML compiler (mlc.sh)
#
# CMSC 22600 --- Compilers for Computer Languages
# Spring 2024
# University of Chicago
#

MLC=$0
BINDIR=${MLC%mlc.sh}

HEAP_SUFFIX=$(sml @SMLsuffix)

HEAP="$BINDIR/mlc.$HEAP_SUFFIX"

if test ! -r "$HEAP" ; then
  echo "$MLC: no heap image; run make in src directrory to build compiler"
  exit 1
fi

exec sml @SMLcmdname=mlc @SMLload="$HEAP" "$@"
