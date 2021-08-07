#!/bin/sh

XATS2CC=/home/hwxi/Research/xanadu-lang/xats2cc/bin/xats2cc

if [ ! "$XATSHOME" ] ; then
  export XATSHOME=/home/hwxi/Research/xanadu-lang/xats2cc/xatsopt
fi

if [ -f "$XATS2CC" ] ; then
  "$XATS2CC" "$@"
else
  # if build failed or "make cleanall" was executed
  echo "please execute `make all` to build [xats2cc]"
fi
