#! /bin/bash

WORK=src/
SRC=Bb.hs
TEST=__test_chop.hs
DESTDIR=../dest/
AOUT=${DESTDIR}/bb

cd ${WORK}
if test "$1" = "clean" -o "$1" = "test" ; then
  rm ${DESTDIR}*
fi

if test "$1" = "test"; then
  ghc -outputdir $DESTDIR -o $AOUT $TEST
else
  ghc -outputdir $DESTDIR -o $AOUT $SRC
fi

if test "$1" = "run" -o "$1" = "test"; then
  $AOUT
fi
