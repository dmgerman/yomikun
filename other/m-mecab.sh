#!/usr/bin/env bash


MDIR="/Users/dmg/bin/osx/MecabUnidic/support"
export LD_LIBRARY_PATH="$MDIR":
export DYLD_LIBRARY_PATH="$MDIR":
#"$MDIR/mecab" -d $MDIR -h
if [ "$1" == "" ] ; then
    "$MDIR/mecab" -d $MDIR -r "$MDIR/mecabrc"
else
    "$MDIR/mecab" -d $MDIR -r "$MDIR/mecabrc" $*
fi
#unidic22
#BOS/EOS,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*,*
