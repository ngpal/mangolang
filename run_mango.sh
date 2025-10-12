#!/bin/bash

# usage: ./run_mango.sh [-d] source_file

source ./.envrc

DEBUG_FLAG=""

# parse -d flag
if [ "$1" = "-d" ]; then
    DEBUG_FLAG="-d"
    shift
fi

if [ -z "$1" ]; then
    echo "usage: $0 [-d] <source_file>"
    exit 1
fi

SRC="$1"
BASENAME=$(basename "$SRC" .mg)
DIRNAME=$(dirname "$SRC")

OUT_DIR="out"
mkdir -p "$OUT_DIR"

echo "compiling $SRC -> $OUT_DIR/$BASENAME.masm"
cargo run --package compiler -q -- "$SRC" -o "$OUT_DIR/$BASENAME.masm" || exit 1

echo "assembling $OUT_DIR/$BASENAME.masm -> $OUT_DIR/$BASENAME.mobj"
cargo run --package assembler -q -- "$OUT_DIR/$BASENAME.masm" -o "$OUT_DIR/$BASENAME.mobj" || exit 1

echo "assembling stdlib/math.masm -> stdlib/math.mobj"
cargo run --package assembler -q -- stdlib/math.masm -o stdlib/math.mobj || exit 1

echo "linking $OUT_DIR/$BASENAME.mobj + stdlib/math.mobj -> $OUT_DIR/$BASENAME.mbin"
cargo run --package linker -q -- "$OUT_DIR/$BASENAME.mobj" stdlib/math.mobj -o "$OUT_DIR/$BASENAME.mbin" || exit 1

echo "loading $OUT_DIR/$BASENAME.mbin to sector 0x0009 of the VM"
cargo run --package vm -q -- dw "$OUT_DIR/$BASENAME.mbin" 0x0009 || exit 1

echo "running $OUT_DIR/$BASENAME.mbin"
cargo run --package vm -q -- run $DEBUG_FLAG
