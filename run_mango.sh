#!/bin/bash

# usage: ./run_mango.sh source_file

source ./.envrc

if [ -z "$1" ]; then
    echo "usage: $0 <source_file>"
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
cargo run --package linker -q --  "$OUT_DIR/$BASENAME.mobj" stdlib/math.mobj -o "$OUT_DIR/$BASENAME.mbin" || exit 1

echo "running $OUT_DIR/$BASENAME.mbin"
cargo run --package vm -q -- "$OUT_DIR/$BASENAME.mbin" -d
