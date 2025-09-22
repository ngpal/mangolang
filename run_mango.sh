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
cargo run --package compiler -- "$SRC" -o "$OUT_DIR/$BASENAME.masm" || exit 1

echo "assembling $OUT_DIR/$BASENAME.masm -> $OUT_DIR/$BASENAME.mobj"
cargo run --package assembler -- "$OUT_DIR/$BASENAME.masm" -o "$OUT_DIR/$BASENAME.mobj" || exit 1

echo "linking $OUT_DIR/$BASENAME.mobj + out/print.mobj -> $OUT_DIR/$BASENAME.mbin"
cargo run --package linker --  "$OUT_DIR/$BASENAME.mobj" out/print.mobj -o "$OUT_DIR/$BASENAME.mbin" || exit 1

echo "running $OUT_DIR/$BASENAME.mbin"
cargo run --package vm -- "$OUT_DIR/$BASENAME.mbin" -d
