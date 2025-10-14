#!/bin/bash
set -e  # stop on error

echo "[*] wiping virtual disk..."
rm -f disk/sector_*

# assemble + link + write function
build_and_write() {
    local src="$1"
    local obj="$2"
    local bin="$3"
    local sector="$4"
    local linker_args="$5"

    echo "[*] assembling $src -> $obj"
    cargo run --quiet --package assembler -- "$src" -o "$obj"

    echo "[*] linking $obj -> $bin $linker_args"
    cargo run --quiet --package linker -- "$obj" -o "$bin" $linker_args

    echo "[*] writing $bin to sector $sector (with -s flag)"
    cargo run --quiet --package vm -- dw "$bin" "$sector" -s
}

# --- BIOS ---
build_and_write "sys/bios.masm" "sys/bios.mobj" "sys/bios.mbin" "0x0000" "-b 0x0000"

# --- INT0 ---
build_and_write "sys/int0.masm" "sys/int0.mobj" "sys/int0.mbin" "0x0001"

# --- INT7 ---
echo "[*] assembling and linking sys/int7..."
cargo run --quiet --package assembler -- sys/int7.masm -o sys/int7.mobj
cargo run --quiet --package linker -- sys/int7.mobj -o sys/int7.mbin

for sector in 0x0002 0x0003 0x0004 0x0005 0x0006 0x0007 0x0008; do
    echo "[*] writing sys/int7.mbin to sector $sector (with -s flag)"
    cargo run --quiet --package vm -- dw sys/int7.mbin "$sector" -s
done

echo "[+] all system files built, linked, and written successfully!"
