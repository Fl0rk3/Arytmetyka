#!/bin/bash

cd "$(dirname "$0")"

echo Kompiluje Arytmetykę

ocamlc -c arytmetyka.mli arytmetyka.ml

for f in tests/*
do
    echo Przetwarzam: $(basename "$f")
    ocamlc -c "$f"
    ocamlc -o "${f%%.*}" arytmetyka.cmo "${f%%.*}".cmo
    time(./"${f%%.*}")
    rm "${f%%.*}" "${f%%.*}".cmo "${f%%.*}".cmi
done
