#!/bin/bash

# Compiles Lisp functions with ASD and runs the code generator
# Takes BLAS function name, microkernel dimensions, float size in bits, and register size in bits
# as arguments
# e.g. sh ./build.sh gemm 8 6 64 256
# NB: first dimension should be a multiple of 4 (for now)
# Only gemm supported for now, other level 3 BLAS operations to be added

if [ $# -ne 5 ]; then
    echo "Requires 5 arguments: $0 <OP_NAME> <MR> <MR> <FLOAT_SIZE> <REGISTER_SIZE>"
    echo "e.g.: $0 gemm 8 6 64 256"
    exit 1
fi

OP_NAME=$1
MR=$2
NR=$3
FLOAT_SIZE=$4
REGISTER_SIZE=$5

cd "$(dirname "$0")"

sbcl --noinform --non-interactive \
     --eval "(require :asdf)" \
     --eval "(push (probe-file \".\") asdf:*central-registry*)" \
     --eval "(asdf:load-system :blas-gen)" \
     --eval "(write-blas)" \
     --quit \
     -- "$OP_NAME" "$MR" "$NR" "$FLOAT_SIZE" "$REGISTER_SIZE"

if [ $? -eq 0 ]; then
    echo "Generated $MR x $NR microkernels"
else
    echo "Something went wrong -- check SBCL"
    exit 1
fi
