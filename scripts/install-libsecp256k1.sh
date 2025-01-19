#!/usr/bin/env bash

set -euovx pipefail

prefix=$1
CARDANO_NODE_VERSION=$2

IOHKNIX_VERSION=$(curl -L https://raw.githubusercontent.com/IntersectMBO/cardano-node/$CARDANO_NODE_VERSION/flake.lock | jq -r '.nodes.iohkNix.locked.rev')

SECP256K1_VERSION=$(curl https://raw.githubusercontent.com/input-output-hk/iohk-nix/$IOHKNIX_VERSION/flake.lock | jq -r '.nodes.secp256k1.original.ref')
echo "Using secp256k1 version: ${SECP256K1_VERSION}"

: ${SECP256K1_VERSION:='v0.3.2'}
[[ -d secp256k1 ]] || git clone --depth 1 --branch ${SECP256K1_VERSION} https://github.com/bitcoin-core/secp256k1
cd secp256k1
./autogen.sh
./configure --enable-module-schnorrsig --enable-experimental --prefix=$prefix --enable-shared=no
make
make install
