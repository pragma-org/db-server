#!/usr/bin/env bash

prefix=$1
CARDANO_NODE_VERSION=$2

IOHKNIX_VERSION=$(curl -L https://raw.githubusercontent.com/IntersectMBO/cardano-node/$CARDANO_NODE_VERSION/flake.lock | jq -r '.nodes.iohkNix.locked.rev')

SODIUM_VERSION=$(curl -L https://raw.githubusercontent.com/input-output-hk/iohk-nix/$IOHKNIX_VERSION/flake.lock | jq -r '.nodes.sodium.original.rev')
echo "Using sodium version: $SODIUM_VERSION"

: ${SODIUM_VERSION:='dbb48cc'}
git clone https://github.com/intersectmbo/libsodium
cd libsodium
git checkout $SODIUM_VERSION
./autogen.sh
# install in local user directory
./configure --prefix=${prefix}
make
make install
