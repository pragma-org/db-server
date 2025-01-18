#!/usr/bin/env bash

set -euovx pipefail

prefix=$1
CARDANO_NODE_VERSION=$2

IOHKNIX_VERSION=$(curl -L https://raw.githubusercontent.com/IntersectMBO/cardano-node/$CARDANO_NODE_VERSION/flake.lock | jq -r '.nodes.iohkNix.locked.rev')

BLST_VERSION=$(curl https://raw.githubusercontent.com/input-output-hk/iohk-nix/$IOHKNIX_VERSION/flake.lock | jq -r '.nodes.blst.original.ref')
echo "Using blst version: ${BLST_VERSION}"

: ${BLST_VERSION:='v0.3.11'}
git clone --depth 1 --branch ${BLST_VERSION} https://github.com/supranational/blst
cd blst
./build.sh
cat >libblst.pc <<EOF
prefix=$prefix
exec_prefix=\${prefix}
libdir=\${exec_prefix}/lib
includedir=\${prefix}/include

Name: libblst
Description: Multilingual BLS12-381 signature library
URL: https://github.com/supranational/blst
Version: ${BLST_VERSION#v}
Cflags: -I\${includedir}
Libs: -L\${libdir} -lblst
EOF
cp libblst.pc $prefix/lib/pkgconfig/
cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp $prefix/include/
cp libblst.a $prefix/lib
chmod u=rw,go=r $prefix/{lib/{libblst.a,pkgconfig/libblst.pc},include/{blst.{h,hpp},blst_aux.h}}
