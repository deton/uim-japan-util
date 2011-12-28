#!/bin/sh
srcdir=$(dirname $0)
scmdir=$(pkg-config --variable=uim_scmdir uim)
pixmapsdir=$(pkg-config --variable=uim_datadir uim)/pixmaps
cp "$srcdir/japan-util.scm" "$srcdir/japan-util-custom.scm" "$scmdir"
cp "$srcdir/pixmaps/japan-util.png" "$srcdir/pixmaps/japan-util_dark_background.png" "$pixmapsdir"
cp "$srcdir/pixmaps/japan-util.svg" "$srcdir/pixmaps/japan-util_dark_background.svg" "$pixmapsdir"
uim-module-manager --register japan-util
