#!/bin/sh
scmdir=$(pkg-config --variable=uim_scmdir uim)
pixmapsdir=$(pkg-config --variable=uim_datadir uim)/pixmaps
cp japan-util.scm japan-util-custom.scm "$scmdir"
cp pixmaps/japan-util.png pixmaps/japan-util_dark_background.png "$pixmapsdir"
cp pixmaps/japan-util.svg pixmaps/japan-util_dark_background.svg "$pixmapsdir"
uim-module-manager --register japan-util
