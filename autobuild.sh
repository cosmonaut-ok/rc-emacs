#!/bin/sh

autoconf
./configure --with-gui=${i} --enable-static
make
