# configure.ac --- autoconfiguration file  -*- Autoconf -*-

# Copyright (C) 2016 Alexander aka 'CosmonauT' Vynnyk

# Maintainer: cosmonaut.ok@zoho.com
# Keywords: internal
# Package: cosmonaut

# This file is part of Restaurant.

# Restaurant is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# Restaurant is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Restaurant.  If not, see <http://www.gnu.org/licenses/>.

# Commentary:

# TODO:

# Code:

AC_PREREQ(2.69)

AC_INIT(Cosmonaut RC emacs, 0.1, cosmonaut.ok@zoho.com)

AC_SUBST(EMACS_URI)
AC_SUBST(BUILD_DIR)
AC_SUBST(GUI_TOOLKIT)
AC_SUBST(RELEASE_FILES)
AC_SUBST(EMACS)
AC_SUBST(LOCAL_EMACS_DIR)
AC_SUBST(BUILD_OPTIONS)
AC_SUBST(BYTE_COMPILE_P)
AC_SUBST(STATIC)

## veriables definition
BUILD_OPTIONS="--without-pop"

# Checks for programs.
AC_ARG_WITH(gui,      [  --with-gui=TOOLKIT      use gtk2, gtk3, motif, athena, no (default gtk3)], GUI_TOOLKIT=$withval, GUI_TOOLKIT="gtk3")
AC_ARG_WITH(emacs,    [  --with-emacs=path       use the custom path to emacs binary], EMACS=$withval, EMACS=$EMACS)
AC_ARG_WITH(gif,      [  --with-gif              build emacs with/without gif support (default no)], GIF=$withval, GIF="no")
AC_ARG_WITH(png,      [  --with-png              build emacs with/without png support (default no)], PNG=$withval, PNG="no")
AC_ARG_WITH(jpeg,     [  --with-jpeg             build emacs with/without jped support (default no)], JPEG=$withval, JPEG="no")
AC_ARG_WITH(tiff,     [  --with-tiff             build emacs with/without tiff support (default no)], TIFF=$withval, TIFF="no")
AC_ARG_WITH(makeinfo,     [  --with-makeinfo         build emacs with/without makeinfo additional information (default no)], MAKEINFO=$withval, MAKEINFO="no")
# AC_ARG_WITH(xml2,     [  --without-xml2             build emacs with/without xml2 support (default no)], ECL_PATH=$withval, XML2="auto")
AC_ARG_ENABLE(byte-compile, [  --enable-byte-compile  Enables Restaurant bute compilation (experimental, default: disabled)], [ BYTE_COMPILE_P=yes ])
AC_ARG_ENABLE(static, [  --enable-static  Enables static precompiled archives, instead of full build], [ STATIC=yes ])

# check for makeinfo
AC_CHECK_PROG(GIT,git,yes,no)
AC_CHECK_PROG(MERCURIAL,hg,yes,no)
AC_CHECK_PROG(RUBY,ruby,yes,no)
AC_CHECK_PROG(BUNDLE,bundle,yes,no)
AC_CHECK_PROG(SSHASKPASS,ssh-askpass,yes,no)
AC_CHECK_PROG(MARKDOWN,markdown,yes,no)

# check for build deps
AC_CHECK_HEADER([X11/Xlib-xcb.h])
AC_CHECK_HEADER([X11/xpm.h])

if test $GIT == "no"; then
   AC_MSG_ERROR([there is no git package in system. This binary required for correct cosmonaut working])
fi

if test $MERCURIAL == "no"; then
   AC_MSG_ERROR([there is no mercurial package (hg) in system])
fi

if test $RUBY == "no"; then
   AC_MSG_WARN([there is no ruby package in system. You can not use cosmonaut (build only)])
fi

if test $BUNDLE == "no"; then
   AC_MSG_WARN([there is no bundler package in system. You can not use cosmonaut (build only)])
fi

if test $SSHASKPASS == "no"; then
   AC_MSG_WARN([there is no ssh-askpass package in system. You can not use github pull requests from cosmonaut (build only)])
fi

if test $MARKDOWN == "no"; then
   AC_MSG_ERROR([there is no markdown package in system. This binary required for correct cosmonaut working])
fi

if test x$GIF == "xno"; then
   BUILD_OPTIONS="$BUILD_OPTIONS --without-gif"
fi

if test x$PNG == "xno"; then
   BUILD_OPTIONS="$BUILD_OPTIONS --without-png"
fi

if test x$JPEG == "xno"; then
   BUILD_OPTIONS="$BUILD_OPTIONS --without-jpeg"
fi

if test x$TIFF == "xno"; then
   BUILD_OPTIONS="$BUILD_OPTIONS --without-tiff"
fi

if test x$MAKEINFO == "xno"; then
   BUILD_OPTIONS="$BUILD_OPTIONS --without-makeinfo"
fi

AC_OUTPUT(Makefile)

