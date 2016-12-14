#!/bin/bash

. config.in

HSVIS_URL="https://www.emacswiki.org/emacs/download/hideshowvis.el"
HL_LINE_PLUS_URL="https://www.emacswiki.org/emacs/download/hl-line%2b.el"
REDO_URL="https://www.emacswiki.org/emacs/download/redo%2b.el"

function pkg_install
{
    get_url_with_name hideshowvis.el $HSVIS_URL
    get_url_with_name hl-line+.el $HL_LINE_PLUS_URL
    get_url_with_name redo+.el $REDO_URL
    copy_to_local hideshowvis.el hideshowvis
    copy_to_local hl-line+.el hl-line-plus
    copy_to_local redo+.el redo+
}


function pkg_update
{
    :
}

. include.in
