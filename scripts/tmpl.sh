#!/bin/bash

. config.in

PAREDIT_URL="http://mumble.net/~campbell/emacs/paredit.el.gz"

function pkg_install
{
    get_url_with_name paredit.el.gz $PAREDIT_URL
    extract paredit.el.gz
    copy_to_local paredit.el paredit
}


function pkg_update
{
    :
}

. include.in
