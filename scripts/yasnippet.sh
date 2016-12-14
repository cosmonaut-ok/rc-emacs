#!/bin/bash

. config.in

YAS_COMPANY_URL="https://raw.githubusercontent.com/company-mode/company-mode/master/company-yasnippet.el"

function pkg_install
{
    get_url_with_name company-yasnippet.el $YAS_COMPANY_URL
    copy_to_local company-yasnippet.el company-yasnippet
}


function pkg_update
{
    :
}

. include.in
