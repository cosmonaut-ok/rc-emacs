#!/bin/bash

. config.in

LATEX_TEMPLATES_URL="https://github.com/cosmonaut-ok/latex-templates/archive/master.zip"

function pkg_install
{
    get_url_with_name latex-templates.zip $LATEX_TEMPLATES_URL
    extract latex-templates.zip
    copy_to_local "latex-templates-master/*" latex-templates
}


function pkg_update
{
    :
}

. include.in
