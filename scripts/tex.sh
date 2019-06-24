#!/bin/bash

. config.in

LATEX_TEMPLATES_URL="https://github.com/cosmonaut-ok/latex-templates/archive/master.zip"
EZBF_URL="https://gist.githubusercontent.com/andersjohansson/fa7ca643782771b6e15da41514e1358a/raw/25e9f875ef6a34bbdd2c24c34637310596755722/emacs-zotero-bib-fetch.el"

function pkg_install
{
    get_url_with_name latex-templates.zip $LATEX_TEMPLATES_URL
    extract latex-templates.zip
    copy_to_local "latex-templates-master/*" latex-templates
    get_url_with_name emacs-zotero-bib-fetch.el $EZBF_URL
    copy_to_local emacs-zotero-bib-fetch.el latex
}


function pkg_update
{
    :
}

. include.in
