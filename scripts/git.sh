#!/bin/bash

. config.in


GITHUB_CLONE_URL="https://raw.githubusercontent.com/dgtized/github-clone.el/master/github-clone.el"
YAGIST_URL="https://raw.githubusercontent.com/mhayashi1120/yagist.el/master/yagist.el"
MAGIT_GH_PULLS="https://raw.githubusercontent.com/sigma/magit-gh-pulls/master/magit-gh-pulls.el"
GITHUB_BROWSE_FILE="https://raw.githubusercontent.com/osener/github-browse-file/master/github-browse-file.el"
GH_NOTIFIER_URL="https://raw.githubusercontent.com/xuchunyang/github-notifier.el/master/github-notifier.el"

function pkg_install
{
    get_url_with_name github-clone.el $GITHUB_CLONE_URL
    get_url_with_name magit-gh-pulls.el $MAGIT_GH_PULLS
    get_url_with_name github-browse-file.el $GITHUB_BROWSE_FILE
    get_url_with_name github-notifier.el $GH_NOTIFIER_URL
    get_url_with_name yagist.el $YAGIST_URL
    copy_to_local github-clone.el github
    copy_to_local github-browse-file.el github
    copy_to_local magit-gh-pulls.el github
    copy_to_local github-notifier.el github
    copy_to_local yagist.el yagist
    
}


function pkg_update
{
    :
}

. include.in
