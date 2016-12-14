#!/bin/bash

. config.in

CHEF_MODE_URL="https://raw.githubusercontent.com/mpasternacki/chef-mode/master/chef-mode.el"
CHEF_URL="https://raw.githubusercontent.com/cosmonaut-ok/emacs-chef/master/chef.el"
KITCHEN_URL="https://raw.githubusercontent.com/restaurant-ide/test-kitchen-el/master/test-kitchen.el"
KNIFE_URL="https://raw.githubusercontent.com/bryanwb/knife-mode/master/knife.el"
BERKS_URL="https://raw.githubusercontent.com/restaurant-ide/berkshelf.el/master/berkshelf.el"
FOODCRITIC_URL="https://raw.githubusercontent.com/restaurant-ide/foodcritic.el/master/foodcritic.el"
function pkg_install
{
    get_url_with_name chef.el $CHEF_URL
    get_url_with_name chef-mode.el $CHEF_MODE_URL
    get_url_with_name test-kitchen.el $KITCHEN_URL
    get_url_with_name knife.el $KNIFE_URL
    get_url_with_name berkshelf.el $BERKS_URL
    get_url_with_name foodcritic.el $FOODCRITIC_URL
    copy_to_local chef.el chef
    copy_to_local chef-mode.el chef
    copy_to_local test-kitchen.el chef
    copy_to_local knife.el chef
    copy_to_local berkshelf.el chef
    copy_to_local foodcritic.el chef
}


function pkg_update
{
    :
}

. include.in
