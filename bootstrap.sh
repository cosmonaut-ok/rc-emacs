#!/bin/sh

set -e

if [ ! -z $1 ]; then
    RUBY_VERSION=$1
else
    RUBY_VERSION=2.2
fi

SCRIPT_HOME="$(dirname `realpath $0`)"
REQUIRED_PACKAGES="git vagrant sbcl emacs,emacs24,emacs aspell texlive-full texlive-lang-cyrillic"

print_message()
{
    message=$@
    msg_engine=""
    ## check
    if [ -n "$(which zenity)" ]; then
	msg_engine="zenity --error --text"
    elif [ -n "$(which kdialog)" ]; then
	msg_engine="kdialog --error"
    elif [ -n "$(which xdialog)" ]; then
	msg_engine="dialog --error"
    else
	msg_engine="echo"
    fi
    $msg_engine "$message"
}

bootstrap_with_packages ()
{
    os="$(which lsb_release 2>/dev/null >/dev/null && lsb_release -s -i)"
    sudo_cmd="$(which sudo || true)"
    su_cmd="su -c"
        
    case $os in
	Debian|Ubuntu)
	    echo "installing required packages"
	    echo sudo: $sudo_cmd
	    echo su  : $su_cmd
	    
	    if [ -n "$sudo_cmd" ]; then
		$sudo_cmd apt-get -y install $(for i in $REQUIRED_PACKAGES; do echo $i|cut -d, -f2; done)
	    else
		$su_cmd "apt-get -y install $(for i in $REQUIRED_PACKAGES; do echo $i|cut -d, -f3; done)"
	    fi
	    ;;
	CentOS|RHEL)
	    echo "installing required packages"
	    if [ -n "$sudo_cmd" ]; then
		$sudo_cmd yum -y install $REQUIRED_PACKAGES
	    else
		$su_cmd "yum -y install $REQUIRED_PACKAGES"
	    fi
	    ;;
	*)
	    for i in $REQUIRED_PACKAGES; do
		if [ -z "$(which $i)" ]; then
		    print_message You must install package \"$i\" firstly
		    exit 1
		fi
	    done
	    ;;
    esac
}

rvm_installed_p ()
{
  which rvm 2>/dev/null
}


bootstrap_rvm ()
{
  if rvm_installed_p; then
      echo "RVM alreary installed. Skipping installation. Installing ruby and required gems"
      rvm install ${RUBY_VERSION}
      rvm use ${RUBY_VERSION}
      gem install bundler
  else
    echo "RVM is not installed. Installing rvm, ruby and required gems"
    # gpg --keyserver ${GNUPG_URL} --recv-keys ${RVM_KEY}
    echo "Importing GPG keys"
    [ -n "$(which gpg)" ] && curl -sSL https://rvm.io/mpapis.asc | gpg --import - 2>/dev/null
    [ -n "$(which gpg2)" ] && curl -sSL https://rvm.io/mpapis.asc | gpg2 --import - 2>/dev/null
    echo "done"
    # bootstrap RVM
    curl -sSL https://get.rvm.io | bash -s stable --ruby=${RUBY_VERSION} --auto-dotfiles --gems=bundler
    echo "RVM Installed"
  fi
  # install required gems
  ## use ``/bin/bash --login`` because rvm is stupid
  /bin/bash --login -c ". ${HOME}/.rvm/scripts/rvm && rvm use $RUBY_VERSION && cd $SCRIPT_HOME && bundle install"
}

bootstrap_with_packages

bootstrap_rvm

FIX_PATH="$(echo $SCRIPT_HOME | sed 's/\//\\\//g')"

printf "\n\nYou must run command 'source ${HOME}/.rvm/scripts/rvm' before launching cosmonaut, or just restart 'cosmonaut' if it already launched\n\n"
