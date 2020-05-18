#!/bin/bash

set -o errexit  # Exit on most errors
set -o errtrace # Make sure any error trap is inherited
set -o nounset  # Disallow expansion of unset variables
set -o pipefail # Use last non-zero exit code in a pipeline
#set -o xtrace  # Trace the execution of the script (debug)

# GENERAL
termux-setup-storage

function install {
  which $1 &> /dev/null

  if [ $? -ne 0 ]; then
    echo "Installing: ${1}..."
    apt -y install $1
  else
    echo "Already installed: ${1}"
  fi
}

apt update
install git
install termux-api
install vim-nox
install openssh
install wget
install curl
install rsync

# Append public key to authorized keys.
cat id_rsa.pub > ~/.ssh/authorized_keys

# Add custom functions to bashrc
echo "source ~/.dotfiles/termux/Omega.sh" > ~/.bashrc

# VIM
if ! [ -d "$HOME/.vim" ];
then
    git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
fi

cp "../global/.vimrc" "$HOME"/
vim +PluginInstall +qall

# Generate and add ssh key to ssh-agent
if ! [ -f "$HOME/.ssh/id_rsa" ];
then
    ssh-keygen -f $HOME/.ssh/id_rsa -t rsa -b 4096 -C "joshua.gf.arul@gmail.com" -N ''
    eval "$(ssh-agent -s)"
    ssh-add ~/.ssh/id_rsa
fi

echo "Done"

