#!/bin/bash

set -o errexit  # Exit on most errors
set -o errtrace # Make sure any error trap is inherited
set -o nounset  # Disallow expansion of unset variables
set -o pipefail # Use last non-zero exit code in a pipeline
#set -o xtrace  # Trace the execution of the script (debug)

sudo apt update
sudo apt install vim wget curl
# VIM
if ! [ -d "$HOME/.vim" ];
then
    git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
    cp "../global/.vimrc" "$HOME/"
    vim +PluginInstall +qall
fi

