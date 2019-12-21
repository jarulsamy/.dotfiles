#!/bin/bash

# GENERAL
termux-setup-storage

# Update and install frequently used apps.
apt update
apt install -y git termux-api termux-widget vim ssh wget curl
# Append public key to authorized keys.
cat id_rsa.pub > ~/.ssh/authorized_keys
# Add custom functions to bashrc
echo "source ~/.dotfiles/termux/Omega.sh" >> ~/.bashrc

# VIM
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
cp ".vimrc" "$HOME"/
vim +PluginInstall +qall

echo "Done"
