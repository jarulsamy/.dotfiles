#!/bin/bash

dotfilesDir=$(pwd)

function linkDotfile() {
  dest="${HOME}/${1}"
  dateStr=$(date +%Y-%m-%d-%H%M)

  if [ -h ~/${1} ]; then
    # Existing symlink
    echo "Removing existing symlink: ${dest}"
    rm ${dest}

  elif [ -f "${dest}" ]; then
    # Existing file
    echo "Backing up existing file: ${dest}"
    mv ${dest}{,.${dateStr}}

  elif [ -d "${dest}" ]; then
    # Existing dir
    echo "Backing up existing dir: ${dest}"
    mv ${dest}{,.${dateStr}}
  fi

  echo "Creating new symlink: ${dest}"
  ln -s ${dotfilesDir}/${1} ${dest}
}

linkDotfile .vim
linkDotfile .vimrc
linkDotfile .zshrc
linkDotfile .gitconfig
linkDotfile .tmux.conf
linkDotfile .tmux.conf.local

# Copy MOTD
sudo rm /etc/update-motd.d/*
sudo cp motd/motd.asc /etc/update-motd.d
sudo cp motd/warning.asc /etc/update-motd.d
sudo cp motd/01-motd-warning /etc/update-motd.d
# Remove default MOTD
sudo truncate -s 0 /etc/motd

mkdir -p $dotfilesDir/.vim/bundle
cd $dotfilesDir/.vim/bundle
git clone git://github.com/VundleVim/Vundle.vim.git
vim +PluginInstall +qall
