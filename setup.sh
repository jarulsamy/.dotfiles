#!/bin/bash

dotfilesDir=$(pwd)

function linkDotfile() {
  dest="${HOME}/${1}"
  dateStr=$(date +%Y-%m-%d-%H%M)

  if [ -h "${HOME}/${1}" ]; then
    # Existing symlink
    echo "Removing existing symlink: ${dest}"
    rm "${dest}"

  elif [ -f "${dest}" ]; then
    # Existing file
    echo "Backing up existing file: ${dest}"
    mv "${dest}{,.${dateStr}}"

  elif [ -d "${dest}" ]; then
    # Existing dir
    echo "Backing up existing dir: ${dest}"
    mv "${dest}{,.${dateStr}}"
  fi

  echo "Creating new symlink: ${dest}"
  ln -s "${dotfilesDir}/${1}" "${dest}"
}

function authorize_github_keys() {
  # Grab username from config.ini
  source <(grep github_username "$HOME/.dotfiles/config.ini" | sed 's/ *= */=/g')

  if [ -z ${github_username+x} ]; then
    echo "github_username unset, skipping..."
    return
  fi

  # URL to keys
  URL="https://github.com/$github_username.keys"
  # Save keys to authorized_keys
  curl "$URL" -o "$HOME/.ssh/authorized_keys" 2>/dev/null 1>/dev/null
  # Ensure permissions are correct
  chmod 700 ~/.ssh
  chmod 600 ~/.ssh/authorized_keys

  echo "Grabbed authorized_keys from $URL"
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

mkdir -p "$dotfilesDir/.vim/bundle"
cd "$dotfilesDir/.vim/bundle"
git clone git://github.com/VundleVim/Vundle.vim.git 2>/dev/null
vim +PluginInstall +qall

# Set black to stable branch
cd "$HOME/.vim/bundle/black"
git checkout origin/stable -b stable 2>/dev/null

# Grab authorized_keys
authorize_github_keys
