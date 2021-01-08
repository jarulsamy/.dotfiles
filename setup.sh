#!/usr/bin/env bash

dotfilesDir=$(pwd)

linkDotfile() {
  dest="${HOME}/${1}"
  dateStr=$(date +%Y-%m-%d-%H%M)

  if [ -h "${HOME}/${1}" ]; then
    # Existing symlink
    echo "Removing existing symlink: ${dest}"
    rm "${dest}"

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
  ln -s "${dotfilesDir}/${1}" "${dest}"
}

authorize_github_keys() {
  # Grab username from config.ini
  source <(grep username .gitconfig | sed 's/ *= */=/g')

  if [ -z ${username+x} ]; then
    echo "Github username unset, skipping..."
    return
  fi

  # URL to keys
  URL="https://github.com/$username.keys"
  # Save keys to authorized_keys

  mkdir -p ~/.ssh

  curl "$URL" -o "$HOME/.ssh/authorized_keys" 2>/dev/null 1>/dev/null
  # Ensure permissions are correct
  chmod 700 ~/.ssh
  chmod 600 ~/.ssh/authorized_keys

  echo "Grabbed authorized_keys from $URL"
}

# Ensure config folders exists
mkdir -p "$HOME/.config"

linkDotfile .vim
linkDotfile .vimrc
linkDotfile .zshrc
linkDotfile .gitconfig
linkDotfile .tmux.conf
linkDotfile .tmux.conf.local
linkDotfile .p10k.zsh
linkDotfile .alacritty.yml
linkDotfile .flake8
linkDotfile .clang-format

linkDotfile .config/i3
linkDotfile .config/polybar
linkDotfile .config/ranger
linkDotfile .config/mpd
linkDotfile .config/ncmpcpp

# Create mpd required playlist folder
mkdir -p "$HOME/.config/mpd/playlists"

# Delete .mpd folder
rm -rf "$HOME/.mpd"

# Install zsh theme
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git "$HOME/.oh-my-zsh/custom/themes/powerlevel10k" 2>/dev/null
# Install autosuggestions plugin
git clone https://github.com/zsh-users/zsh-autosuggestions "$HOME/.oh-my-zsh/custom/plugins/zsh-autosuggestions" 2>/dev/null

# Debian based distro specific motd.
if [ -f "$(grep -Ei 'debian|buntu|mint' /etc/*release)" ]; then
  sudo rm /etc/update-motd.d/* 2>/dev/null
  sudo cp motd/motd.asc /etc/update-motd.d 2>/dev/null
  sudo cp motd/warning.asc /etc/update-motd.d 2>/dev/null
  sudo cp motd/01-motd-warning /etc/update-motd.d 2>/dev/null
  # Remove default MOTD
  sudo truncate -s 0 /etc/motd 2>/dev/null
fi

# Grab authorized_keys
authorize_github_keys

# Install Vundle
mkdir -p "$dotfilesDir/.vim/bundle"
cd "$dotfilesDir/.vim/bundle" || exit
git clone git://github.com/VundleVim/Vundle.vim.git 2>/dev/null
vim +PluginInstall +qall
