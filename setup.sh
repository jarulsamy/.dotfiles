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

linkDotfile .vimrc
linkDotfile .zshrc
linkDotfile .gitconfig
linkDotfile .gitconfig-work
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
linkDotfile .config/redshift
linkDotfile .config/dunst

# Create mpd required playlist folder
mkdir -p "$HOME/.config/mpd/playlists"

# Delete .mpd folder
rm -rf "$HOME/.mpd"

# Install zsh theme
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git "$HOME/.oh-my-zsh/custom/themes/powerlevel10k"
# Install autosuggestions plugin
git clone --depth=1 https://github.com/zsh-users/zsh-autosuggestions "$HOME/.oh-my-zsh/custom/plugins/zsh-autosuggestions"
# Install syntax highlighting plugin
git clone --depth=1 https://github.com/zsh-users/zsh-syntax-highlighting "$HOME/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting"

# Grab authorized_keys
authorize_github_keys
