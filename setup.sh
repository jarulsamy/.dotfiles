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

# Ensure config folders exists
mkdir -p "$HOME/.config" "$HOME/.doom.d"

linkDotfile .vimrc
linkDotfile .zshrc
linkDotfile .bashrc
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
linkDotfile .config/redshift
linkDotfile .config/dunst

# Emacs
linkDotfile .doom.d/config.el
linkDotfile .doom.d/custom.el
linkDotfile .doom.d/init.el
linkDotfile .doom.d/packages.el

# For some reason, git doesn't support symlink for work
cp .gitconfig-work ~/.gitconfig-work

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

# Install doom emacs
if [ ! -f "$HOME/.emacs.d/bin/doom" ]; then
  rm -rf "$HOME/.emacs.d"
  git clone --depth 1 https://github.com/hlissner/doom-emacs "$HOME/.emacs.d"
  "$HOME/.emacs.d/bin/doom" install
fi

# Grab authorized_keys
source "$HOME/.dotfiles/zfunc/aliases.sh"
authorize_github_keys
