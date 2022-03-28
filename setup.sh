#!/usr/bin/env bash

dotfilesDir=$(pwd)

linkDot() {
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
mkdir -p "$HOME/.config"

linkDot .alacritty.yml
linkDot .bashrc
linkDot .clang-format
linkDot .flake8
linkDot .gitconfig
linkDot .mbsyncrc
linkDot .p10k.zsh
linkDot .tmux.conf
linkDot .tmux.conf.local
linkDot .virc
linkDot .vimrc
linkDot .zshrc

linkDot .config/dunst
linkDot .config/i3
linkDot .config/kitty
linkDot .config/mpd
linkDot .config/mypy
linkDot .config/ncmpcpp
linkDot .config/polybar
linkDot .config/ranger
linkDot .config/redshift
linkDot .config/yay

# Emacs
linkDot .doom.d

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
