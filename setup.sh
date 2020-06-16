#!/usr/bin/env bash

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
    mv ${dest}{,.${dateStr}}

  elif [ -d "${dest}" ]; then
    # Existing dir
    echo "Backing up existing dir: ${dest}"
    mv ${dest}{,.${dateStr}}
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
linkDotfile .p10k.zsh
linkDotfile .alacritty.yml
linkDotfile .flake8

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

mkdir -p "$dotfilesDir/.vim/bundle"
cd "$dotfilesDir/.vim/bundle"
git clone git://github.com/VundleVim/Vundle.vim.git 2>/dev/null
vim +PluginInstall +qall

# Set black to stable branch
cd "$HOME/.vim/bundle/black"
git checkout origin/stable -b stable 2>/dev/null

# Grab authorized_keys
authorize_github_keys
