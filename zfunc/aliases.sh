#!/usr/bin/env bash

# Try to keep this file as shell-agnostic as possible (at least support bash and zsh)

# For a full list of active aliases, run `alias`.
alias zshconfig="vim $HOME/.zshrc"
alias ohmyzsh="vim $HOME/.oh-my-zsh"
alias vimconfig="vim $HOME/.vimrc"
alias i3config="vim $HOME/.config/i3/config"
alias polyconfig="vim $HOME/.config/polybar/"

# Aliases for quick adding to clipboard.
alias setclip="xclip -selection c"
alias getclip="xclip -selection c -o"

# Pretty docker commands
alias dcls="docker container ls --format 'table {{.Names}}\t{{.ID}}\t{{.Image}}\t{{.Ports}}\t{{.Status}}'"
alias dcdump="docker container ls --format='{{json .}}' | jq"

# Activate or reconnect to a tmux ssh session
alias tmux-ssh="exec tmux new-session -A -s tmux-ssh"

# vi for vim
alias vi="vim"

# Reload zsh
alias zshreload="source $HOME/.zshrc"

# Dump gnome keybinds to file
alias gnome-keybinds-export="dconf dump / | sed -n '/\[org.gnome.settings-daemon.plugins.media-keys/,/^$/p' >$HOME/.dotfiles/dconf/custom-shortcuts.ini"
alias gnome-keybinds-import="dconf load / < $HOME/.dotfiles/dconf/custom-shortcuts.ini"

# Boot into windows
alias reboot-win="sudo grub2-reboot 'Windows Boot Manager'; sudo reboot"

# Valgrind
alias memcheck="valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes --verbose"
alias profile="valgrind --tool=callgrind --dump-instr=yes --collect-jumps=yes"

# cd into last directory alphanumerically in CWD
# https://unix.stackexchange.com/a/257880/484896
cdl() {
  set ./*/
  shift "$(($# - 1))"
  cd "$1"
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
