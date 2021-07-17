#!/usr/bin/env bash

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
