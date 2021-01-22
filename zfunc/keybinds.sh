#!/usr/bin/env bash

# ctrl-s adds sudo to start
add_sudo() {
  sudo_present="$(echo "$BUFFER" | cut -d " " -f1)"
  if [[ $sudo_present = "sudo" ]]; then
    BUFFER="$(echo "$BUFFER" | cut -d " " -f2-)"
  else
    BUFFER="sudo $BUFFER"
  fi
  zle end-of-line
}
zle -N add_sudo
bindkey "^s" add_sudo

# ctrl-k moves up one directory
up() {
  BUFFER="cd .."
  zle accept-line
}
zle -N up
bindkey "^k" up

# ctrl-q kills all other tmux sessions
kill_other() {
  tmux kill-session -a
  tmux rename-session 0
}
zle -N kill_other
bindkey "^q" kill_other
