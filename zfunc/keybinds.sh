#!/usr/bin/env bash

# ctrl-s adds sudo to start
add_sudo() {
    BUFFER="sudo $BUFFER"
    zle end-of-line
}
zle -N add_sudo
bindkey "^s" add_sudo

# Move up one directory with ctrl+k
up() {
    BUFFER="cd .."
    zle accept-line
}
zle -N up
bindkey "^k" up

# Kill all other termux sessions with ctrl+q
kill_other() {
    tmux kill-session -a
    tmux rename-session 0
}
zle -N kill_other
bindkey "^q" kill_other
