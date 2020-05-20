# Sudo
function add_sudo() {
    BUFFER="sudo $BUFFER"
    zle end-of-line
}
zle -N add_sudo
bindkey "^s" add_sudo

# Move up one directory with ctrl+k
function up() {
    BUFFER="cd .."
    zle accept-line
}
zle -N up
bindkey "^k" up
