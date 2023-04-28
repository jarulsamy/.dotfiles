#!/usr/bin/env bash

# Try to keep this file as shell-agnostic as possible (at least support bash and zsh)

# For a full list of active aliases, run `alias`.
alias zshconfig="vim $HOME/.zshrc"
alias ohmyzsh="vim $HOME/.oh-my-zsh"
alias vimconfig="vim $HOME/.vimrc"
alias i3config="vim $HOME/.config/i3/config"
alias polyconfig="vim $HOME/.config/polybar/"

# Aliases for quick adding to clipboard.
if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
	alias setclip="wl-copy -n"
	alias getclip="wl-paste -n"
else
	alias setclip="xclip -selection c"
	alias getclip="xclip -selection c -o"
fi

# Pretty docker commands
alias dcls="docker container ls --format 'table {{.Names}}\t{{.ID}}\t{{.Image}}\t{{.Ports}}\t{{.Status}}'"
alias dcdump="docker container ls --format='{{json .}}' | jq"

# Activate or reconnect to a tmux ssh session
alias tmux-ssh="exec tmux new-session -A -s tmux-ssh"

# vi for vim
alias vi="vim"

# Reload zsh
alias zshreload="source $HOME/.zshrc"

alias to_pdf="libreoffice --headless --convert-to pdf"
alias ports="ss -Olt4"

# Dump gnome keybinds to file
gnome-keybinds-export() {
	dconf dump / | sed -n '/\[org.gnome.desktop.wm.keybindings/,/^$/p' >"$HOME/.dotfiles/dconf/custom-shortcuts.ini"
	dconf dump / | sed -n '/\[org.gnome.settings-daemon.plugins.media-keys/,/^$/p' >>"$HOME/.dotfiles/dconf/custom-shortcuts.ini"
	dconf dump / | sed -n '/\[org.gnome.shell.keybindings/,/^$/p' >>"$HOME/.dotfiles/dconf/custom-shortcuts.ini"
}
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
	cd "$1" || exit
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

# Use doas instead of sudo if it is available.
if
	type doas &>/dev/null
then
	alias sudo="doas"
	alias sudoedit="doas vim"
fi

alias doomclone="git clone --depth 1 https://github.com/hlissner/doom-emacs $HOME/.emacs.d"

# General purpose functions
chr() {
	printf \\$(printf '%03o' $1)
	echo
}

ord() {
	printf "%d\n" "'$1"
}

dec2hex() {
	echo "obase=16; ibase=10; $1" | bc
}

hex2dec() {
	echo "obase=10; ibase=16; $1" | bc
}

hex2chr() {
	dec="$(hex2dec "$1")"
	chr "${dec}"
}

hex2blk() {
	dec="$(hex2dec "$1")"
	# First 64-bytes:  superblock of disk, partition layout.
	# Second 64-bytes: Partition 1 bitvector
	# Third 64-bytes:  Partition 1 root directory.

	blk=$((dec - 64))
	blk=$((blk / 64))
	printf "%s\n" "$blk"
}

blk2hex() {
	blk=$(($1 * 64))
	blk=$((blk + 64))
	printf "%s\n" "$(dec2hex $blk)"
}

backup_home() {
	local backup_dst

	if (($# != 1)); then
		backup_dst="/mnt/backup"
	else
		backup_dst="$1"
	fi

	declare -r source_dir="$HOME"
	declare -r datetime="$(date '+%Y-%m-%d_%H-%M-%S')"
	declare -r hostname="$(hostnamectl hostname)"
	declare -r dst="${backup_dst}/${hostname}_${datetime}"

	rsync -ahv --progress --delete \
		"$source_dir" \
		--exclude ".android" \
		--exclude ".cache" \
		--exclude ".cargo" \
		--exclude ".config/Code" \
		--exclude ".config/Prospect Mail" \
		--exclude ".config/Slack" \
		--exclude ".config/discord" \
		--exclude ".config/google-chrome" \
		--exclude ".config/spotify" \
		--exclude ".cpan" \
		--exclude ".emacs.d" \
		--exclude ".gradle" \
		--exclude ".java" \
		--exclude ".mozilla" \
		--exclude ".mypy_cache" \
		--exclude ".npm" \
		--exclude ".texlive" \
		--exclude ".themes" \
		--exclude ".vim" \
		--exclude ".virtualenvs" \
		--exclude ".vscode" \
		--exclude ".vscode-server" \
		--exclude ".oh-my-zsh" \
		--exclude "Android" \
		--exclude "AndroidStudioProjects" \
		"${dst}"
}
