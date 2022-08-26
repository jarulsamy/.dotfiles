#!/usr/bin/env bash
#
# Emulate rofi by using a full screen fzf window.
# Typically you would have a gnome keybind with this as the command
#   kitty --start-as fullscreen bash "/home/joshua/.dotfiles/zfunc/power.sh"

ops=("Quit" "Screen Saver" "Lock" "Logout" "Suspend" "Hibernate" "Reboot" "Poweroff" "Shutdown")
fzf_opts="--height 100% --border --margin 38% --header=Power --header-first --cycle --layout=reverse-list --no-sort"
selection="$(printf "%s\n" "${ops[@]}" | fzf ${fzf_opts})"

case "$selection" in
"Screen Saver")
	xdg-screensaver activate
	;;
"Lock")
	xdg-screensaver lock
	;;
"Logout")
	gnome-session-quit --logout --no-prompt
	;;
"Suspend")
	systemctl suspend
	;;
"Hibernate")
	systemctl hibernate
	;;
"Reboot")
	systemctl reboot
	;;
"Poweroff") ;&
"Shutdown")
	systemctl poweroff
	;;
*) ;;
esac
