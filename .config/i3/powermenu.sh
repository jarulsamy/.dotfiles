#!/usr/bin/env bash

# options to be displayed
option0="Lock"
option1="Logout"
option2="Suspend"
option3="Scheduled Suspend (10min)"
option4="scheduled Suspend (20min)"
option5="Scheduled Suspend (30min)"
option6="Reboot"
option7="Poweroff"

# options passed into variable
options="$option0\n$option1\n$option2\n$option3\n$option4\n$option5\n$option6\n$option7"

chosen="$(echo -e "$options" | rofi -lines 8 -dmenu -p "Power" -i)"
case $chosen in
    $option0)
        i3lock -c 000000;;
    $option1)
        i3-msg exit;;
    $option2)
        systemctl suspend;;
	$option3)
		sleep 600 && systemctl suspend;;
	$option4)
		sleep 1200 && systemctl suspend;;
	$option5)
		sleep 1800 && systemctl suspend;;
    $option6)
        reboot;;
	$option7)
        poweroff;;
esac

