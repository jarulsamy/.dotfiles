#!/usr/bin/env sh

## Add this to your wm startup file.

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bar1 and bar2
# polybar -c ~/.config/polybar/config.ini main >> /dev/null 2>&1 &

CONFIG="$HOME/.config/polybar/config.ini"
if type "xrandr"; then
    for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
        MONITOR=$m polybar --reload -c "$CONFIG" main >> /dev/null 2>&1 &
    done
else
    polybar --reload -c "$CONFIG" main >> /dev/null 2>&1 &
fi
