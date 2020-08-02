#!/usr/bin/env bash

PDIR="$HOME/.config/polybar"
LAUNCH="polybar-msg cmd restart"

clean() {
    rm -rf "$PDIR/scripts"
    rm -rf "$PDIR/bars.ini"
    rm -rf "$PDIR/colors.ini"
    rm -rf "$PDIR/config.ini"
    rm -rf "$PDIR/modules.ini"
    rm -rf "$PDIR/tester-file-1.ini"
    rm -rf "$PDIR/tester-file-2.ini"
    rm -rf "$PDIR/tester-file-3.ini"
    rm -rf "$PDIR/user_modules.ini"
}

if [[ $1 = "-Feather" ]]; then
    # Removing Old File
    clean
    # Coping New File
    cp -r "$PDIR/source/feather/*" $PDIR
    # Restarting polybar
    $LAUNCH &

elif [[ $1 = "-Material" ]]; then
    # Removing Old File
    clean
    # Coping New File
    cp -r "$PDIR/source/material/*" $PDIR
    # Restarting polybar
    $LAUNCH &

elif [[ $1 = "-Siji" ]]; then
    # Removing Old File
    clean
    # Coping New File
    cp -r "$PDIR/source/siji/*" "$PDIR"
    # Restarting polybar
    $LAUNCH &

elif [[ $1 = "-Typicons" ]]; then
    # Removing Old File
    clean
    # Coping New File
    cp -r "$PDIR/source/typicons/*" "$PDIR"
    # Restarting polybar
    $LAUNCH &

else
    echo "Available options:
    Feather		Material		Siji
    Typicons"
fi
