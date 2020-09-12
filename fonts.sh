#!/usr/bin/env bash
fonts="$(pwd)/fonts"
fontsDest="$HOME/.local/share/fonts"

# Ensure dest exists
mkdir -p "${fontsDest}"
# Copy font files over
find "${fonts}/" -mindepth 1 -maxdepth 1 -type f -exec cp {} ${fontsDest}/ ';'
find "${fonts}/" -mindepth 1 -maxdepth 1 -type d -exec cp -r {} ${fontsDest}/ ';'

# Install nerdfonts if pacman is available
if [[ -f "/etc/arch-release" ]]; then
    sudo pacman -S --noconfirm ttf-nerd-fonts-symbols noto-fonts-emoji
fi

# Reload font cache
2>/dev/null 1>&2 fc-cache -f -v
