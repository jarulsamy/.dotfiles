#!/usr/bin/env bash

fonts="$(pwd)/fonts"
fontsDest="$HOME/.local/share/fonts"

# Ensure dest exists
mkdir -p "${fontsDest}"

# Copy font files over
find "${fonts}/" -mindepth 1 -maxdepth 1 -type f -exec cp {} "${fontsDest}/" ';'
find "${fonts}/" -mindepth 1 -maxdepth 1 -type d -exec cp -r {} "${fontsDest}"/ ';'

# Install nerdfonts if pacman is available
if [[ -f "/etc/arch-release" ]]; then
  sudo pacman -S --noconfirm ttf-fira-code ttf-nerd-fonts-symbols noto-fonts-emoji
else
  echo "You're not running an Arch based distro!"
  echo "Install nerd fonts, noto fonts, and fira code yourself."
fi

# Reload font cache
fc-cache 2>/dev/null 1>&2 -f -v

echo "Done installing fonts!"
