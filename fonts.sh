#!/usr/bin/env bash

# Assume the fonts directory is where it should be.
fonts="$HOME/.dotfiles/fonts"
fontsDest="$HOME/.local/share/fonts"

# Ensure the fonts directory is where it should be.
if [ ! -d "$fonts" ]; then
  printf "Can't find source fonts directory. Was .dotfiles cloned to \$HOME?\n"
  exit 1
fi

# Ensure dest exists
mkdir -p "${fontsDest}"

# Copy font files over
find "${fonts}/" -mindepth 1 -maxdepth 1 -exec cp -r {} "${fontsDest}/" ';'

# Install nerdfonts if pacman is available
pacmanFonts=("ttf-fira-code" "ttf-jetbrains-mono" "ttf-cascadia-code" "ttf-nerd-fonts-symbols" "noto-fonts-emoji")
if [[ -f "/etc/arch-release" ]]; then
  for i in "${pacmanFonts[@]}"; do
    sudo pacman -S --noconfirm --needed "$i"
  done
else
  printf "You're not running an Arch based distro!\n"
  printf "Install the following fonts yourself:\n"
  for i in "${pacmanFonts[@]}"; do
    printf "  * %s\n" "$i"
  done
fi

# Reload font cache
fc-cache 2>/dev/null 1>&2 -f -v

printf "Done installing fonts!\n"
