#!/usr/bin/env bash

# Install all dependencies for dev environment
# Only supports Arch-based distros.

set -e

MAIN_DEPS=(
	clang
	doas
	docker
	docker-compose
	emacs-nativecomp
	gcc
	go
	jre-openjdk-headless
	nodejs
	npm
	pgformatter
	rust
	rust-analyzer
	texlive-bin
	texlive-core
	texlive-science
	vim
)

AUR_DEPS=(
	google-chrome
	onedrive-abraunegg
	slack-desktop
	spotify
	texlive-latexindent-meta
	visual-studio-code-bin
)

is_command() {
	# Checks to see if the given command (passed as a string argument) exists on the system.
	# Returns 0 (success) if the command exists, and 1 if it doesn't.
	local check_command="$1"
	command -v "${check_command}" >/dev/null 2>&1
}

if ! is_command pacman; then
	printf "OS distribution not supported\n"
	exit 1
fi

if ! is_command yay; then
	printf "Please install yay first: https://aur.archlinux.org/packages/yay\n"
	exit 1
fi

# Install all mainline packages
for package in "${MAIN_DEPS[@]}"; do
	sudo pacman -S --noconfirm --needed "$package"
done

# Install all AUR packages
for package in "${AUR_DEPS[@]}"; do
	yay -S --noconfirm --needed "$package"
done

# Install all the vim goodness (sweet, sweet coconut oil).
YCM_DIR="${HOME}/.vim/plugged/YouCompleteMe"
if [ ! -f "${YCM_DIR}/installed" ]; then
	vim -c ":PlugInstall | :qa"
	"${YCM_DIR}/install.py" --all --verbose
	touch "${HOME}/.vim/plugged/YouCompleteMe/installed"
	printf "Installed Vim plugins and YCM\n"
fi

# Install doom emacs (Evil >:D)
if [ ! -f "$HOME/.emacs.d/bin/doom" ]; then
	rm -rf "$HOME/.emacs.d"
	git clone --depth 1 https://github.com/hlissner/doom-emacs "$HOME/.emacs.d"
	"$HOME/.emacs.d/bin/doom" install --no-config --env --install --fonts --force
	printf "Installed Doom Emacs"
fi

sudo usermod -aG docker "$USER"
printf "Added %s to group 'docker'\nLogout and log back in for this to take affect.\n" "$USER"

# Gnome specific
if is_command gsettings; then
	favorite_apps="['firefox.desktop', 'org.gnome.Nautilus.desktop', 'kitty.desktop', 'spotify.desktop', 'emacs.desktop', 'vim.desktop', 'visual-studio-code.desktop', 'discord.desktop', 'slack.desktop']"
	gsettings set org.gnome.shell favorite-apps "$favorite_apps"
	dconf load / <"$HOME/.dotfiles/dconf/custom-shortcuts.ini"
	printf "Loaded custom Gnome settings\n"
fi

# Doas config
DOAS_CONFIG="/etc/doas.conf"
if [ ! -f "$DOAS_CONFIG" ]; then
	printf "permit :wheel\npermit nopass %s as root\n" "$USER" | /bin/sudo tee "$DOAS_CONFIG"
fi
