#!/usr/bin/env bash
#
# Install all dependencies for dev environment
# Only supports Arch-based distros.
#
# Given the size of this, might just want to write a python
# script at some point.
#

set -e

is_command() {
	# Checks to see if the given command (passed as a string argument) exists on
	# the system. Returns 0 (success) if the command exists, and 1 if it doesn't.
	local check_command="$1"
	command -v "${check_command}" >/dev/null 2>&1
}

install_mainline_packages() {
	local MAIN_DEPS=(
		aspell-en
		clang
		discord
		doas
		docker
		docker-compose
		emacs-nativecomp
		fd
		gcc
		git-delta
		go
		hunspell-en_us
		jre-openjdk-headless
		nodejs
		npm
		pgformatter
		python-pip
		python3
		reflector
		ripgrep
		rust
		rust-analyzer
		texlive-most
		vim
		wl-clipboard
		wmctrl
	)

	local PACMAN_FONTS=(
		noto-fonts
		noto-fonts-emoji
		ttf-cascadia-code
		ttf-fantasque-sans-mono
		ttf-fira-code
		ttf-hack
		ttf-iosevka-nerd
		ttf-jetbrains-mono
		ttf-nerd-fonts-symbols-2048-em
	)

	for package in "${MAIN_DEPS[@]}"; do
		pacman -Qm "${package}" 2>/dev/null || sudo pacman -S --noconfirm --needed "${package}"
	done

	for package in "${PACMAN_FONTS[@]}"; do
		sudo pacman -S --noconfirm --needed "$package"
	done
}

install_aur_packages() {
	local AUR_DEPS=(
		google-chrome
		onedrive-abraunegg
		slack-desktop
		spotify
		texlive-latexindent-meta
		ttf-all-the-icons
		visual-studio-code-bin
	)

	local AUR_FONTS=(
		siji-ng
		ttf-icomoon-feather
		ttf-typicons
	)

	# Install all AUR packages
	for package in "${AUR_DEPS[@]}"; do
		pacman -Qm "${package}" 2>/dev/null || yay -S --noconfirm --needed "${package}"
	done

	for package in "${AUR_FONTS[@]}"; do
		pacman -Qm "${package}" || yay -S --noconfirm --needed "${package}"
	done
}

install_cargo_packages() {
	cargo install --locked --git https://github.com/latex-lsp/texlab.git
}

setup_doas() {
	# Doas config, set this before yay.
	# since yay uses doas internally.
	DOAS_CONFIG="/etc/doas.conf"
	if [ ! -f "$DOAS_CONFIG" ]; then
		printf "%s\n" "Setting doas config"
		printf "permit :wheel\npermit nopass %s as root\n" "$USER" | /bin/sudo tee "$DOAS_CONFIG"
	fi
}

setup_vim() {
	# Install all the vim goodness (sweet, sweet coconut oil).
	YCM_DIR="${HOME}/.vim/plugged/YouCompleteMe"
	if [ ! -f "${YCM_DIR}/installed" ]; then
		vim -c ":PlugInstall | :qa"
		"${YCM_DIR}/install.py" --all --verbose
		touch "${HOME}/.vim/plugged/YouCompleteMe/installed"
		printf "Installed Vim plugins and YCM\n"
	fi
}

setup_emacs() {
	# Install doom emacs (Evil >:D)
	if [ ! -f "$HOME/.emacs.d/bin/doom" ]; then
		rm -rf "$HOME/.emacs.d"
		git clone --depth 1 https://github.com/hlissner/doom-emacs "$HOME/.emacs.d"
		"$HOME/.emacs.d/bin/doom" install --no-config --env --install --fonts --force

		# Fix missing personal dictionary directory.
		ispell_dir="$HOME/.emacs.d/.local/etc/ispell"
		if [ ! -d "$ispell_dir" ]; then
			mkdir -p "$ispell_dir"
			printf "personal_ws-1.1 en 0\n" >"${ispell_dir}/.pws"
		fi

		printf "Installed Doom Emacs\n"
	else
		"$HOME/.emacs.d/bin/doom" sync
	fi
}

setup_gnome() {
	if is_command gsettings; then
		favorite_apps="['firefox.desktop', 'org.gnome.Nautilus.desktop', 'kitty.desktop', 'spotify.desktop', 'emacs.desktop', 'vim.desktop', 'visual-studio-code.desktop', 'discord.desktop', 'slack.desktop']"
		gsettings set org.gnome.shell favorite-apps "$favorite_apps"
		dconf load / <"$HOME/.dotfiles/dconf/custom-shortcuts.ini"
		printf "Loaded custom Gnome settings\n"
	fi
}

setup_fonts() {
	# Assume the fonts directory is where it should be.
	fonts="$HOME/.dotfiles/fonts"
	fontsDest="$HOME/.local/share/fonts"
	# Ensure the fonts directory is where it should be.
	if [ -d "$fonts" ]; then
		# Ensure dest exists
		mkdir -p "${fontsDest}"
		# Copy font files over
		find "${fonts}/" -mindepth 1 -maxdepth 1 -exec cp -r {} "${fontsDest}/" ';'
		fc-cache 2>/dev/null 1>&2 -f -v
	else
		printf "Can't find source fonts directory.\n"
	fi
}

setup_reflector() {
	sudo cp "$HOME/.dotfiles/reflector/reflector.conf" /etc/xdg/reflector/reflector.conf
	sudo systemctl enable --now reflector.timer
	printf "Syncing mirrors...\n"
	sudo systemctl start reflector.service
}

setup_misc() {
	# Misc
	sudo usermod -aG docker "$USER"
	printf "Added %s to group 'docker'\nLogout and log back in for this to take affect.\n" "$USER"

	# Ignore changes to yay's config file. It refuses to keep envvars for the paths.
	git update-index --skip-worktree .config/yay/config.json
}

# General validation
if ! is_command pacman; then
	printf "OS distribution not supported\n"
	exit 1
fi

if ! is_command yay; then
	printf "Please install yay first: https://aur.archlinux.org/packages/yay\n"
	exit 1
fi

# Ensure we are in the right place
EXPECTED_DIR="$HOME/.dotfiles"
if [ ! "${PWD}" == "$EXPECTED_DIR" ]; then
	printf "Ensure .dotfiles is cloned to '%s'\n" "$EXPECTED_DIR"
	printf "Also ensure your CWD is '%s'\n" "$EXPECTED_DIR"
	exit 1
fi

setup_doas

install_mainline_packages
install_aur_packages
install_cargo_packages

setup_vim
setup_emacs
setup_gnome
setup_fonts
setup_reflector
setup_misc
