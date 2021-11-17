#!/usr/bin/env bash

# Echo to stderr
echoerr() { echo "$@" 1>&2; }

# Display an error message and quit
error() {
  echoerr "Error, $1"
  exit 1
}

is_command() {
  # Checks to see if the given command (passed as a string argument) exists on the system.
  # Returns 0 (success) if the command exists, and 1 if it doesn't.
  local check_command="$1"

  command -v "${check_command}" >/dev/null 2>&1
}

# Check distro and determine what packages to install, then install.
install_packages() {
  # If apt-get is installed, then we know it's part of the Debian family
  if is_command apt-get; then
    PKG_MANAGER="apt-get"
    PKG_INSTALL=("${PKG_MANAGER}" -qq --no-install-recommends install)

    INSTALLER_DEPS=(cmake curl gcc git libcurl4-openssl-dev make zsh jq)
    DEPS=(emacs fzf g++ lolcat ripgrep shellcheck shfmt tmux vim wget xclip xdg-utils)

    # If apt-get is not found, check for rpm to see if it's a Red Hat family OS
  elif is_command rpm; then
    # Then check if dnf or yum is the package manager
    if is_command dnf; then
      PKG_MANAGER="dnf"
    else
      PKG_MANAGER="yum"
    fi

    # These variable names match the ones in the Debian family. See above for an explanation of what they are for.
    PKG_INSTALL=("${PKG_MANAGER}" install -y)
    INSTALLER_DEPS=(cmake curl gcc git libcurl libcurl-devel make zsh)
    DEPS=(ShellCheck fzf g++ lolcat ripgrep shfmt tmux vim wget xclip xdg-utils)

    # If the host OS is Fedora,
    if grep -qiE 'fedora|fedberry' /etc/redhat-release; then
      # all required packages should be available by default with the latest fedora release
      : # continue
      # or if host OS is CentOS,
    elif grep -qiE 'centos|scientific' /etc/redhat-release; then
      # Pi-Hole currently supports CentOS 7+ with PHP7+
      SUPPORTED_CENTOS_VERSION=7
      # Check current CentOS major release version
      CURRENT_CENTOS_VERSION=$(grep -oP '(?<= )[0-9]+(?=\.?)' /etc/redhat-release)
      # Check if CentOS version is supported
      if [[ $CURRENT_CENTOS_VERSION -lt $SUPPORTED_CENTOS_VERSION ]]; then
        printf "  %b CentOS %s is not supported.\\n" "${CROSS}" "${CURRENT_CENTOS_VERSION}"
        printf "      Please update to CentOS release %s or later.\\n" "${SUPPORTED_CENTOS_VERSION}"
        # exit the installer
        exit
      fi
      # CentOS requires the EPEL repository to gain access to Fedora packages
      EPEL_PKG="epel-release"
      rpm -q ${EPEL_PKG} &>/dev/null || rc=$?
      if [[ $rc -ne 0 ]]; then
        printf "  %b Enabling EPEL package repository (https://fedoraproject.org/wiki/EPEL)\\n" "${INFO}"
        "${PKG_INSTALL[@]}" ${EPEL_PKG} &>/dev/null
        printf "  %b Installed %s\\n" "${TICK}" "${EPEL_PKG}"
      fi
    fi

  elif is_command pacman; then
    PKG_MANAGER="pacman"
    PKG_INSTALL=("${PKG_MANAGER}" -S --noconfirm)

    INSTALLER_DEPS=(cmake curl gcc git make zsh)
    DEPS=(fzf lolcat ripgrep shellcheck shfmt tmux vim wget xclip xdg-utils)

  # If not apt-get, yum/dnf, or pacman, not supported.
  else
    # it's not an OS we can support,
    printf "  %b OS distribution not supported\\n" "${CROSS}"
    # so exit the installer
    exit
  fi

  # We've determined that we have a valid package manager, and set the necessary packages for this specific distro.
  # Go ahead and install everything.
  for package in "${INSTALLER_DEPS[@]}"; do
    if ! is_command "${package}"; then
      sudo "${PKG_INSTALL[@]}" "${package}"
    fi
  done

  for package in "${DEPS[@]}"; do
    if ! is_command "${package}"; then
      sudo "${PKG_INSTALL[@]}" "${package}"
    fi
  done
}

install_dotEngine() {
  local api_endpoint
  local dest_dir
  local download_link

  api_endpoint="https://api.github.com/repos/jarulsamy/dotEngine/releases/latest"
  dest_dir="$HOME/.local/bin"

  download_link=$(curl --silent "$api_endpoint" | jq -r ".assets[0].browser_download_url")
  curl -L "$download_link" -o "$dest_dir"/dotEngine

  chmod +x "$dest_dir"/dotEngine
}

install_ohmyzsh() {
  # Install oh-my-zsh, if not already present
  if [ ! -d "$HOME/.oh-my-zsh" ]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" --unattended
  else
    echoerr "Skipping oh-my-zsh install, $HOME/.oh-my-zsh already exists."
  fi
}

CWD=$(pwd)

install_packages
install_dotEngine
install_ohmyzsh

cd "$CWD" || return
