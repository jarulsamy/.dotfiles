#!/usr/bin/env bash

# Echo to stderr
echoerr() { echo "$@" 1>&2; }

# Display an error message and quit
error() {
  echoerr "Error, $1"
  exit 1
}

# Pick a package manager.
declare -A osInfo
osInfo["/etc/debian_version"]="apt-get install -y"
osInfo["/etc/alpine-release"]="apk --update add"
osInfo["/etc/centos-release"]="yum install -y"
osInfo["/etc/fedora-release"]="dnf install -y"
osInfo["/etc/arch-release"]="pacman -S --noconfirm --needed"

for f in ${!osInfo[@]}; do
  if [[ -f $f ]]; then
    pac="${osInfo[$f]}"
  fi
done

# Install a package
install() {
  which "$1" &>/dev/null

  if [ $? -ne 0 ]; then
    echoerr "Installing: ${1}..."
    sudo ${pac} ${1}
  else
    echoerr "Already installed: ${1}"
  fi
}

# Install dependencies
install vim
install fzf
install ripgrep
install tmux
install wget
install curl
install zsh
install lolcat
install xclip
install xdg-utils
install shfmt
install shellcheck
install cmake
install make
install gcc

# Install oh-my-zsh, if not already present
if [ ! -d "$HOME/.oh-my-zsh" ]; then
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
else
  echoerr "Skipping oh-my-zsh install, $HOME/.oh-my-zsh already exists."
fi

# Compile and install dotEngine, if it doesn't exist
if [ ! -f "$HOME/.local/bin/dotEngine" ]; then
  # Prep, clone and setup build dir
  compile_dir="$(mktemp -d)"
  build_dir="$compile_dir/build"
  install_prefix="$HOME/.local"

  cd "$compile_dir" || error "Couldn't install dotEngine"
  git clone https://github.com/jarulsamy/dotEngine "$compile_dir"
  mkdir -p "$build_dir" "$install_prefix"
  cd "$build_dir" || error "Couldn't install dotEngine"

  # Actual compilation and installation
  cmake .. -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX="$install_prefix"
  make
  make install

  # Fix permission of resulting binary
  chmod +x "$install_prefix"/bin/dotEngine
  # Add compiled libraries to search path
  sudo ldconfig "$install_prefix"/lib

  # Cleanup
  rm -rf "$compile_dir"
else
  echoerr "Skipping dotEngine install, $HOME/.local/bin/dotEngine already exists."
fi

echo "DONE"
