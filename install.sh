#!/bin/bash

# Pick a package manager.
declare -A osInfo;
osInfo[/etc/debian_version]="apt-get install -y"
osInfo[/etc/alpine-release]="apk --update add"
osInfo[/etc/centos-release]="yum install -y"
osInfo[/etc/fedora-release]="dnf install -y"
osInfo[/etc/arch-release]="pacman -S --noconfirm"

for f in ${!osInfo[@]}
do
    if [[ -f $f ]];then
        package_manager=${osInfo[$f]}
    fi
done

function install() {
  which "$1" &>/dev/null

  if [ $? -ne 0 ]; then
    echo "Installing: ${1}..."
    sudo "${package_manager}" "${1}"
  else
    echo "Already installed: ${1}"
  fi
}

# Install dependencies
install vim
install tmux
install wget
install curl
install zsh
install lolcat
install xclip
install xdg-utils

# Install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

