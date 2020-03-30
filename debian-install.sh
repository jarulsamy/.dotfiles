#!/bin/bash

function install {
  which $1 &> /dev/null

  if [ $? -ne 0 ]; then
    echo "Installing: ${1}..."
    sudo apt -y install $1
  else
    echo "Already installed: ${1}"
  fi
}

install vim-nox
install tmux
install wget
install curl
install zsh
install lolcat

sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
