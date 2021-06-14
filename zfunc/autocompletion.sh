#!/usr/bin/env zsh

_clone() {
  _arguments \
    '--help[Show help]' \
    '--dry-run[Do not actually clone, just echo the remote url of the repo]' \
    '--http[Use HTTP instead of SSH]' \
    '--version[Show version]' \

  _describe 'command' "($(dotEngine -z))"
}
compdef _clone clone
