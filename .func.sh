#!/usr/bin/env bash

function clone ()
{
    while test $# -gt 0; do
        case "$1" in
            -h|--help)
                echo "$package - Simplifies the github experience."
                echo " "
                echo "Options:"
                echo "-h, --help        show brief help"
                echo "-i                https instead of ssh"
                break
                ;;
            -i)
                shift
                if test $# -gt 0; then
                    git clone "https://github.com/jarulsamy/$1"
                else
                    git clone "git@github.com:jarulsamy/$1"
                fi
                shift
                ;;
            *)
                break
                ;;
        esac
    done
}

