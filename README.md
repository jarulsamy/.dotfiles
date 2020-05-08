# Dotfiles

> User-specific application configuration is traditionally stored in so called dotfiles (files whose filename starts with a dot). It is common practice to track dotfiles with a version control system such as Git to keep track of changes and synchronize dotfiles across various hosts. There are various approaches to managing your dotfiles (e.g. directly tracking dotfiles in the home directory v.s. storing them in a subdirectory and symlinking/copying/generating files with a shell script or a dedicated tool).

\- Arch Wiki

This is a series of scripts and configurations pertaining to my environment.

## Editor

Vim Master Race!

My vim setup is optimized for Python, C/C++, and general unix configuration files.

This is accomplished with the following plugins:

-   IndentPython

-   Syntastic

-   Vim-flake8

-   Vim-airline / Vim-airline-themes

-   Vim-gitgutter

-   Vim-autopep8

-   Nerdtree

-   Nerdtree-git

## Shell

ZSH + Termux = :heart:

I use a heavily customized version of ZSH with Termux to create an optimal terminal experience.

I use oh-my-zsh and oh-my-termux.

![Terminal](/assets/terminal.png)

Termux header features include, session counter, interactive weather tracker, battery tracker, date/time, user, root warning, and hostname.

## Custom ZSH Functions

I am actively developing a handful of convience scripts that I use daily.

[clone]("/zfunc/clone") - Shortens github clone commands.

> For example, `git clone git@github.com:jarulsamy/example` becomes `clone example`

[gh-ssh]("/zfunc/gh-ssh") - Automatically generates and adds a SSH key to the SSH agent and copies to clipboard. Helpful for setting up new systems.

## Setup

1.  Clone this repo to your home directory.

2.  Install all the required dependencies with:

        ./install.sh

    > Hopefully distro agnostic :)

3.  Symbolic link all the dotfiles using:

        ./setup.sh

    > All the vim plugins should automatically be installed with Vundle.
