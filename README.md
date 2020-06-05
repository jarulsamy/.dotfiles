# Dotfiles

> User-specific application configuration is traditionally stored in so called dotfiles (files whose filename starts with a dot). It is common practice to track dotfiles with a version control system such as Git to keep track of changes and synchronize dotfiles across various hosts. There are various approaches to managing your dotfiles (e.g. directly tracking dotfiles in the home directory v.s. storing them in a subdirectory and symlinking/copying/generating files with a shell script or a dedicated tool).

\- Arch Wiki

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/f00ab44f952048b983da8904ef775232)](https://app.codacy.com/manual/jarulsamy/.dotfiles?utm_source=github.com&utm_medium=referral&utm_content=jarulsamy/.dotfiles&utm_campaign=Badge_Grade_Dashboard)
[![Open Source Love](https://badges.frapsoft.com/os/v2/open-source.png?v=103)](https://github.com/ellerbrock/open-source-badges/)
[![GPL Licence](https://badges.frapsoft.com/os/gpl/gpl.svg?v=103)](https://opensource.org/licenses/GPL-3.0/)

This is a series of scripts and configurations pertaining to my environment.

## Editor

Main Development - VSCode.

I use VSCode with a series of plugins. Using this [settings sync plugin](https://marketplace.visualstudio.com/items?itemName=Shan.code-settings-sync), all my configuration are stored on this [gist](https://gist.github.com/jarulsamy/6c3ff1d6f599d703cf0fba2b050fedec).

Headless - Vim

My vim setup is optimized for Python, C/C++, and general unix configuration files.

This is accomplished with the following plugins:

-   [IndentPython](https://github.com/vim-scripts/indentpython.vim)
-   [Syntastic](https://github.com/vim-syntastic/syntastic)
-   [Vim-airline](https://github.com/vim-airline/vim-airline) / [Vim-airline-themes](https://github.com/vim-airline/vim-airline-themes)
-   [Nerdtree](https://github.com/preservim/nerdtree)
-   [Nerdtree-git](https://github.com/Xuyuanp/nerdtree-git-plugin)
-   [Jedi](https://github.com/davidhalter/jedi-vim)
-   [Black](https://github.com/psf/black)
-   [Gruvbox](https://github.com/morhetz/gruvbox)

All, of course, detailed in the [vimrc](/.vimrc).

![vim](assets/vim.png)

## Shell

ZSH + Tmux = :heart:

I use a heavily customized version of ZSH and Tmux (using oh-my-zsh and oh-my-tmux) to create an optimal terminal experience.

![Terminal](/assets/neofetch.png)

Tmux header features include, session counter, interactive weather tracker, battery tracker, date/time, user, root warning, and hostname.

## Custom ZSH Functions

I am actively developing a handful of convience scripts that I use daily.

[clone]("zfunc/clone") - Shortens github clone commands.

> For example, `git clone git@github.com:jarulsamy/example` becomes `clone example`

[gh-ssh]("zfunc/gh-ssh") - Automatically generates and adds a SSH key to the SSH agent and copies to clipboard. Helpful for setting up new systems.

[reddit]("zfunc/reddit) - Auto create my daily driver conda environment with commonly used tools.

> Essentially, creates python 3.8 conda environment named `reddit` with various autoformatters preinstalled.

## Custom MOTD

By default, `setup.sh` should also install a custom MOTD.

The text can be customized by editing the files in [motd](/motd).

![MOTD](assets/motd.png)

## Setup

1.  Clone this repo to your home directory.

2.  Install all the required dependencies with:

        ./install.sh

    > Hopefully distro agnostic :)

3.  Symbolic link all the dotfiles using:

        ./setup.sh

    > All the vim plugins should automatically be installed with Vundle.
    > Note: this symbolic links ALL relevant files in this repository. Ensure you edit `.gitconfig` to change your identity.

4.  Install a powerline compatible font. I usually use [Cascadia Code PL](https://github.com/microsoft/cascadia-code) or [Ubuntu Mono](https://design.ubuntu.com/font).
