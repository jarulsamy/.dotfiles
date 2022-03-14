# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=31

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="false"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    colored-man-pages
    docker
    docker-compose
    fzf
    git
    zsh-autosuggestions
    zsh-syntax-highlighting
)

source "$ZSH/oh-my-zsh.sh"

# User configuration
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
export EDITOR="vim"
export VISUAL="$EDITOR"
export SUDO_EDITOR="$EDITOR"
export GIT_EDITOR="vim -c'startinsert|norm! ggA'"

# Compilation flags
export NUM_THREADS="$(nproc)"
export ARCHFLAGS="-march=native"
export MAKEOPTS="-j${NUM_THREADS}"
export MAKEFLAGS="$MAKEOPTS"

# Number of threads for zstd to use by default
export ZSTD_NBTHREADS="${NUM_THREADS}"

# Use clang if possible
if
    type clang &>/dev/null
then
    export CC=clang
    export CXX=clang++
fi

# Add "$HOME/.../bin" directories to PATH.
export PATH="$HOME/.local/bin:\
$HOME/.local/bin:\
$HOME/.dotfiles/zfunc:\
$HOME/.emacs.d/bin:\
$HOME/.cargo/bin:\
$PATH"

# Ensure dir exists before sourcing.
if [ -d "$HOME/.dotfiles" ]; then
    # Keybinds
    source "$HOME/.dotfiles/zfunc/keybinds.sh"
    # Aliases
    source "$HOME/.dotfiles/zfunc/aliases.sh"
    # Autocompletion
    source "$HOME/.dotfiles/zfunc/autocompletion.sh"
fi

# Auto ls on cd
chpwd() ls

# Disable 'auto cd'
unsetopt AUTO_CD

# FZF
if
    type rg &>/dev/null
then
    export FZF_DEFAULT_COMMAND="rg --files"
fi
export FZF_DEFAULT_OPTS="-m --height 50% --border"

# Virtualenv
export WORKON_HOME="$HOME/.virtualenvs"
export PROJECT_HOME="$HOME/repos"
export VIRTUALENVWRAPPER_PYTHON="$(command -v python3)"
export VIRTUALENVWRAPPER_HOOK_DIR="$HOME/.dotfiles/virtualenv_hooks"
export VIRTUALENVWRAPPER_SCRIPT="$HOME/.local/bin/virtualenvwrapper.sh"
if [[ -f "$VIRTUALENVWRAPPER_SCRIPT" ]] then;
    source "$VIRTUALENVWRAPPER_SCRIPT"
    # Auto activate base virtualenv
    envs=$(workon)
    if [[ $envs == *"base"* ]]; then
        workon base
    fi
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f "$HOME/.p10k.zsh" ]] || source "$HOME/.p10k.zsh"
