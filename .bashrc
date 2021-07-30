# .bashrc

# Source global definitions
# if [ -f /etc/bashrc ]; then
#     . /etc/bashrc
# fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# Virtualenv
export WORKON_HOME="$HOME/.virtualenvs"
export PROJECT_HOME="$HOME/repos"
export VIRTUALENVWRAPPER_SCRIPT="/usr/local/bin/virtualenvwrapper.sh"
[[ ! -f "$VIRTUALENVWRAPPER_SCRIPT" ]] || source "$VIRTUALENVWRAPPER_SCRIPT"

VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3

alias project="cd /project/mallet/jarulsam; module load gcc; module load swset; module load boost/1.72.0; module load cmake/3.16.5; module load miniconda3; source activate /project/mallet/jarulsam/job_py"
alias ls="ls --color=tty"

# Ensure dir exists before sourcing.
if [ -d "$HOME/.dotfiles" ]; then
    # Load aliases
    source "$HOME/.dotfiles/zfunc/aliases.sh"
fi

# Editor
export EDITOR=vim

# Disable the bell
bind "set bell-style none"

# Causes bash to append to history instead of overwriting it so if you start a new terminal, you have old session history
shopt -s histappend
PROMPT_COMMAND='history -a'

# Scroll through auto-completion and ignore case
bind "TAB:menu-complete"
bind "set show-all-if-ambiguous on"
bind "set completion-ignore-case on"

# Colors
export CLICOLOR=1
export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=01;37;41:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=01;36:*.au=01;36:*.flac=01;36:*.m4a=01;36:*.mid=01;36:*.midi=01;36:*.mka=01;36:*.mp3=01;36:*.mpc=01;36:*.ogg=01;36:*.ra=01;36:*.wav=01;36:*.oga=01;36:*.opus=01;36:*.spx=01;36:*.xspf=01;36:'

# Shell prompt:
# USER@HOST CWD
# $
PS1='\[\033[1;32m\]\u@\h \[\033[1;34m\]\w\[\033[0;37m\]\n$\[\033[0m\] '

# ctrl-k goes up 1 dir
bind '"\C-k":"cd ..\n"'
