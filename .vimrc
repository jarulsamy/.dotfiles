" ====================== General Config ======================

syntax on                      " Enable sytax highlighting
set backspace=indent,eol,start " Allow backspace in insert mode
set history=1000               " Store lots of cmd history
set showcmd                    " Show incomplete cmds down at the bottom
set showmode                   " Show current mode
set guicursor=a:blinkon0       " Disable cursor blink
set visualbell                 " Disable sounds
set autoread                   " Reload files changed outside of vim

set ruler                      " Show the line and column number of the cursor
set number                     " Show line number
set relativenumber             " Show relative line numbers
set cursorline                 " Highlight line cursor is on
set background=dark            " Dark background
set textwidth=0                " Stop auto line breaking on paste

set termguicolors
set updatetime=100
set clipboard=unnamedplus
set hidden

" Encoding
set encoding=utf-8
scriptencoding utf-8

" Flag bad whitespace
" au BufNewFile, BufRead *.py,*.c,*.h,*.cpp,*.hpp,*.sh,*.conf match BadWhitespace /\s\+$/

" ====================== Vundle ======================

set runtimepath+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'vim-scripts/indentpython.vim'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'morhetz/gruvbox'
Plugin 'wakatime/vim-wakatime'
Plugin 'ycm-core/YouCompleteMe'
Plugin 'tpope/vim-surround'
Plugin 'airblade/vim-gitgutter'
Plugin 'junegunn/fzf.vim'
Plugin 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plugin 'tpope/vim-commentary'
Plugin 'dense-analysis/ale'

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Older Plugins Bundles
" <Bundle HERE>

" All of Plugins must be added before the following line
call vundle#end()

" ====================== Swap Files ======================

set noswapfile
set nobackup
set nowritebackup

" ====================== Persistent Undo ======================

" Put plugins and dictionaries in this dir (also on Windows)
let vimDir = '$HOME/.vim'

" Keep undo history across sessions by storing it in a file
if has('persistent_undo')
    let myUndoDir = expand(vimDir . '/undodir')
    " Create dirs
    call system('mkdir ' . vimDir)
    call system('mkdir ' . myUndoDir)
    let &undodir = myUndoDir
    set undofile
endif

" ====================== Persistent Cursor Pos ======================

set viminfo='10,\"100,:20,%,n~/.viminfo
function! ResCur()
    if line("'\"") <= line('$')
        normal! g`"
        return 1
    endif
endfunction

augroup resCur
    au!
    au BufWinEnter * call ResCur()
augroup END

" ====================== Indentation ======================

set autoindent
set smartindent
set smarttab
set shiftwidth=4
set softtabstop=4
set tabstop=4
set expandtab

augroup FileExtIdents
    " File extension specific
    au BufNewFile,BufRead *.c,*.cpp,*.h,*.hpp,*.lisp,*.vim,*.zsh,*.sh
                \ set tabstop=2 |
                \ set softtabstop=2 |
                \ set shiftwidth=2

    " Auto remove trailing whitespace
    au BufWritePre *.* :%s/\s\+$//e
augroup END

" Auto indent pasted text
nnoremap p p=`]<C-o>
nnoremap P P=`]<C-o>

filetype plugin on
filetype indent on

" Display tabs and trailing spaces visually
set list listchars=tab:\ \ ,trail:·

set nowrap       "Don't wrap lines
set linebreak    "Wrap lines at convenient points

" ====================== Scrolling ======================

set scrolloff=8         "Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=1

" ====================== Search ======================

set incsearch       " Find the next match as we type the search
set hlsearch        " Highlight searches by default
set ignorecase      " Ignore case when searching...
set smartcase       " ...unless we type a capital

" ====================== Security ======================

set modelines=0
set nomodeline

" ====================== Theme ======================

augroup Theme
    let g:gruvbox_italic=1
    let g:gruvbox_contrast_dark='medium'
    let g:gruvbox_contrast_light='soft'
    au vimenter * ++nested colorscheme gruvbox
augroup END

" ====================== Airline ======================

let g:airline_powerline_fonts = 1
let g:airline_highlighting_cache = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline_skip_empty_sections = 1

" ====================== ALE ======================

let g:ale_linters = {
            \ 'python': ['flake8', 'pydocstyle', 'bandit', 'mypy'],
            \ 'sh': ['shellcheck']
            \}

let g:ale_fixers = {
            \ '*': ['remove_trailing_lines', 'trim_whitespace'],
            \ 'python': ['black', 'isort'],
            \ 'sh': ['shfmt']
            \}

let g:ale_lint_on_insert_leave = 1
let g:ale_fix_on_save = 1
nmap <silent> <F4> <Plug>(ale_next_wrap)

" ====================== YCM ======================

let g:ycm_autoclose_preview_window_after_completion = 1

" ====================== Binds ======================

" F2 toggle paste mode
set pastetoggle=<F2>
" F3 yank entire file to clipboard
nnoremap <F3> :%y+<CR>

" :Q force quits everything
command Q qa!

" Quickly insert newlines without entering insert mode
nnoremap <silent> <Leader>o :<C-u>call append(line("."), repeat([""], v:count1))<CR>
nnoremap <silent> <Leader>O :<C-u>call append(line(".")-1, repeat([""], v:count1))<CR>

" Left/Right arrows cycle buffers
nnoremap <Right> :bnext<CR>
nnoremap <Left> :bprevious<CR>
" Disable unused arrows
noremap <Up> <Nop>
noremap <Down> <Nop>

" FZF
nnoremap <silent> <Leader>b :Buffers<CR>
nnoremap <silent> <C-p> :Files<CR>
nnoremap <silent> <Leader>f :Rg<CR>
nnoremap <silent> <Leader>/ :BLines<CR>
nnoremap <silent> <Leader>' :Marks<CR>
nnoremap <silent> <Leader>g :Commits<CR>
nnoremap <silent> <Leader>H :Helptags<CR>
nnoremap <silent> <Leader>hh :History<CR>
nnoremap <silent> <Leader>h: :History:<CR>
nnoremap <silent> <Leader>q/ :History/<CR>

" Disable highlighting with return
nnoremap <CR> :noh<CR><CR>
" Switch to light mode
nnoremap <silent> <F12> :let &bg=(&bg=='light'?'dark':'light')<cr>

" Switch between different windows by their direction`
no <C-j> <C-w>j|
no <C-k> <C-w>k|
no <C-l> <C-w>l|
no <C-h> <C-w>h|

" Write as sudo with :w!!
cnoremap w!! execute 'silent! write !sudo tee % > /dev/null' <bar> edit!

" ====================== Misc ======================

" Auto change cursor shape (alacritty)
let &t_SI = "\<ESC>[6 q"
let &t_SR = "\<ESC>[4 q"
let &t_EI = "\<ESC>[0 q"

" Supress 'warning changing a readonly file'
augroup ReadonlyWarning
    au BufEnter * set noro
augroup END
