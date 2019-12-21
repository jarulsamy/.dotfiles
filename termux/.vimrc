set nocompatible
filetype off

" Set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Newer Plugins .vim
Plugin 'vim-scripts/indentpython.vim'
Plugin 'vim-syntastic/syntastic'
Plugin 'nvie/vim-flake8'
Plugin 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Older Plugins Bundles
" Bundle 'Valloric/YouCompleteMe'

" All of Plugins must be added before the following line
call vundle#end()
filetype plugin indent on

" File specific tweaks
au BufNewFile,BufRead *.py,*.cpp,*.h,*.c,*.hpp,*.sh
    \ set tabstop=4 |
    \ set softtabstop=4 |
    \ set shiftwidth=4 |
    \ set textwidth=79 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix

" Flag whitespace
au BufNewFile, BufRead *.py,*.cpp,*.h,*.hpp,*.sh match BadWhitespace /\s\+$/

nnoremap <CR> :noh<CR><CR>
inoremap <silent> <Esc> <C-O>:stopinsert<CR>

" Visuals
syntax on
let python_highlight_all=1
set hlsearch
set background=dark
set nu

" Misc
set encoding=utf-8
set clipboard=unnamed

" :W sudo saves the file
command W w !sudo tee % > /dev/null
