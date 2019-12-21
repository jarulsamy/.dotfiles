set nocompatible              " required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')
Plugin 'altercation/vim-colors-solarized'
Plugin 'vim-syntastic/syntastic'
Plugin 'nvie/vim-flake8'
Plugin 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'} 
Plugin 'vim-scripts/indentpython.vim'

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" add all your plugins here (note older versions of Vundle
" used Bundle instead of Plugin)
Bundle 'Valloric/YouCompleteMe'

" All of Plugins must be added before the following line
"
call vundle#end()            " required
filetype plugin indent on    " required
au BufNewFile,BufRead *.py,*.cpp,*.h,*.c,*.hpp,*.sh
    \ set tabstop=4 |
    \ set softtabstop=4 |
    \ set shiftwidth=4 |
    \ set textwidth=79 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix

au BufNewFile, BufRead *.py,*.cpp,*.h,*.hpp,*.sh match BadWhitespace /\s\+$/

nnoremap <CR> :noh<CR><CR>
inoremap <silent> <Esc> <C-O>:stopinsert<CR>
syntax on
let python_highlight_all=1
set background=dark
set encoding=utf-8
set hlsearch
set nu
set clipboard=unnamed

" :W sudo saves the file                                                                         
command W w !sudo tee % > /dev/null
