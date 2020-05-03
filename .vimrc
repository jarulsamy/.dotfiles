set nocompatible
filetype off

" Set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'vim-scripts/indentpython.vim'
Plugin 'vim-syntastic/syntastic'
Plugin 'nvie/vim-flake8'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'vim-airline/vim-airline'
Plugin 'airblade/vim-gitgutter'
Plugin 'tell-k/vim-autopep8'
Plugin 'scrooloose/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Older Plugins Bundles
" <Bundle HERE>

" All of Plugins must be added before the following line
call vundle#end()
filetype plugin indent on

" File specific tweaks
au BufNewFile,BufRead *.py,*.c,*.h,*.cpp,*.hpp,*.sh,*.conf,*.nginx
    \ set tabstop=4 |
    \ set softtabstop=4 |
    \ set shiftwidth=4 |
    \ set textwidth=79 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix

" Flag whitespace
au BufNewFile, BufRead *.py,*.c,*.h,*.cpp,*.hpp,*.sh,*.conf,*.nginx match BadWhitespace /\s\+$/

" PEP8
let g:autopep8_max_line_length=100

" NerdTree
"autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * if argc() == 0 && !exists(“s:std_in”) | NERDTree | endif
"let NERDTreeQuitOnOpen = 1
"autocmd bufenter * if (winnr(“$”) == 1 && exists(“b:NERDTreeType”) && b:NERDTreeType == “primary”) | q | endif
let NERDTreeAutoDeleteBuffer = 1
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1


" Binds

" Clear highlights on enter
nnoremap <CR> :noh<CR><CR>
" inoremap <silent> <Esc> <C-O>:stopinsert<CR>

" F8 Calls Autopep8 for formatting
autocmd FileType python noremap <buffer> <F8> :call Autopep8()<CR>

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
