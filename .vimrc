set nocompatible
filetype off

" Set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'vim-scripts/indentpython.vim'
Plugin 'vim-syntastic/syntastic'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'preservim/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'psf/black'
Plugin 'morhetz/gruvbox'
Plugin 'wakatime/vim-wakatime'
Plugin 'ycm-core/YouCompleteMe'
Plugin 'tpope/vim-surround'

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Older Plugins Bundles
" <Bundle HERE>

" All of Plugins must be added before the following line
call vundle#end()
filetype plugin indent on

" Theme
autocmd vimenter * colorscheme gruvbox
" Airline
let g:airline_powerline_fonts = 1
let g:airline_highlighting_cache = 1

" Editor Tweaks
set tabstop=4 |
set softtabstop=4 |
set shiftwidth=4 |
set textwidth=120 |
set expandtab |
set autoindent |
set fileformat=unix
set backspace=indent,eol,start

" Lisp specific stuff
au BufNewFile,BufRead *.lisp
    \ set tabstop=2 |
    \ set softtabstop=2 |
    \ set shiftwidth=2

" Flag whitespace
au BufNewFile, BufRead *.py,*.c,*.h,*.cpp,*.hpp,*.sh,*.conf,*.nginx match BadWhitespace /\s\+$/

" Auto remove trailing whitespace
autocmd BufWritePre *.* :%s/\s\+$//e

" Autoformat python files with Black
autocmd BufWritePre *.py execute ':Black'

" Binds
" Clear highlights on enter
nnoremap <CR> :noh<CR><CR>
" Format on F9
nnoremap <F9> :Black<CR>
" Paste Toggle F2
set pastetoggle=<F2>
" Syntastic toggle passive mode
nnoremap <F3> :SyntasticToggleMode<CR>

" Nerdtree
map <C-n> :NERDTreeToggle<CR>
 autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
 " If more than one window and previous buffer was NERDTree, go back to it.
 autocmd BufEnter * if bufname('#') =~# "^NERD_tree_" && winnr('$') > 1 | b# | endif
" Show dotfiles
let NERDTreeShowHidden=1
" Ignore files in tree
let NERDTreeIgnore = ['\.pyc$', '__pycache__/', '\.swp$']
"Switch between different windows by their direction`
no <C-j> <C-w>j| "switching to below window
no <C-k> <C-w>k| "switching to above window
no <C-l> <C-w>l| "switching to right window
no <C-h> <C-w>h| "switching to left window

" Black
let g:black_fast = 1
let g:black_linelength = 120

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_python_checker = ['flake8']
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
" let g:syntastic_mode_map = { 'mode': 'passive', 'active_filetypes': [],'passive_filetypes': [] }
" nnoremap <C-w>E :SyntasticCheck<CR>

" Visuals
syntax on
let python_highlight_all=1
set hlsearch
set background=dark
set nu

" Misc
set encoding=utf-8
set textwidth=0
set wrapmargin=0
" System clipboard - Most likely have to compile vim from source
set clipboard=unnamedplus

" :W sudo saves the file
command W w !sudo tee % > /dev/null
" :Q -> :qa!
command Q qa!

" Save cursor position
" Tell vim to remember certain things when we exit
"  '10  :  marks will be remembered for up to 10 previously edited files
"  "100 :  will save up to 100 lines for each register
"  :20  :  up to 20 lines of command-line history will be remembered
"  %    :  saves and restores the buffer list
"  n... :  where to save the viminfo files
set viminfo='10,\"100,:20,%,n~/.viminfo

function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction

augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END
