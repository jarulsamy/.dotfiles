set nocompatible

" Set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
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
filetype plugin indent on

" Theme
let g:gruvbox_italic=1
let g:gruvbox_contrast_dark='medium'
let g:gruvbox_contrast_light='soft'
autocmd vimenter * ++nested colorscheme gruvbox

" Airline
let g:airline_powerline_fonts = 1
let g:airline_highlighting_cache = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline_skip_empty_sections = 1

" Editor Tweaks
syntax on
set ruler
set number
set relativenumber
set cursorline
set nowrap
set hlsearch
set background=dark
set encoding=utf-8
set textwidth=0
set wrapmargin=0
set backspace=indent,eol,start
set ignorecase
set smartcase
set termguicolors
set updatetime=100
" System clipboard - Most likely have to compile vim from source
set clipboard=unnamedplus

" Default file specs
set tabstop=4
set softtabstop=4
set shiftwidth=4
set textwidth=120
set expandtab
set autoindent
set fileformat=unix
set autoread
set hidden
set history=1000

" Extension specific tweaks
au BufNewFile,BufRead *.c,*.cpp,*.h,*.hpp,*.lisp
    \ set tabstop=2 |
    \ set softtabstop=2 |
    \ set shiftwidth=2

" Flag bad whitespace
au BufNewFile, BufRead *.py,*.c,*.h,*.cpp,*.hpp,*.sh,*.conf match BadWhitespace /\s\+$/
" Auto remove trailing whitespace
autocmd BufWritePre *.* :%s/\s\+$//e

" ALE
let g:ale_linters = {'python': ['flake8', 'pydocstyle', 'bandit', 'mypy']}
let g:ale_fixers = {
            \ '*': ['remove_trailing_lines', 'trim_whitespace'],
            \ 'python': ['black', 'isort']
            \}
let g:ale_lint_on_insert_leave = 1
let g:ale_fix_on_save = 1
nmap <silent> <C-@> <Plug>(ale_next_wrap)

" Binds
" Paste mode toggle F2
set pastetoggle=<F2>
" F5 yank entire file to clipboard - ACL2 D:
nnoremap <F5> :%y+<CR>

" :Q force quits everything
command Q qa!

" Quickly insert an empty line without entering insert mode
nnoremap <silent> <Leader>o :<C-u>call append(line("."), repeat([""], v:count1))<CR>
nnoremap <silent> <Leader>O :<C-u>call append(line(".")-1, repeat([""], v:count1))<CR>

" Cycle buffers with arrows
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

" Auto change cursor shape (alacritty)
let &t_SI = "\<ESC>[6 q"
let &t_SR = "\<ESC>[4 q"
let &t_EI = "\<ESC>[0 q"

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

" Supress 'warning changing a readonly file'
au BufEnter * set noro

" Write as sudo with :w!!
cnoremap w!! execute 'silent! write !sudo tee % > /dev/null' <bar> edit!
