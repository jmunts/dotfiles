call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" My plugins

Plug 'tpope/vim-rails'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
Plug 'thoughtbot/vim-rspec'
Plug 'tomasr/molokai'
Plug 'tpope/vim-fugitive'     " Git wrapper
Plug 'slim-template/vim-slim' " Slim syntax highlighting
Plug 'scrooloose/syntastic'   " Syntax highlighter

" Add plugins to &runtimepath
call plug#end()

set grepprg=ag\ --nogroup\ --nocolor
" Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
let g:ctrlp_user_command = 'ag %s -l --hidden --nocolor -g ""'

" ag is fast enought that CtrlP doesn't need to cache
let g:ctrlp_use_caching = 0

" My config

let mapleader = ","

nmap <Leader>src :source ~/.vimrc<cr>
nmap <Leader>gbl :Gblame<cr>
nmap <Leader>h :nohlsearch<cr>
map <Leader>i mmgg=G`c

set softtabstop=2
set shiftwidth=2
set expandtab
set number
set relativenumber
set incsearch         " highlight while searching
set hlsearch          " highlight cll matches after search
set ignorecase        " case sensitive search pattern matching
set smartcase         " overrides ignorecase if pattern contains upcase
set statusline=%f
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%=      "left/right separator
set statusline+=%c,     "cursor column
set statusline+=%l/%L   "cursor line/total lines
set statusline+=\ %P    "percent through file

command! Q q " Bind :Q to :q
command! Qall qall
command! E e
command! W w
command! Wq wq

" molokai config
let g:molokai_original = 1
