call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" My plugins

Plug 'tpope/vim-rails'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
Plug 'thoughtbot/vim-rspec'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-fugitive'     " Git wrapper
Plug 'slim-template/vim-slim' " Slim syntax highlighting
Plug 'scrooloose/syntastic'   " Syntax highlighter
Plug 'tpope/vim-eunuch'       " UNIX shell commands
Plug 'mileszs/ack.vim'
Plug 'elixir-lang/vim-elixir'
Plug 'slashmili/alchemist.vim'
Plug 'nathanaelkane/vim-indent-guides'

" Add plugins to &runtimepath
call plug#end()

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
set autoread
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

" Ack.vim
nnoremap <leader>a :Ack
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag over ack
  let g:ackprg="ag --vimgrep"

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --hidden --nocolor -g ""'

  " ag is fast enought that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0

endif

" hybrid materialize config
set background=dark
colorscheme hybrid_material
let g:airline_theme = "hybrid"
" unicode symbols
let g:airline_left_sep = '▶'
let g:airline_right_sep = '◀'
