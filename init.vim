if &compatible
  set nocompatible
endif
" Add the dein installation directory into runtimepath
set runtimepath+=~/.cache/dein/repos/github.com/Shougo/dein.vim

if dein#load_state('~/.cache/dein')
  call dein#begin('~/.cache/dein')

  call dein#add('~/.cache/dein/repos/github.com/Shougo/dein.vim')
  call dein#add('Shougo/deoplete.nvim')
  call dein#load_toml('~/.config/nvim/dein.toml', {'lazy': 0})
  call dein#load_toml('~/.config/nvim/dein_lazy.toml', {'lazy': 1})
  if !has('nvim')
    call dein#add('roxma/nvim-yarp')
    call dein#add('roxma/vim-hug-neovim-rpc')
  endif

  call dein#end()
  call dein#save_state()
endif

syntax enable
filetype indent plugin on

if dein#check_install()
  call dein#install()
endif


set number
set cursorline
set showmatch
set autoindent
set tabstop=4
set shiftwidth=4
set expandtab
set backspace=indent,eol,start
set clipboard=unnamedplus
set whichwrap=h,l,<,>,[,]
set wrap
"complete command
set wildmenu 

set swapfile
set directory=~/.vim/backups
set backup
set backupdir=~/.vim/backups
set undofile
set undodir=~/.vim/backups

"fold according current syntax
set foldmethod=syntax

"always show status bar
:set laststatus=2

set scrolloff=10
set hlsearch
set matchpairs& matchpairs+=<:>

"use make file
"set makeprg=g++\ %:S\ -Wall\ -Wextra\ -fsyntax-only\ -DDEBUG

inoremap {<ENTER> {}<LEFT><CR><ESC><S-o>
inoremap jj <ESC>
inoremap fd <ESC>


nnoremap x "_x
nnoremap s "_s
nnoremap ; :
nnoremap : ;
"if wraped a line, move according to visual
nnoremap j gj
nnoremap k gk

tnoremap fd <C-\><C-n>

let mapleader = "\<Space>"
nmap <Leader>w <Window>
nnoremap <Window> <C-W>
nnoremap <Window>v :vs<CR>
nnoremap <Window>s :split<CR>
nnoremap <Window>d :q!<CR>
nnoremap <Leader><Leader> :
nnoremap <Leader>' :terminal<CR>

nmap <Leader>t <TAB-MODE>
nnoremap <TAB-MODE>l gt
nnoremap <TAB-MODE>h gT
nnoremap <TAB-MODE>d :tabclose<CR>
nnoremap <TAB-MODE>n :tabnew<CR>

nmap <Leader>f <FILE>
nnoremap <FILE> :<BS>
nnoremap <FILE>t :NERDTree<CR>
