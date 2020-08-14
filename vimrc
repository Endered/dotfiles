syntax enable
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
inoremap /* /**/<LEFT><LEFT>


nnoremap <C-h> <LEFT>x
nnoremap <C-a> ggVG
nnoremap <C-i> mIggVG=`I
nnoremap x "_x
nnoremap s "_s
"if wraped a line, move according to visual
nnoremap j gj
nnoremap k gk
filetype indent plugin on

tnoremap fd <C-\><C-n>

let mapleader = "\<Space>"
nnoremap <Leader>w <C-W>
nnoremap <Leader>wv :vs<CR>
nnoremap <Leader>ws :split<CR>
nnoremap <Leader>wd :q!<CR>
nnoremap <Leader><Leader> :
nnoremap <Leader>' :terminal<CR>
nnoremap <Leader>tl gt
nnoremap <Leader>th gT
nnoremap <Leader>td :tabclose<CR>
nnoremap <Leader>tn :tabnew<CR>
nnoremap <Leader>ft :NERDTree<CR>
