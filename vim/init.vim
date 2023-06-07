" ********
" VIM Plug
" ********

call plug#begin('~/.vim/plugged')

let g:plug_window='tabnew'

Plug 'mileszs/ack.vim', { 'as': 'ack' }
Plug 'ctrlpvim/ctrlp.vim', { 'as': 'ctrlp' }
Plug 'Shougo/deoplete.nvim', { 'as': 'deoplete' }
Plug 'deoplete-plugins/deoplete-jedi', { 'as': 'deoplete-jedi' }
Plug 'tpope/vim-fugitive', { 'as': 'fugitive' }
Plug 'cohama/lexima.vim', { 'as': 'lexima' }
Plug 'itchyny/lightline.vim', { 'as': 'lightline' }
Plug 'tpope/vim-repeat', { 'as': 'repeat' }
Plug 'msanders/snipmate.vim', { 'as': 'snipmate' }
Plug 'tpope/vim-surround', { 'as': 'surround' }
Plug 'scrooloose/syntastic', { 'as': 'syntastic' }
Plug 'ntpeters/vim-better-whitespace', { 'as': 'better-whitespace' }

call plug#end()

" *******
" General
" *******

let mapleader=' '

" Set defaults
filetype plugin indent on
syntax enable

" Allow to abandon modified buffers
set hidden

set ruler      " show the cursor position all the time
set showcmd    " display incomplete commands

" Save backups
set backup
set backupdir=~/.vim/backup,/tmp

" Save undo history
set undofile
set undodir=~/.vim/undo,/tmp

" Hide mode text
set noshowmode

" Line Numbering
set number

" Expand tabs
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4

set scrolloff=15
set clipboard=unnamedplus

" Set colorscheme
colorscheme xoria256

" 80
let &colorcolumn=join(range(80,999),",")

" Highlight end of line whitespace.
hi WhitespaceEOL guifg=#ff0000
match WhitespaceEOL /\s\+$/

" Punkt-Abkürzung verbessern
nmap . .`[

" Ignore binary files
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.class,*.pyc

" Forgotten sudo?
cmap w!! w !sudo tee >/dev/null %

" Useful tab completion
set wildmode=longest,list,full

" Easy split navigation
nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>

" Easy tab navigation
nmap <c-n> :tabn<cr>
nmap <c-p> :tabp<cr>

" Improve completion
set completeopt=longest,menuone

" Fix terminal issues
set guicursor=

" *******
" Plugins
" *******

" CtrlP
let g:ctrlp_map = '<c-e>'
noremap <c-b> :CtrlPBuffer<cr>
let g:ctrlp_user_command = {
	\ 'types': {
		\ 1: ['.git', 'cd %s && git ls-files . -co --exclude-standard'],
		\ 2: ['.hg', 'hg --cwd %s locate -I .'],
		\ },
	\ 'fallback': 'find %s -type f'
	\ }

" Syntastic
let g:syntastic_python_checkers=['flake8']
let g:syntastic_python_flake8_args="--ignore=E123,E124,E126,E128"
let g:syntastic_markdown_mdl_args="--rules=~MD001,~MD003"

" Lightline
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'component': {
      \   'readonly': '%{&readonly?"RO":""}',
      \ },
      \ }

" Deoplete
let g:deoplete#enable_at_startup = 1
