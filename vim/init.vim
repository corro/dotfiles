" ********
" Pathogen
" ********

" To disable a plugin, add it's bundle name to the following list
let g:pathogen_disabled = ['nerdtree']

" Call pathogen
call pathogen#infect()
call pathogen#helptags()

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

" *******
" Plugins
" *******

" NERDCommenter
let g:NERDCustomDelimiters = {
    \ 'rst': { 'left': '.. '},
\ }

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

" Lightline
let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'component': {
      \   'readonly': '%{&readonly?"RO":""}',
      \ },
      \ }

" NERDTree
nmap <c-t> :NERDTreeToggle<cr>
let NERDTreeShowBookmarks=1
