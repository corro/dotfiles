let mapleader=','

filetype off

" To disable a plugin, add it's bundle name to the following list
let g:pathogen_disabled = ['autoclose']

" Call pathogen
call pathogen#infect()
call pathogen#helptags()

" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" Hidden does not what you meant, it simply keeps undo history and stuff
set hidden

" No more <Press ENTER blablabla>
set shortmess=atI

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("vms")
  set nobackup		" do not keep a backup file, use versions instead
else
  set backup		" keep a backup file
endif
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  " Also don't do it when the mark is in the first line, that is the default
  " position when opening a file.
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

" Backup-Directory
set backupdir=~/.vim/backup,/tmp

" Save undo history
set undofile

" Set undo directory
set undodir=~/.vim/undo,/tmp

" Statusleiste anzeigen
set laststatus=2

" Tabs sind doof...
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4

" Buffer Explorer
let g:miniBufExplMapWindowNavVim=1
let g:miniBufExplModSelTarget=1
nmap <C-N> :bn<cr>
nmap <C-P> :bp<cr>
nmap <C-W> :bd<cr>

" Set colorscheme
set t_Co=256
colorscheme xoria256

" Line Numbering
set number

set scrolloff=15
set clipboard=unnamed

" 80
au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>79v.\+', -1)

" Highlight end of line whitespace.
hi WhitespaceEOL guifg=#ff0000
match WhitespaceEOL /\s\+$/

" Punkt-Abkürzung verbessern
nmap . .`[

" Comments
nmap cc <leader>c<space>
vmap cc <leader>c<space>

" CtrlP
let g:ctrlp_map = 'e'

" Ignore binary files
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.class,*.pyc

" PythonMode Options
let g:pymode_lint_cwindow = 1
let g:pymode_lint_write = 1
let g:pymode_lint_onfly = 1
let g:pymode_lint_checker = "pyflakes"
let g:pymode_indent = 0
let g:pymode_rope_guess_project = 0
let pymode_rope_extended_complete = 1
let pymode_breakpoint = 0
let pymode_rope_vim_completion = 1

" Forgotten sudo?
cmap w!! w !sudo tee >/dev/null %

" Useful tab completion
set wildmode=longest,list,full
set wildmenu

" Kein Code Folding by Default
set nofoldenable

" EasyMotion options
let g:EasyMotion_leader_key = 'f'
let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyz'
let g:EasyMotion_grouping = 2
