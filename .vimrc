" Duplicating default Debian behavior
syntax on
"set background=dark
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
set showcmd          " Show (partial) command in status line.
set showmatch        " Show matching brackets.
set ignorecase       " Do case insensitive matching
set smartcase        " Do smart case matching
set incsearch        " Incremental search
set autowrite        " Automatically save before commands like :next and :make
set hidden           " Hide buffers when they are abandoned
set mouse=a          " Enable mouse usage (all modes)

" Formatting
set sw=4
set ts=4
set bs=2
set et
set cindent
set fo=tcroql
set sta
filetype indent on
filetype plugin on

" Editing
set clipboard+=unnamed  " Yanks go on clipboard instead.
set tm=250  " Time to wait after ESC (default causes an annoying delay)
"colo murphy
colo slate

" Visual
set showmatch
set laststatus=2

" Display
set nu
set ru

" Backups and files
set backup                     " Enable creation of backup file.
set backupdir=~/.vim/backups " Where backups will go.
set directory=~/.vim/tmp     " Where temporary files will go.

if has("gui_running")
  set gfn=Terminus 10
endif
