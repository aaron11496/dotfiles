syntax on            " Color or highlight text based on syntax 

filetype indent on   " Load indentation scheme based on detected file type 
filetype plugin on   " Load plug based on detected file type 

"set ignorecase       " Do case insensitive matching
set smartcase        " Do smart case matching
set incsearch        " Do search incrementally while typing search term
set hidden           " Hide buffers when they are abandoned
set mouse=a          " Enable mouse usage (all modes)

" Formatting
set autoindent       " Use current line's indent level on new lines
set smartindent      " Guess indent level of new lines for C-like languages
set expandtab        " Use the right number of spaces to insert a tab
set shiftwidth=4     " Use 4 spaces for indenting
set tabstop=4        " Tabs are 4 spaces
set backspace=2      " Allow backspacing over autoindents, line breaks, start of insert

" Editing
set autowrite        " Automatically save before commands like :next and :make
set clipboard+=unnamed  " Yanks go on clipboard instead.
set tm=250  " Time to wait after ESC (default causes an annoying delay)

" Visual
color slate          " Color scheme
"set number          " Show line numbers  
set ruler            " Show status line at bottom of each window
set showcmd          " Show (partial) command in status line.
set showmatch        " Briefly highlight matching brace/paranthese/bracket
"set laststatus=2     " Always show status line, even when there's only one window

" Backups and files
set backup                     " Enable creation of backup file.
"set backupdir=~/.vim/backups " Where backups will go.
"set directory=~/.vim/tmp     " Where temporary files will go.
