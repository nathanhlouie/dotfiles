let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()

Plug 'jiangmiao/auto-pairs'
Plug 'tomtom/tcomment_vim'
Plug 'romainl/vim-cool'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/vim-peekaboo'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-sleuth'
Plug 'justinmk/vim-sneak'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'

call plug#end()

set nocompatible
syntax on

set background=light

vnoremap < <gv 
vnoremap > >gv 

set autoindent
set autoread
set autowrite
set backspace=indent,eol,start
set clipboard=unnamed
set complete-=i
set completeopt=menu,menuone,noselect
set cursorline
set display+=lastline
set formatoptions+=j
set history=1000
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
set mouse=a
set nobackup
set noexpandtab
set nolangremap
set noswapfile
set number
set pumheight=10
set relativenumber
set ruler
set scrolloff=1
set sessionoptions-=options
set shiftround
set shiftwidth=4
set shortmess=WIcC
set sidescroll=4
set sidescrolloff=4 
set signcolumn=yes
set smartcase
set smartindent
set smarttab
set softtabstop=4
set splitbelow
set splitkeep=screen
set splitright
set tabpagemax=50
set tabstop=8
set termguicolors
set title
set ttimeout
set ttimeoutlen=100
set updatetime=100
set viewoptions-=options
set viminfo^=!
set wildmenu
set wildmode=longest:full,full
set winminwidth=4
setglobal tags-=./tags tags-=./tags; tags^=./tags;

augroup RelativeNormal
  autocmd!
  autocmd InsertLeave * silent! set relativenumber
augroup END

augroup LineNumberInsert
  autocmd!
  autocmd InsertEnter * silent! set number norelativenumber
augroup END

augroup highlightYankedText
  autocmd!
  autocmd TextYankPost * silent! call FlashYankedText()
augroup END

function! FlashYankedText()
  if (!exists('g:yankedTextMatches'))
    let g:yankedTextMatches = []
  endif

  let matchId = matchadd('IncSearch', ".\\%>'\\[\\_.*\\%<']..")
  let windowId = winnr()

  call add(g:yankedTextMatches, [windowId, matchId])
  call timer_start(300, 'DeleteTemporaryMatch')
endfunction

function! DeleteTemporaryMatch(timerId)
  while !empty(g:yankedTextMatches)
    let match = remove(g:yankedTextMatches, 0)
    let windowID = match[0]
    let matchID = match[1]

    try
      call matchdelete(matchID, windowID)
    endtry
  endwhile
endfunction
