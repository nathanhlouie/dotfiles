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
set norelativenumber
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
