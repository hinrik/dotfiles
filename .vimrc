" Vim syntax file
" Filename: .vimrc
" Author: Hinrik Örn Sigurðsson <hinrik.sig at gmail dot com>
" Last Change: Apr 9th 2009
" URL: http://github.com/hinrik/dotfiles/blob/master/.vimrc

" This needs to come first
set nocompatible                   " Use Vim defaults, not vi

" General stuff
set backspace=indent,eol,start     " Allow backspacing over everything
set backupdir^=~/.tmp//            " Store backup files in ~/.tmp
set directory^=~/.tmp//            " Store swap files in ~/.tmp
set display+=lastline              " show last line even if it doesn't fit
set display+=uhex                  " show unprintable chars as hex numbers
set encoding=utf-8                 " Use UTF-8 for everything
set foldclose=all                  " close a fold when the cursor leaves it
set foldlevelstart=99              " start with all folds open
set formatoptions+=w               " Whitespace continues a paragraph
set guioptions+=c                  " Prefer console dialogs to GUI ones
set guioptions+=f                  " Stay in the foreground (don't fork)
set guioptions-=m                  " Don't show the menubar
set guioptions-=T                  " Don't show the toolbar
set guioptions-=t                  " Don't show the tear-off menu items
set history=50                     " keep 50 lines of command history
set hlsearch                       " highlight all pattern matches
set ignorecase                     " Do case-insensitive matching
set incsearch                      " Highlight search results immediately
set laststatus=2                   " Always show the status line
set listchars=tab:>.               " Show tabs sensibly with 'list'
set number                         " Show line numbers on the side
set numberwidth=3                  " Minimum size of line number column
set pastetoggle=<F12>              " Toggle paste mode with F12
set report=0                       " Report all changes
set ruler                          " Always show the bottom ruler
set scrolloff=999                  " keep cursor line in middle of window
set shortmess+=I                   " Disable the welcome screen
set shortmess-=l                   " Show lines,characters instead of L,C
set showcmd                        " Show commands as they're being issued
set sidescrolloff=5                " Scroll when 5 columns from the side
set smartcase                      " Case-sensitive matching when desired
set splitbelow                     " new windows appear below
set splitright                     " ...and to the right
set viminfo='20,\"500              " Keep a .viminfo file
set visualbell t_vb=               " No terminal bells or screen flashes
set wildmenu                       " Show a menu when tab-completing
filetype plugin on                 " Many filetype-specific enhancements

" Indenting
filetype indent on                 " Filetype-specific indenting tweaks
set expandtab                      " Indent with spaces
set shiftround                     " Indent to a multiple of shiftwidth
set shiftwidth=4                   " 4 column indents
set smarttab                       " Smart tabs
set softtabstop=4                  " 4 column indents again...

" Show ↪ at the beginning of wrapped lines
let &sbr = nr2char(8618).' '

" Always assume bash when filetype=sh
let g:is_bash=1

" Perl syntax
let perl_include_pod=1             " Pod highlighting
let perl_fold=1                    " Fold Perl constructs by default
let perl_nofold_packages=1         " Not packages, though
let perl_string_as_statement=1     " Make quoting operators stand out

" Perl testing
map ,t <Esc>:!prove -vl %<CR>
map ,T <Esc>:!prove -vl % \|vimpager<CR>

" I keep hitting Q by accident, might as well remap it
map Q :q

" Color stuff
if &t_Co > 2 || has("gui_running")
    set background=dark
    syntax on                      " syntax highlighting
    colorscheme literal_tango      " my colorscheme
endif

" Give windows equal space after resizing or opening a new window
autocmd VimResized * wincmd =
autocmd BufWinEnter * wincmd =

" Only enforce textwidth on plain text files
autocmd FileType text setlocal textwidth=78

" Jump to the last known position when reopening a file
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \     exe "normal! g'\"" |
    \ endif

" Highlight text that exceeds 'textwidth'
"exec 'match Error /\%>' . &textwidth . 'v.\+/'

" Make the GUI look nice
if has("gui_running")
    if has("gui_gtk2")
        set guifont=Mono\ 8.5
    endif
endif

" Identify the highlight group of the currect cursor position
function! SyntaxItem()
    return synIDattr(synID(line("."),col("."),1),"name")
endfunction

" Current statusline, assumes literal_tango colorscheme.
hi User1 ctermbg=0 ctermfg=1 cterm=bold guibg=#2e3435 guifg=#ef2929 gui=bold
hi User2 ctermbg=0 ctermfg=2 cterm=bold guibg=#2e3435 guifg=#8ae234 gui=bold
hi User3 ctermbg=0 ctermfg=3 guibg=#2e3435 guifg=#c4a000
hi User4 ctermbg=0 ctermfg=6 guibg=#2e3435 guifg=#06989a

" Non-current statusline, assumes literal_tango colorscheme. The 'bold' here
" is actually to make sure it will *not* be bold. See below.
hi User5 ctermbg=0 ctermfg=1 cterm=bold guibg=#2e3435 guifg=#ef2929 gui=bold
hi User6 ctermbg=0 ctermfg=2 cterm=bold guibg=#2e3435 guifg=#8ae234 gui=bold
hi User7 ctermbg=0 ctermfg=3 cterm=bold guibg=#2e3435 guifg=#c4a000 gui=bold
hi User8 ctermbg=0 ctermfg=6 cterm=bold guibg=#2e3435 guifg=#06989a gui=bold

" Here comes the status line
set statusline=%1*%-2.2n%*                           " buffer number
set statusline+=\ %t                                 " filename
set statusline+=\ (%2*%{(&fenc==\"\"?&enc:&fenc)},%{&fileformat}%*) " encoding
set statusline+=%(\ [%3*%H%R%M%*]%)                  " flags
set statusline+=%=                                   " left-right alignment
set statusline+=%(%4*%{SyntaxItem()}%*%)             " highlighting group
set statusline+=\ %-14.(%l/%L,%v%)                   " line/column number
set statusline+=\ %P                                 " % through file

" This trick is so I can better control the colors of the non-current
" statusline. By default it flips the bold attribute flag in all cases,
" which I don't like. See the %* field in :help 'statusline'
let g:c_statusline = &g:statusline
let g:nc_statusline =
  \ substitute(
    \ substitute(
      \ substitute(
        \ substitute(g:c_statusline, '%1', '%5', 'g'),
        \ '%2', '%6', 'g'),
      \ '%3', '%7', 'g'),
    \ '%4', '%8', 'g')

" If we need don't use both BufEnter/BufLeave and WinEnter/WinLeave then
" we'll get wrong colors after opening and closing windows.
autocmd BufEnter * let &l:statusline = g:c_statusline
autocmd WinEnter * let &l:statusline = g:c_statusline
autocmd BufLeave * let &l:statusline = g:nc_statusline
autocmd WinLeave * let &l:statusline = g:nc_statusline
