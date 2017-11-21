" Vim syntax file
" Filename: .vimrc
" Author: Hinrik Örn Sigurðsson <hinrik.sig at gmail dot com>
" URL: http://github.com/hinrik/dotfiles/blob/master/.vimrc

" Download vundle
if !isdirectory(expand("~/.vim/bundle/Vundle.vim"))
    !mkdir -p ~/.vim/bundle
    !git clone git://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
    let s:bootstrap=1
endif

" Load bundles with vundle
set runtimepath+=~/.vim/bundle/Vundle.vim
call vundle#rc()
Plugin 'gmarik/Vundle.vim'
Plugin 'avakhov/vim-yaml'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'hinrik/color-scheme-literal-tango'
Plugin 'kchmck/vim-coffee-script'
Plugin 'lfairy/lilyvim'
Plugin 'vim-scripts/VimClojure'
Plugin 'slim-template/vim-slim'
Plugin 'vim-perl/vim-perl'
Plugin 'vim-scripts/perl_synwrite.vim'
Plugin 'tpope/vim-markdown'
Plugin 'zaiste/tmux.vim'
Plugin 'elixir-lang/vim-elixir'
Plugin 'rhysd/vim-crystal'

" Bootstrap vundle
if exists("s:bootstrap") && s:bootstrap
    unlet s:bootstrap
    BundleInstall
endif

" General stuff
set backspace=indent,eol,start     " Allow backspacing over everything
set backupdir^=~/.vim/tmp/         " Store backup files in ~/.vim/tmp/
set directory^=~/.vim/tmp/         " Store swap files in ~/.vim/tmp/
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
set list                           " Enable list mode (see following lines)
set listchars=tab:▸\               " Indicate tabs with ▸
set listchars+=trail:.             " Show trailing spaces as .
set listchars+=extends:$           " Show $ when a line extends off-screen
set listchars+=nbsp:.              " Show non-breaking spaces as .
set modeline                       " Adhere to modelines
set number                         " Show line numbers on the side
if has("numberwidth")
    set numberwidth=3              " Minimum size of line number column
endif
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
set tabline=%!MyTabLine()          " Custom tabline (see function below)
set title                          " set the terminal title
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
set tabstop=4                      " Make hard tabs span 4 columns

" Color stuff
"if &t_Co > 16
"    set t_Co=16
"endif
if &t_Co > 2 || has("gui_running")
    set background=dark
    syntax on                      " syntax highlighting
    colorscheme literal_tango      " my colorscheme
endif

" Make the GUI look nice
if has("gui_running") && has("gui_gtk2")
    set guifont=Mono\ 8.5
endif

" Filetype-specific settings
let is_bash=1                         " Always assume bash when filetype=sh
let perl_include_pod=1                " Pod highlighting
let perl_string_as_statement=1        " Make quoting operators stand out
let ruby_operators=1                  " Highlight operators in Ruby
let vimclojureHighlightBuiltins=1     " Highlight Clojure builtins
let vimclojure#ParenRainbow=1         " Distinct nested parens in Clojure
let lisp_instring=1                   " Highlight string escapes in Lisp
let sql_type_default="mysql"          " Assume SQL is MySQL
let org_indent=1                      " Indent paragraphs
let org_heading_shade_leading_stars=0 " Looks better this way
let org_heading_highlight_colors = [
    \ 'Constant', 'Title', 'Identifier', 'Special', 'PreProc', 'Comment'
    \ ] " This goes well with my colorscheme

" Show ↪ at the beginning of wrapped lines
let &sbr = nr2char(8618).' '

" The comma key is more convenient for me than backslash
let mapleader=","
let maplocalleader=","

" I keep hitting Q by accident, might as well remap it
map Q :q

" map C-l to :nohl (disables current search highlighting)
nnoremap <silent> <C-l> :nohl<CR><C-l>

" make Home key move to first nonblank character on the line
noremap <expr> <silent> <Home> col('.') == match(getline('.'),'\S')+1 ? '0' : '^'
imap <silent> <Home> <C-O><Home>

" Give windows equal space after resizing or opening a new window
autocmd BufWinEnter * wincmd =
if version >= 700
    autocmd VimResized * wincmd =
endif

autocmd BufRead,BufNewFile *.cr set filetype=crystal

" My *.md files are Markdown, not Modula 2
autocmd BufRead,BufNewFile *.md set filetype=markdown

" My *.comp files are Mason
autocmd BufRead,BufNewFile *.comp set filetype=mason

" Only enforce textwidth on plain text files
autocmd FileType text setlocal textwidth=78

" Use 2-column indenting in Lisp, Ruby, Sass, Haml, and YAML, CoffeeScript, etc
autocmd FileType lisp,ruby,eruby,scss,sass,haml,yaml,coffee,eco,lua,moon,crystal
    \ setlocal shiftwidth=2 softtabstop=2 expandtab

" Jump to the last known position when reopening a file
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \     exe "normal! g'\"" |
    \ endif

" Highlight text that exceeds 'textwidth'
"exec 'match Error /\%>' . &textwidth . 'v.\+/'

" Identify the highlight group of the currect cursor position
function! SyntaxItem()
    return synIDattr(synID(line("."),col("."),1),"name")
endfunction

" be evil and override the statusline highlighting
hi StatusLine   ctermfg=NONE ctermbg=0 cterm=bold
hi StatusLineNC ctermfg=NONE ctermbg=0 cterm=NONE
hi StatusLine   guifg=#d3d7cf guibg=#2e3436 gui=bold
hi StatusLineNC guifg=#d3d7cf guibg=#2e3436 gui=NONE

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
set statusline+=\ %{getfperm(expand('%'))}           " file permissions
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

" If we don't use both BufEnter/BufLeave and WinEnter/WinLeave then
" we'll get wrong colors after opening and closing windows.
autocmd BufEnter * let &l:statusline = g:c_statusline
autocmd WinEnter * let &l:statusline = g:c_statusline
autocmd BufLeave * let &l:statusline = g:nc_statusline
autocmd WinLeave * let &l:statusline = g:nc_statusline

" taken from :help setting-tabline
function MyTabLine()
    let s = ''
    for i in range(tabpagenr('$'))
    " select the highlighting
    if i + 1 == tabpagenr()
        let s .= '%#TabLineSel#'
    else
        let s .= '%#TabLine#'
    endif

    " set the tab page number (for mouse clicks)
    let s .= '%' . (i + 1) . 'T'

    " the label is made by MyTabLabel()
    let s .= ' %{MyTabLabel(' . (i + 1) . ')} '
    endfor

    " after the last tab fill with TabLineFill and reset tab page nr
    let s .= '%#TabLineFill#%T'

    " right-align the label to close the current tab page
    if tabpagenr('$') > 1
    let s .= '%=%#TabLine#%999Xclose'
    endif

    return s
endfunction

" show tab number before filename
function MyTabLabel(n)
    let buflist = tabpagebuflist(a:n)
    let winnr = tabpagewinnr(a:n)
    return buflist[winnr - 1] .' '. bufname(buflist[winnr - 1])
endfunction
