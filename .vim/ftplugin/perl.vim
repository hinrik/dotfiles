" Perl testing
map <LocalLeader>t <Esc>:!prove -vl %<CR>
map <LocalLeader>T <Esc>:!prove -vl % \|vimpager<CR>

" PerlTidy
map <LocalLeader>pt <Esc>:%! perltidy<CR>
map <LocalLeader>ptv <Esc>:'<,'>! perltidy<CR>
