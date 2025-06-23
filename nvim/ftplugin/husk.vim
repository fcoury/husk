" Vim filetype plugin file
" Language:     Husk
" Maintainer:   Your Name
" Last Change:  2024

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

" Set comment string
setlocal commentstring=//\ %s

" Set 'formatoptions' to break comment lines but not other lines
setlocal formatoptions-=t formatoptions+=croql

" Match words with underscores
setlocal iskeyword+=_

" Set path for gf and similar commands to work with use statements
setlocal path+=.,src/**,tests/**

" Basic folding support
setlocal foldmethod=syntax
setlocal foldlevel=99

" Match pairs for % command
setlocal matchpairs+=<:>