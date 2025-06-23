" Vim indent file
" Language:     Husk
" Maintainer:   Your Name
" Last Change:  2024

if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal cindent
setlocal cinkeys=0{,0},0),0],!^F,o,O,e
setlocal cinoptions=Ls,m1,j1,J1

" Set the function to do the work
setlocal indentexpr=GetHuskIndent()

" Only define the function once
if exists("*GetHuskIndent")
  finish
endif

function! GetHuskIndent()
  " Find a non-blank line above the current line
  let lnum = prevnonblank(v:lnum - 1)

  " If there's no previous line, start with no indent
  if lnum == 0
    return 0
  endif

  " Get the indent of the previous line
  let ind = indent(lnum)
  let prevline = getline(lnum)
  let thisline = getline(v:lnum)

  " If the previous line opened a block, indent
  if prevline =~ '{\s*$'
    let ind = ind + &shiftwidth
  endif

  " If the previous line is a match arm, indent
  if prevline =~ '=>\s*$'
    let ind = ind + &shiftwidth
  endif

  " If the current line closes a block, unindent
  if thisline =~ '^\s*}'
    let ind = ind - &shiftwidth
  endif

  " If the current line is a match arm, align with previous arm or match keyword
  if thisline =~ '^\s*\w\+.*=>'
    " Look for the match keyword
    let match_lnum = lnum
    while match_lnum > 0
      if getline(match_lnum) =~ '\<match\>'
        return indent(match_lnum) + &shiftwidth
      endif
      let match_lnum = match_lnum - 1
    endwhile
  endif

  return ind
endfunction