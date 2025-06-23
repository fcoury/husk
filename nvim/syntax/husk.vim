" Vim syntax file
" Language:     Husk
" Maintainer:   Your Name
" Last Change:  2024

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword huskKeyword let fn struct enum impl if else match for while loop break continue return self use
syn keyword huskType int float bool string

" Boolean values
syn keyword huskBoolean true false

" Comments
syn match huskLineComment "//.*$"
syn region huskBlockComment start="/\*" end="\*/" contains=huskBlockComment

" Strings
syn region huskString start='"' skip='\\"' end='"'

" Numbers
syn match huskNumber '\<\d\+\>'
syn match huskFloat '\<\d\+\.\d*\>'

" Identifiers
syn match huskFunction '\<\w\+\ze\s*('
syn match huskStructField '\.\s*\zs\w\+'
syn match huskEnumVariant '\<[A-Z]\w*\ze\s*::'
syn match huskEnumAccess '::\s*\zs\w\+'

" Operators
syn match huskOperator '[+\-*/%]'
syn match huskOperator '[+\-*/%]='
syn match huskComparisonOperator '[<>]=\?'
syn match huskComparisonOperator '[!=]='
syn match huskLogicalOperator '&&\|||'
syn match huskLogicalOperator '!'
syn match huskRangeOperator '\.\.'
syn match huskRangeOperator '\.\.='

" Built-in functions
syn keyword huskBuiltIn println

" Use statements
syn match huskUseKeyword '\<use\>' contained
syn match huskPath '\<\w\+\(::\w\+\)*' contained
syn match huskLocalPath '\<local\(::\w\+\)*' contained
syn region huskUseStatement start='^\s*use\>' end=';' contains=huskUseKeyword,huskPath,huskLocalPath,huskComment

" Type annotations
syn match huskTypeAnnotation ':\s*\zs\w\+' contained
syn region huskFunctionSignature start='fn\s\+\w\+\s*(' end=')' contains=huskKeyword,huskTypeAnnotation,huskIdentifier
syn match huskReturnType '->\s*\zs\w\+'

" Struct and enum definitions
syn region huskStructBlock start='{' end='}' contained contains=huskIdentifier,huskTypeAnnotation,huskComment
syn match huskStructDef 'struct\s\+\w\+' contains=huskKeyword
syn match huskEnumDef 'enum\s\+\w\+' contains=huskKeyword

" Match expressions
syn region huskMatchArm start='=>' end='[,}]' contains=ALL

" Define default highlighting
hi def link huskKeyword Keyword
hi def link huskType Type
hi def link huskBoolean Boolean
hi def link huskLineComment Comment
hi def link huskBlockComment Comment
hi def link huskString String
hi def link huskNumber Number
hi def link huskFloat Float
hi def link huskFunction Function
hi def link huskStructField Identifier
hi def link huskEnumVariant Type
hi def link huskEnumAccess Identifier
hi def link huskOperator Operator
hi def link huskComparisonOperator Operator
hi def link huskLogicalOperator Operator
hi def link huskRangeOperator Operator
hi def link huskBuiltIn Special
hi def link huskTypeAnnotation Type
hi def link huskReturnType Type
hi def link huskStructDef Structure
hi def link huskEnumDef Structure
hi def link huskUseKeyword Keyword
hi def link huskPath Include
hi def link huskLocalPath Include

let b:current_syntax = "husk"