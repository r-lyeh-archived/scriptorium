" Vim syntax file
" Language: LIL
" Maintainer:   Kostas Michalopoulos <badsector@runtimelegend.com>
" Last Change:  2011/08/07
" Version:      1.0
" URL:          https://github.com/badsector/lil
"
" Loosely based on Vim's Tcl syntax file

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Almost all LIL functions as defined in the lil.c file's register_stdcmds
" function
syn keyword lilFunction     reflect func rename unusedname quote set local write print eval topeval
syn keyword lilFunction     upeval downeval enveval jaileval count index indexof filter list append slice
syn keyword lilFunction     subst concat foreach return expr inc dec read store if while for
syn keyword lilFunction     char charat codeat substr strpos length trim ltrim rtrim strcmp
syn keyword lilFunction     streq repstr split try error exit source lmap rand catcher

" LIL Variables for $blah
" syn match lilVarRef "$\(\([^;$[\]{}"' 	]*\)\|\({[^}]*}\)\|\(\"[^\"]*\"\)\|\(\'[^\']*\'\)\)"
syn match lilVarRef "\(${[^}]*}\)\|\($[^;$[\]{}"' 	]*\)"

" Line continuation
syn match lilLineContinue '\\$'

" Strings
syn region lilEmbeddedRegion start='\[' end='\]' contained contains=lilVarRef
syn region lilString start=+"+ end=+"+ skip=+\\\\\|\\"+ contains=lilVarRef,lilEmbeddedRegion
syn region lilString start=+'+ end=+'+ skip=+\\\\\|\\'+ contains=lilVarRef,lilEmbeddedRegion
"syn region lilString "'.*'" contains=lilEmbeddedRegion

" Comments
syn match lilComment "#.*$"
syn region lilComment start=+##+ end=+##+

" Special characters
syn match lilSpecials "[{}[\]]"

" Set keyword characters
set iskeyword=@,48-57,_,-,192-255

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_lil_syntax_inits")
  if version < 508
    let did_lil_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink lilFunction    Statement
  HiLink lilVarRef      Identifier
  HiLink lilString      String
  HiLink lilComment     Comment
  HiLink lilSpecials    Special

  delcommand HiLink
endif

let b:current_syntax = "lil"

" vim: ts=8 noet
