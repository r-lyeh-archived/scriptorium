" Vim syntax file
" Language:	Dao Help 1.0
" Created based on the syntax file for Lua, C, C++ etc.
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif
  
syntax clear

let b:current_syntax = "daohelp"

syn case match

syn match  daoSpecial		"@\[[0-9a-zA-Z_ .-:=()]*\]"

syn region  daoConstant		start="\z(@\[red[0-9a-zA-Z_ .-:=()]*\]\)" end="\z1"
syn region  daoStructure	start="\z(@\[green[0-9a-zA-Z_ .-:=()]*\]\)" end="\z1"
syn region  daoComment		start="\z(@\[blue[0-9a-zA-Z_ .-:=()]*\]\)" end="\z1"


"syncing method
syn sync minlines=100

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_daohelp_syntax_inits")
  if version < 508
    let did_daohelp_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink daoConstant		Constant
  HiLink daoComment			Comment
  HiLink daoStructure		Structure
  HiLink daoSpecial			SpecialChar

  delcommand HiLink
endif

let b:current_syntax = "daohelp"

finish

let b:current_syntax = ''
unlet b:current_syntax
syntax include @Code syntax/dao.vim
syntax region  vbtCode  matchgroup=vbt start="\z(@@\?\[\s*code\(\|[ .-:=()][0-9a-zA-Z_ .-:=()]*\)\]\)" end="\z1" contains=@Code
hi link vbt Special

let b:current_syntax = ''
unlet b:current_syntax
syntax include @Cpp syntax/cpp.vim
syntax region  vbtCpp  matchgroup=vbt start="\z(@@\?\[\s*\(cpp\|cxx\)\(\|[ .-:=()][0-9a-zA-Z_ .-:=()]*\)\]\)" end="\z1" contains=@Cpp
hi link vbt Special

let b:current_syntax = ''
unlet b:current_syntax
syntax include @Lua syntax/lua.vim
syntax region  vbtLua  matchgroup=vbt start="\z(@@\?\[\s*lua\(\|[ .-:=()][0-9a-zA-Z_ .-:=()]*\)\]\)" end="\z1" contains=@Lua
hi link vbt Special

let b:current_syntax = ''
unlet b:current_syntax
syntax include @Html syntax/html.vim
syntax region  vbtHtml  matchgroup=vbt start="\z(@@\?\[\s*html\(\|[ .-:=()][0-9a-zA-Z_ .-:=()]*\)\]\)" end="\z1" contains=@Html
hi link vbt Special

let b:current_syntax = ''
unlet b:current_syntax
syntax include @Tex syntax/tex.vim
syntax region  vbtTex  matchgroup=vbt start="\z(@@\?\[\s*tex\(\|[ .-:=()][0-9a-zA-Z_ .-:=()]*\)\]\)" end="\z1" contains=@Tex
hi link vbt Special

let b:current_syntax = "daohelp"

