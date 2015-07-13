" Vim syntax file
" Language:	Dao 1.0
" Created based on the syntax file for Lua, C, C++ etc.
" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

if !exists("dao_syntax_loaded")
    let dao_syntax_loaded = 1
else
    let dao_syntax_loaded2 = 1
endif

inoremap # X#

syn case match

" Comments
syn keyword daoTodo		contained TODO FIXME XXX
syn match   daoComment		"#.*$" contains=daoTodo
syn region  daoComment		start="#{" end="#}" contains=daoTodo,daoInnerComment

" catch errors caused by wrong parenthesis and wrong curly brackets or
" keywords placed outside their respective blocks

syn region  daoParen		transparent start='(' end=')' contains=ALLBUT,daoError,daoTodo,daoSpecial,daoCond,daoCondElseif,daoCondEnd,daoCondStart,daoBlock,daoRepeatBlock,daoRepeat,daoStatement
syn match   daoError		")"
syn match   daoError		"}"
syn match   daoError		"\<\%(else\)\>"

syn keyword daoConditional	if else switch case and or not in
syn keyword daoRepeat		while for do
syn keyword daoStructure	interface class routine as type int float complex string enum tuple array list map any

" other keywords
syn keyword daoStatement	defer return skip break static const load import public protected private case default
syn keyword daoConstant		var invar none false true self

" syn match   daoPreProc	  "^\s*$\%(debug\|nodebug\|if\|ifnot\|end\|else\|endinput\)\>"

" Strings
syn match   daoSpecial		contained "\\[\\abfnrtv\'\"[\]]\|\\\d\{,3}"
syn region  daoString		start=+'+  end=+'+ skip=+\\\\\|\\'+ contains=daoSpecial
syn region  daoString		start=+"+  end=+"+ skip=+\\\\\|\\"+ contains=daoSpecial
syn region  daoString		start="\z(@@\?\[[0-9a-zA-Z_ .+-:=()]*\]\)" end="\z1"

" integer number
syn match daoNumber		"\<[0-9]\+\>"
syn match daoNumber		"\<[0-9]\+L\>"
syn match daoFloat		"\<[0-9]\+\.\?[0-9]*[FDC]\>"
syn match daoFloat		"\<[0-9]*\.\?[0-9]\+[FDC]\>"
" floating point number, with dot, optional exponent
syn match daoFloat		"\<[0-9]\+\.[0-9]*\%(e[-+]\=[0-9]\+\)\=\>"
" floating point number, starting with a dot, optional exponent
syn match daoFloat		"\.[0-9]\+\%(e[-+]\=[0-9]\+\)\=\>"
" floating point number, without dot, with exponent
syn match daoFloat		"\<[0-9]\+e[-+]\=[0-9]\+\>"
syn match daoNumber             "0x\x\+\(u\=l\{0,2}\|ll\=u\)\>"

syn match   daoSpecReg		"$\w\+"
syn match   daoSpecReg		"@[ifcsIFCS][0-9]{0,2}"
syn match   daoSpecReg		":="
syn match   daoSpecReg		"<@>"
syn match   daoSpecReg		"$\(EXP\|VAR\|ID\|OP\|BL\)\w*"
syn match   daoMacro		"\\\(!\|?\|\*\|+\)"
syn match   daoMethDecl		"\\\((\|)\|\[\|\]\|{\|}\|\"\|\'\)"
syn match   daoMethDecl		"\w\+\(::\w\+\)\+"

syn region  daoTableBlock       transparent matchgroup=daoTable start="{" end="}" contains=ALLBUT,daoTodo,daoSpecial,daoCond,daoCondElseif,daoCondEnd,daoCondStart,daoBlock,daoRepeatBlock

syn keyword   daoFunc	io
syn keyword   daoFunc	mt
syn keyword   daoFunc	std

"syncing method
syn sync minlines=100

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_dao_syntax_inits")
  if version < 508
    let did_dao_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink daoStatement		Statement
  HiLink daoRepeat		Repeat
  HiLink daoString		String
  HiLink daoString2		String
  HiLink daoNumber		Number
  HiLink daoFloat		Float
  HiLink daoConstant		Constant
  HiLink daoCond		Conditional
  HiLink daoConditional		Conditional
  HiLink daoFunction		Function
  HiLink daoComment		Comment
  HiLink daoTodo		Todo
  HiLink daoTable		Structure
  HiLink daoStructure		Structure
  HiLink daoError		Error
  HiLink daoSpecial		SpecialChar
  HiLink daoMacro		Structure
  HiLink daoSpecReg		Identifier
  HiLink daoMethDecl		Function
  HiLink daoFunc		Identifier

  delcommand HiLink
endif


if exists("dao_syntax_loaded2")
    finish
endif


let b:current_syntax = ''
unlet b:current_syntax
syntax include @DaoHelp syntax/daohelp.vim
syntax region  vbtDaoHelp  matchgroup=vbt start="\z(@@\?\[\s*\(name\|title\|text\|list\)\(\|[ .-:=()][0-9a-zA-Z_ .-:=()]*\)\]\)" end="\z1" contains=@DaoHelp
hi link vbt String

let b:current_syntax = ''
unlet b:current_syntax
syntax include @Code syntax/dao.vim
syntax region  vbtCode  matchgroup=vbt start="\z(@@\?\[\s*\(code\|test\)\(\|[ .-:=()][0-9a-zA-Z_ .-:=()]*\)\]\)" end="\z1" contains=@Code
hi link vbt String

let b:current_syntax = ''
unlet b:current_syntax
syntax include @Cpp syntax/cpp.vim
syntax region  vbtCpp  matchgroup=vbt start="\z(@@\?\[\s*\(cpp\|cxx\)\(\|[ .-:=()][0-9a-zA-Z_ .-:=()]*\)\]\)" end="\z1" contains=@Cpp
hi link vbt String

let b:current_syntax = ''
unlet b:current_syntax
syntax include @Lua syntax/lua.vim
syntax region  vbtLua  matchgroup=vbt start="\z(@@\?\[\s*lua\(\|[ .-:=()][0-9a-zA-Z_ .-:=()]*\)\]\)" end="\z1" contains=@Lua
hi link vbt String

let b:current_syntax = ''
unlet b:current_syntax
syntax include @Html syntax/html.vim
syntax region  vbtHtml  matchgroup=vbt start="\z(@@\?\[\s*html\(\|[ .-:=()][0-9a-zA-Z_ .-:=()]*\)\]\)" end="\z1" contains=@Html
hi link vbt String

let b:current_syntax = ''
unlet b:current_syntax
syntax include @Tex syntax/tex.vim
syntax region  vbtTex  matchgroup=vbt start="\z(@@\?\[\s*tex\(\|[ .-:=()][0-9a-zA-Z_ .-:=()]*\)\]\)" end="\z1" contains=@Tex
hi link vbt String

let b:current_syntax = "dao"

" vim: noet ts=8
