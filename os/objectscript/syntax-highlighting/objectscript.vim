" Vim syntax file
" Language:	ObjectScript
" Maintainer:
" Updaters:
" URL:
" Changes:
" Comment: Based on JavaScript syntax file
" Note: don't forget to add string 'au BufRead,BufNewFile *.os setfiletype objectscript' to your
"       filetype.vim

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
" tuning parameters:
" unlet objectScript_fold

if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  let main_syntax = 'objectscript'
elseif exists("b:current_syntax") && b:current_syntax == "objectscript"
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

" Drop fold if it set but vim doesn't support it.
if version < 600 && exists("objectScript_fold")
  unlet objectScript_fold
endif


syn keyword objectScriptCommentTodo    TODO FIXME XXX TBD contained
syn match   objectScriptLineComment    "\/\/.*" contains=@Spell,objectScriptCommentTodo
syn match   objectScriptCommentSkip    "^[ \t]*\*\($\|[ \t]\+\)"
syn region  objectScriptComment	       start="/\*"  end="\*/" contains=@Spell,objectScriptCommentTodo
syn match   objectScriptSpecial	       "\\\d\d\d\|\\."
syn match   objectScriptNestedCode     "\$" nextgroup=objectScriptNestedBlock
syn region  objectScriptNestedBlock    start=+{+ end=+}+ contained contains=objectScriptNestedBlock
syn region  objectScriptStringD	       start=+"+  skip=+\\\\\|\\"+  end=+"\|$+	contains=objectScriptSpecial,objectScriptNestedCode,@htmlPreproc
syn region  objectScriptStringS	       start=+'+  skip=+\\\\\|\\'+  end=+'\|$+	contains=objectScriptSpecial,@htmlPreproc
syn region  objectScriptStringF	       start=+`+  end=+`+

syn match   objectScriptSpecialCharacter "'\\.'"
syn match   objectScriptNumber	       "-\=\<\d\+L\=\>\|0[xX][0-9a-fA-F]\+\>"
syn match   objectScriptName	       "[_a-zA-Z]+[_0-9a-zA-Z$@]*"
syn match   objectScriptThisSugar      "@" nextgroup=objectScriptName

syn keyword objectScriptMagic           __construct __iter __get __set __del __getdim __setdim
syn keyword objectScriptMagic           __deldim __getempty __setempty __delempty
syn keyword objectScriptMagic           __cmp __rcmp __bitor __rbitor __bitand __rbitand
syn keyword objectScriptMagic           __bitxor __rbitxor __lshift __rlshift __rshift __rrshift
syn keyword objectScriptMagic           __add __radd __sub __rsub __mul __rmul __div __rdiv
syn keyword objectScriptMagic           __mod __rmod __pow __rpow __plus __minus __bitnot __len

syn keyword objectScriptConditional	if else elseif switch
syn keyword objectScriptRepeat		while for do in
syn keyword objectScriptBranch		break continue
syn keyword objectScriptOperator	delete typeof typeOf is numberOf stringOf objectOf arrayOf
syn keyword objectScriptOperator	functionOf booleanOf userdataOf typeOf require eval
syn keyword objectScriptOperator	toString toNumber toBoolean terminate extends super
syn keyword objectScriptType		Array Boolean Date Function Number Object String Regexp
syn keyword objectScriptType		RegexpException Userdata Exception
syn keyword objectScriptStatement	return with
syn keyword objectScriptBoolean		true false
syn keyword objectScriptNull		null
syn keyword objectScriptIdentifier	arguments this var local
syn keyword objectScriptLabel		case default
syn keyword objectScriptException	try catch throw
syn keyword objectScriptGlobal		echo compileFile compileText debugBackTrace
syn keyword objectScriptReserved	new instanceof undefined abstract boolean byte char class
syn keyword objectScriptReserved	const debugger double enum export final float goto
syn keyword objectScriptReserved	implements import int interface long native package private
syn keyword objectScriptReserved	protected finally public short static synchronized
syn keyword objectScriptReserved	throws transient volatile let yield

if exists("objectScript_fold")
    syn match	objectScriptFunction	"\<function\>"
    syn region	objectScriptFunctionFold	start="\<function\>.*[^};]$" end="^\z1}.*$" transparent fold keepend

    syn sync match objectScriptSync	grouphere objectScriptFunctionFold "\<function\>"
    syn sync match objectScriptSync	grouphere NONE "^}"

    setlocal foldmethod=syntax
    setlocal foldtext=getline(v:foldstart)
else
    syn keyword objectScriptFunction	function
    syn match	objectScriptBraces	   "[{}\[\]]"
    syn match	objectScriptParens	   "[()]"
endif

syn sync fromstart
syn sync maxlines=100

if main_syntax == "objectscript"
  syn sync ccomment objectScriptComment
endif

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_javascript_syn_inits")
  if version < 508
    let did_javascript_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink objectScriptComment		Comment
  HiLink objectScriptLineComment	Comment
  HiLink objectScriptCommentTodo	Todo
  HiLink objectScriptSpecial		Special
  HiLink objectScriptStringS		String
  HiLink objectScriptStringF		String
  HiLink objectScriptStringD		String
  HiLink objectScriptCharacter		Character
  HiLink objectScriptSpecialCharacter	objectScriptSpecial
  HiLink objectScriptNumber		objectScriptValue
  HiLink objectScriptConditional	Conditional
  HiLink objectScriptRepeat		Repeat
  HiLink objectScriptBranch		Conditional
  HiLink objectScriptOperator		Operator
  HiLink objectScriptType		Type
  HiLink objectScriptStatement		Statement
  HiLink objectScriptFunction		Function
  HiLink objectScriptBraces		Function
  HiLink objectScriptNull		Keyword
  HiLink objectScriptBoolean		Boolean

  HiLink objectScriptIdentifier		Identifier
  HiLink objectScriptLabel		Label
  HiLink objectScriptException		Exception
  HiLink objectScriptGlobal		Keyword
  HiLink objectScriptReserved		Error
  HiLink objectScriptDebug		Debug
  HiLink objectScriptConstant		Label

  HiLink objectScriptNestedCode		Label
  HiLink objectScriptNestedBlock	Label
  HiLink objectScriptThisSugar		Identifier
  HiLink objectScriptMagic		Keyword

  delcommand HiLink
endif

let b:current_syntax = "objectscript"
if main_syntax == 'objectscript'
  unlet main_syntax
endif
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: ts=8
