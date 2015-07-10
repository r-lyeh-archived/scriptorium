[styling]
default=default
comment=comment
commentline=comment_line
commentdoc=comment_doc
preprocessorcomment=comment
number=number_1
word=keyword_1
word2=keyword_2
string=string_1
stringraw=string_2
character=character
uuid=other
preprocessor=preprocessor
operator=operator
identifier=identifier_1
stringeol=string_eol
verbatim=string_2
regex=regex
commentlinedoc=comment_line_doc
commentdockeyword=comment_doc_keyword
commentdockeyworderror=comment_doc_keyword_error
globalclass=class
tripleverbatim=string_2
hashquotedstring=string_2

[keywords]
primary=break case const continue string default do else enum float for goto if return switch typedef void while false nil true
secondary=
docComment=

[lexer_properties]
styling.within.preprocessor=1
lexer.cpp.track.preprocessor=0
preprocessor.symbol.$(file.patterns.cpp)=#
preprocessor.start.$(file.patterns.cpp)=if ifdef ifndef
preprocessor.middle.$(file.patterns.cpp)=else elif
preprocessor.end.$(file.patterns.cpp)=endif

[settings]
extension=qc
comment_single=//
comment_open=/*
comment_close=*/
comment_use_indent=true
context_action_cmd=

[indentation]
width=4
type=0

[build_settings]
compiler=gmqcc -Wall "%f" -o "%e"
linker=
run_cmd=qcvm "./%e"
