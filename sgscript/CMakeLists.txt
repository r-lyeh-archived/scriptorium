
cmake_minimum_required( VERSION 2.6 )

project( SGScript )

set( SGSCRIPT_FILES
	src/sgs_bcg.c
	src/sgs_ctx.c
	src/sgs_fnt.c
	src/sgs_proc.c
	src/sgs_regex.c
	src/sgs_std.c
	src/sgs_stdL.c
	src/sgs_tok.c
	src/sgs_util.c
	src/sgs_xpc.c
)
add_library( sgscript SHARED ${SGSCRIPT_FILES} )
set_target_properties( sgscript PROPERTIES PREFIX "" )
set_target_properties( sgscript PROPERTIES COMPILE_FLAGS
	"-fwrapv -Wall -Wconversion -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -static-libgcc -m32"
)
set_target_properties( sgscript PROPERTIES LINK_FLAGS "-m32" )

