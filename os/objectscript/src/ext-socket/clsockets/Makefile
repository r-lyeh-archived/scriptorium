##############################################################################
# SimpleSocket Library Makefile
###############################################################################
#
# The following variables must be set for the makefile to work correctly
# (although they can be set to NULL):
#
# TARG_DIR               - Directory of sources
# TARG_TYPE              - library or executable
# TARG_NAME              - Name of final executable/library
# TARG_OPTIONS           - options to be used for compiling all version 
# TARG_DEBUG_OPTIONS     - target specific options to use when compiling
#                          a debug version (BUILD=Debug)
# TARG_OPTIMIZED_OPTIONS - target specific options to use when compiling
#                          a non-debug execution (BUILD=Release)
# TARG_SOURCES           - Additional files used by this target (.cpp .h)
# 
##############################################################################
include ./Makefiles/Makefile.platform

TARG_BUILD_MAJOR       =1
TARG_BUILD_MINOR       =4
TARG_BUILD_VERSION     =0
TARG_DIR               = .
TARG_TYPE              = library
TARG_NAME              = clsocket
TARG_INSTALL           = /usr/local/lib

TARG_OPTIONS           = -Wall -pedantic -fPIC
TARG_DEBUG_OPTIONS     = 
TARG_OPTIMIZED_OPTIONS = 
TARG_LDFLAGS           = 

TARG_HEADERS           = ActiveSocket.h  \
			 Host.h          \
			 PassiveSocket.h \
			 SimpleSocket.h  \
			 StatTimer.h

TARG_SOURCES           = SimpleSocket.cpp  \
			 ActiveSocket.cpp  \
			 PassiveSocket.cpp 

###############################################################################
# Include a Makefile to configure the compiler, etc for the current platform
# (This is relative to the Makefile that included this Makefile!)
###############################################################################
include ./Makefiles/Makefile.macros
include ./Makefiles/Makefile.basic

install: $(TARGET)
	cp -f $(TARG_HEADERS) /usr/local/include
	cp -f $(TARGET) /usr/local/lib/$(TARGET_A)
	cp -f $(TARGET_SO) /usr/local/lib/$(TARGET_REAL_NAME)
	ln -sf /usr/local/lib/$(TARGET_REAL_NAME) /usr/local/lib/$(TARGET_SO_NAME)
	ln -sf /usr/local/lib/$(TARGET_REAL_NAME) /usr/local/lib/$(TARGET_LINKER_NAME)
	ldconfig /usr/local/lib

test:
