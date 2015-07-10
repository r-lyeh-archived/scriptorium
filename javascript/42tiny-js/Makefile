
GCC_GTEQ_430 := $(shell expr `gcc -dumpversion | sed -e 's/\.\([0-9][0-9]\)/\1/g' -e 's/\.\([0-9]\)/0\1/g' -e 's/^[0-9]\{3,4\}$$/&00/'` \>= 40300)
GCC_GTEQ_470 := $(shell expr `gcc -dumpversion | sed -e 's/\.\([0-9][0-9]\)/\1/g' -e 's/\.\([0-9]\)/0\1/g' -e 's/^[0-9]\{3,4\}$$/&00/'` \>= 40700)

CC      = g++ 
CFLAGS  = -c -g -Wall -rdynamic
LDFLAGS = -g -rdynamic
LIBS    = -lpthread -L $(CURDIR) -l42tiny-js

ifeq "$(GCC_GTEQ_470)" "1"
CFLAGS += -std=c++11
else ifeq "$(GCC_GTEQ_430)" "1"
CFLAGS += -std=c++0x
endif

ifeq "$(DEBUG)" "1"
CFLAGS += -D_DEBUG
endif


ifeq "$(NO_POOL_ALLOCATOR)" "1"
CFLAGS += -DNO_POOL_ALLOCATOR
endif

ifeq "$(NO_REGEXP)" "1"
CFLAGS += -DNO_REGEXP
else ifeq "$(HAVE_TR1_REGEX)" "1"
CFLAGS += -DHAVE_TR1_REGEX
else ifeq "$(HAVE_BOOST_REGEX)" "1"
CFLAGS += -DHAVE_BOOST_REGEX
else ifeq "$(HAVE_CXX_REGEX)" "1"
CFLAGS += -DHAVE_CXX_REGEX
endif

ifeq "$(WITH_TIME_LOGGER)" "1"
CFLAGS += -DWITH_TIME_LOGGER
endif

SOURCES=  \
	TinyJS.cpp \
	pool_allocator.cpp \
	TinyJS_Functions.cpp \
	TinyJS_MathFunctions.cpp \
	TinyJS_StringFunctions.cpp \
	TinyJS_DateFunctions.cpp \
	TinyJS_Threading.cpp

OBJECTS=$(SOURCES:.cpp=.o)

all:  run_tests Script

run_tests: lib42tiny-js.a run_tests.o
	@echo $@
	@$(CC) $(LDFLAGS) run_tests.o $(LIBS) -o $@

Script: lib42tiny-js.a Script.o
	@echo $@
	@$(CC) $(LDFLAGS) Script.o $(LIBS) -o $@

lib42tiny-js.a: $(OBJECTS)

#-------------- rules --------------

.cpp.o:
	@echo $<
	@$(CC) -MMD -MP -MF $*.dep $(CFLAGS) $< -o $@

-include *.dep

clean:
	@echo "clean"
	@rm -f run_tests Script run_tests.o Script.o $(OBJECTS) *.dep

%.a:
	@echo $(notdir $@)
	@rm -f $@
	$(AR) -rc $@ $^

