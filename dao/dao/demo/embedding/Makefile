
CXX = g++ # -ggdb -fPIC -I. -shared

# UNIX, MAC_OSX, or WIN32 must be defined for corresponding OS.
CFLG = -DMAC_OSX -DDAO_DIRECT_API
CFLG = -ggdb -DMAC_OSX -DDAO_DIRECT_API

WRAP = dao_greeting.o dao_greeting2.o dao_greeting3.o

all: greeting.o main.o $(WRAP)
	$(CXX) greeting.o main.o $(WRAP) -o greeting -ldao -ldl

test: greeting.o main.o $(WRAP)
	$(CXX) greeting.o main.o $(WRAP) ../../dao.a -o greeting


fake_list: fake_list.o
	$(CXX) fake_list.o -o fake_list -ldao -ldl

fake_list.o: fake_list.cpp
	$(CXX) $(CFLG) -c fake_list.cpp -o fake_list.o


fake_number: fake_number.o
	$(CXX) fake_number.o -o fake_number -ldao -ldl

fake_number.o: fake_number.cpp
	$(CXX) $(CFLG) -c fake_number.cpp -o fake_number.o


greeting.o: greeting.h greeting.cpp
	$(CXX) $(CFLG) -c greeting.cpp -o greeting.o

main.o: main.cpp greeting.h
	$(CXX) $(CFLG) -c main.cpp -o main.o

dao_greeting.o: dao_greeting.h dao_greeting.cpp
	$(CXX) $(CFLG) -c dao_greeting.cpp -o dao_greeting.o

dao_greeting2.o: dao_greeting.h dao_greeting2.cpp
	$(CXX) $(CFLG) -c dao_greeting2.cpp -o dao_greeting2.o

dao_greeting3.o: dao_greeting.h dao_greeting3.cpp
	$(CXX) $(CFLG) -c dao_greeting3.cpp -o dao_greeting3.o

bind:
	autobind.dao greeting.h lang=cpp wrap_name=greeting del_tests="Greeting@self!=GetGreetingObject()"

local-bind:
	../../dao ../../tools/autobind.dao greeting.h lang=cpp wrap_name=greeting

bind-valgrind:
	valgrind --tool=memcheck --leak-check=full --dsymutil=yes ../../dao ../../tools/autobind.dao greeting.h lang=cpp wrap_name=greeting

clean:
	rm *.o
