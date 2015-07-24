HEADERS = libraries/jog/jog.h libraries/ref_counted.h libraries/string_builder.h libraries/array_list.h
INCLUDE_PATH = -I libraries -I libraries/jog

all: ./jog run

./jog: build/jog.o build/jog_scanner.o build/jog_analyzer.o build/jog_parser.o build/jog_vm.o build/jog_native.o build/test.o
	g++ -Wall build/test.o build/jog.o build/jog_scanner.o build/jog_analyzer.o build/jog_parser.o build/jog_vm.o build/jog_native.o -o jog

build/jog.o: libraries/jog/jog.cpp $(HEADERS)
	mkdir -p build
	g++ -Wall $(INCLUDE_PATH) libraries/jog/jog.cpp -c -o build/jog.o

build/jog_scanner.o: libraries/jog/jog_scanner.cpp $(HEADERS)
	mkdir -p build
	g++ -Wall $(INCLUDE_PATH) libraries/jog/jog_scanner.cpp -c -o build/jog_scanner.o

build/jog_analyzer.o: libraries/jog/jog_analyzer.cpp $(HEADERS)
	mkdir -p build
	g++ -Wall $(INCLUDE_PATH) libraries/jog/jog_analyzer.cpp -c -o build/jog_analyzer.o

build/jog_parser.o: libraries/jog/jog_parser.cpp $(HEADERS)
	mkdir -p build
	g++ -Wall $(INCLUDE_PATH) libraries/jog/jog_parser.cpp -c -o build/jog_parser.o

build/jog_vm.o: libraries/jog/jog_vm.cpp $(HEADERS)
	mkdir -p build
	g++ -Wall $(INCLUDE_PATH) libraries/jog/jog_vm.cpp -c -o build/jog_vm.o

build/jog_native.o: libraries/jog/jog_native.cpp $(HEADERS)
	mkdir -p build
	g++ -Wall $(INCLUDE_PATH) libraries/jog/jog_native.cpp -c -o build/jog_native.o

build/test.o: test.cpp $(HEADERS)
	mkdir -p build
	g++ -Wall $(INCLUDE_PATH) test.cpp -c -o build/test.o

run:
	./jog

