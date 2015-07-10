BIN=paren

$(BIN): paren.cpp libparen.cpp libparen.h
	$(CXX) -std=c++11 -s -Wall -O3 -static -o $(BIN) paren.cpp libparen.cpp

run: $(BIN)
	./$(BIN)

clean:
	rm -f $(BIN)
