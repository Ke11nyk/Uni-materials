# Compiler and flags
CXX = g++
CXXFLAGS = -Wall -Wextra -g3 -std=c++11

# Target executable name
TARGET = javascript_lexer

# Object files
OBJECTS = main.o JavaScriptLexer.o Token.o

# Default target
all: $(TARGET)

# Build the main executable
$(TARGET): $(OBJECTS)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(OBJECTS)

# Compile main.cpp
main.o: main.cpp JavaScriptLexer.h Token.h
	$(CXX) $(CXXFLAGS) -c main.cpp

# Compile JavaScriptLexer.cpp
JavaScriptLexer.o: JavaScriptLexer.cpp JavaScriptLexer.h Token.h
	$(CXX) $(CXXFLAGS) -c JavaScriptLexer.cpp

# Compile Token.cpp
Token.o: Token.cpp Token.h
	$(CXX) $(CXXFLAGS) -c Token.cpp

# Clean up build files
clean:
	rm -f $(OBJECTS) $(TARGET)

# Clean and rebuild
rebuild: clean all

# Install target (optional)
install: $(TARGET)
	cp $(TARGET) /usr/local/bin/

# Run with test file
test: $(TARGET)
	echo 'const greet = (name) => `Hello, $${name}!`; class User { async getData() { return data?.result ?? null; } } const {id, ...rest} = user;' > test.js
	./$(TARGET) test.js
	rm -f test.js

# Help target
help:
	@echo "Available targets:"
	@echo "  all       - Build the lexer (default)"
	@echo "  clean     - Remove build files"
	@echo "  rebuild   - Clean and rebuild"
	@echo "  install   - Install to /usr/local/bin"
	@echo "  test      - Run with a test file"
	@echo "  help      - Show this help message"

.PHONY: all clean rebuild install test help