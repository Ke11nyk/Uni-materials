#include "JavaScriptLexer.h"
#include "Token.h"
#include <iostream>
#include <unordered_set>

// Main function with file input support
int main(int argc, char* argv[]) {
    std::string jsCode;
    
    // Check if filename provided as command line argument
    if (argc > 1) {
        std::string filename = argv[1];
        jsCode = JavaScriptLexer::readFileContent(filename);
        
        if (jsCode.empty()) {
            std::cerr << "Error: File is empty or could not be read." << std::endl;
            return 1;
        }
        
        std::cout << "=== Analyzing file: " << filename << " ===" << std::endl;
    } else {
        // Default JavaScript code for demonstration
        jsCode = R"(
            // Example JavaScript code with non-trivial constructs
            const greeting = `Hello, ${name}!`;
            
            const sum = (a, b) => a + b;
            
            async function fetchData() {
                const data = await fetch('/api');
                return data?.json() ?? {};
            }
            
            class Person {
                constructor(name, age) {
                    this.name = name;
                    this.age = age;
                }
            }
            
            const {name, age} = person;
            const numbers = [1, 2, ...otherNumbers];
            
            import { Component } from 'react';
            export default MyComponent;
        )";
        
        std::cout << "=== Analyzing default JavaScript code ===" << std::endl;
        std::cout << "Usage: " << argv[0] << " <filename.js>" << std::endl;
        std::cout << "Running with default code...\n" << std::endl;
    }
    
    // Create lexer and tokenize
    JavaScriptLexer lexer(jsCode);
    std::vector<Token> tokens = lexer.tokenize();
    
    std::cout << "=== JavaScript Lexer Results ===" << std::endl;
    std::cout << "Detected non-trivial tokens:" << std::endl << std::endl;
    
    // Display only non-trivial tokens
    std::unordered_set<TokenType> nontrivialTypes = {
        FUNCTION, ARROW_FUNCTION, TEMPLATE_LITERAL, SPREAD_OPERATOR,
        OPTIONAL_CHAINING, NULLISH_COALESCING, ASYNC_AWAIT, CLASS, IMPORT_EXPORT
    };
    
    for (const auto& token : tokens) {
        if (nontrivialTypes.find(token.type) != nontrivialTypes.end()) {
            std::cout << "Type: " << tokenTypeToString(token.type) 
                      << ", Value: '" << token.value 
                      << "', Line: " << token.line 
                      << ", Column: " << token.column << std::endl;
        }
    }
    
    // Check for destructuring patterns
    if (lexer.hasDestructuring(tokens)) {
        std::cout << "\nDestructuring detected in code!" << std::endl;
    }
    
    std::cout << "\n=== All tokens ===" << std::endl;
    for (const auto& token : tokens) {
        if (token.type != WHITESPACE && token.type != END_OF_FILE) {
            std::cout << tokenTypeToString(token.type) << ": '" << token.value << "'" << std::endl;
        }
    }
    
    return 0;
}