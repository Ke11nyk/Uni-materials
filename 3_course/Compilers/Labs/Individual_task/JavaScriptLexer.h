#ifndef JAVASCRIPT_LEXER_H
#define JAVASCRIPT_LEXER_H

#include "Token.h"
#include <vector>
#include <unordered_set>
#include <string>

class JavaScriptLexer {
private:
    std::string input;              // Input JavaScript code
    size_t position;                // Current position in input
    int line;                       // Current line number
    int column;                     // Current column number
    
    // JavaScript keywords set for recognition
    std::unordered_set<std::string> keywords;
    
    // Private helper methods for finite automaton implementation
    char currentChar();                          // Get current character
    char peekChar(int offset = 1);              // Peek at character with offset
    void advance();                             // Advance position
    void skipWhitespace();                      // Skip whitespace characters
    
    // Token reading methods using finite automaton approach
    Token readString(char quote);               // Read string literals
    Token readTemplateLiteral();               // Read template literals with interpolation
    Token readNumber();                        // Read numeric literals
    Token readIdentifier();                    // Read identifiers and keywords
    Token readComment();                       // Read single/multi-line comments
    
    // Initialize keywords set
    void initializeKeywords();

public:
    // Constructor: initialize lexer with input code
    explicit JavaScriptLexer(const std::string& code);
    
    // Main tokenization method using finite automaton approach
    std::vector<Token> tokenize();
    
    // Method to detect destructuring patterns using FA approach
    bool hasDestructuring(const std::vector<Token>& tokens);
    
    // Static utility methods
    static std::string readFileContent(const std::string& filename);
};

#endif // JAVASCRIPT_LEXER_H