#include "JavaScriptLexer.h"
#include <cctype>
#include <fstream>
#include <sstream>
#include <iostream>

// Constructor: initialize lexer with input code
JavaScriptLexer::JavaScriptLexer(const std::string& code) 
    : input(code), position(0), line(1), column(1) {
    initializeKeywords();
}

// Initialize JavaScript keywords set
void JavaScriptLexer::initializeKeywords() {
    keywords = {
        "abstract", "async", "await", "boolean", "break", "byte", "case", "catch",
        "char", "class", "const", "continue", "debugger", "default", "delete", "do",
        "double", "else", "enum", "export", "extends", "false", "final", "finally",
        "float", "for", "function", "goto", "if", "implements", "import", "in",
        "instanceof", "int", "interface", "let", "long", "native", "new", "null",
        "package", "private", "protected", "public", "return", "short", "static",
        "super", "switch", "synchronized", "this", "throw", "throws", "transient",
        "true", "try", "typeof", "var", "void", "volatile", "while", "with", "yield"
    };
}

// Get current character at position
char JavaScriptLexer::currentChar() {
    if (position >= input.length()) return '\0';
    return input[position];
}

// Peek at character with offset (finite automaton state transition)
char JavaScriptLexer::peekChar(int offset) {
    size_t pos = position + offset;
    if (pos >= input.length()) return '\0';
    return input[pos];
}

// Advance position and update line/column counters
void JavaScriptLexer::advance() {
    if (position < input.length()) {
        if (input[position] == '\n') {
            line++;
            column = 1;
        } else {
            column++;
        }
        position++;
    }
}

// Skip whitespace characters
void JavaScriptLexer::skipWhitespace() {
    while (std::isspace(currentChar())) {
        advance();
    }
}

// Read string literals (finite automaton for string recognition)
Token JavaScriptLexer::readString(char quote) {
    std::string value;
    int startLine = line, startColumn = column;
    
    advance(); // Skip opening quote
    
    // String FA: process characters until closing quote
    while (currentChar() != '\0' && currentChar() != quote) {
        if (currentChar() == '\\') {
            // Handle escape sequences
            advance();
            if (currentChar() != '\0') {
                value += currentChar();
                advance();
            }
        } else {
            value += currentChar();
            advance();
        }
    }
    
    if (currentChar() == quote) {
        advance(); // Skip closing quote
    }
    
    return Token(STRING, value, startLine, startColumn);
}

// Read template literals with expression interpolation (complex FA)
Token JavaScriptLexer::readTemplateLiteral() {
    std::string value;
    int startLine = line, startColumn = column;
    
    advance(); // Skip opening backtick
    
    // Template literal FA: handle ${...} expressions
    while (currentChar() != '\0' && currentChar() != '`') {
        if (currentChar() == '\\') {
            // Handle escape sequences
            advance();
            if (currentChar() != '\0') {
                value += currentChar();
                advance();
            }
        } else if (currentChar() == '$' && peekChar() == '{') {
            // Template expression FA: handle ${...} interpolation
            value += "${";
            advance(); // $
            advance(); // {
            
            int braceCount = 1;
            // Nested braces handling
            while (currentChar() != '\0' && braceCount > 0) {
                if (currentChar() == '{') braceCount++;
                else if (currentChar() == '}') braceCount--;
                
                value += currentChar();
                advance();
            }
        } else {
            value += currentChar();
            advance();
        }
    }
    
    if (currentChar() == '`') {
        advance(); // Skip closing backtick
    }
    
    return Token(TEMPLATE_LITERAL, value, startLine, startColumn);
}

// Read numeric literals (FA for number recognition including scientific notation)
Token JavaScriptLexer::readNumber() {
    std::string value;
    int startLine = line, startColumn = column;
    
    // Number FA: integer and decimal parts
    while (std::isdigit(currentChar()) || currentChar() == '.') {
        value += currentChar();
        advance();
    }
    
    // Scientific notation FA extension
    if (currentChar() == 'e' || currentChar() == 'E') {
        value += currentChar();
        advance();
        if (currentChar() == '+' || currentChar() == '-') {
            value += currentChar();
            advance();
        }
        while (std::isdigit(currentChar())) {
            value += currentChar();
            advance();
        }
    }
    
    return Token(NUMBER, value, startLine, startColumn);
}

// Read identifiers and keywords (FA for identifier recognition)
Token JavaScriptLexer::readIdentifier() {
    std::string value;
    int startLine = line, startColumn = column;
    
    // Identifier FA: alphanumeric + underscore + dollar
    while (std::isalnum(currentChar()) || currentChar() == '_' || currentChar() == '$') {
        value += currentChar();
        advance();
    }
    
    // Check for keywords and non-trivial constructs
    TokenType type = IDENTIFIER;
    
    if (keywords.find(value) != keywords.end()) {
        type = KEYWORD;
        
        // Specific non-trivial token classification
        if (value == "function") type = FUNCTION;
        else if (value == "class") type = CLASS;
        else if (value == "async" || value == "await") type = ASYNC_AWAIT;
        else if (value == "import" || value == "export") type = IMPORT_EXPORT;
    }
    
    return Token(type, value, startLine, startColumn);
}

// Read comments (FA for single-line and multi-line comments)
Token JavaScriptLexer::readComment() {
    std::string value;
    int startLine = line, startColumn = column;
    
    if (currentChar() == '/' && peekChar() == '/') {
        // Single line comment FA
        while (currentChar() != '\0' && currentChar() != '\n') {
            value += currentChar();
            advance();
        }
    } else if (currentChar() == '/' && peekChar() == '*') {
        // Multi-line comment FA
        advance(); // /
        advance(); // *
        value += "/*";
        
        // Comment end detection FA
        while (currentChar() != '\0') {
            if (currentChar() == '*' && peekChar() == '/') {
                value += "*/";
                advance(); // *
                advance(); // /
                break;
            }
            value += currentChar();
            advance();
        }
    }
    
    return Token(COMMENT, value, startLine, startColumn);
}

// Main tokenization method using finite automaton approach
std::vector<Token> JavaScriptLexer::tokenize() {
    std::vector<Token> tokens;
    
    // Main lexical analysis loop
    while (position < input.length()) {
        char ch = currentChar();
        
        // Skip whitespace
        if (std::isspace(ch)) {
            skipWhitespace();
            continue;
        }
        
        // Comment recognition FA
        if (ch == '/' && (peekChar() == '/' || peekChar() == '*')) {
            tokens.push_back(readComment());
            continue;
        }
        
        // Template literal recognition FA
        if (ch == '`') {
            tokens.push_back(readTemplateLiteral());
            continue;
        }
        
        // String literal recognition FA
        if (ch == '"' || ch == '\'') {
            tokens.push_back(readString(ch));
            continue;
        }
        
        // Number recognition FA
        if (std::isdigit(ch)) {
            tokens.push_back(readNumber());
            continue;
        }
        
        // Identifier and keyword recognition FA
        if (std::isalpha(ch) || ch == '_' || ch == '$') {
            tokens.push_back(readIdentifier());
            continue;
        }
        
        // Non-trivial operator recognition using FA
        int startLine = line, startColumn = column;
        
        // Arrow function FA: = -> =>
        if (ch == '=' && peekChar() == '>') {
            tokens.push_back(Token(ARROW_FUNCTION, "=>", startLine, startColumn));
            advance();
            advance();
            continue;
        }
        
        // Spread operator FA: . -> .. -> ...
        if (ch == '.' && peekChar() == '.' && peekChar(2) == '.') {
            tokens.push_back(Token(SPREAD_OPERATOR, "...", startLine, startColumn));
            advance();
            advance();
            advance();
            continue;
        }
        
        // Optional chaining FA: ? -> ?.
        if (ch == '?' && peekChar() == '.') {
            tokens.push_back(Token(OPTIONAL_CHAINING, "?.", startLine, startColumn));
            advance();
            advance();
            continue;
        }
        
        // Nullish coalescing FA: ? -> ??
        if (ch == '?' && peekChar() == '?') {
            tokens.push_back(Token(NULLISH_COALESCING, "??", startLine, startColumn));
            advance();
            advance();
            continue;
        }
        
        // Other operators and punctuation FA
        std::string op;
        op += ch;
        
        // Two-character operator recognition FA
        if ((ch == '=' && peekChar() == '=') ||
            (ch == '!' && peekChar() == '=') ||
            (ch == '<' && peekChar() == '=') ||
            (ch == '>' && peekChar() == '=') ||
            (ch == '&' && peekChar() == '&') ||
            (ch == '|' && peekChar() == '|') ||
            (ch == '+' && peekChar() == '+') ||
            (ch == '-' && peekChar() == '-')) {
            op += peekChar();
            advance();
        }
        
        TokenType type = (std::ispunct(ch)) ? PUNCTUATION : OPERATOR;
        tokens.push_back(Token(type, op, startLine, startColumn));
        advance();
    }
    
    tokens.push_back(Token(END_OF_FILE, "", line, column));
    return tokens;
}

// Method to detect destructuring patterns using FA approach
bool JavaScriptLexer::hasDestructuring(const std::vector<Token>& tokens) {
    for (size_t i = 0; i < tokens.size() - 2; i++) {
        if (tokens[i].value == "{" || tokens[i].value == "[") {
            // Search for destructuring pattern using FA
            for (size_t j = i + 1; j < tokens.size(); j++) {
                if (tokens[j].value == "=" && 
                    ((tokens[i].value == "{" && tokens[j-1].value == "}") ||
                    (tokens[i].value == "[" && tokens[j-1].value == "]"))) {
                    return true;
                }
            }
        }
    }
    return false;
}

// Function to read JavaScript code from file
std::string JavaScriptLexer::readFileContent(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error: Cannot open file " << filename << std::endl;
        return "";
    }
    
    std::stringstream buffer;
    buffer << file.rdbuf();
    file.close();
    
    return buffer.str();
}