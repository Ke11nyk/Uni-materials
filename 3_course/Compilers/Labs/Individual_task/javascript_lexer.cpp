#include <iostream>
#include <string>
#include <vector>
#include <cctype>
#include <unordered_set>
#include <unordered_map>

enum TokenType {
    // Non-trivial tokens for detection
    FUNCTION,           // function
    ARROW_FUNCTION,     // =>
    TEMPLATE_LITERAL,   // `template string`
    DESTRUCTURING,      // { a, b } = obj
    SPREAD_OPERATOR,    // ...
    OPTIONAL_CHAINING,  // ?.
    NULLISH_COALESCING, // ??
    ASYNC_AWAIT,        // async/await
    CLASS,              // class
    IMPORT_EXPORT,      // import/export
    
    // Base tokens
    IDENTIFIER,
    NUMBER,
    STRING,
    OPERATOR,
    PUNCTUATION,
    KEYWORD,
    COMMENT,
    WHITESPACE,
    END_OF_FILE,
    UNKNOWN
};

struct Token {
    TokenType type;
    std::string value;
    int line;
    int column;
    
    Token(TokenType t, const std::string& v, int l, int c) 
        : type(t), value(v), line(l), column(c) {}
};

class JavaScriptLexer {
private:
    std::string input;
    size_t position;
    int line;
    int column;
    
    // Key words of JavaScript
    std::unordered_set<std::string> keywords = {
        "abstract", "async", "await", "boolean", "break", "byte", "case", "catch",
        "char", "class", "const", "continue", "debugger", "default", "delete", "do",
        "double", "else", "enum", "export", "extends", "false", "final", "finally",
        "float", "for", "function", "goto", "if", "implements", "import", "in",
        "instanceof", "int", "interface", "let", "long", "native", "new", "null",
        "package", "private", "protected", "public", "return", "short", "static",
        "super", "switch", "synchronized", "this", "throw", "throws", "transient",
        "true", "try", "typeof", "var", "void", "volatile", "while", "with", "yield"
    };
    
    char currentChar() {
        if (position >= input.length()) return '\0';
        return input[position];
    }
    
    char peekChar(int offset = 1) {
        size_t pos = position + offset;
        if (pos >= input.length()) return '\0';
        return input[pos];
    }
    
    void advance() {
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
    
    void skipWhitespace() {
        while (std::isspace(currentChar())) {
            advance();
        }
    }
    
    Token readString(char quote) {
        std::string value;
        int startLine = line, startColumn = column;
        
        advance(); // Skip opening quote
        
        while (currentChar() != '\0' && currentChar() != quote) {
            if (currentChar() == '\\') {
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
    
    Token readTemplateLiteral() {
        std::string value;
        int startLine = line, startColumn = column;
        
        advance(); // Skip opening backtick
        
        while (currentChar() != '\0' && currentChar() != '`') {
            if (currentChar() == '\\') {
                advance();
                if (currentChar() != '\0') {
                    value += currentChar();
                    advance();
                }
            } else if (currentChar() == '$' && peekChar() == '{') {
                // Handle template expression ${...}
                value += "${";
                advance(); // $
                advance(); // {
                
                int braceCount = 1;
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
    
    Token readNumber() {
        std::string value;
        int startLine = line, startColumn = column;
        
        while (std::isdigit(currentChar()) || currentChar() == '.') {
            value += currentChar();
            advance();
        }
        
        // Handle scientific notation
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
    
    Token readIdentifier() {
        std::string value;
        int startLine = line, startColumn = column;
        
        while (std::isalnum(currentChar()) || currentChar() == '_' || currentChar() == '$') {
            value += currentChar();
            advance();
        }
        
        // Checking for keywords and non-trivial constructs
        TokenType type = IDENTIFIER;
        
        if (keywords.find(value) != keywords.end()) {
            type = KEYWORD;
            
            // Specific non-trivial tokens
            if (value == "function") type = FUNCTION;
            else if (value == "class") type = CLASS;
            else if (value == "async" || value == "await") type = ASYNC_AWAIT;
            else if (value == "import" || value == "export") type = IMPORT_EXPORT;
        }
        
        return Token(type, value, startLine, startColumn);
    }
    
    Token readComment() {
        std::string value;
        int startLine = line, startColumn = column;
        
        if (currentChar() == '/' && peekChar() == '/') {
            // Single line comment
            while (currentChar() != '\0' && currentChar() != '\n') {
                value += currentChar();
                advance();
            }
        } else if (currentChar() == '/' && peekChar() == '*') {
            // Multi-line comment
            advance(); // /
            advance(); // *
            value += "/*";
            
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
    
public:
    JavaScriptLexer(const std::string& code) 
        : input(code), position(0), line(1), column(1) {}
    
    std::vector<Token> tokenize() {
        std::vector<Token> tokens;
        
        while (position < input.length()) {
            char ch = currentChar();
            
            if (std::isspace(ch)) {
                skipWhitespace();
                continue;
            }
            
            // Comments
            if (ch == '/' && (peekChar() == '/' || peekChar() == '*')) {
                tokens.push_back(readComment());
                continue;
            }
            
            // Template literals
            if (ch == '`') {
                tokens.push_back(readTemplateLiteral());
                continue;
            }
            
            // Strings
            if (ch == '"' || ch == '\'') {
                tokens.push_back(readString(ch));
                continue;
            }
            
            // Numbers
            if (std::isdigit(ch)) {
                tokens.push_back(readNumber());
                continue;
            }
            
            // Identifiers and keywords
            if (std::isalpha(ch) || ch == '_' || ch == '$') {
                tokens.push_back(readIdentifier());
                continue;
            }
            
            // Non-trivial operators
            int startLine = line, startColumn = column;
            
            // Arrow function =>
            if (ch == '=' && peekChar() == '>') {
                tokens.push_back(Token(ARROW_FUNCTION, "=>", startLine, startColumn));
                advance();
                advance();
                continue;
            }
            
            // Spread operator ...
            if (ch == '.' && peekChar() == '.' && peekChar(2) == '.') {
                tokens.push_back(Token(SPREAD_OPERATOR, "...", startLine, startColumn));
                advance();
                advance();
                advance();
                continue;
            }
            
            // Optional chaining ?.
            if (ch == '?' && peekChar() == '.') {
                tokens.push_back(Token(OPTIONAL_CHAINING, "?.", startLine, startColumn));
                advance();
                advance();
                continue;
            }
            
            // Nullish coalescing ??
            if (ch == '?' && peekChar() == '?') {
                tokens.push_back(Token(NULLISH_COALESCING, "??", startLine, startColumn));
                advance();
                advance();
                continue;
            }
            
            // Other operators and punctuation
            std::string op;
            op += ch;
            
            // Two-character operators
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
    
    // Methods for detecting non-trivial structures
    bool hasDestructuring(const std::vector<Token>& tokens) {
        for (size_t i = 0; i < tokens.size() - 2; i++) {
            if (tokens[i].value == "{" || tokens[i].value == "[") {
                // Finding a destructuring pattern
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
};

// Helper function for deriving token type
std::string tokenTypeToString(TokenType type) {
    switch (type) {
        case FUNCTION: return "FUNCTION";
        case ARROW_FUNCTION: return "ARROW_FUNCTION";
        case TEMPLATE_LITERAL: return "TEMPLATE_LITERAL";
        case DESTRUCTURING: return "DESTRUCTURING";
        case SPREAD_OPERATOR: return "SPREAD_OPERATOR";
        case OPTIONAL_CHAINING: return "OPTIONAL_CHAINING";
        case NULLISH_COALESCING: return "NULLISH_COALESCING";
        case ASYNC_AWAIT: return "ASYNC_AWAIT";
        case CLASS: return "CLASS";
        case IMPORT_EXPORT: return "IMPORT_EXPORT";
        case IDENTIFIER: return "IDENTIFIER";
        case NUMBER: return "NUMBER";
        case STRING: return "STRING";
        case OPERATOR: return "OPERATOR";
        case PUNCTUATION: return "PUNCTUATION";
        case KEYWORD: return "KEYWORD";
        case COMMENT: return "COMMENT";
        case WHITESPACE: return "WHITESPACE";
        case END_OF_FILE: return "END_OF_FILE";
        default: return "UNKNOWN";
    }
}

int main() {
    std::string jsCode = R"(
        // Example of JavaScript code with non-trivial constructs
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
    
    JavaScriptLexer lexer(jsCode);
    std::vector<Token> tokens = lexer.tokenize();
    
    std::cout << "=== JavaScript Lexer Results ===" << std::endl;
    std::cout << "Non-trivial tokens detected:" << std::endl << std::endl;
    
    // Виведення тільки нетривіальних токенів
    std::unordered_set<TokenType> nontrivialTypes = {
        FUNCTION, ARROW_FUNCTION, TEMPLATE_LITERAL, SPREAD_OPERATOR,
        OPTIONAL_CHAINING, NULLISH_COALESCING, ASYNC_AWAIT, CLASS, IMPORT_EXPORT
    };
    
    for (const auto& token : tokens) {
        if (nontrivialTypes.find(token.type) != nontrivialTypes.end()) {
            std::cout << "Type: " << tokenTypeToString(token.type) 
                      << ", Value: '" << token.value 
                      << "', Row: " << token.line 
                      << ", Column: " << token.column << std::endl;
        }
    }
    
    // Перевірка на деструктуризацію
    if (lexer.hasDestructuring(tokens)) {
        std::cout << "\nDestructuring detected in the code!" << std::endl;
    }
    
    std::cout << "\n=== All tokens ===" << std::endl;
    for (const auto& token : tokens) {
        if (token.type != WHITESPACE && token.type != END_OF_FILE) {
            std::cout << tokenTypeToString(token.type) << ": '" << token.value << "'" << std::endl;
        }
    }
    
    return 0;
}