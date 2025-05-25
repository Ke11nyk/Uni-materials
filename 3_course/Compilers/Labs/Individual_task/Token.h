#ifndef TOKEN_H
#define TOKEN_H

#include <string>

// Enumeration of all token types
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
    
    // Basic tokens
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

// Token structure containing type, value and position information
struct Token {
    TokenType type;
    std::string value;
    int line;
    int column;
    
    Token(TokenType t, const std::string& v, int l, int c);
};

// Helper function to convert token type to string representation
std::string tokenTypeToString(TokenType type);

#endif // TOKEN_H