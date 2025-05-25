#include "Token.h"

// Token constructor
Token::Token(TokenType t, const std::string& v, int l, int c) 
    : type(t), value(v), line(l), column(c) {}

// Helper function to convert token type to string representation
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