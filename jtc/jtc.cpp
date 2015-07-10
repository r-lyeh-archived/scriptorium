#include <iostream>
#include <fstream>
#include <cctype>
#include <cmath>
#include <string>
#include <vector>
#include <array>
#include <functional>
#include <unordered_map>
#include <stdexcept>
#include <memory>

// utility

template<class T>
bool isint(T x) {
    return x == std::trunc(x);
}

std::string unescape(const std::string &str) {
    std::string result;
    for(auto i = str.begin();i!=str.end();++i) {
        if(*i == '\\') {
            switch(*(++i)) {
                case 'a': result += '\a'; break;
                case 'b': result += '\b'; break;
                case 'f': result += '\f'; break;
                case 'n': result += '\n'; break;
                case 'r': result += '\r'; break;
                case 't': result += '\t'; break;
                case 'v': result += '\v'; break;
                case '\'': result += '\''; break;
                case '\"': result += '\"'; break;
            }
        } else {
            result += *i;
        }
    }
    return result;
}

enum TokenType {
    NUMBER, IDENTIFIER, STRING, LEFT_PAREN, RIGHT_PAREN, LEFT_BRACKET,
    RIGHT_BRACKET, LEFT_BRACE, RIGHT_BRACE, PLUS, DASH, STAR, PERCENT,
    SLASH, PLUS_EQUAL, DASH_EQUAL, STAR_EQUAL, PERCENT_EQUAL,
    SLASH_EQUAL,NOT, COMMA, SEMICOLON, DOT, COLON, EQUAL, EQ_OP,
    NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, LOGICAL_AND,
    LOGICAL_OR, IF, ELSE, WHILE, FOR, RETURN, BREAK, CONTINUE, FUNCTION,
    EXPRESSION, STATEMENT, ROOT, NIL, EVAL, PARSE_STATEMENT,
    PARSE_EXPRESSION, GLOBAL, LOCAL, PRINT, TYPE, SIZE
};

const std::unordered_map<std::string, TokenType> keywords = {
    {"if", IF}, {"else", ELSE}, {"while", WHILE}, {"for", FOR},
    {"return", RETURN}, {"break", BREAK}, {"continue", CONTINUE},
    {"function", FUNCTION}, {"nil", NIL}, {"expression", EXPRESSION},
    {"statement", STATEMENT}, {"eval", EVAL},
    {"parse_statement", PARSE_STATEMENT},
    {"parse_expression", PARSE_EXPRESSION}, {"global", GLOBAL},
    {"local", LOCAL}, {"root", ROOT}, {"type", TYPE}, {"size", SIZE}
};

template<class Iter>
struct Token {
    Token(TokenType type_, Iter begin_, Iter end_)
        : type(type_), begin(begin_), end(end_) { }
    const TokenType type;
    const Iter begin, end;
};

// helper to find line & column position on demand
template<class Iter>
std::pair<int, int> line_column(Iter begin, Iter pos) {
    int line = 1, column = 0;
    for(Iter i = begin;i!=pos;++i) {
        if(*i == '\n'){
            ++line;
            column = 0;
        }
        ++column;
    }
    return std::make_pair(line, column);
}

template<class Iter>
std::shared_ptr<std::vector<Token<Iter>>> tokenize(Iter begin, Iter end)
{
    auto tokens_ptr = std::make_shared<std::vector<Token<Iter>>>();
    std::vector<Token<Iter>> &tokens = *tokens_ptr;
    Iter i = begin;
    while(i!=end)
    {
        Iter start = i;
        switch(*i)
        {
            case ' ': case '\t': case '\n': // ignore whitespaces
                ++i; break;
            case '(':
                tokens.emplace_back(LEFT_PAREN, i, i+1); ++i; break;
            case ')':
                tokens.emplace_back(RIGHT_PAREN, i, i+1); ++i; break;
            case '[':
                tokens.emplace_back(LEFT_BRACKET, i, i+1); ++i; break;
            case ']':
                tokens.emplace_back(RIGHT_BRACKET, i, i+1); ++i; break;
            case '{':
                tokens.emplace_back(LEFT_BRACE, i, i+1); ++i; break;
            case '}':
                tokens.emplace_back(RIGHT_BRACE, i, i+1); ++i; break;
            case ',':
                tokens.emplace_back(COMMA, i, i+1); ++i; break;
            case '.':
                tokens.emplace_back(DOT, i, i+1); ++i; break;
            case ';':
                tokens.emplace_back(SEMICOLON, i, i+1); ++i; break;
            case ':':
                tokens.emplace_back(COLON, i, i+1); ++i; break;
            case '+':
                if(i+1 != end && *(i+1) == '='){
                    tokens.emplace_back(PLUS_EQUAL, i, i+2); i+=2;
                } else {
                    tokens.emplace_back(PLUS, i, i+1); ++i;
                }
                break;
            case '-':
                if(i+1 != end && *(i+1) == '='){
                    tokens.emplace_back(DASH_EQUAL, i, i+2); i+=2;
                } else {
                    tokens.emplace_back(DASH, i, i+1); ++i;
                }
                break;
            case '*':
                if(i+1 != end && *(i+1) == '='){
                    tokens.emplace_back(STAR_EQUAL, i, i+2); i+=2;
                } else {
                    tokens.emplace_back(STAR, i, i+1); ++i;
                }
                break;
            case '%':
                if(i+1 != end && *(i+1) == '='){
                    tokens.emplace_back(PERCENT_EQUAL, i, i+2); i+=2;
                } else {
                    tokens.emplace_back(PERCENT, i, i+1); ++i;
                }
                break;
            case '/':
                if(i+1 != end && *(i+1) == '/') {
                    ++i; while(i!=end && *i != '\n') ++i; ++i;
                } else if(i+1 != end && *(i+1) == '='){
                    tokens.emplace_back(SLASH_EQUAL, i, i+2); i+=2;
                } else {
                    tokens.emplace_back(SLASH, i, i+1); ++i;
                }
                break;
            case '<':
                if(i+1 != end && *(i+1) == '='){
                    tokens.emplace_back(LESS_EQUAL, i, i+2); i+=2;
                } else {
                    tokens.emplace_back(LESS, i, i+1); ++i;
                }
                break;
            case '>':
                if(i+1 != end && *(i+1) == '='){
                    tokens.emplace_back(GREATER_EQUAL, i, i+2); i+=2;
                } else {
                    tokens.emplace_back(GREATER, i, i+1); ++i;
                }
                break;
            case '=':
                if(i+1 != end && *(i+1) == '='){
                    tokens.emplace_back(EQ_OP, i, i+2); i+=2;
                } else {
                    tokens.emplace_back(EQUAL, i, i+1); ++i;
                }
                break;
            case '!':
                if(i+1 != end && *(i+1) == '='){
                    tokens.emplace_back(NOT_EQUAL, i, i+2); i+=2;
                } else {
                    tokens.emplace_back(NOT, i, i+1); i+=2;
                }
                break;
            case '&':
                if(i+1 != end && *(i+1) == '&'){
                    tokens.emplace_back(LOGICAL_AND, i, i+2); i+=2;
                } else {
                    auto lc = line_column(begin, i);
                    throw std::runtime_error(std::to_string(lc.first)+":"+std::to_string(lc.second)+": unrecognized symbol: '" + *i + "'");
                }
                break;
            case '|':
                if(i+1 != end && *(i+1) == '|'){
                    tokens.emplace_back(LOGICAL_OR, i, i+2); i+=2;
                } else {
                    auto lc = line_column(begin, i);
                    throw std::runtime_error(std::to_string(lc.first)+":"+std::to_string(lc.second)+": unrecognized symbol: '" + *i + "'");
                }
                break;
            case '"':
                ++i;
                while(i!=end && !(*i == '"' && *(i-1) != '\\')) ++i;
                ++i;
                tokens.emplace_back(STRING, start, i);
                break;
            default:
                if(std::isdigit(*i)) {
                    while(i!=end && std::isdigit(*i)) ++i;
                    if(i!=end && *i=='.') {
                        ++i;
                        while(i!=end && std::isdigit(*i)) ++i;
                    }
                    if(i!=end && *i=='e') {
                        ++i;
                        if(i!=end && *i=='-') ++i;
                        while(i!=end && std::isdigit(*i)) ++i;
                    }
                    tokens.emplace_back(NUMBER, start, i);
                    break;
                } else if(std::isalpha(*i) || *i == '_') {
                    ++i;
                    while(i!=end && (std::isalnum(*i) || *i == '_')) ++i;
                    auto kw = keywords.find(std::string(start, i));
                    if(kw == keywords.end())
                        tokens.emplace_back(IDENTIFIER, start, i);
                    else
                        tokens.emplace_back(kw->second, start, i);
                    break;
                } else {
                    auto lc = line_column(begin, i);
                    throw std::runtime_error(std::to_string(lc.first)+":"+std::to_string(lc.second)+": unrecognized symbol: '" + *i + "'");
                }
        }
    }
    return tokens_ptr;
}

// Abstract Syntax Tree node types
template<class Iter, class R, class F>
struct ASTNode : public std::enable_shared_from_this<ASTNode<Iter,R,F>>{
    Iter begin, end;
    virtual R accept(F&) = 0;
    virtual std::shared_ptr<ASTNode<Iter,R,F>>& operator[](size_t) = 0;
    virtual size_t size() const = 0;
    virtual void inject_dependencies() = 0;
    std::shared_ptr<std::string> source;
    std::shared_ptr<std::vector<Token<std::string::iterator>>> tokens;
    std::weak_ptr<ASTNode<Iter,R,F>> root;
};

template<class Iter, class R, class F>
struct Variadic : public ASTNode<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
    std::vector<std::shared_ptr<ASTNode<Iter,R,F>>> children;
    virtual std::shared_ptr<ASTNode<Iter,R,F>>& operator[](size_t i) { return children[i]; }
    virtual size_t size() const { return children.size(); }
    virtual void inject_dependencies() {
        for(auto& child : children) {
            child->root = this->root;
            child->source = this->source;
            child->tokens = this->tokens;
            child->inject_dependencies();
        }
    }
};

template<class Iter, class R, class F>
struct Ternary : public ASTNode<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
    std::array<std::shared_ptr<ASTNode<Iter,R,F>>, 3> children;
    virtual std::shared_ptr<ASTNode<Iter,R,F>>& operator[](size_t i) { return children[i]; }
    virtual size_t size() const { return children.size(); }
    virtual void inject_dependencies() {
        for(auto& child : children) {
            child->root = this->root;
            child->source = this->source;
            child->tokens = this->tokens;
            child->inject_dependencies();
        }
    }
};

template<class Iter, class R, class F>
struct Binary : public ASTNode<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
    std::array<std::shared_ptr<ASTNode<Iter,R,F>>, 2> children;
    virtual std::shared_ptr<ASTNode<Iter,R,F>>& operator[](size_t i) { return children[i]; }
    virtual size_t size() const { return children.size(); }
    virtual void inject_dependencies() {
        for(auto& child : children) {
            child->root = this->root;
            child->source = this->source;
            child->tokens = this->tokens;
            child->inject_dependencies();
        }
    }
};

template<class Iter, class R, class F>
struct Unary : public ASTNode<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
    std::array<std::shared_ptr<ASTNode<Iter,R,F>>, 1> children;
    virtual std::shared_ptr<ASTNode<Iter,R,F>>& operator[](size_t i) { return children[i]; }
    virtual size_t size() const { return children.size(); }
    virtual void inject_dependencies() {
        for(auto& child : children) {
            child->root = this->root;
            child->source = this->source;
            child->tokens = this->tokens;
            child->inject_dependencies();
        }
    }
};

template<class Iter, class R, class F>
struct Nullary : public ASTNode<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
    std::array<std::shared_ptr<ASTNode<Iter,R,F>>, 0> children;
    virtual std::shared_ptr<ASTNode<Iter,R,F>>& operator[](size_t i) { return children[i]; }
    virtual size_t size() const { return children.size(); }
    virtual void inject_dependencies() {
        for(auto& child : children) {
            child->root = this->root;
            child->source = this->source;
            child->tokens = this->tokens;
            child->inject_dependencies();
        }
    }
};

template<class Iter, class R, class F>
struct Variable : public Nullary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
    R name;
};

template<class Iter, class R, class F>
struct GlobalVariable : public Variable<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct LocalVariable : public Variable<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct FieldName : public Nullary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
    R name;
};

template<class Iter, class R, class F>
struct Nop : public Nullary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
    double value;
};

template<class Iter, class R, class F>
struct Constant : public Nullary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
    R value;
};

template<class Iter, class R, class F>
struct Nil : public Nullary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct String : public Nullary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
    R value;
};

template<class Iter, class R, class F>
struct FieldAccess : public Binary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct FieldAssignment : public Ternary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct AddFieldAssignment : public FieldAssignment<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct SubFieldAssignment : public FieldAssignment<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct MulFieldAssignment : public FieldAssignment<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct DivFieldAssignment : public FieldAssignment<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct ModFieldAssignment : public FieldAssignment<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct Call : public Variadic<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct MemberCall : public Variadic<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct EvalStatement : public Unary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct Eval : public Unary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct ParseStatement : public Unary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct ParseExpression : public Unary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct UnaryExpression : public Unary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct UnaryPlusExpression : public UnaryExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct UnaryMinusExpression : public UnaryExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct UnaryNotExpression : public UnaryExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct MultiplicativeExpression : public Binary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct MultiplyExpression : public MultiplicativeExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct DivideExpression : public MultiplicativeExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct ModuloExpression : public MultiplicativeExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct AdditiveExpression : public Binary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct AddExpression : public AdditiveExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct SubtractExpression : public AdditiveExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct RelationalExpression : public Binary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct LessExpression : public RelationalExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct LessEqualExpression : public RelationalExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct GreaterExpression : public RelationalExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct GreaterEqualExpression : public RelationalExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct EqualityExpression : public Binary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct EqualExpression : public EqualityExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct NotEqualExpression : public EqualityExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct LogicalAndExpression : public Binary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct LogicalOrExpression : public Binary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct AssignmentExpression : public Binary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct GlobalAssignmentExpression : public AssignmentExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct LocalAssignmentExpression : public AssignmentExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct AddAssignmentExpression : public AssignmentExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct SubAssignmentExpression : public AssignmentExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct MulAssignmentExpression : public AssignmentExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct DivAssignmentExpression : public AssignmentExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct ModAssignmentExpression : public AssignmentExpression<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct InitializerAssignmentExpression : public Variadic<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct Block : public Unary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct TableInitializer : public Variadic<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct ArrayInitializer : public Variadic<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct IdentifierList : public Variadic<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct Function : public Binary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct Tree : public Unary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct Root : public Variadic<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct Type : public Unary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct Size : public Unary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct StatementList : public Variadic<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct IfStatement : public Variadic<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct WhileStatement : public Binary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct ForStatement : public Variadic<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct ForEachStatement : public Variadic<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct ReturnStatement : public Variadic<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct ContinueStatement : public Nullary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct BreakStatement : public Nullary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct ExpressionStatement : public Unary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class Iter, class R, class F>
struct BuiltinFunction : public Nullary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
    std::function<void(std::vector<R>&)> function;
};

template<class Iter, class R, class F>
struct Parens : public Unary<Iter,R,F> {
    virtual R accept(F &f) { return f(*this); }
};

template<class R, class F>
class Parser {
public:
    typedef std::vector<Token<std::string::iterator>>::iterator Iter;
    typedef std::shared_ptr<ASTNode<Iter,R,F>> return_t;

    template<class StrIter>
    return_t operator()(StrIter sbegin, StrIter send)
    {
        auto source = std::make_shared<std::string>(sbegin, send);
        auto tokens = tokenize(source->begin(), source->end());
        this->begin = tokens->begin();
        this->end = tokens->end();
        i = this->begin;
        accepted = this->end;
        auto node = statement_list();
        node->source = source;
        node->tokens = tokens;
        node->root = node;
        node->inject_dependencies();
        return node;
    }

    template<class StrIter>
    return_t parse_expression(StrIter sbegin, StrIter send)
    {
        auto source = std::make_shared<std::string>(sbegin, send);
        auto tokens = tokenize(source->begin(), source->end());
        this->begin = tokens->begin();
        this->end = tokens->end();
        i = this->begin;
        accepted = this->end;
        auto node = expression();
        node->source = source;
        node->tokens = tokens;
        node->root = node;
        node->inject_dependencies();
        return node;
    }

private:
    Iter i, accepted;
    Iter begin;
    Iter end;

    void advance() {
        if(i==end) {
            Iter last = end-1;
            auto lc = line_column(begin->begin, last->end);
            throw std::runtime_error(std::to_string(lc.first)+":"+std::to_string(lc.second)+": unexpected end of input at");
        } else {
            ++i;
        }
    }

    void rewind(Iter pos) {
        i = pos;
    }

    bool peek(TokenType t, int amount = 0) {
        return i+amount != end && (i+amount)->type == t;
    }

    bool accept(TokenType t) {
        if(i != end && i->type == t) {
            accepted = i; advance();
            return true;
        } else {
            return false;
        }
    }
    bool end_of_input() {
        return i == end;
    }

    bool expect(TokenType t) {
        if(accept(t)) {
            return true;
        } else {
            i = std::min(i, end-1);
            auto lc = line_column(begin->begin, i->begin);
            throw std::runtime_error(std::to_string(lc.first)+":"+std::to_string(lc.second)+": unexpected token '" + std::string(i->begin, i->end) + "'");
            return false;
        }
    }

    void expect(const std::string &expected) {
        i = std::min(i, end-1);
        auto lc = line_column(begin->begin, i->begin);
        throw std::runtime_error(std::to_string(lc.first)+":"+std::to_string(lc.second)+": expected " + expected+ " but got '" + std::string(i->begin, i->end) + "'");
    }

    return_t variable() {
        expect(IDENTIFIER);
        std::shared_ptr<Variable<Iter,R,F>> node = std::make_shared<Variable<Iter,R,F>>();
        node->begin = accepted;
        node->end = accepted+1;
        node->name = R(accepted->begin, accepted->end);
        return std::move(node);
    }

    return_t field_name() {
        expect(IDENTIFIER);
        std::shared_ptr<FieldName<Iter,R,F>> node = std::make_shared<FieldName<Iter,R,F>>();
        node->begin = accepted;
        node->end = accepted+1;
        node->name = R(accepted->begin, accepted->end);
        return std::move(node);
    }

    return_t identifier_list() {
        std::shared_ptr<IdentifierList<Iter,R,F>> node = std::make_shared<IdentifierList<Iter,R,F>>();
        node->begin = i;
        expect(LEFT_PAREN);
        while(!accept(RIGHT_PAREN)) {
            node->children.push_back(field_name());
            if(!peek(RIGHT_PAREN))
                accept(COMMA);
        }
        node->end = accepted+1;
        return std::move(node);
    }

    return_t function() {
        std::shared_ptr<Function<Iter,R,F>> node = std::make_shared<Function<Iter,R,F>>();
        node->begin = i;
        expect(FUNCTION);
        node->children[0] = identifier_list();
        node->children[1] = statement();
        node->end = accepted+1;
        return std::move(node);
    }

    return_t parse_expression() {
        std::shared_ptr<ParseExpression<Iter,R,F>> node = std::make_shared<ParseExpression<Iter,R,F>>();
        node->begin = i;
        expect(PARSE_EXPRESSION);
        expect(LEFT_PAREN);
        node->children[0] = expression();
        expect(RIGHT_PAREN);
        node->end = accepted+1;
        return std::move(node);
    }

    return_t parse_statement() {
        std::shared_ptr<ParseStatement<Iter,R,F>> node = std::make_shared<ParseStatement<Iter,R,F>>();
        node->begin = i;
        expect(PARSE_STATEMENT);
        expect(LEFT_PAREN);
        node->children[0] = expression();
        expect(RIGHT_PAREN);
        node->end = accepted+1;
        return std::move(node);
    }

    return_t tree() {
        std::shared_ptr<Tree<Iter,R,F>> node = std::make_shared<Tree<Iter,R,F>>();
        node->begin = i;
        if(accept(EXPRESSION)) {
            expect(LEFT_PAREN);
            node->children[0] = expression();
            expect(RIGHT_PAREN);
        } else {
            expect(STATEMENT);
            expect(LEFT_PAREN);
            node->children[0] = statement();
            expect(RIGHT_PAREN);
        }
        node->end = accepted+1;
        return std::move(node);
    }

    return_t root() {
        std::shared_ptr<Root<Iter,R,F>> node = std::make_shared<Root<Iter,R,F>>();
        node->begin = i;
        expect(ROOT);
        if(accept(LEFT_PAREN)) {
            node->children.push_back(expression());
            expect(RIGHT_PAREN);
        }
        node->end = accepted+1;
        return std::move(node);
    }

    return_t eval() {
        std::shared_ptr<Eval<Iter,R,F>> node = std::make_shared<Eval<Iter,R,F>>();
        node->begin = i;
        expect(EVAL);
        expect(LEFT_PAREN);
        node->children[0] = unary_expression();
        expect(RIGHT_PAREN);
        node->end = accepted+1;
        return std::move(node);
    }

    return_t type() {
        std::shared_ptr<Type<Iter,R,F>> node = std::make_shared<Type<Iter,R,F>>();
        node->begin = i;
        expect(TYPE);
        if(accept(LEFT_PAREN)) {
            node->children[0] = expression();
            expect(RIGHT_PAREN);
        }
        node->end = accepted+1;
        return std::move(node);
    }

    return_t size() {
        std::shared_ptr<Size<Iter,R,F>> node = std::make_shared<Size<Iter,R,F>>();
        node->begin = i;
        expect(SIZE);
        if(accept(LEFT_PAREN)) {
            node->children[0] = expression();
            expect(RIGHT_PAREN);
        }
        node->end = accepted+1;
        return std::move(node);
    }

    return_t table_initializer() {
        std::shared_ptr<TableInitializer<Iter,R,F>> node = std::make_shared<TableInitializer<Iter,R,F>>();
        node->begin = i;
        expect(LEFT_BRACE);
        while(!accept(RIGHT_BRACE)) {
            node->children.push_back(initializer_assignment_expression());
            if(!peek(RIGHT_BRACE))
                accept(COMMA);
        }
        node->end = accepted+1;
        return std::move(node);
    }

    return_t array_initializer() {
        std::shared_ptr<ArrayInitializer<Iter,R,F>> node = std::make_shared<ArrayInitializer<Iter,R,F>>();
        node->begin = i;
        expect(LEFT_BRACKET);
        while(!accept(RIGHT_BRACKET)) {
            node->children.push_back(logical_or_expression());
            if(!peek(RIGHT_BRACKET))
                accept(COMMA);
        }
        node->end = accepted+1;
        return std::move(node);
    }

    return_t primary_expression() {
        if (peek(IDENTIFIER) || peek(GLOBAL) || peek(LOCAL)) {
            return variable();
        } else if (accept(NUMBER)) {
            std::shared_ptr<Constant<Iter,R,F>> node = std::make_shared<Constant<Iter,R,F>>();
            node->begin = accepted;
            node->end = accepted+1;
            node->value = std::stod(std::string(accepted->begin, accepted->end));
            return std::move(node);
        } else if (accept(NIL)) {
            std::shared_ptr<Nil<Iter,R,F>> node = std::make_shared<Nil<Iter,R,F>>();
            node->begin = accepted;
            node->end = accepted+1;
            return std::move(node);
        } else if (accept(STRING)) {
            std::shared_ptr<String<Iter,R,F>> node = std::make_shared<String<Iter,R,F>>();
            node->begin = accepted;
            node->end = accepted+1;
            node->value = R(unescape(std::string(accepted->begin+1, accepted->end-1)));
            return std::move(node);
        } else if (accept(LEFT_PAREN)) {
            std::shared_ptr<Parens<Iter,R,F>> node = std::make_shared<Parens<Iter,R,F>>();
            node->begin = accepted;
            node->children[0] = expression();
            expect(RIGHT_PAREN);
            node->end = accepted+1;
            return std::move(node);
        } else if (peek(LEFT_BRACE)) {
            return table_initializer();
        } else if (peek(LEFT_BRACKET)) {
            return array_initializer();
        } else if (peek(EXPRESSION) || peek(STATEMENT)) {
            return tree();
        } else if (peek(ROOT)) {
            return root();
        } else if (peek(TYPE)) {
            return type();
        } else if (peek(EVAL)) {
            return eval();
        } else if (peek(SIZE)) {
            return size();
        } else if (peek(PARSE_EXPRESSION)) {
            return parse_expression();
        } else if (peek(PARSE_STATEMENT)) {
            return parse_statement();
        } else if (peek(FUNCTION)) {
            return function();
        } else {
            expect("primary expression");
            return nullptr;
        }
    }

    return_t postfix_expression() {
        Iter from = i;
        return_t left = primary_expression();
        while(true) {
            if(accept(LEFT_BRACKET)) {
                return_t right = expression();
                expect(RIGHT_BRACKET);

                if(peek(EQUAL) || peek(PLUS_EQUAL) || peek(DASH_EQUAL) ||
                        peek(STAR_EQUAL) || peek(SLASH_EQUAL) || peek(PERCENT_EQUAL)) {

                    std::shared_ptr<FieldAssignment<Iter,R,F>> node;
                    if(accept(EQUAL)) {
                        node = std::make_shared<FieldAssignment<Iter,R,F>>();
                    } else if(accept(PLUS_EQUAL)) {
                        node = std::make_shared<AddFieldAssignment<Iter,R,F>>();
                    } else if(accept(DASH_EQUAL)) {
                        node = std::make_shared<SubFieldAssignment<Iter,R,F>>();
                    } else if(accept(STAR_EQUAL)) {
                        node = std::make_shared<MulFieldAssignment<Iter,R,F>>();
                    } else if(accept(SLASH_EQUAL)) {
                        node = std::make_shared<DivFieldAssignment<Iter,R,F>>();
                    } else if(accept(PERCENT_EQUAL)) {
                        node = std::make_shared<ModFieldAssignment<Iter,R,F>>();
                    }
                    node->begin = from;
                    node->children[0] = std::move(left);
                    node->children[1] = std::move(right);
                    node->children[2] = expression();
                    node->end = accepted+1;
                    left = std::move(node);
                } else {
                    std::shared_ptr<FieldAccess<Iter,R,F>> node = std::make_shared<FieldAccess<Iter,R,F>>();
                    node->begin = from;
                    node->children[0] = std::move(left);
                    node->children[1] = std::move(right);
                    node->end = accepted+1;
                    left = std::move(node);
                }
            } else if(accept(DOT)) {
                return_t right = field_name();
                if(peek(EQUAL) || peek(PLUS_EQUAL) || peek(DASH_EQUAL) ||
                        peek(STAR_EQUAL) || peek(SLASH_EQUAL) || peek(PERCENT_EQUAL)) {

                    std::shared_ptr<FieldAssignment<Iter,R,F>> node;
                    if(accept(EQUAL)) {
                        node = std::make_shared<FieldAssignment<Iter,R,F>>();
                    } else if(accept(PLUS_EQUAL)) {
                        node = std::make_shared<AddFieldAssignment<Iter,R,F>>();
                    } else if(accept(DASH_EQUAL)) {
                        node = std::make_shared<SubFieldAssignment<Iter,R,F>>();
                    } else if(accept(STAR_EQUAL)) {
                        node = std::make_shared<MulFieldAssignment<Iter,R,F>>();
                    } else if(accept(SLASH_EQUAL)) {
                        node = std::make_shared<DivFieldAssignment<Iter,R,F>>();
                    } else if(accept(PERCENT_EQUAL)) {
                        node = std::make_shared<ModFieldAssignment<Iter,R,F>>();
                    }
                    node->begin = from;
                    node->children[0] = std::move(left);
                    node->children[1] = std::move(right);
                    node->children[2] = expression();
                    node->end = accepted+1;
                    left = std::move(node);
                } else if(accept(LEFT_PAREN)) {
                    std::shared_ptr<MemberCall<Iter,R,F>> node = std::make_shared<MemberCall<Iter,R,F>>();
                    node->begin = from;
                    node->children.push_back(std::move(left));
                    node->children.push_back(std::move(right));
                    while(!accept(RIGHT_PAREN)) {
                        node->children.push_back(expression());
                        if(!peek(RIGHT_PAREN))
                            accept(COMMA);
                    }
                    node->end = accepted+1;
                    left = std::move(node);
                } else {
                    std::shared_ptr<FieldAccess<Iter,R,F>> node = std::make_shared<FieldAccess<Iter,R,F>>();
                    node->begin = from;
                    node->children[0] = std::move(left);
                    node->children[1] = std::move(right);
                    node->end = accepted+1;
                    left = std::move(node);
                }
            } else if(accept(LEFT_PAREN)) {
                std::shared_ptr<Call<Iter,R,F>> node = std::make_shared<Call<Iter,R,F>>();
                node->begin = from;
                node->children.push_back(std::move(left));
                while(!accept(RIGHT_PAREN)) {
                    node->children.push_back(expression());
                    if(!peek(RIGHT_PAREN))
                        accept(COMMA);
                }
                node->end = accepted+1;
                left = std::move(node);
            } else {
                return std::move(left);
            }
        }
    }

    return_t unary_expression() {
        if(accept(PLUS) || accept(DASH) || accept(NOT)) {
            typedef std::shared_ptr<UnaryExpression<Iter,R,F>> node_t;
            node_t node;
            switch(accepted->type) {
                case PLUS: node = node_t(new UnaryPlusExpression<Iter,R,F>); break;
                case DASH: node = node_t(new UnaryMinusExpression<Iter,R,F>); break;
                case NOT: node = node_t(new UnaryNotExpression<Iter,R,F>); break;
                default: break;
            }
            node->begin = accepted;
            node->children[0] = postfix_expression();
            node->end = accepted+1;
            return std::move(node);
        } else {
            return postfix_expression();
        }
    }

    return_t multiplicative_expression() {
        Iter from = i;
        return_t left = unary_expression();
        if(accept(STAR) || accept(SLASH) || accept(PERCENT)) {
            typedef std::shared_ptr<MultiplicativeExpression<Iter,R,F>> node_t;
            node_t node;
            switch(accepted->type) {
                case STAR: node = node_t(new MultiplyExpression<Iter,R,F>); break;
                case SLASH: node = node_t(new DivideExpression<Iter,R,F>); break;
                case PERCENT: node = node_t(new ModuloExpression<Iter,R,F>); break;
                default: break;
            }
            node->begin = from;
            node->children[0] = std::move(left);
            node->children[1] = multiplicative_expression();
            node->end = accepted+1;
            return std::move(node);
        } else {
            return std::move(left);
        }
    }

    return_t additive_expression() {
        Iter from = i;
        return_t left = multiplicative_expression();
        if(accept(PLUS) || accept(DASH)) {
            typedef std::shared_ptr<AdditiveExpression<Iter,R,F>> node_t;
            node_t node;
            switch(accepted->type) {
                case PLUS: node = node_t(new AddExpression<Iter,R,F>); break;
                case DASH: node = node_t(new SubtractExpression<Iter,R,F>); break;
                default: break;
            }
            node->begin = from;
            node->children[0] = std::move(left);
            node->children[1] = additive_expression();
            node->end = accepted+1;
            return std::move(node);
        } else {
            return std::move(left);
        }
    }

    return_t relational_expression() {
        Iter from = i;
        return_t left = additive_expression();
        if(accept(LESS) || accept(LESS_EQUAL) || accept(GREATER) || accept(GREATER_EQUAL)) {
            typedef std::shared_ptr<RelationalExpression<Iter,R,F>> node_t;
            node_t node;
            switch(accepted->type) {
                case LESS: node = node_t(new LessExpression<Iter,R,F>); break;
                case LESS_EQUAL: node = node_t(new LessEqualExpression<Iter,R,F>); break;
                case GREATER: node = node_t(new GreaterExpression<Iter,R,F>); break;
                case GREATER_EQUAL: node = node_t(new GreaterEqualExpression<Iter,R,F>); break;
                default: break;
            }
            node->begin = from;
            node->children[0] = std::move(left);
            node->children[1] = relational_expression();
            node->end = accepted+1;
            return std::move(node);
        } else {
            return std::move(left);
        }
    }

    return_t equality_expression() {
        Iter from = i;
        return_t left = relational_expression();
        if(accept(EQ_OP) || accept(NOT_EQUAL)) {
            typedef std::shared_ptr<EqualityExpression<Iter,R,F>> node_t;
            node_t node;
            switch(accepted->type) {
                case EQ_OP: node = node_t(new EqualExpression<Iter,R,F>); break;
                case NOT_EQUAL: node = node_t(new NotEqualExpression<Iter,R,F>); break;
                default: break;
            }
            node->begin = from;
            node->children[0] = std::move(left);
            node->children[1] = relational_expression();
            node->end = accepted+1;
            return std::move(node);
        } else {
            return std::move(left);
        }
    }

    return_t logical_and_expression() {
        Iter from = i;
        return_t left = equality_expression();
        if(accept(LOGICAL_AND)) {
            std::shared_ptr<LogicalAndExpression<Iter,R,F>> node = std::make_shared<LogicalAndExpression<Iter,R,F>>();
            node->begin = from;
            node->children[0] = std::move(left);
            node->children[1] = equality_expression();
            node->end = accepted+1;
            return std::move(node);
        } else {
            return std::move(left);
        }
    }

    return_t logical_or_expression() {
        Iter from = i;
        return_t left = logical_and_expression();
        if(accept(LOGICAL_OR)) {
            std::shared_ptr<LogicalOrExpression<Iter,R,F>> node = std::make_shared<LogicalOrExpression<Iter,R,F>>();
            node->begin = from;
            node->children[0] = std::move(left);
            node->children[1] = logical_and_expression();
            node->end = accepted+1;
            return std::move(node);
        } else {
            return std::move(left);
        }
    }

    return_t assignment_expression() {
        Iter from = i;
        if(peek(GLOBAL) || peek(LOCAL))
        {
            bool global = accept(GLOBAL);
            bool local = accept(LOCAL);
            return_t left = field_name();
            if(accept(EQUAL)) {
                std::shared_ptr<AssignmentExpression<Iter,R,F>> node;
                node = global?
                        std::make_shared<GlobalAssignmentExpression<Iter,R,F>>():
                        local?
                            std::make_shared<LocalAssignmentExpression<Iter,R,F>>():
                            std::make_shared<AssignmentExpression<Iter,R,F>>();
                node->begin = from;
                node->children[0] = std::move(left);
                node->children[1] = assignment_expression();
                node->end = accepted+1;
                return std::move(node);
            }
        } else if(peek(IDENTIFIER)) {
            return_t left = field_name();
            std::shared_ptr<AssignmentExpression<Iter,R,F>> node;
            if(accept(EQUAL)) {
                node = std::make_shared<AssignmentExpression<Iter,R,F>>();
            } else if(accept(PLUS_EQUAL)) {
                node = std::make_shared<AddAssignmentExpression<Iter,R,F>>();
            } else if(accept(DASH_EQUAL)) {
                node = std::make_shared<SubAssignmentExpression<Iter,R,F>>();
            } else if(accept(STAR_EQUAL)) {
                node = std::make_shared<MulAssignmentExpression<Iter,R,F>>();
            } else if(accept(SLASH_EQUAL)) {
                node = std::make_shared<DivAssignmentExpression<Iter,R,F>>();
            } else if(accept(PERCENT_EQUAL)) {
                node = std::make_shared<ModAssignmentExpression<Iter,R,F>>();
            } else {
                rewind(from);
                return logical_or_expression();
            }
            node->begin = from;
            node->children[0] = std::move(left);
            node->children[1] = assignment_expression();
            node->end = accepted+1;
            return std::move(node);
        }
        rewind(from);
        return logical_or_expression();
    }

    return_t initializer_assignment_expression() {
        std::shared_ptr<InitializerAssignmentExpression<Iter,R,F>> node = std::make_shared<InitializerAssignmentExpression<Iter,R,F>>();
        node->begin = i;
        node->children.push_back(logical_or_expression());
        if(accept(EQUAL)) {
            node->children.push_back(logical_or_expression());
        }
        node->end = accepted+1;
        return std::move(node);
    }

    return_t expression() {
        return assignment_expression();
    }

    return_t block() {
        std::shared_ptr<Block<Iter,R,F>> node = std::make_shared<Block<Iter,R,F>>();
        node->begin = i;
        expect(LEFT_BRACE);
        node->children[0] = statement_list();
        expect(RIGHT_BRACE);
        node->end = accepted+1;
        return std::move(node);
    }

    return_t if_statement() {
        std::shared_ptr<IfStatement<Iter,R,F>> node = std::make_shared<IfStatement<Iter,R,F>>();
        node->begin = i;
        expect(IF);
        expect(LEFT_PAREN);
        node->children.push_back(expression());
        expect(RIGHT_PAREN);
        node->children.push_back(statement());
        if(accept(ELSE)) {
            node->children.push_back(statement());
        }
        node->end = accepted+1;
        return std::move(node);
    }

    return_t while_statement() {
        std::shared_ptr<WhileStatement<Iter,R,F>> node = std::make_shared<WhileStatement<Iter,R,F>>();
        node->begin = i;
        expect(WHILE);
        expect(LEFT_PAREN);
        node->children[0] = expression();
        expect(RIGHT_PAREN);
        node->children[1] = statement();
        node->end = accepted+1;
        return std::move(node);
    }

    return_t nop() {
        std::shared_ptr<Nop<Iter,R,F>> node = std::make_shared<Nop<Iter,R,F>>();
        node->begin = i;
        node->end = i;
        return std::move(node);
    }

    return_t for_statement() {
        Iter from = i;
        expect(FOR);
        expect(LEFT_PAREN);
        if(peek(IDENTIFIER) && (peek(COMMA, 1) || peek(COLON, 1))) {
            std::shared_ptr<ForEachStatement<Iter,R,F>> node = std::make_shared<ForEachStatement<Iter,R,F>>();
            node->begin = from;
            node->children.push_back(field_name());
            if(accept(COMMA)) {
                node->children.push_back(field_name());
            }
            expect(COLON);
            node->children.push_back(expression());
            expect(RIGHT_PAREN);
            node->children.push_back(statement());
            node->end = accepted+1;
            return std::move(node);
        } else {
            std::shared_ptr<ForStatement<Iter,R,F>> node = std::make_shared<ForStatement<Iter,R,F>>();
            node->begin = from;
            if(accept(SEMICOLON)) {
                node->children.push_back(nop());
            } else {
                node->children.push_back(expression());
                expect(SEMICOLON);
            }
            if(accept(SEMICOLON)) {
                node->children.push_back(nop());
            } else {
                node->children.push_back(expression());
                expect(SEMICOLON);
            }
            if(accept(RIGHT_PAREN)) {
                node->children.push_back(nop());
            } else {
                node->children.push_back(expression());
                expect(RIGHT_PAREN);
            }
            node->children.push_back(statement());
            node->end = accepted+1;
            return std::move(node);
        }
    }

    return_t break_statement() {
        std::shared_ptr<BreakStatement<Iter,R,F>> node = std::make_shared<BreakStatement<Iter,R,F>>();
        node->begin = i;
        expect(BREAK);
        accept(SEMICOLON);
        node->end = accepted+1;
        return std::move(node);
    }

    return_t continue_statement() {
        std::shared_ptr<ContinueStatement<Iter,R,F>> node = std::make_shared<ContinueStatement<Iter,R,F>>();
        node->begin = i;
        expect(CONTINUE);
        accept(SEMICOLON);
        node->end = accepted+1;
        return std::move(node);
    }

    return_t return_statement() {
        std::shared_ptr<ReturnStatement<Iter,R,F>> node = std::make_shared<ReturnStatement<Iter,R,F>>();
        node->begin = i;
        expect(RETURN);
        if(accept(SEMICOLON)) {
            node->end = accepted+1;
            return std::move(node);
        }
        node->children.push_back(expression());
        accept(SEMICOLON);
        node->end = accepted+1;
        return std::move(node);
    }

    return_t statement() {
        if(peek(LEFT_BRACE)) {
            return block();
        } else if (peek(IF)) {
            return if_statement();
        } else if (peek(WHILE)) {
            return while_statement();
        } else if (peek(FOR)) {
            return for_statement();
        } else if (peek(RETURN)) {
            return return_statement();
        } else if (peek(CONTINUE)) {
            return continue_statement();
        } else if (peek(BREAK)) {
            return break_statement();
        } else {
            std::shared_ptr<ExpressionStatement<Iter,R,F>> node = std::make_shared<ExpressionStatement<Iter,R,F>>();
            node->begin = i;
            node->children[0] = expression();
            accept(SEMICOLON);
            node->end = accepted+1;        
            return std::move(node);
        }
    }

    return_t statement_list() {
        std::shared_ptr<StatementList<Iter,R,F>> node = std::make_shared<StatementList<Iter,R,F>>();
        node->begin = i;
        while(!end_of_input() && !peek(RIGHT_BRACE)) {
            node->children.push_back(statement());
        }
        node->end = accepted+1;
        return std::move(node);
    }
};

template<class T>
class Table;

template<class T>
class Array;

template<class Iter, class F>
struct Value {
    typedef std::string string_t;
    typedef string_t::iterator str_iter;
    struct proto_string_t {
        proto_string_t(str_iter begin_, str_iter end_)
        : begin(begin_), end(end_), hash(std::hash<string_t>()(string_t(begin_, end_)))
        { }
        str_iter begin, end; size_t hash;
    };
    typedef double number_t;
    typedef std::shared_ptr<ASTNode<Iter,Value,F>> pointer_t;
    typedef std::shared_ptr<Function<Iter,Value,F>> function_t;
    typedef pointer_t tree_t;
    typedef std::shared_ptr<Array<Value>> array_t;
    typedef std::shared_ptr<Table<Value>> table_t;
    class return_tag { };
    class break_tag { };
    class continue_tag { };
    enum Type {NIL, NUMBER, STRING, PROTO_STRING, TREE, FUNCTION, TABLE, ARRAY, RETURN, BREAK, CONTINUE};
private:
    Type type_;
    union {
        number_t number_;
        pointer_t pointer_;
        table_t table_;
        array_t array_;
        string_t str_;
        proto_string_t proto_str_;
    };
public:
    Type type() const {
        if(type_ == PROTO_STRING)
            return STRING;
        else if(type_>=RETURN && type_ <= CONTINUE)
            return NIL;
        return type_;
    }
    Type internal_type() const { return type_; }

    const number_t& number() const { return number_; }
    number_t& number() { return number_; }

    const pointer_t& pointer() const { return pointer_; }
    pointer_t& pointer() { return pointer_; }

    const function_t function() const { return std::static_pointer_cast<Function<Iter,Value,F>>(pointer_); }
    function_t function() { return std::static_pointer_cast<Function<Iter,Value,F>>(pointer_); }

    const tree_t tree() const { return pointer_; }
    tree_t tree() { return pointer_; }

    const table_t& table() const { return table_; }
    table_t& table() { return table_; }

    const string_t string() const {
        if(type_==PROTO_STRING)
            return std::string(proto_str_.begin, proto_str_.end);
        return str_;
    }
    string_t& string() { decay(); return str_; }

    const array_t& array() const { return array_; }
    array_t& array() { return array_; }

    void decay() {
        if(type_ == PROTO_STRING) {
            str_iter tmpbegin = proto_str_.begin;
            str_iter tmpend = proto_str_.end;
            type_ = STRING;
            new (&str_) string_t(tmpbegin, tmpend);
        } else if(type_>=RETURN && type_ <= CONTINUE) {
            type_ = NIL;
        }
    }

    Value() : type_(NIL), number_(0) { }
    Value(const return_tag&) : type_(RETURN), number_(0) { }
    Value(const break_tag&) : type_(BREAK), number_(0) { }
    Value(const continue_tag&) : type_(CONTINUE), number_(0) { }
    Value(double value) : type_(NUMBER), number_(value) { }
    Value(Type t, const pointer_t &ptr) : type_(t), pointer_(ptr) { }
    Value(const table_t &ptr) : type_(TABLE), table_(ptr) { }
    Value(const array_t &ptr) : type_(ARRAY), array_(ptr) { }
    Value(const string_t &value) : type_(STRING), str_(value) { }
    Value(str_iter begin, str_iter end) : type_(PROTO_STRING), proto_str_(begin, end) { }

    Value(const Value &that) : type_(that.type_) {
        switch(type_) {
            case RETURN: case BREAK: case CONTINUE: case NIL:
                new (&number_) number_t(0); break;
            case NUMBER: new (&number_) number_t(that.number_); break;
            case STRING: new (&str_) string_t(that.str_); break;
            case PROTO_STRING: new (&proto_str_) proto_string_t(that.proto_str_); break;
            case TREE: case FUNCTION:
                new (&pointer_) pointer_t(that.pointer_); break;
            case TABLE: new (&table_) table_t(that.table_); break;
            case ARRAY: new (&array_) array_t(that.array_); break;
            default: break;
        }
    }

    Value& operator=(const Value &that) {
        switch(type_) {
            case STRING: str_.~string_t(); break;
            case TREE: case FUNCTION: pointer_.~pointer_t(); break;
            case TABLE: table_.~table_t(); break;
            case ARRAY: array_.~array_t(); break;
            default: break;
        }
        type_ = that.type_;
        switch(type_) {
            case RETURN: case BREAK: case CONTINUE: case NIL:
                new (&number_) number_t(0); break;
            case NUMBER: new (&number_) number_t(that.number_); break;
            case STRING: new (&str_) string_t(that.str_); break;
            case PROTO_STRING: new (&proto_str_) proto_string_t(that.proto_str_); break;
            case TREE: case FUNCTION:
                new (&pointer_) pointer_t(that.pointer_); break;
            case TABLE: new (&table_) table_t(that.table_); break;
            case ARRAY: new (&array_) array_t(that.array_); break;
            default: break;
        }
        return *this;
    }

    ~Value() {
        switch(type_) {
            case STRING: str_.~string_t(); break;
            case TREE: case FUNCTION: pointer_.~pointer_t(); break;
            case TABLE: table_.~table_t(); break;
            case ARRAY: array_.~array_t(); break;
            default: break;
        }
    }

    bool operator==(const Value &that) const {

        if(type() != that.type())
            return false;

        switch(type_) {
            case RETURN: case BREAK: case CONTINUE: case NIL: case NUMBER:
                return number() == that.number();
            case TREE: case FUNCTION:
                return pointer() == that.pointer();
            case TABLE:
                return table() == that.table();
            case ARRAY:
                return array() == that.array();
            case STRING:
                if(that.type_ == STRING) {
                    return str_ == that.str_;
                } else {
                    if(str_.end()-str_.begin() != that.proto_str_.end - that.proto_str_.begin) return false;
                    return std::equal(str_.begin(), str_.end(), that.proto_str_.begin);
                }
            case PROTO_STRING:
                if(that.type_ == STRING) {
                    if(proto_str_.end - proto_str_.begin != that.str_.end() - that.str_.begin()) return false;
                    return std::equal(proto_str_.begin, proto_str_.end, that.str_.begin());
                } else {
                    if(proto_str_.end - proto_str_.begin != that.proto_str_.end - that.proto_str_.begin) return false;
                    return std::equal(proto_str_.begin, proto_str_.end, that.proto_str_.begin);
                }
            default: return false;
        }
    }

    friend struct std::hash<Value<Iter, F>>;
};

namespace std {
    template<class Iter, class F>
    struct hash<Value<Iter, F>> {
    public:
        typedef Value<Iter, F> T;
        std::size_t operator()(Value<Iter, F> const& x) const
        {
            switch(x.type_) {
                case T::NIL: case T::NUMBER:
                    return std::hash<typename T::number_t>()(x.number());
                case T::STRING:
                    return std::hash<typename T::string_t>()(x.string());
                case T::PROTO_STRING:
                    return x.proto_str_.hash;
                case T::TREE: case T::FUNCTION:
                    return std::hash<typename T::pointer_t>()(x.pointer());
                case T::TABLE:
                    return std::hash<typename T::table_t>()(x.table());
                case T::ARRAY:
                    return std::hash<typename T::array_t>()(x.array());
                default: return 0;
            }
        }
    };
}

template<class Iter, class F>
std::ostream& operator<<(std::ostream &out, const Value<Iter,F> &v) {
    typedef Value<Iter,F> T;
    switch(v.type()) {
        case T::NIL: return out << "nil";
        case T::NUMBER:
            return out << v.number();
        case T::STRING:
            return out << v.string();
        case T::TREE:
            return out << "tree: " << v.tree();
        case T::FUNCTION:
            return out << "function: " << v.function();
        case T::TABLE:
            return out << "table: " << v.table();
        case T::ARRAY:
            return out << "array: " << v.array();
        default: return out;
    }
}

template<class T>
class Table {
public:
    typedef typename std::unordered_map<T, T>::iterator iterator;
    void set(T key, T value) {
        if(key.type() == T::NIL) return;
        if(value.type() == T::NIL) data.erase(key);
        data[key] = value;
    }
    T get(T key) {
        auto iter = data.find(key);
        if(iter == data.end()) return nil;
        else return iter->second;
    }
    void append(const Table &that) { data.insert(that.data.begin(), that.data.end()); }
    size_t size() const { return data.size(); }
    iterator begin() { return data.begin(); }
    iterator end() { return data.end(); }
private:
    std::unordered_map<T, T> data;
    T nil;
};

template<class T>
class Array {
public:
    typedef typename std::vector<T>::iterator iterator;
    void set(T key, T value) {
        int index = key.number();
        data[index] = value;
    }
    T get(T key) {
        int index = key.number();
        return data[index];
    }
    void append(const Array &that) { data.insert(data.end(), that.data.begin(), that.data.end()); }
    void add(const T &value) { data.push_back(value); }
    size_t size() const { return data.size(); }
    iterator begin() { return data.begin(); }
    iterator end() { return data.end(); }
private:
    std::vector<T> data;
    T nil;
};

struct Interpreter {
    typedef std::vector<Token<std::string::iterator>>::iterator Iter;
    typedef Interpreter F;
    typedef Value<Iter,F> R;

    struct FunctionScope {
        FunctionScope(Interpreter &ev_) : ev(ev_) { ev.beginFunctionScope(); }
        ~FunctionScope() { ev.endFunctionScope(); }
        Interpreter &ev;
    };

    struct LocalScope {
        LocalScope(Interpreter &ev_) : ev(ev_) { ev.beginLocalScope(); }
        ~LocalScope() { ev.endLocalScope(); }
        Interpreter &ev;
    };

    R operator()(Variable<Iter,R,F> &node) {
        return get(node.name);
    }

    R operator()(FieldName<Iter,R,F> &node) {
        return node.name;
    }

    R operator()(Constant<Iter,R,F> &node) {
        return node.value;
    }

    R operator()(Nop<Iter,R,F> &) {
        return R(1.0);
    }

    R operator()(Nil<Iter,R,F> &) {
        return R();
    }

    R operator()(String<Iter,R,F> &node) {
        return node.value;
    }

    R operator()(Parens<Iter,R,F> &node) {
        return node.children[0]->accept(*this);
    }

    R operator()(ExpressionStatement<Iter,R,F> &node) {
        return node.children[0]->accept(*this);
    }

    R operator()(Type<Iter,R,F> &node) {
        R expr = node.children[0]->accept(*this);
        switch(expr.type()) {
            case R::STRING: return R(std::string("string"));
            case R::NUMBER: return R(std::string("number"));
            case R::TREE: return R(std::string("tree"));
            case R::FUNCTION: return R(std::string("function"));
            case R::TABLE: return R(std::string("table"));
            case R::ARRAY: return R(std::string("array"));
            case R::NIL: return R(std::string("nil"));
            default: error("unknown type", node); return R();
        }
    }

    R operator()(Size<Iter,R,F> &node) {
        R expr = node.children[0]->accept(*this);
        switch(expr.type()) {
            case R::STRING: return R(expr.string().size());
            case R::NUMBER: return R(1);
            case R::TREE: return R(expr.pointer()->size());
            case R::FUNCTION: return R(expr.pointer()->size());
            case R::TABLE: return R(expr.table()->size());
            case R::ARRAY: return R(expr.array()->size());
            case R::NIL: return R(0);
            default: error("unknown type", node); return R();
        }
    }

    R operator()(FieldAccess<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        if(left.type() == R::TABLE) {
            R right = node.children[1]->accept(*this);
            return left.table()->get(right);
        } else if(left.type() == R::ARRAY) {
            R right = node.children[1]->accept(*this);
            if(right.type() != R::NUMBER || !isint(right.number())) error("array index not a integer.", node);
            int idx = right.number();
            if(idx<0 || idx>=int(left.array()->size())) error("array index out of range.", node);
            return left.array()->get(right);
        } else {
            error("can only index tables or arrays.", node);
            return R();
        }
    }

    R operator()(FieldAssignment<Iter,R,F> &node) {
        R table = node.children[0]->accept(*this);
        if(table.type() == R::TABLE) {
            R index = node.children[1]->accept(*this);
            R value = node.children[2]->accept(*this);
            table.table()->set(index, value);
            return value;
        } else if(table.type() == R::ARRAY) {
            R index = node.children[1]->accept(*this);
            if(index.type() != R::NUMBER || !isint(index.number())) error("array index not a integer.", node);
            int idx = index.number();
            if(idx<0 || idx>=int(table.array()->size())) error("array index out of range.", node);
            R value = node.children[2]->accept(*this);
            table.array()->set(index, value);
            return value;
        } else {
            error("can only index tables or arrays.", node);
            return R();
        }
    }

    R operator()(AddFieldAssignment<Iter,R,F> &node) {
        R table = node.children[0]->accept(*this);
        R index, left, right;
        R::Type t = table.type();
        if(t == R::TABLE) {
            index = node.children[1]->accept(*this);
            left = table.table()->get(index);
            right = node.children[2]->accept(*this);
        } else if(t == R::ARRAY) {
            index = node.children[1]->accept(*this);
            if(index.type() != R::NUMBER || !isint(index.number())) error("array index not a integer.", node);
            int idx = index.number();
            if(idx<0 || idx>=int(table.array()->size())) error("array index out of range.", node);
            left = table.array()->get(index);
            right = node.children[2]->accept(*this);
        } else {
            error("can only index tables and arrays.", node);
            return R();
        }

        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            left = R(left.number() + right.number());
        } else if(left.type() == R::STRING && right.type() == R::STRING) {
            left.string() += right.string();
        } else if(left.type() == R::TABLE && right.type() == R::TABLE) {
            left.table()->append(*right.table());
        } else if(left.type() == R::ARRAY && right.type() == R::ARRAY) {
            left.array()->append(*right.array());
        } else {
            error("invalid operands '+='.", node);
            return R();
        }

        if(t == R::TABLE) {
            table.table()->set(index, left);
        } else if(t == R::ARRAY) {
            table.array()->set(index, left);
        }
        return left;
    }

    R operator()(SubFieldAssignment<Iter,R,F> &node) {
        R table = node.children[0]->accept(*this);
        R index, left, right;
        R::Type t = table.type();
        if(t == R::TABLE) {
            index = node.children[1]->accept(*this);
            left = table.table()->get(index);
            right = node.children[2]->accept(*this);
        } else if(t == R::ARRAY) {
            index = node.children[1]->accept(*this);
            if(index.type() != R::NUMBER || !isint(index.number())) error("array index not a integer.", node);
            int idx = index.number();
            if(idx<0 || idx>=int(table.array()->size())) error("array index out of range.", node);
            left = table.array()->get(index);
            right = node.children[2]->accept(*this);
        } else {
            error("can only index tables and arrays.", node);
            return R();
        }

        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            left = R(left.number() - right.number());
        } else {
            error("invalid operands to '-='.", node);
            return R();
        }

        if(t == R::TABLE) {
            table.table()->set(index, left);
        } else if(t == R::ARRAY) {
            table.array()->set(index, left);
        }
        return left;
    }

    R operator()(MulFieldAssignment<Iter,R,F> &node) {
        R table = node.children[0]->accept(*this);
        R index, left, right;
        R::Type t = table.type();
        if(t == R::TABLE) {
            index = node.children[1]->accept(*this);
            left = table.table()->get(index);
            right = node.children[2]->accept(*this);
        } else if(t == R::ARRAY) {
            index = node.children[1]->accept(*this);
            if(index.type() != R::NUMBER || !isint(index.number())) error("array index not a integer.", node);
            int idx = index.number();
            if(idx<0 || idx>=int(table.array()->size())) error("array index out of range.", node);
            left = table.array()->get(index);
            right = node.children[2]->accept(*this);
        } else {
            error("can only index tables and arrays.", node);
            return R();
        }

        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            left = R(left.number() * right.number());
        } else {
            error("invalid operands to '*='.", node);
            return R();
        }

        if(t == R::TABLE) {
            table.table()->set(index, left);
        } else if(t == R::ARRAY) {
            table.array()->set(index, left);
        }
        return left;
    }

    R operator()(DivFieldAssignment<Iter,R,F> &node) {
        R table = node.children[0]->accept(*this);
        R index, left, right;
        R::Type t = table.type();
        if(t == R::TABLE) {
            index = node.children[1]->accept(*this);
            left = table.table()->get(index);
            right = node.children[2]->accept(*this);
        } else if(t == R::ARRAY) {
            index = node.children[1]->accept(*this);
            if(index.type() != R::NUMBER || !isint(index.number())) error("array index not a integer.", node);
            int idx = index.number();
            if(idx<0 || idx>=int(table.array()->size())) error("array index out of range.", node);
            left = table.array()->get(index);
            right = node.children[2]->accept(*this);
        } else {
            error("can only index tables and arrays.", node);
            return R();
        }

        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            left = R(left.number() / right.number());
        } else {
            error("invalid operands to '/='.", node);
            return R();
        }

        if(t == R::TABLE) {
            table.table()->set(index, left);
        } else if(t == R::ARRAY) {
            table.array()->set(index, left);
        }
        return left;
    }

    R operator()(ModFieldAssignment<Iter,R,F> &node) {
        R table = node.children[0]->accept(*this);
        R index, left, right;
        R::Type t = table.type();
        if(t == R::TABLE) {
            index = node.children[1]->accept(*this);
            left = table.table()->get(index);
            right = node.children[2]->accept(*this);
        } else if(t == R::ARRAY) {
            index = node.children[1]->accept(*this);
            if(index.type() != R::NUMBER || !isint(index.number())) error("array index not a integer.", node);
            int idx = index.number();
            if(idx<0 || idx>=int(table.array()->size())) error("array index out of range.", node);
            left = table.array()->get(index);
            right = node.children[2]->accept(*this);
        } else {
            error("can only index tables and arrays.", node);
            return R();
        }

        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            left = R(std::fmod(left.number(), right.number()));
        } else {
            error("invalid operands to '%='.", node);
            return R();
        }

        if(t == R::TABLE) {
            table.table()->set(index, left);
        } else if(t == R::ARRAY) {
            table.array()->set(index, left);
        }
        return left;
    }

    R operator()(UnaryPlusExpression<Iter,R,F> &node) {
        R operand = node.children[0]->accept(*this);
        if(operand.type() != R::NUMBER) error("invalid operand to '+'.", node);
        return operand;
    }

    R operator()(UnaryMinusExpression<Iter,R,F> &node) {
        R operand = node.children[0]->accept(*this);
        if(operand.type() != R::NUMBER) error("invalid operand to '-'.", node);
        return R(-operand.number());
    }

    R operator()(UnaryNotExpression<Iter,R,F> &node) {
        R operand = node.children[0]->accept(*this);
        if(operand.type() != R::NUMBER) error("invalid operand to '!'.", node);
        return R(operand.number()==0.0?1.0:0.0);
    }

    R operator()(MultiplyExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        R right = node.children[1]->accept(*this);
        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            return R(left.number() * right.number());
        } else {
            error("invalid operands to '*'.", node);
            return R();
        }
    }

    R operator()(DivideExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        R right = node.children[1]->accept(*this);
        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            return R(left.number() / right.number());
        } else {
            error("invalid operands to '/'.", node);
            return R();
        }
    }

    R operator()(ModuloExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        R right = node.children[1]->accept(*this);
        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            return R(std::fmod(left.number(), right.number()));
        } else {
            error("invalid operands to '%'.", node);
            return R();
        }
    }

    R operator()(AddExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        R right = node.children[1]->accept(*this);
        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            return R(left.number() + right.number());
        } else if(left.type() == R::STRING && right.type() == R::STRING) {
            return R(left.string() + right.string());
        } else if(left.type() == R::TABLE && right.type() == R::TABLE) {
            R::table_t table(new Table<R>);
            table->append(*left.table());
            table->append(*right.table());
            return R(table);
        } else if(left.type() == R::ARRAY && right.type() == R::ARRAY) {
            R::array_t array(new Array<R>);
            array->append(*left.array());
            array->append(*right.array());
            return R(array);
        } else {
            error("invalid operands to '+'.", node);
            return R();
        }
    }

    R operator()(SubtractExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        R right = node.children[1]->accept(*this);
        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            return R(left.number() - right.number());
        } else {
            error("invalid operands to '-'.", node);
            return R();
        }
    }

    R operator()(LessExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        R right = node.children[1]->accept(*this);
        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            return R(left.number() < right.number());
        } else if(left.type() == R::STRING && right.type() == R::STRING) {
            return R(left.string() < right.string());
        } else {
            error("invalid operands to '<'.", node);
            return R();
        }
    }

    R operator()(LessEqualExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        R right = node.children[1]->accept(*this);
        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            return R(left.number() <= right.number());
        } else if(left.type() == R::STRING && right.type() == R::STRING) {
            return R(left.string() <= right.string());
        } else {
            error("invalid operands to '<='.", node);
            return R();
        }
    }

    R operator()(GreaterExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        R right = node.children[1]->accept(*this);
        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            return R(left.number() > right.number());
        } else if(left.type() == R::STRING && right.type() == R::STRING) {
            return R(left.string() > right.string());
        } else {
            error("invalid operands to '>'.", node);
            return R();
        }
    }

    R operator()(GreaterEqualExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        R right = node.children[1]->accept(*this);
        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            return R(left.number() >= right.number());
        } else if(left.type() == R::STRING && right.type() == R::STRING) {
            return R(left.string() >= right.string());
        } else {
            error("invalid operands to '>='.", node);
            return R();
        }
    }

    R operator()(EqualExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        R right = node.children[1]->accept(*this);
        return R(left == right);
    }

    R operator()(NotEqualExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        R right = node.children[1]->accept(*this);
        return R(!(left == right));
    }

    R operator()(LogicalAndExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        if(left.type() == R::NUMBER) {
            if(left.number() != 0.0) {
                R right = node.children[1]->accept(*this);
                if(right.type() == R::NUMBER) {
                    return R(right.number() != 0.0);
                } else {
                    error("invalid operands to '&&'.", node);
                    return R();
                }
            } else {
                return R(0.0);
            }
        } else {
            error("invalid operands to '&&'.", node);
            return R();
        }
    }

    R operator()(LogicalOrExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        if(left.type() == R::NUMBER) {
            if(left.number() == 0.0) {
                R right = node.children[1]->accept(*this);
                if(right.type() == R::NUMBER) {
                    return R(right.number() != 0.0);
                } else {
                    error("invalid operands to '||'.", node);
                    return R();
                }
            } else {
                return R(1.0);
            }
        } else {
            error("invalid operands to '||'.", node);
            return R();
        }
    }

    R operator()(AssignmentExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        R right = node.children[1]->accept(*this);
        set(left, right);
        return right;
    }

    R operator()(GlobalAssignmentExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        R right = node.children[1]->accept(*this);
        setGlobal(left, right);
        return right;
    }

    R operator()(LocalAssignmentExpression<Iter,R,F> &node) {
        R left = node.children[0]->accept(*this);
        R right = node.children[1]->accept(*this);
        setLocal(left, right);
        return right;
    }

    R operator()(AddAssignmentExpression<Iter,R,F> &node) {
        R name = node.children[0]->accept(*this);
        R left = get(name);
        R right = node.children[1]->accept(*this);
        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            left = R(left.number() + right.number());
        } else if(left.type() == R::STRING && right.type() == R::STRING) {
            left.string() += right.string();
        } else if(left.type() == R::TABLE && right.type() == R::TABLE) {
            left.table()->append(*right.table());
        } else if(left.type() == R::ARRAY && right.type() == R::ARRAY) {
            left.array()->append(*right.array());
        } else {
            error("invalid operands to '+='.", node);
            return R();
        }
        set(name, left);
        return left;
    }

    R operator()(SubAssignmentExpression<Iter,R,F> &node) {
        R name = node.children[0]->accept(*this);
        R left = get(name);
        R right = node.children[1]->accept(*this);
        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            left = R(left.number() - right.number());
        } else {
            error("invalid operands to '-='.", node);
            return R();
        }
        set(name, left);
        return left;
    }

    R operator()(MulAssignmentExpression<Iter,R,F> &node) {
        R name = node.children[0]->accept(*this);
        R left = get(name);
        R right = node.children[1]->accept(*this);
        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            left = R(left.number() * right.number());
        } else {
            error("invalid operands to '*='.", node);
            return R();
        }
        set(name, left);
        return left;
    }

    R operator()(DivAssignmentExpression<Iter,R,F> &node) {
        R name = node.children[0]->accept(*this);
        R left = get(name);
        R right = node.children[1]->accept(*this);
        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            left = R(left.number() / right.number());
        } else {
            error("invalid operands to '/='.", node);
            return R();
        }
        set(name, left);
        return left;
    }

    R operator()(ModAssignmentExpression<Iter,R,F> &node) {
        R name = node.children[0]->accept(*this);
        R left = get(name);
        R right = node.children[1]->accept(*this);
        if(left.type() == R::NUMBER && right.type() == R::NUMBER) {
            left = R(std::fmod(left.number(), right.number()));
        } else {
            error("invalid operands to '%='.", node);
            return R();
        }
        set(name, left);
        return left;
    }

    R operator()(Block<Iter,R,F> &node) {
        LocalScope localscope(*this);
        return node.children[0]->accept(*this);
    }

    R operator()(TableInitializer<Iter,R,F> &node) {
        double current = 0;
        R::table_t table(new Table<R>);
        for(auto tmpchild : node.children) {
            auto child = static_cast<InitializerAssignmentExpression<Iter,R,F>*>(tmpchild.get());
            if(child->children.size() == 1) {
                table->set(R(current++), child->children[0]->accept(*this));
            } else {
                R index = child->children[0]->accept(*this);
                if(index.type() == R::NUMBER && isint(index.number())) current = index.number()+1;
                table->set(index, child->children[1]->accept(*this));
            }
        }
        return R(table);
    }

    R operator()(ArrayInitializer<Iter,R,F> &node) {
        R::array_t array(new Array<R>);
        for(auto& tmpchild : node.children) {
            array->add(tmpchild->accept(*this));
        }
        return R(array);
    }

    R operator()(Function<Iter,R,F> &node) {
        return R(R::FUNCTION, node.shared_from_this());
    }

    R operator()(ReturnStatement<Iter,R,F> &node) {
        if(node.children.size()>0) {
            function_stack.push_back(node.children[0]->accept(*this));
        }
        return R(R::return_tag());
    }

    R operator()(IdentifierList<Iter,R,F> &node) {
        auto i = function_stack.begin();
        auto end = function_stack.end();

        set(R("this"), *i++);
        for(auto child : node.children) {
            if(i==end) break;
            set(child->accept(*this), *i++);
        }
        function_stack.clear();
        return R();
    }

    R operator()(BuiltinFunction<Iter,R,F> &node) {
        node.function(function_stack);
        return R();
    }

    R operator()(Call<Iter,R,F> &node) {
        R fun = node.children[0]->accept(*this);
        if(fun.type() != R::FUNCTION) {
            error("can not call non-function.", node);
            return R();
        }
        auto function = fun.function();

        std::vector<R> args;
        for(size_t i = 1;i<node.children.size();++i)
            args.push_back(node.children[i]->accept(*this));

        function_stack.push_back(R());
        for(size_t i = 1;i<node.children.size();++i)
            function_stack.push_back(args[i-1]);

        FunctionScope functionscope(*this);

        function->children[0]->accept(*this);
        function->children[1]->accept(*this);
        if(!function_stack.empty()) {
            R result = function_stack.back();
            function_stack.clear();
            return result;
        } else {
            return R();
        }
    }

    R operator()(MemberCall<Iter,R,F> &node) {
        R table = node.children[0]->accept(*this);

        if(table.type() != R::TABLE) {
            error("can only index tables.", node);
            return R();
        }
        R name = node.children[1]->accept(*this);
        R fun = table.table()->get(name);
        if(fun.type() != R::FUNCTION) {
            error("can not call non-function.", node);
            return R();
        }
        auto function = fun.function();

        std::vector<R> args;
        for(size_t i = 2;i<node.children.size();++i)
            args.push_back(node.children[i]->accept(*this));

        function_stack.push_back(table);
        for(size_t i = 2;i<node.children.size();++i)
            function_stack.push_back(args[i-2]);

        FunctionScope functionscope(*this);
        function->children[0]->accept(*this);
        function->children[1]->accept(*this);
        if(!function_stack.empty()) {
            R result = function_stack.back();
            function_stack.clear();
            return result;
        } else {
            return R();
        }
    }

    R operator()(EvalStatement<Iter,R,F> &node) {
        R treeval = node.children[0]->accept(*this);
        if(treeval.type() != R::TREE) {
            error("cannot eval non-tree.", node);
            return R();
        }
        return treeval.tree()->accept(*this);
    }

    R operator()(Eval<Iter,R,F> &node) {
        R treeval = node.children[0]->accept(*this);
        if(treeval.type() != R::TREE) {
            error("cannot eval non-tree.", node);
            return R();
        }
        return treeval.tree()->accept(*this);
    }

    R operator()(Tree<Iter,R,F> &node) {
        return R(R::TREE, node.children[0]);
    }

    R operator()(Root<Iter,R,F> &node) {
        if(node.children.empty()) {
            return R(R::TREE, node.root.lock());
        } else {
            R tree = node.children[0]->accept(*this);
            if(tree.type() == R::TREE || tree.type() == R::FUNCTION) {
                return R(R::TREE, tree.pointer()->root.lock());
            } else {
                error("cannot find root of non-tree.", node);
                return R();
            }
        }
    }

    R operator()(ParseStatement<Iter,R,F> &node) {
        R val = node.children[0]->accept(*this);
        if(val.type() != R::STRING) {
            error("parse argument needs to be string.", node);
            return R();
        }
        return R(R::TREE, parser(val.string().begin(), val.string().end()));
    }

    R operator()(ParseExpression<Iter,R,F> &node) {
        R val = node.children[0]->accept(*this);
        if(val.type() != R::STRING) {
            error("parse argument needs to be string.", node);
            return R();
        }
        return R(R::TREE, parser.parse_expression(val.string().begin(), val.string().end()));
    }

    R operator()(StatementList<Iter,R,F> &node) {
        for(auto child : node.children) {
            R result = child->accept(*this);
            R::Type t = result.internal_type();
            if(t >= R::RETURN && t<= R::CONTINUE)
                return result;
        }
        return R();
    }

    R operator()(IfStatement<Iter,R,F> &node) {
        R cond = node.children[0]->accept(*this);
        if(cond.type() != R::NUMBER) {
            error("if condition not a number.", node);
            return R();
        }
        if(cond.number() != 0.0)
            return node.children[1]->accept(*this);
        else if(node.children.size()>2)
            return node.children[2]->accept(*this);
        return R();
    }

    R operator()(WhileStatement<Iter,R,F> &node) {
        while(true) {
            R cond = node.children[0]->accept(*this);
            if(cond.type() != R::NUMBER) error("while condition not a number.", node);
            if(cond == 0.0) break;
            R result = node.children[1]->accept(*this);
            R::Type t = result.internal_type();
            if(t == R::BREAK || t == R::RETURN)
                return result;
        }
        return R();
    }

    R operator()(ForStatement<Iter,R,F> &node) {
        LocalScope(*this);
        node.children[0]->accept(*this);
        while(true) {
            R cond = node.children[1]->accept(*this);
            if(cond.type() != R::NUMBER) error("while condition not a number.", node);
            if(cond == 0.0) break;
            R result = node.children[3]->accept(*this);
            R::Type t = result.internal_type();
            if(t == R::BREAK || t == R::RETURN)
                return result;
            node.children[2]->accept(*this);
        }
        return R();
    }

    R operator()(ForEachStatement<Iter,R,F> &node) {
        if(node.children.size()==3) {
            R value = node.children[0]->accept(*this);
            R table = node.children[1]->accept(*this);
            R::Type t = table.type();
            if(t != R::TABLE && t != R::ARRAY) error("for each argument is not a table or array", node);
            LocalScope local(*this);
            if(t == R::TABLE) {
                for(auto entry : *table.table()) {
                    setLocal(value, entry.second);
                    R result = node.children[2]->accept(*this);
                    R::Type result_t = result.internal_type();
                    if(result_t == R::BREAK || result_t == R::RETURN)
                        return result;
                }
            } else {
                for(auto entry : *table.array()) {
                    setLocal(value, entry);
                    R result = node.children[2]->accept(*this);
                    R::Type result_t = result.type();
                    if(result_t == R::BREAK || result_t == R::RETURN)
                        return result;
                }
            }
        } else {
            R key = node.children[0]->accept(*this);
            R value = node.children[1]->accept(*this);
            R table = node.children[2]->accept(*this);
            if(table.type() != R::TABLE) error("for each argument is not a table", node);
            LocalScope local(*this);
            for(auto entry : *table.table()) {
                setLocal(key, entry.first);
                setLocal(value, entry.second);
                R result = node.children[3]->accept(*this);
                R::Type t = result.type();
                if(t == R::BREAK || t == R::RETURN)
                    return result;
            }
        }
        return R();
    }

    R operator()(ContinueStatement<Iter,R,F> &) {
        return R(R::continue_tag());
    }

    R operator()(BreakStatement<Iter,R,F> &) {
        return R(R::break_tag());
    }

    template<class T>
    R operator()(T &node) {
        error("unimplemented ast node.", node); return R();
    }

    Parser<R, Interpreter> parser;

    std::vector<R> function_stack;

    std::vector<std::unordered_map<std::string, R>> scopes;
    typedef std::shared_ptr<Table<R>> env_t;
    typedef std::vector<env_t> envstack_t;
    typedef std::vector<size_t> funstack_t;
    envstack_t envstack;
    funstack_t funstack;
    void beginLocalScope() {
        envstack.push_back(std::make_shared<Table<R>>());
    }
    void endLocalScope() {
        envstack.pop_back();
    }
    void beginFunctionScope() {
        funstack.push_back(envstack.size());
        envstack.push_back(std::make_shared<Table<R>>());
    }
    void endFunctionScope() {
        envstack.pop_back();
        funstack.pop_back();
    }
    R get(R key) {
        int top = envstack.size()-1;
        int bottom = funstack.back();
        for(int i = top;i>=bottom;--i) {
            R value = envstack[i]->get(key);
            if(value.type() != R::NIL) return value;
        }
        return envstack[0]->get(key);
    }
    void set(R key, R value) {
        envstack[funstack.back()]->set(key, value);
    }
    void setLocal(R key, R value) {
        envstack.back()->set(key, value);
    }
    void setGlobal(R key, R value) {
        envstack[0]->set(key, value);
    }

    template<class T>
    void error(const std::string text, T &node) {
        auto lc = line_column(node.source->begin(), node.begin->begin);
        auto from = node.begin;
        auto to = node.end-1;
        throw std::runtime_error(
            std::to_string(lc.first)+":"+std::to_string(lc.second)+
            ": error: " + text + " in: '" + std::string(from->begin, to->end) + "'");
    }

    void addFunction(const std::string &name, std::function<void(std::vector<R>&)> fun) {
        auto node = std::make_shared<Function<Iter,R,F>>();
        node->begin = node->end;
        auto nop = std::make_shared<Nop<Iter,R,F>>();
        nop->begin = nop->end;
        node->children[0] = nop;
        auto builtin = std::make_shared<BuiltinFunction<Iter,R,F>>();
        builtin->begin = builtin->end;
        builtin->function = fun;
        node->children[1] = builtin;
        setGlobal(R(name), R(R::FUNCTION, node));
    }

    FunctionScope global;

    Interpreter() : global(*this) {

    }

    R run(const std::string &source) {
        auto root = parser(source.begin(), source.end());

        R result = root->accept(*this);

        return result;
    }
};

void print(std::vector<Interpreter::R> &stack) {

    for(size_t i = 1;i<stack.size();++i) {
        std::cout << stack[i];
    }
    stack.clear();
}

void readline(std::vector<Interpreter::R> &stack) {
    stack.clear();
    std::string str;
    std::getline(std::cin, str);
    stack.push_back(Interpreter::R(str));
}

void children(std::vector<Interpreter::R> &stack) {
    typedef Interpreter::R R;
    if(stack.empty()) return;
    auto arg = stack[1];
    R::Type t = arg.type();
    stack.clear();
    if(t == R::TREE || t == R::FUNCTION) {
        auto result = std::make_shared<Array<Interpreter::R>>();
        for(size_t i = 0, s = arg.pointer()->size();i<s;++i) {
            result->add(R(R::TREE, (*arg.pointer())[i]));
        }
        stack.push_back(result);
    }
}

void tokens(std::vector<Interpreter::R> &stack) {
    typedef Interpreter::R R;
    if(stack.empty()) return;
    auto arg = stack[1];
    R::Type t = arg.type();
    stack.clear();
    if(t == R::TREE || t == R::FUNCTION) {
        auto result = std::make_shared<Array<Interpreter::R>>();
        auto node = arg.pointer();
        for(auto i = node->begin;i!=node->end;++i) {
            result->add(R(std::string(i->begin, i->end)));
        }
        stack.push_back(result);
    }
}

void expand_node(std::vector<Interpreter::R> &stack) {
    typedef Interpreter::R R;
    if(stack.empty()) return;
    auto arg = stack[1];
    R::Type t = arg.type();
    stack.clear();
    if(t == R::TREE || t == R::FUNCTION) {
        auto result = std::make_shared<Array<Interpreter::R>>();
        auto &node = *arg.pointer();
        auto token = node.begin;
        for(size_t i = 0, s = arg.pointer()->size();i<s;++i) {
            for(;token!=node[i]->begin;++token) {
                result->add(R(std::string(token->begin, token->end)));
            }
            result->add(R(R::TREE, node[i]));
            token = node[i]->end;
        }
        for(;token!=node.end;++token) {
            result->add(R(std::string(token->begin, token->end)));
        }
        stack.push_back(result);
    }
}

void tostring(std::vector<Interpreter::R> &stack) {
    typedef Interpreter::R R;
    if(stack.empty()) return;
    auto arg = stack[1];
    stack.clear();
    switch(arg.type()) {
        case R::TREE:
            stack.emplace_back(std::string(arg.tree()->begin->begin, (arg.tree()->end-1)->end));
            return;
        case R::FUNCTION:
            stack.emplace_back(std::string(arg.function()->begin->begin, (arg.function()->end-1)->end));
            return;
        case R::NUMBER:
            stack.emplace_back(std::to_string(arg.number()));
            return;
        case R::STRING:
            stack.push_back(arg);
            return;
        default:
            stack.clear();
            return;
    }
}

void tonumber(std::vector<Interpreter::R> &stack) {
    typedef Interpreter::R R;
    if(stack.empty()) return;
    auto arg = stack[1];
    stack.clear();
    if(arg.type() == R::STRING) {
        double result = std::strtod(arg.string().c_str(), nullptr);
        if(result != HUGE_VAL)
            stack.emplace_back(result);
    }
}

int main(int argc, char *argv[]) {
    if(argc < 2) {
        std::cerr << "no input file provided" << std::endl;
        return 1;
    }

    std::ifstream in(argv[1]);
    std::string source((std::istreambuf_iterator<char>(in)), (std::istreambuf_iterator<char>()));

    Interpreter interpreter;
    interpreter.addFunction("print", print);
    interpreter.addFunction("readline", readline);
    interpreter.addFunction("children", children);
    interpreter.addFunction("tokens", tokens);
    interpreter.addFunction("expand_node", expand_node);
    interpreter.addFunction("tostring", tostring);
    interpreter.addFunction("tonumber", tonumber);
    interpreter.run(source);

    return 0;
}
