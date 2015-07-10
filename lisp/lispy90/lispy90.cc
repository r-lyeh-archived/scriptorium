// Scheme Interpreter in 90 lines of C++ (not counting lines after the first 90).
// Inspired by Peter Norvig's Lis.py.

// Made by Anthony C. Hay in 2010. See http://howtowriteaprogram.blogspot.co.uk/
// This is free and unencumbered public domain software, see http://unlicense.org/
// This code is known to have faults. E.g. it leaks memory. Use at your own risk.

#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <list>
#include <map>
#include <stdlib.h>

// return given mumber as a string
std::string str(long n) { std::ostringstream os; os << n; return os.str(); }

// return true iff given character is '0'..'9'
bool isdig(char c) { return isdigit(static_cast<unsigned char>(c)) != 0; }


////////////////////// cell

enum cell_type { Symbol, Number, List, Proc, Lambda };

struct environment; // forward declaration; cell and environment reference each other

// a variant that can hold any kind of lisp value
struct cell {
    typedef cell (*proc_type)(const std::vector<cell> &);
    typedef std::vector<cell>::const_iterator iter;
    typedef std::map<std::string, cell> map;
    cell_type type; std::string val; std::vector<cell> list; proc_type proc; environment * env;
    cell(cell_type type = Symbol) : type(type), env(0) {}
    cell(cell_type type, const std::string & val) : type(type), val(val), env(0) {}
    cell(proc_type proc) : type(Proc), proc(proc), env(0) {}
};

typedef std::vector<cell> cells;
typedef cells::const_iterator cellit;

const cell false_sym(Symbol, "#f");
const cell true_sym(Symbol, "#t"); // anything that isn't false_sym is true
const cell nil(Symbol, "nil");


////////////////////// environment

// a dictionary that (a) associates symbols with cells, and
// (b) can chain to an "outer" dictionary
struct environment {
    environment(environment * outer = 0) : outer_(outer) {}

    environment(const cells & parms, const cells & args, environment* outer)
    : outer_(outer)
    {
        cellit a = args.begin();
        int s = args.size();
        int i = 0;
        for (cellit p = parms.begin(); p != parms.end() && i != s; ++p, i++) {
            env_[p->val] = *a++;
        }
    }

    // map a variable name onto a cell
    typedef std::map<std::string, cell> map;

    // return a reference to the innermost environment where 'var' appears
    map & find(const std::string & var)
    {
        if (env_.find(var) != env_.end())
            return env_; // the symbol exists in this environment
        if (outer_)
            return outer_->find(var); // attempt to find the symbol in some "outer" env
        std::cout << "unbound symbol '" << var << "'\n";
        exit(1);
    }

    // return a reference to the cell associated with the given symbol 'var'
    cell & operator[] (const std::string & var)
    {
        return env_[var];
    }
    
private:
    map env_; // inner symbol->cell mapping
    environment * outer_; // next adjacent outer env, or 0 if there are no further environments
};


////////////////////// built-in primitive procedures

cell proc_add(const cells & c)
{
    long n(atol(c[0].val.c_str()));
    for (cellit i = c.begin()+1; i != c.end(); ++i) n += atol(i->val.c_str());
    return cell(Number, str(n));
}

cell proc_sub(const cells & c)
{
    long n(atol(c[0].val.c_str()));
    for (cellit i = c.begin()+1; i != c.end(); ++i) n -= atol(i->val.c_str());
    return cell(Number, str(n));
}

cell proc_mul(const cells & c)
{
    long n(1);
    for (cellit i = c.begin(); i != c.end(); ++i) n *= atol(i->val.c_str());
    return cell(Number, str(n));
}

cell proc_div(const cells & c)
{
    long n(atol(c[0].val.c_str()));
    for (cellit i = c.begin()+1; i != c.end(); ++i) n /= atol(i->val.c_str());
    return cell(Number, str(n));
}

cell proc_greater(const cells & c)
{
    long n(atol(c[0].val.c_str()));
    for (cellit i = c.begin()+1; i != c.end(); ++i)
        if (n <= atol(i->val.c_str()))
            return false_sym;
    return true_sym;
}

cell proc_less(const cells & c)
{
    long n(atol(c[0].val.c_str()));
    for (cellit i = c.begin()+1; i != c.end(); ++i)
        if (n >= atol(i->val.c_str()))
            return false_sym;
    return true_sym;
}

cell proc_less_equal(const cells & c)
{
    long n(atol(c[0].val.c_str()));
    for (cellit i = c.begin()+1; i != c.end(); ++i)
        if (n > atol(i->val.c_str()))
            return false_sym;
    return true_sym;
}

cell proc_length(const cells & c) { return cell(Number, str(c[0].list.size())); }
cell proc_nullp(const cells & c)  { return c[0].list.empty() ? true_sym : false_sym; }
cell proc_car(const cells & c)    { return c[0].list[0]; }

cell proc_cdr(const cells & c)
{
    if (c[0].list.size() < 2)
        return nil;
    cell result(c[0]);
    result.list.erase(result.list.begin());
    return result;
}

cell proc_append(const cells & c)
{
    cell result(List);
    result.list = c[0].list;
    for (cellit i = c[1].list.begin(); i != c[1].list.end(); ++i) result.list.push_back(*i);
    return result;
}

cell proc_cons(const cells & c)
{
    cell result(List);
    result.list.push_back(c[0]);
    for (cellit i = c[1].list.begin(); i != c[1].list.end(); ++i) result.list.push_back(*i);
    return result;
}

cell proc_list(const cells & c)
{
    cell result(List); result.list = c;
    return result;
}

cell proc_print(const cells & c)
{
    printf("%s\n", c[0].val.c_str());
    return c[0];
}

// define the bare minimum set of primintives necessary to pass the unit tests
void add_globals(environment & env)
{
    env["nil"] = nil;   env["#f"] = false_sym;  env["#t"] = true_sym;
    env["append"] = cell(&proc_append);   env["car"]  = cell(&proc_car);
    env["cdr"]    = cell(&proc_cdr);      env["cons"] = cell(&proc_cons);
    env["length"] = cell(&proc_length);   env["list"] = cell(&proc_list);
    env["null?"]  = cell(&proc_nullp);    env["+"]    = cell(&proc_add);
    env["-"]      = cell(&proc_sub);      env["*"]    = cell(&proc_mul);
    env["/"]      = cell(&proc_div);      env[">"]    = cell(&proc_greater);
    env["<"]      = cell(&proc_less);     env["<="]   = cell(&proc_less_equal);
    env["print"]  = cell(&proc_print);
}


////////////////////// eval

cell eval(cell x, environment * env)
{
    if (x.type == Symbol)
        return env->find(x.val)[x.val];
    if (x.type == Number)
        return x;
    if (x.list.empty())
        return nil;
    if (x.list[0].type == Symbol) {
        if (x.list[0].val == "quote")       // (quote exp)
            return x.list[1];
        if (x.list[0].val == "if")          // (if test conseq [alt])
            return eval(eval(x.list[1], env).val == "#f" ? (x.list.size() < 4 ? nil : x.list[3]) : x.list[2], env);
        if (x.list[0].val == "set!")        // (set! var exp)
            return env->find(x.list[1].val)[x.list[1].val] = eval(x.list[2], env);
        if (x.list[0].val == "define")      // (define var exp)
            return (*env)[x.list[1].val] = eval(x.list[2], env);
        if (x.list[0].val == "lambda") {    // (lambda (var*) exp)
            x.type = Lambda;
            // keep a reference to the environment that exists now (when the
            // lambda is being defined) because that's the outer environment
            // we'll need to use when the lambda is executed
            x.env = env;
            return x;
        }
        if (x.list[0].val == "begin") {     // (begin exp*)
            for (size_t i = 1; i < x.list.size() - 1; ++i)
                eval(x.list[i], env);
            return eval(x.list[x.list.size() - 1], env);
        }
    }
                                            // (proc exp*)
    cell proc(eval(x.list[0], env));
    cells exps;
    for (cell::iter exp = x.list.begin() + 1; exp != x.list.end(); ++exp)
        exps.push_back(eval(*exp, env));
    if (proc.type == Lambda) {
        // check if we provided too many arguments to the lambda object
        if(proc.list[1].list.size() < exps.size()) {
            std::cerr << "Error : too many arguments\n";
            // if the number of argument is equal to the number of parameters required, then eval
        } else if (proc.list[1].list.size() == exps.size()) {
            return eval(/*body*/proc.list[2], new environment(/*parms*/proc.list[1].list, /*args*/exps, proc.env));
        } else {
            // if there is not enough arguments, build a new lambda with the first arguments provided linked
            cell result;
            result.type = Lambda;
            result.env = new environment(proc.list[1].list,exps,proc.env);
            result.list = cells(3);
            result.list[2] = proc.list[2]; // copy the body
            // copy the remaining arguments
            for(int i = exps.size(); i < proc.list[1].list.size(); i++) {
                result.list[1].list.push_back(proc.list[1].list[i]);
            }
            return result;
        }
    }
    else if (proc.type == Proc)
        return proc.proc(exps);

    std::cout << "not a function\n";
    exit(1);
}


////////////////////// parse, read and user interaction

// convert given string to list of tokens
std::list<std::string> tokenize(const std::string & str)
{
    std::list<std::string> tokens;
    const char * s = str.c_str();
    while (*s) {
        while (*s == ' ')
            ++s;
        if (*s == '(' || *s == ')')
            tokens.push_back(*s++ == '(' ? "(" : ")");
        else {
            const char * t = s;
            while (*t && *t != ' ' && *t != '(' && *t != ')')
                ++t;
            tokens.push_back(std::string(s, t));
            s = t;
        }
    }
    return tokens;
}

// numbers become Numbers; every other token is a Symbol
cell atom(const std::string & token)
{
    if (isdig(token[0]) || (token[0] == '-' && isdig(token[1])))
        return cell(Number, token);
    return cell(Symbol, token);
}

// return the Lisp expression in the given tokens
cell read_from(std::list<std::string> & tokens)
{
    const std::string token(tokens.front());
    tokens.pop_front();
    if (token == "(") {
        cell c(List);
        while (tokens.front() != ")")
            c.list.push_back(read_from(tokens));
        tokens.pop_front();
        return c;
    }
    else
        return atom(token);
}

// return the Lisp expression represented by the given string
cell read(const std::string & s)
{
    std::list<std::string> tokens(tokenize(s));
    return read_from(tokens);
}

// convert given cell to a Lisp-readable string
std::string to_string(const cell & exp)
{
    if (exp.type == List) {
        std::string s("(");
        for (cell::iter e = exp.list.begin(); e != exp.list.end(); ++e)
            s += to_string(*e) + ' ';
        if (s[s.size() - 1] == ' ')
            s.erase(s.size() - 1);
        return s + ')';
    }
    else if (exp.type == Lambda)
        return "<Lambda>";
    else if (exp.type == Proc)
        return "<Proc>";
    return exp.val;
}

// the default read-eval-print-loop
void repl(const std::string & prompt, environment * env)
{
    for (;;) {
        std::cout << prompt;
        std::string line; std::getline(std::cin, line);
        std::cout << to_string(eval(read(line), env)) << '\n';
    }
}

void main2 ()
{
    environment global_env; add_globals(global_env);
    repl("90> ", &global_env);
}

////////////////////// unit tests

unsigned g_test_count;      // count of number of unit tests executed
unsigned g_fault_count;     // count of number of unit tests that fail

template <typename T1, typename T2>
void test_equal_(const T1 & value, const T2 & expected_value, const char * file, int line)
{
    ++g_test_count;
    if (value != expected_value) {
        std::cout
            << file << '(' << line << ") : "
            << " expected " << expected_value
            << ", got " << value
            << '\n';
        ++g_fault_count;
    }
}

// write a message to std::cout if value != expected_value
#define TEST_EQUAL(value, expected_value) test_equal_(value, expected_value, __FILE__, __LINE__)

// evaluate the given Lisp expression and compare the result against the given expected_result
#define TEST(expr, expected_result) TEST_EQUAL(to_string(eval(read(expr), &global_env)), expected_result)


#include <fstream>
#include <string>

int main ( int argc, const char **argv )
{
    environment global_env; add_globals(global_env);

    if( argc > 1 )
    {
        std::ifstream ifs(argv[1]);
        if( ifs.good() ) {
            std::stringstream buf;
            buf << ifs.rdbuf();
            std::string buffer = std::string("(begin ") + buf.str() + " )";
            if( !buffer.empty() ) {
                for(auto &ch : buffer) {
                    if( ch == '\r' || ch == '\n' ||
                        ch == '\t' || ch == '\f' || ch == '\v' ) ch = ' ';
                }
                std::cout << to_string(eval(read(buffer), &global_env)) << '\n';
                return 0;
            } else {
                return 1;
            }
        }
        return 1;
    }

    // the 29 unit tests for lis.py
    TEST("(quote (testing 1 (2.0) -3.14e159))", "(testing 1 (2.0) -3.14e159)");
    TEST("(+ 2 2)", "4");
    TEST("(+ (* 2 100) (* 1 10))", "210");
    TEST("(if (> 6 5) (+ 1 1) (+ 2 2))", "2");
    TEST("(if (< 6 5) (+ 1 1) (+ 2 2))", "4");
    TEST("(define x 3)", "3");
    TEST("x", "3");
    TEST("(+ x x)", "6");
    TEST("(begin (define x 1) (set! x (+ x 1)) (+ x 1))", "3");
    TEST("((lambda (x) (+ x x)) 5)", "10");
    TEST("(define twice (lambda (x) (* 2 x)))", "<Lambda>");
    TEST("(twice 5)", "10");
    TEST("(define compose (lambda (f g) (lambda (x) (f (g x)))))", "<Lambda>");
    TEST("((compose list twice) 5)", "(10)");
    TEST("(define repeat (lambda (f) (compose f f)))", "<Lambda>");
    TEST("((repeat twice) 5)", "20");
    TEST("((repeat (repeat twice)) 5)", "80");
    TEST("(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))", "<Lambda>");
    TEST("(fact 3)", "6");
    //TEST("(fact 50)", "30414093201713378043612608166064768844377641568960512000000000000");
    TEST("(fact 12)", "479001600"); // no bignums; this is as far as we go with 32 bits
    TEST("(define abs (lambda (n) ((if (> n 0) + -) 0 n)))", "<Lambda>");
    TEST("(list (abs -3) (abs 0) (abs 3))", "(3 0 3)");
    TEST("(define combine (lambda (f)"
             "(lambda (x y)"
                "(if (null? x) (quote ())"
                "(f (list (car x) (car y))"
                "((combine f) (cdr x) (cdr y)))))))", "<Lambda>");
    TEST("(define zip (combine cons))", "<Lambda>");
    TEST("(zip (list 1 2 3 4) (list 5 6 7 8))", "((1 5) (2 6) (3 7) (4 8))");
    TEST("(define riff-shuffle (lambda (deck) (begin"
            "(define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq))))))"
            "(define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))"
            "(define mid (lambda (seq) (/ (length seq) 2)))"
            "((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))", "<Lambda>");
    TEST("(riff-shuffle (list 1 2 3 4 5 6 7 8))", "(1 5 2 6 3 7 4 8)");
    TEST("((repeat riff-shuffle) (list 1 2 3 4 5 6 7 8))",  "(1 3 5 7 2 4 6 8)");
    TEST("(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))", "(1 2 3 4 5 6 7 8)");

    std::cout
        << "total tests " << g_test_count
        << ", total failures " << g_fault_count
        << "\n";
    
    main2();

    return g_fault_count ? EXIT_FAILURE : EXIT_SUCCESS;
}