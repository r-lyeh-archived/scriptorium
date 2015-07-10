// (C) 2013-2014 Kim, Taegyoon
// Paren language core

#include "libparen.h"
#include <algorithm>

namespace libparen {
    using namespace std;
	senvironment global_env; // variables

	snode make_snode(const node& n) {
		return make_shared<node>(n);
	}

	node::node() : type(T_NIL) {}
	node::node(int a) : type(T_INT), v_int(a) {}
	node::node(double a) : type(T_DOUBLE), v_double(a) {}
	node::node(bool a) : type(T_BOOL), v_bool(a) {}
	node::node(const string &a) : type(T_STRING), v_string(a) {}
	node::node(const vector<snode> &a) : type(T_LIST), v_list(a) {}
	node::node(builtin a) : type(T_BUILTIN), v_builtin(a) {}
	snode make_special(builtin a) {
		node n;
		n.type = node::T_SPECIAL;
		n.v_builtin = a;
		return make_snode(n);
	}

	snode node_true(make_snode(node(true)));
	snode node_false(make_snode(node(false)));
	snode node_0(make_snode(node(0)));
	snode node_1(make_snode(node(1)));
	snode nil(make_snode(node()));	

    inline int node::to_int() {
        switch (type) {
        case T_INT:
            return v_int;
        case T_DOUBLE:
            return (int) v_double;
        case T_BOOL:
            return (int) v_bool;
        case T_STRING:
            return atoi(v_string.c_str());
        default:
            return 0;
        }
    }

    inline double node::to_double() {
        switch (type) {
        case T_INT:
            return v_int;
        case T_DOUBLE:
            return v_double;
        case T_BOOL:
            return v_bool;
        case T_STRING:
            return atof(v_string.c_str());
        default:
            return 0.0;
        }
    }
    inline string node::to_string() {
		char buf[32];
		string ret;
        switch (type) {
        case T_NIL: break;
        case T_INT:
			sprintf(buf, "%d", v_int);
			ret = buf; break;
        case T_BUILTIN:
			//sprintf(buf, "%d", v_int);
			//ret = "builtin." + string(buf); break;
			sprintf(buf, "#<builtin:%p>", v_builtin);
			ret = buf; break;
		case T_SPECIAL:
			sprintf(buf, "#<builtin:%p>", v_builtin);
			ret = buf; break;
        case T_DOUBLE:
			sprintf(buf, "%.16g", v_double);
            ret = buf; break;
        case T_BOOL:
            return (v_bool ? "true" : "false");
        case T_STRING:
        case T_SYMBOL:
            return v_string;
        case T_FN:
        case T_LIST:
            {
                ret = '(';
                for (vector<snode>::iterator iter = v_list.begin(); iter != v_list.end(); iter++) {
                    if (iter != v_list.begin()) ret += ' ';
                    ret += (*iter)->to_string();
                }
                ret += ')';
                break;
            }
		default:;
        }
		return ret;
    }
    inline string node::type_str() {
        switch (type) {
        case T_NIL:
            return "nil";
        case T_INT:
            return "int";
        case T_DOUBLE:
            return "double";
        case T_BOOL:
            return "bool";
        case T_STRING:
            return "string";
        case T_SYMBOL:
            return "symbol";
        case T_LIST:
            return "list";
        case T_BUILTIN:
            return "builtin";
		case T_SPECIAL:
			return "special";
        case T_FN:
            return "fn";
		case T_THREAD: return "thread";
        default:
            return "invalid type";
        }
    }
    inline string node::str_with_type() {
        return to_string() + " : " + type_str();
    }

    inline double rand_double() {
        return (double) rand() / ((double) RAND_MAX + 1.0);
    }

    class tokenizer {
    private:
        vector<string> ret;
        string acc; // accumulator
        string s;        
        void emit() { // add accumulated string to token list
            if (acc.length() > 0) {ret.push_back(acc); acc = "";}
        }
    public:
        int unclosed; // number of unclosed parenthesis ( or quotation "
        tokenizer(const string &s): s(s), unclosed(0) {}
        
        vector<string> tokenize() {
            int last = s.length() - 1;
            for (int pos=0; pos <= last; pos++) {
                char c = s.at(pos);
                if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
                    emit();
                }
				else if (c == ';' || (pos < last && c == '#' && s[pos + 1] == '!')) { // end-of-line comment: ; or #!
                    emit();
                    do pos++; while (pos <= last && s.at(pos) != '\n');
                }
                else if (c == '"') { // beginning of string
                    unclosed++;
                    emit();
                    acc += '"';
                    pos++;
                    while (pos <= last) {
                        if (s.at(pos) == '"') {unclosed--; break;}
                        if (s.at(pos) == '\\') { // escape
                            char next = s.at(pos+1);
                            if (next == 'r') next = '\r';
                            else if (next == 'n') next = '\n';
                            else if (next == 't') next = '\t';
                            acc += next;
                            pos += 2;
                        }
                        else {
                            acc += s.at(pos);
                            pos++;
                        }
                    }
                    emit();
                }
                else if (c == '(') {
                    unclosed++;
                    emit();
                    acc += c;
                    emit();
                }
                else if (c == ')') {
                    unclosed--;
                    emit();
                    acc += c;
                    emit();
                }
                else {
                    acc += c;
                }
            }
            emit();
            return ret;
        }
    };

    vector<string> tokenize(const string &s) {
        return tokenizer(s).tokenize();
    }
    
    map<string, int> symcode;
    vector<string> symname;
    int ToCode(const string &name) {
        int r;
		map<string, int>::iterator found = symcode.find(name);
		if (found != symcode.end()) {
			return found->second;
		} else {
			r = symcode.size();
			symcode[name] = r;
			symname.push_back(name);
			return r;
		}        
    }

    class parser {        
    private:
        int pos;
        vector<string> tokens;
    public:
        parser(const vector<string> &tokens): pos(0), tokens(tokens) {}
        vector<snode> parse() {
            vector<snode> ret;
            int last = tokens.size() - 1;
            for (;pos <= last; pos++) {		
                string tok = tokens.at(pos);
				if (tok[0] < 0) break; // MSVC bug fix
                if (tok.at(0) == '"') { // double-quoted string
                    ret.push_back(make_snode(tok.substr(1)));
                }
                else if (tok == "(") { // list
                    pos++;
                    ret.push_back(make_snode(parse()));
                }
                else if (tok == ")") { // end of list
                    break;
                }
                else if (isdigit(tok.at(0)) || (tok.at(0) == '-' && tok.length() >= 2 && isdigit(tok.at(1)))) { // number
                    if (tok.find('.') != string::npos || tok.find('e') != string::npos) { // double
                        ret.push_back(make_snode(atof(tok.c_str())));
                    } else {
                        ret.push_back(make_snode(atoi(tok.c_str())));
                    }
                } else { // symbol
                    node n;
                    n.type = node::T_SYMBOL;
                    n.v_string = tok;
//					n.p_node = NULL;
					n.code = ToCode(tok);
                    ret.push_back(make_snode(n));
                }
            }
            return ret;
        }        
    };
    
    vector<snode> parse(const string &s) {
        return parser(tokenize(s)).parse();
    }

    environment::environment(): outer(NULL) {}
	environment::environment(senvironment outer) : outer(outer) {}

    snode environment::get(int code) {
        map<int, snode>::iterator found = env.find(code);
        if (found != env.end()) {
            return found->second;
        }
        else {
            if (outer != NULL) {
                return outer->get(code);
            }
            else {
                return nil;
            }
        }
    }

	snode environment::get(snode &k) {
		return get(k->code);
		//if (k->p_node != NULL) { // cached
		//	return make_snode(*k->p_node);
		//} else {
		//	return make_snode(*(k->p_node = get(k->code).get())); // make cache
		//}
	}

	snode environment::set(snode &k, snode &v) {
		return env[k->code] = v;
		//if (k->p_node != NULL) { // cached
		//	*k->p_node = *v;
		//} else {
		//	k->p_node = (env[k->code] = v).get(); // make cache
		//}
		//return make_snode(*k->p_node);
	}

    //snode make_builtin(builtin b) {
    //    snode n(make_snode(b));
    //    n->type = node::T_BUILTIN;
    //    return n;
    //}

	//snode make_special(builtin b) {
	//	snode n(make_snode(b));
	//	n->type = node::T_SPECIAL;
	//	return n;
	//}

    snode fn(snode n, shared_ptr<environment> outer_env) {
        snode n2(n);
        n2->type = node::T_FN;
		n2->outer_env = outer_env;
        return n2;
    }

    map<string, vector<snode> > macros;

    snode apply_macro(snode body, map<string, snode> vars) {
        if (body->type == node::T_LIST) {            
            vector<snode> &bvec = body->v_list;
            vector<snode> ret;            
            for (unsigned int i = 0; i < bvec.size(); i++) {
                snode b = bvec.at(i);
                if (b->v_string == "...") {
                    snode &vargs = vars[b->v_string];
                    ret.insert(ret.end(), vargs->v_list.begin(), vargs->v_list.end());                    
                } else ret.push_back(apply_macro(bvec.at(i), vars));
            }
            return make_snode(ret);
        } else {
            string &bstr = body->v_string;
            if (vars.find(bstr) != vars.end()) return vars[bstr]; else return body;
        }
    }

    snode macroexpand(snode n) {        
        vector<snode> nList = n->v_list;
        if (macros.find(nList[0]->v_string) != macros.end()) {            
            vector<snode> &macro = macros[nList[0]->v_string];
            map<string, snode> macrovars;
            vector<snode> &argsyms = macro[0]->v_list;            
            for (unsigned int i = 0; i < argsyms.size(); i++) {
                string argsym = argsyms.at(i)->v_string;
                if (argsym == "...") {
                    vector<snode> ellipsis;
                    snode n2(make_snode(ellipsis));
                    vector<snode> &ellipsis2 = n2->v_list;
                    for (unsigned int i2 = i + 1; i2 < nList.size(); i2++)
                        ellipsis2.push_back(nList.at(i2));
                    macrovars[argsym] = n2;
                    break;
                } else {
                    macrovars[argsyms.at(i)->v_string] = nList.at(i+1);                    
                }
            }
            return apply_macro(macro[1], macrovars);
        } else
            return n;
    }
		
    snode compile(snode &n) {
        switch (n->type) {
        case node::T_LIST: // function (FUNCTION ARGUMENT ..)
            {
				if (n->v_list.size() == 0) return n;
                snode func = compile(n->v_list[0]);                
				if (func->type == node::T_SYMBOL && func->v_string == "defmacro") { // (defmacro add (a b) (+ a b)) ; define macro
					vector<snode> v;
					v.push_back(n->v_list[2]);
					v.push_back(n->v_list[3]);
					macros[n->v_list[1]->v_string] = v;
					return nil;
				}
				else if (func->type == node::T_SYMBOL && func->v_string == "quote") { // ignore macro
					return n;
				}
                else {
					if (macros.find(func->v_string) != macros.end()) {
						snode expanded = macroexpand(n);
                        return compile(expanded);
                    } else {
                        vector<snode> r;
                        for (vector<snode>::iterator i = n->v_list.begin(); i != n->v_list.end(); i++) {
                            r.push_back(compile(*i));
                        }
                        return make_snode(r);
                    } 
                }
            }
		default: return n;
        }
    }

    snode eval(snode &n, senvironment &env) {
        switch (n->type) {
        case node::T_SYMBOL:
			{
				//snode n2 = env.get(n);
				//if (n2->type != node::T_NIL) {
				//	return n2;
				//}
				//else {
				//	cerr << "Unknown variable: " << n->v_string << endl;
				//	return nil;
				//}
				return env->get(n);
			}
        case node::T_LIST: // function (FUNCTION ARGUMENT ..)
            {
                snode func = eval(n->v_list[0], env);
				switch (func->type) {
				case node::T_SPECIAL:
				{
					return func->v_builtin(n->v_list, env);
				}
				case node::T_BUILTIN:
				case node::T_FN:
				{
					// evaluate arguments
					vector<snode> args;
					environment env2(func->outer_env);
					args.reserve(n->v_list.size() - 1);
					for (auto i = n->v_list.begin() + 1; i != n->v_list.end(); i++) {
						args.push_back(eval(*i, env));
					}
					senvironment senv2(make_shared<environment>(env2));
					return apply(func, args, senv2);
				}
				default:
					return nil;
				} // end switch (func->type)
            }
        default:
			return n;
        }
    }

    vector<snode> compile_all(vector<snode> &lst) {
        vector<snode> compiled;
        int last = lst.size() - 1;        
        for (int i = 0; i <= last; i++) {
            compiled.push_back(compile(lst[i]));
        }
        return compiled;
    }

    snode eval_all(vector<snode> &lst) {
        int last = lst.size() - 1;
        if (last < 0) return nil;
        for (int i = 0; i < last; i++) {
            eval(lst[i], global_env);
        }
        return eval(lst[last], global_env);
    }
        
    template <typename T>
    void print_map_keys(map<string, T> &m) {
        int i=0;
        for (typename map<string, T>::iterator iter = m.begin(); iter != m.end(); iter++) {
            printf(" %s", iter->first.c_str());
            i++;
            if (i % 10 == 0) puts("");
        }
        puts("");
    }
    
    void print_logo() {
        printf("Paren %s (C) 2013-2014 Kim, Taegyoon (https://bitbucket.org/ktg/paren)\n", PAREN_VERSION);        
        puts("Predefined Symbols:");
        //print_map_keys(global_env->env);
		vector<string> v;
		for (map<int, snode>::iterator iter = global_env->env.begin(); iter != global_env->env.end(); iter++) {			
			v.push_back(symname[iter->first]);
		}
		sort(v.begin(), v.end());
		for (vector<string>::iterator iter = v.begin(); iter != v.end(); iter++) {
			printf(" %s", (*iter).c_str());
		}
		puts("");

		puts("Macros:");
        print_map_keys(macros);
    } 

    void prompt() {
        printf("> ");
    }

    void prompt2() {
        printf("  ");
    }

    snode eval_string(string &s) {
        vector<snode> vec = parse(s);
		vector<snode> compiled = compile_all(vec);
        return eval_all(compiled);
    }

    snode eval_string(const char* s) {
        string s2(s);
        return eval_string(s2);
    }

    inline void eval_print(string &s) {
		puts(eval_string(s)->str_with_type().c_str());
    }

    // read-eval-print loop
    void repl() {
        string code;
        while (true) {
            if (code.length() == 0) prompt(); else prompt2();
            string line;
            if (!getline(cin, line)) { // EOF
                eval_print(code);
                return;
            }
            code += '\n' + line;
            tokenizer t(code);
            t.tokenize();
            if (t.unclosed <= 0) { // no unmatched parenthesis nor quotation
                eval_print(code);
                code = "";
            }
        }
    }

    snode get(const char* name) {
        string s(name);
        return global_env->get(ToCode(s));
    }

	void set(const char* name, node value) {
		string s(name);
		global_env->env[ToCode(s)] = make_snode(value);
	}

	// extracts characters from filename and stores them into str
	bool slurp(string filename, string &str) {        
		FILE *file = fopen(filename.c_str(), "r");
		if (file != NULL) {
			fseek(file, 0, SEEK_END);
			long size = ftell(file);
			fseek(file, 0, SEEK_SET);
			char *buf = new char[size];
			fread(buf, sizeof(char), size, file);
			fclose(file);			

			str = string(buf, size);
			delete[] buf;
			return true;
		}
		else {
			return false;
		}
	}

	// Opposite of slurp. Writes str to filename.
	int spit(string filename, const string &str) {
		FILE *file = fopen(filename.c_str(), "w");
		if (file != NULL) {
			int written = fwrite(str.data(), sizeof(char), str.length(), file);
			fclose(file);
			return written;		
		}
		else {
			return -1;
		}
	}

	snode special_def(vector<snode> &raw_args, senvironment &env) { // (def SYMBOL VALUE) ; set in the current environment
		snode value = make_snode(*eval(raw_args[2], env));
		return env->set(raw_args[1], value);
	}

	snode special_if(vector<snode> &raw_args, senvironment &env) { // (if CONDITION THEN_EXPR {ELSE_EXPR})
		snode &cond = raw_args[1];
		if (eval(cond, env)->v_bool) {
			return eval(raw_args[2], env);
		}
		else {
			if (raw_args.size() < 4) return nil;
			return eval(raw_args[3], env);
		}
	}

	snode special_set(vector<snode> &raw_args, senvironment &env) { // (set SYMBOL-OR-PLACE VALUE)
		snode var = eval(raw_args[1], env);
		snode value = make_snode(*eval(raw_args[2], env));
		if (raw_args[1]->type == node::T_SYMBOL && var == nil) {// new variable
			return env->set(raw_args[1], value);
		} else {
			*var = *value;
			return var;
		}
	}

	snode special_fn(vector<snode> &raw_args, senvironment &env) { // (fn (ARGUMENT ..) BODY) => lexical closure
		snode n2 = fn(make_snode(raw_args), make_shared<environment>(env));
		return n2;
	}

	snode builtin_plus(vector<snode> &args, senvironment &env) { // (+ X ..)
		int len = args.size();
		if (len <= 0) return node_0;
		snode first = args[0];
		if (first->type == node::T_INT) {
			int sum = first->v_int;
			for (vector<snode>::iterator i = args.begin() + 1; i != args.end(); i++) {
				sum += (*i)->to_int();
			}
			return make_snode(sum);
		}
		else {
			double sum = first->v_double;
			for (vector<snode>::iterator i = args.begin() + 1; i != args.end(); i++) {
				sum += (*i)->to_double();
			}
			return make_snode(sum);
		}
	}

	snode builtin_minus(vector<snode> &args, senvironment &env) { // (- X ..)
		int len = args.size();
		if (len <= 0) return node_0;
		snode first = args[0];
		if (first->type == node::T_INT) {
			int sum = first->v_int;
			for (vector<snode>::iterator i = args.begin() + 1; i != args.end(); i++) {
				sum -= (*i)->to_int();
			}
			return make_snode(sum);
		}
		else {
			double sum = first->v_double;
			for (vector<snode>::iterator i = args.begin() + 1; i != args.end(); i++) {
				sum -= (*i)->to_double();
			}
			return make_snode(sum);
		}
	}

	snode builtin_mul(vector<snode> &args, senvironment &env) { // (* X ..)
		int len = args.size();
		if (len <= 0) return node_1;
		snode first = args[0];
		if (first->type == node::T_INT) {
			int sum = first->v_int;
			for (vector<snode>::iterator i = args.begin() + 1; i != args.end(); i++) {
				sum *= (*i)->to_int();
			}
			return make_snode(sum);
		}
		else {
			double sum = first->v_double;
			for (vector<snode>::iterator i = args.begin() + 1; i != args.end(); i++) {
				sum *= (*i)->to_double();
			}
			return make_snode(sum);
		}
	}

	snode builtin_div(vector<snode> &args, senvironment &env) { // (/ X ..)
		int len = args.size();
		if (len <= 0) return node_1;
		snode first = args[0];
		if (first->type == node::T_INT) {
			int sum = first->v_int;
			for (vector<snode>::iterator i = args.begin() + 1; i != args.end(); i++) {
				sum /= (*i)->to_int();
			}
			return make_snode(sum);
		}
		else {
			double sum = first->v_double;
			for (vector<snode>::iterator i = args.begin() + 1; i != args.end(); i++) {
				sum /= (*i)->to_double();
			}
			return make_snode(sum);
		}
	}

	snode builtin_lt(vector<snode> &args, senvironment &env) { // (< X Y)
		if (args[0]->type == node::T_INT) {
			return make_snode(args[0]->v_int < args[1]->to_int());
		}
		else {
			return make_snode(args[0]->v_double < args[1]->to_double());
		}
	}

	snode builtin_caret(vector<snode> &args, senvironment &env) { // (^ BASE EXPONENT)
		return make_snode(pow(args[0]->to_double(), args[1]->to_double()));
	}

	snode builtin_percent(vector<snode> &args, senvironment &env) { // (% DIVIDEND DIVISOR)
		return make_snode(args[0]->to_int() % args[1]->to_int());
	}

	snode builtin_sqrt(vector<snode> &args, senvironment &env) { // (sqrt X)
		return make_snode(sqrt(args[0]->to_double()));
	}

	snode builtin_plusplus(vector<snode> &args, senvironment &env) { // (++ X)
		int len = args.size();
		if (len <= 0) return make_snode(0);
		snode first = args[0];
		if (first->type == node::T_INT) {
			first->v_int++;
		}
		else {
			first->v_double++;
		}
		return first;
	}

	snode builtin_minusminus(vector<snode> &args, senvironment &env) { // (-- X)
		int len = args.size();
		if (len <= 0) return make_snode(0);
		snode first = args[0];
		if (first->type == node::T_INT) {
			first->v_int--;
		}
		else {
			first->v_double--;
		}
		return first;
	}

	snode builtin_floor(vector<snode> &args, senvironment &env) { // (floor X)
		return make_snode(floor(args[0]->to_double()));
	}

	snode builtin_ceil(vector<snode> &args, senvironment &env) { // (ceil X)
		return make_snode(ceil(args[0]->to_double()));
	}

	snode builtin_ln(vector<snode> &args, senvironment &env) { // (ln X)
		return make_snode(log(args[0]->to_double()));
	}

	snode builtin_log10(vector<snode> &args, senvironment &env) { // (log10 X)
		return make_snode(log10(args[0]->to_double()));
	}

	snode builtin_rand(vector<snode> &args, senvironment &env) { // (rand)
		return make_snode(rand_double());
	}

	snode builtin_eqeq(vector<snode> &args, senvironment &env) { // (== X ..) short-circuit                    
	    snode first = args[0];
	    if (first->type == node::T_INT) {
	        int firstv = first->v_int;
	        for (vector<snode>::iterator i = args.begin() + 1; i != args.end(); i++) {
	            if ((*i)->to_int() != firstv) {return node_false;}
	        }
	    }
	    else {
	        double firstv = first->v_double;
			for (vector<snode>::iterator i = args.begin() + 1; i != args.end(); i++) {
				if ((*i)->to_double() != firstv) { return node_false; }
			}
	    }
	    return node_true;
	}

	snode special_andand(vector<snode> &raw_args, senvironment &env) { // (&& X ..) short-circuit
		for (vector<snode>::iterator i = raw_args.begin() + 1; i != raw_args.end(); i++) {
			if (!(eval(*i, env))->v_bool) { return node_false; }
		}
		return node_true;
	}

	snode special_oror(vector<snode> &raw_args, senvironment &env) { // (|| X ..) short-circuit
		for (vector<snode>::iterator i = raw_args.begin() + 1; i != raw_args.end(); i++) {
			if ((eval(*i, env))->v_bool) { return node_true; }
		}
		return node_false;
	}

	snode builtin_not(vector<snode> &args, senvironment &env) { // (! X)
		return make_snode(!(args[0]->v_bool));
	}

	snode special_while(vector<snode> &raw_args, senvironment &env) { // (while CONDITION EXPR ..)
		snode &cond = raw_args[1];
		int len = raw_args.size();
		while (eval(cond, env)->v_bool) {
		    for (int i = 2; i < len; i++) {
		        eval(raw_args[i], env);
		    }
		}
		return nil; 
	}

	snode builtin_strlen(vector<snode> &args, senvironment &env) { // (strlen X)
		return make_snode((int)args[0]->v_string.size());
	}

	snode builtin_string(vector<snode> &args, senvironment &env) { // (string X ..)
		int len = args.size();
		if (len <= 1) return make_snode(string());		
		string acc;
		for (vector<snode>::iterator i = args.begin(); i != args.end(); i++) {
		    acc += (*i)->to_string();
		}
		return make_snode(acc);
	}

	snode builtin_char_at(vector<snode> &args, senvironment &env) { // (char-at X)
		return make_snode(args[0]->v_string[args[1]->to_int()]);
	}

	snode builtin_chr(vector<snode> &args, senvironment &env) { // (chr X)
		char temp[2] = " ";
		temp[0] = (char) args[0]->to_int();
		return make_snode(string(temp));
	}
	
	snode builtin_double(vector<snode> &args, senvironment &env) { // (double X)
		return make_snode(args[0]->to_double());
	}

	snode builtin_int(vector<snode> &args, senvironment &env) { // (int X)
		return make_snode(args[0]->to_int());
	}

	snode builtin_read_string(vector<snode> &args, senvironment &env) { // (read-string X)
		return parse(args[0]->to_string())[0];
	}
	
	snode builtin_type(vector<snode> &args, senvironment &env) { // (type X)
		return make_snode(args[0]->type_str());
	}

	snode builtin_eval(vector<snode> &args, senvironment &env) { // (eval X)		
		return eval(args[0], env);
	}

	snode special_quote(vector<snode> &raw_args, senvironment &env) { // (quote X)
		return raw_args[1];
	}

	snode builtin_list(vector<snode> &args, senvironment &env) { // (list X ..)
		vector<snode> ret;
		ret.reserve(args.size());
		for (auto &n : args) {
			ret.push_back(n);
		}
		return make_snode(ret);
	}

	snode apply(snode &func, vector<snode> &args, senvironment &env) {
		if (func->type == node::T_BUILTIN) {
			return func->v_builtin(args, env);
		}
		else if (func->type == node::T_FN) {
			vector<snode> &f = func->v_list;			
			// anonymous function application-> lexical scoping
			// (fn (ARGUMENT ..) BODY ..)
			vector<snode> &arg_syms = f[1]->v_list;
			
			shared_ptr<environment> local_env(make_shared<environment>(environment(func->outer_env)));
			
			int alen = arg_syms.size();
			for (int i = 0; i < alen; i++) { // assign arguments
				//string &k = arg_syms.at(i)->v_string;
				//local_env.env[k] = eval(n->v_list.at(i+1), env);
				snode &k = arg_syms[i];
				local_env->env[k->code] = args[i];
			}

			int last = f.size() - 1;
			if (last < 2) return nil;
			for (int i = 2; i < last; i++) { // body
				eval(f.at(i), local_env);
			}
			return eval(f[last], local_env);
		}
		else {
			return nil;
		}
	}

	snode builtin_apply(vector<snode> &args, senvironment &env) { // (apply FUNC LIST)
		snode func = args[0];
		vector<snode> lst = args[1]->v_list;		
		return apply(func, lst, env);
	}

	snode builtin_fold(vector<snode> &args, senvironment &env) { // (fold FUNC LIST)
		snode f = args[0];
		snode lst = args[1];
		snode acc = lst->v_list[0];
		vector<snode> args2;
		args2.reserve(2);
		args2.push_back(nil); // first argument
		args2.push_back(nil); // second argument
		for (unsigned int i = 1; i < lst->v_list.size(); i++) {
			args2[0] = acc;
			args2[1] = lst->v_list.at(i);			
			acc = apply(f, args2, env);
		}
		return acc;
	}

	snode builtin_map(vector<snode> &args, senvironment &env) { // (map FUNC LIST)
		snode f = args[0];
		snode lst = args[1];
		vector<snode> acc;
		vector<snode> args2;
		auto len = lst->v_list.size();
		acc.reserve(len);
		args2.push_back(nil);
		for (unsigned int i = 0; i < len; i++) {
			args2[0] = lst->v_list[i];
			acc.push_back(apply(f, args2, env));
		}
		return make_snode(acc);
	}

	snode builtin_filter(vector<snode> &args, senvironment &env) { // (filter FUNC LIST)
		snode f = args[0];
		snode lst = args[1];
		vector<snode> acc;
		vector<snode> args2;
		args2.push_back(nil);
		for (unsigned int i = 0; i < lst->v_list.size(); i++) {
			snode item = lst->v_list.at(i);
			args2[0] = item;			
			snode ret = apply(f, args2, env);
			if (ret->v_bool) acc.push_back(item);
		}
		return make_snode(acc);
	}

	snode builtin_push_backd(vector<snode> &args, senvironment &env) { // (push-back! LIST ITEM) ; destructive
		args[0]->v_list.push_back(make_snode(*args[1]));
		return args[0];
	}

	snode builtin_pop_backd(vector<snode> &args, senvironment &env) { // (pop-back! LIST) ; destructive
		auto &v = args[0]->v_list;
		snode n = v.back();
		v.pop_back();
		return n;
	}

	snode builtin_nth(vector<snode> &args, senvironment &env) { // (nth INDEX LIST)
		return args[1]->v_list[args[0]->to_int()];
	}

	snode builtin_length(vector<snode> &args, senvironment &env) { // (length LIST)
		return make_snode((int)args[0]->v_list.size());
	}

	snode special_begin(vector<snode> &raw_args, senvironment &env) { // (begin X ..)
		int last = raw_args.size() - 1;
		if (last <= 0) return nil;
		for (int i = 1; i < last; i++) {
		    eval(raw_args[i], env);
		}
		return eval(raw_args[last], env);
	}
	
	snode builtin_pr(vector<snode> &args, senvironment &env) { // (pr X ..)
		vector<snode>::iterator first = args.begin();
		for (vector<snode>::iterator i = first; i != args.end(); i++) {
		    if (i != first) printf(" ");
		    printf("%s", (*i)->to_string().c_str());
		}
		return nil;
	}
	
	snode builtin_prn(vector<snode> &args, senvironment &env) { // (prn X ..)
		builtin_pr(args, env);
		puts("");
		return nil;
	}

	snode builtin_exit(vector<snode> &args, senvironment &env) { // (exit {X})
		puts("");
		if (args.size() == 0) exit(0);
		exit(args[0]->to_int());
		return nil; 
	}

	snode builtin_system(vector<snode> &args, senvironment &env) { // (system X ..) ; Invokes the command processor to execute a command.
		string cmd;
		for (snode &n : args) {
			cmd += n->to_string();
		}
		return make_snode(system(cmd.c_str()));
	}
	
	snode builtin_cons(vector<snode> &args, senvironment &env) { // (cons X LST): Returns a new list where x is the first element and lst is the rest.
		snode x = args[0];
		snode lst = args[1];
		vector<snode> r;
		r.push_back(x);
		for (vector<snode>::iterator i = lst->v_list.begin(); i != lst->v_list.end(); i++) {
			r.push_back(*i);
		}
		return make_snode(r);
	}

	snode builtin_read_line(vector<snode> &args, senvironment &env) { // (read-line)
		string line;
		if (!getline(cin, line)) { // EOF								
			return nil;
		} else {
			return make_snode(line);
		}
	}
	
	snode builtin_slurp(vector<snode> &args, senvironment &env) { // (slurp FILENAME)
		string filename = args[0]->to_string();
		string str;
		if (slurp(filename, str))
			return make_snode(str);
		else
			return nil;
	}

	snode builtin_spit(vector<snode> &args, senvironment &env) { // (spit FILENAME STRING)
		string filename = args[0]->to_string();
		string str = args[1]->to_string();
		return make_snode(spit(filename, str));
	}

	snode special_thread(vector<snode> &raw_args, senvironment &env) { // (thread EXPR ..): Creates new thread and starts it.
		node n2;
		n2.type = node::T_THREAD;
		// You can not use shared_ptr for thread. It is deleted early.
		n2.p_thread = new thread([&]() {
			vector<snode> exprs(raw_args.begin() + 1, raw_args.end()); // to prevent early deletion and keep expressions in memory.
			for (auto &sn : exprs) { eval(sn, env); }
		});
		return make_snode(n2);
	}

	snode builtin_join(vector<snode> &args, senvironment &env) { // (join THREAD): wait for THREAD to end
		snode t = args[0];
		pthread &pt = t->p_thread;
		if (pt != nullptr) {
			pt->join();
			delete pt;
			pt = nullptr;
		}
		return nil;
	}

	void init() {
		srand((unsigned int)time(0));
				
		global_env = make_shared<environment>(environment());

		global_env->env[ToCode("true")] = make_snode(true);
		global_env->env[ToCode("false")] = make_snode(false);
		global_env->env[ToCode("E")] = make_snode(2.71828182845904523536);
		global_env->env[ToCode("PI")] = make_snode(3.14159265358979323846);

		global_env->env[ToCode("def")] = make_special(special_def);
		global_env->env[ToCode("set")] = make_special(special_set);
		global_env->env[ToCode("if")] = make_special(special_if);
		global_env->env[ToCode("fn")] = make_special(special_fn);
		global_env->env[ToCode("begin")] = make_special(special_begin);
		global_env->env[ToCode("while")] = make_special(special_while);
		global_env->env[ToCode("quote")] = make_special(special_quote);
		global_env->env[ToCode("&&")] = make_special(special_andand);
		global_env->env[ToCode("||")] = make_special(special_oror);
		global_env->env[ToCode("thread")] = make_snode(special_thread);

		global_env->env[ToCode("eval")] = make_snode(builtin_eval);
		global_env->env[ToCode("+")] = make_snode(builtin_plus);
		global_env->env[ToCode("-")] = make_snode(builtin_minus);
		global_env->env[ToCode("*")] = make_snode(builtin_mul);
		global_env->env[ToCode("/")] = make_snode(builtin_div);
		global_env->env[ToCode("<")] = make_snode(builtin_lt);		
		global_env->env[ToCode("^")] = make_snode(builtin_caret);
		global_env->env[ToCode("%")] = make_snode(builtin_percent);
		global_env->env[ToCode("sqrt")] = make_snode(builtin_sqrt);
		global_env->env[ToCode("++")] = make_snode(builtin_plusplus);
		global_env->env[ToCode("--")] = make_snode(builtin_minusminus);
		global_env->env[ToCode("floor")] = make_snode(builtin_floor);
		global_env->env[ToCode("ceil")] = make_snode(builtin_ceil);
		global_env->env[ToCode("ln")] = make_snode(builtin_ln);
		global_env->env[ToCode("log10")] = make_snode(builtin_log10);
		global_env->env[ToCode("rand")] = make_snode(builtin_rand);
		global_env->env[ToCode("==")] = make_snode(builtin_eqeq);
		global_env->env[ToCode("<")] = make_snode(builtin_lt);
		global_env->env[ToCode("!")] = make_snode(builtin_not);
		global_env->env[ToCode("strlen")] = make_snode(builtin_strlen);		
		global_env->env[ToCode("char-at")] = make_snode(builtin_char_at);
		global_env->env[ToCode("chr")] = make_snode(builtin_chr);
		global_env->env[ToCode("int")] = make_snode(builtin_int);
		global_env->env[ToCode("double")] = make_snode(builtin_double);
		global_env->env[ToCode("string")] = make_snode(builtin_string);
		global_env->env[ToCode("read-string")] = make_snode(builtin_read_string);
		global_env->env[ToCode("type")] = make_snode(builtin_type);		
		global_env->env[ToCode("list")] = make_snode(builtin_list);
		global_env->env[ToCode("apply")] = make_snode(builtin_apply);
		global_env->env[ToCode("fold")] = make_snode(builtin_fold);
		global_env->env[ToCode("map")] = make_snode(builtin_map);
		global_env->env[ToCode("filter")] = make_snode(builtin_filter);
		global_env->env[ToCode("push-back!")] = make_snode(builtin_push_backd);
		global_env->env[ToCode("pop-back!")] = make_snode(builtin_pop_backd);
		global_env->env[ToCode("nth")] = make_snode(builtin_nth);
		global_env->env[ToCode("length")] = make_snode(builtin_length);		
		global_env->env[ToCode("pr")] = make_snode(builtin_pr);
		global_env->env[ToCode("prn")] = make_snode(builtin_prn);
		global_env->env[ToCode("exit")] = make_snode(builtin_exit);
		global_env->env[ToCode("system")] = make_snode(builtin_system);
		global_env->env[ToCode("cons")] = make_snode(builtin_cons);
		global_env->env[ToCode("read-line")] = make_snode(builtin_read_line);
		global_env->env[ToCode("slurp")] = make_snode(builtin_slurp);
		global_env->env[ToCode("spit")] = make_snode(builtin_spit);
		global_env->env[ToCode("join")] = make_snode(builtin_join);

		char library[] = "library.paren";
		string code;
		if (libparen::slurp(library, code)) {
			libparen::eval_string(code);
		}
		else {
			printf("Error loading %s\n", library);			
		}		
	}
} // namespace libparen
