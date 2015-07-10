// (C) 2013-2014 Kim, Taegyoon
// Paren language core

#pragma once
#ifndef LIBPAREN_H
#define LIBPAREN_H

#include <iostream>
#include <string>
#include <cstdio>
#include <vector>
#include <cstdlib>
#include <cmath>
#include <map>
#include <ctime>
#include <memory>
#include <thread>

#define PAREN_VERSION "1.9.8"

namespace libparen {
    using namespace std;

	struct node;
	struct environment;
	struct paren;
	typedef shared_ptr<node> snode;
	typedef shared_ptr<thread> sthread;
	typedef shared_ptr<environment> senvironment;
	typedef thread *pthread;
	typedef snode(*builtin)(vector<snode> &args, senvironment &env);

    struct node {
        enum {T_NIL, T_INT, T_DOUBLE, T_BOOL, T_STRING, T_SYMBOL, T_LIST, T_SPECIAL, T_BUILTIN, T_FN, T_THREAD} type;
        union {
            int v_int, code; // if T_SYMBOL, code for faster access			
            double v_double;
            bool v_bool;
			//node *p_node; // if T_SYMBOL, cached address of the data			
			pthread p_thread = nullptr;
			builtin v_builtin;
        };
        string v_string;
        vector<snode> v_list;
		senvironment outer_env; // if T_FN
		//sthread s_thread;

        node();
        node(int a);
        node(double a);
        node(bool a);
        node(const string &a);
        node(const vector<snode> &a);
		node(builtin a);		

        int to_int(); // convert to int
        double to_double(); // convert to double
        string to_string(); // convert to string
        string type_str();
        string str_with_type();
    };

    struct environment {
        map<int, snode> env;
		senvironment outer;
        environment();
		environment(senvironment outer);
        snode get(int code);
		snode get(snode &k);
		snode set(snode &k, snode &v);
    };

    
    void init();
                
    snode eval(snode &n, senvironment &env);        
    snode eval_all(vector<snode> &lst);
    snode compile(snode &n);
    vector<snode> compile_all(vector<snode> &lst);
	snode apply(snode &func, vector<snode> &args, senvironment &env);
    void print_logo();
    void prompt();
    void prompt2();    
    snode eval_string(string &s);
    snode eval_string(const char* s);
    inline void eval_print(string &s);
    void repl(); // read-eval-print loop

    snode get(const char *name);
	void set(const char *name, node value);
	
	inline double rand_double();
	bool slurp(string filename, string &str);
	int spit(string filename, const string &str);
	int ToCode(const string &name);
	snode make_snode(const node& n);
	
	vector<string> tokenize(const string &s);
	vector<snode> parse(const string &s);
} // namespace libparen
#endif
