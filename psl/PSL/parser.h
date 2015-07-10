
class Parser
{
public:
	Parser(Tokenizer *to)
	{
		t = to;
		error = 0;
	}
	void Parse(variable &v)
	{
		string arg = "arg";
		v.pushcode(new Variable::PUSH_NULL);
		v.pushcode(new Variable::LOCAL(arg));
		v.pushcode(new Variable::ARGUMENT);
		while (t->checkNext())
			ParseStatement(v, v);
		if (error)
			PSL_PRINTF(("%d error in compile %s\n", error, t->getFile().c_str()));
//			PSL_PRINTF(("%d error%s in compile %s\n", error, error == 1 ? "" : "s", t->getFile().c_str()));
	}
	int getErrorNum()	{return error;}
private:
	enum ErrorID
	{
		_ERROR = 1,
		TINA,
		TINSC,
		TINLAG,
		TINIA,
		NOTT,
		UKT,
		TINCOT,
		TINAE,
		WARNING,
		IIAE,
		BINC,
	};
	void Error(ErrorID n, int op = 0, const char *str = "", int line = -1)
	{
		if (line < 0)
			line = t->getLine();
		if (n < WARNING)
		{
			++error;
			PSL_PRINTF(("error %s %d,%d: ", t->getFile().c_str(), line, t->getByte()));
		}
		else
		{
			PSL_PRINTF(("warning %s %d,%d: ", t->getFile().c_str(), line, t->getByte()));
		}
		switch (n)
		{
		case TINA:	PSL_PRINTF(("There is no %c after %s\n", op, str));break;
		case TINSC:	PSL_PRINTF(("There is no %s-statement condition\n", str));break;
		case TINLAG:PSL_PRINTF(("There is no label after goto\n"));break;
		case NOTT:	PSL_PRINTF(("not term : %c\n", op));break;
		case UKT:	PSL_PRINTF(("unknown token\n"));break;
		case TINIA:	PSL_PRINTF(("There is no identifier after %s\n", str));break;
		case TINCOT:PSL_PRINTF(("There is no : on ternary operation\n"));break;
		case TINAE:	PSL_PRINTF(("There is no %c at expression end\n", op));break;
		case IIAE:	PSL_PRINTF(("'%s' is already exsit\n", str));break;
		case BINC:	PSL_PRINTF(("block not closed from %d\n", op));break;
		default:	PSL_PRINTF(("unknown error\n"));
		}
	}
	enum Declare
	{
		DECLARE_NONE = 0,
		DECLARE_GLOBAL,
		DECLARE_STATIC,
		DECLARE_LOCAL,
	};
	void Declaration(variable &c, Declare d, string &name)
	{
		if (d == DECLARE_GLOBAL)		c.pushcode(new Variable::GLOBAL(name));
		else if (d == DECLARE_STATIC)	c.pushcode(new Variable::STATIC(name));
		else if (d == DECLARE_LOCAL)	c.pushcode(new Variable::LOCAL(name));
		else							c.pushcode(new Variable::DECLARATION(name));
	}
	void ParseDangling(variable &g, variable &c)
	{
		if (t->getNextIf('{'/*'}'*/))
			ParseBlock(g, c);
		else
			ParseStatement(g, c);
	}
	void ParseIf(variable &g, variable &c)
	{
		variable v;
		if (t->getNextIf('('/*')'*/))
			ParseExpression(v, /*'('*/')');
		else
			Error(TINA, '('/*')'*/, "if");
		size_t l = v.codelength();
		Variable::OpCode *oc = NULL;
		if (l)
		{
			oc = new Variable::JRF(0);
			v.pushcode(oc);
			l = v.codelength();
		}
		else
		{
			Error(TINSC, 0, "if");
		}
		ParseDangling(g, v);
		if (t->checkNext() == Tokenizer::IDENTIFIER && t->nstr == "else")
		{
			t->getNext();
			if (oc)
				oc->set(static_cast<int>(v.codelength()+1-l));
			oc = new Variable::JR(0);
			v.pushcode(oc);
			l = v.codelength();
			ParseDangling(g, v);
			oc->set(static_cast<int>(v.codelength()-l));
		}
		else
		{
			if (oc)
				oc->set(static_cast<int>(v.codelength()-l));
		}
		if (Variable::Code *x = v.getcode())
		{
		#ifdef PSL_IF_STATEMENT_NOT_SCOPE
			Variable::Code *code = c.getcode();
			if (!code)	c = v;
			else		code->push(x);
		#else
			#ifdef PSL_DEBUG
			c.pushcode(new Variable::IF(x));
			#else
			c.pushcode(new Variable::SCOPE(x));
			#endif
		#endif
		}
	}
	void ParseFor(variable &g, variable &c)
	{
		variable v;
		size_t l = 0;
		if (!t->getNextIf('('/*')'*/))
			Error(TINA, '('/*')'*/, "for");
		if (!t->getNextIf(';'))
		{
			v.pushcode(new Variable::POP);
			ParseExpression(v, ';');
			l = v.codelength();
		}
		Variable::OpCode *oc = NULL;
		if (!t->getNextIf(';'))
		{
			ParseExpression(v, ';');
			if (v.codelength() - l)
			{
				oc = new Variable::JF(static_cast<int>(v.codelength()));
				v.pushcode(oc);
			}
		}
		variable x;
		if (!t->getNextIf(/*'('*/')'))
			ParseExpression(x, /*'('*/')');
		ParseDangling(g, v);
		size_t cline = v.codelength();
		if (x.codelength())
		{
			v.pushcode(new Variable::POP);
			v.getcode()->push(x.getcode());
		}
		v.pushcode(new Variable::JMP(static_cast<int>(l)));
		if (oc)
			oc->set(static_cast<int>(v.codelength()));
		if (t->checkNext() == Tokenizer::IDENTIFIER && t->nstr == "else")
		{
			t->getNext();
			ParseDangling(g, v);
		}
		if (Variable::Code *z = v.getcode())
			c.pushcode(new Variable::LOOP(z, cline));
	}
	void ParseWhile(variable &g, variable &c)
	{
		variable v;
		if (t->getNextIf('('/*')'*/))
			ParseExpression(v, /*'('*/')');
		else
			Error(TINA, '('/*')'*/, "while");
		size_t l = v.codelength();
		Variable::OpCode *oc = NULL;
		if (l)
		{
			oc = new Variable::JF(static_cast<int>(l));
			v.pushcode(oc);
		}
		else
			Error(TINSC, 0, "while");

		ParseDangling(g, v);

		v.pushcode(new Variable::JMP(0));
		if (oc)
			oc->set(static_cast<int>(v.codelength()));
		if (t->checkNext() == Tokenizer::IDENTIFIER && t->nstr == "else")
		{
			t->getNext();
			ParseDangling(g, v);
		}
		if (Variable::Code *x = v.getcode())
			c.pushcode(new Variable::LOOP(x, 0));
	}
	void ParseStatement(variable &g, variable &c)
	{
		int n = t->checkNext();
		if (n == ';')	// empty
		{
			t->getNext();
			return;
		}
		#ifdef PSL_DEBUG
		c.pushlabel(t->getFile() + " " + t->getLine());
		#endif
		if (n == '{'/*'}'*/)	// anonymouse scope
		{
			t->getNext();
			variable v;
			ParseBlock(g, v);
			if (v.codelength())
				c.pushcode(new Variable::SCOPE(v.getcode()));
			return;
		}
		if (n == Tokenizer::IDENTIFIER)
		{
			if (t->nstr == "global" || t->nstr == "static" || t->nstr == "local" || t->nstr == "yield")
			{
				c.pushcode(new Variable::POP);
				ParseExpression(c, ';');
				return;
			}
			t->getNext();
			if (t->nstr == "if")
			{
				ParseIf(g, c);
				return;
			}
			if (t->nstr == "for")
			{
				ParseFor(g, c);
				return;
			}
			if (t->nstr == "while")
			{
				ParseWhile(g, c);
				return;
			}
			if (t->nstr == "return")
			{
				if (t->checkNext() != ';')
				{
					c.pushcode(new Variable::POP);
					ParseExpression(c, ';');
				}
				c.pushcode(new Variable::RETURN);
				return;
			}
			if (t->nstr == "continue")
			{
				if (!t->getNextIf(';'))	Error(TINA, ';', "continue");
				c.pushcode(new Variable::CONTINUE);
				return;
			}
			if (t->nstr == "break")
			{
				if (!t->getNextIf(';'))	Error(TINA, ';', "break");
				c.pushcode(new Variable::BREAK);
				return;
			}
			if (t->nstr == "goto")
			{
				if (!t->getNextIf(Tokenizer::IDENTIFIER))
					Error(TINLAG);
				else
				{
					c.pushcode(new Variable::GOTO(t->nstr));
					if (!t->getNextIf(';'))
						Error(TINA, ';', "goto");
				}
				return;
			}

			string name = t->nstr;
			int line = t->getLine();
			n = t->checkNext();
			if (n == ':')	// label
			{
				t->getNext();
				c.pushlabel(name);
				return;
			}
			if (n == '{'/*'}'*/)	// class definition
			{
				t->getNext();
				if (g.exist(name))
					Error(IIAE, 0, name, line);
				variable m = g[name];
				ParseBlock(m, m);
				return;
			}
			if (n == '('/*')'*/)
			{
				variable arg;
				t->getNext();
				if (!t->getNextIf(/*'('*/')'))
					ParseExpression(arg, /*'('*/')');
				else
					arg.pushcode(new Variable::PUSH_NULL);
				n = t->checkNext();
				if (n == '{'/*'}'*/ || n == Tokenizer::IDENTIFIER)	// function definition
				{
					if (g.exist(name))
						Error(IIAE, 0, name, line);
					variable m = g[name];
					m = arg;
					m.pushcode(new Variable::ARGUMENT);
					if (n == '{'/*'}'*/)
					{
						t->getNext();
						ParseBlock(m, m);
					}
					else if (n == Tokenizer::IDENTIFIER)
						ParseStatement(m, m);
					return;
				}
				else	// call
				{
					c.pushcode(new Variable::POP);
					c.pushcode(new Variable::VARIABLE(name));
					c.getcode()->push(arg.getcode());
					c.pushcode(new Variable::CALL);
				}
			}
			else
			{
				c.pushcode(new Variable::POP);
				c.pushcode(new Variable::VARIABLE(name));
			}
			getSuffOp(c);
			if (t->getNextIf(Tokenizer::IDENTIFIER))
			{
				c.pushcode(new Variable::INSTANCE);
				c.pushcode(new Variable::DECLARATION(t->nstr));
				getSuffOp(c);
			}
			ParseExpression(c, ';', true);
		}
		else
		{
			c.pushcode(new Variable::POP);
			ParseExpression(c, ';');
		}
	}
	void ParseBlock(variable &g, variable &c)
	{
		int line = t->getLine();
		while (int n = t->checkNext())
		{
			if (n == /*'{'*/'}')
			{
				t->getNext();
				return;
			}
			ParseStatement(g, c);
		}
		Error(BINC, line);
	}
	void ParseTable(variable &c)
	{
		c.pushcode(new Variable::PUSH_NULL);
		while (int n = t->checkNext())
		{
			if (n == ']')
			{
				t->getNext();
				return;
			}
			else if (n == Tokenizer::IDENTIFIER || n == Tokenizer::STRING)
			{
				string name = t->nstr;
				t->getNext();
				if (t->getNextIf(':'))
				{
					getexp11(c);
					c.pushcode(new Variable::SET_MEMBER(name));
				}
				else
				{
					if (n == Tokenizer::IDENTIFIER)
						c.pushcode(new Variable::VARIABLE(name));
					else
						c.pushcode(new Variable::PUSH_STRING(name));
					getSuffOp(c);
					getexp11(c, true);
					c.pushcode(new Variable::ARRAY_PUSH);
				}
			}
			else if (n == Tokenizer::INT)
			{
				int i = t->nint;
				t->getNext();
				if (t->getNextIf(':'))
				{
					getexp11(c);
					c.pushcode(new Variable::SET_INDEX(i));
				}
				else
				{
					c.pushcode(new Variable::PUSH_INT(i));
					getSuffOp(c);
					getexp11(c, true);
					c.pushcode(new Variable::ARRAY_PUSH);
				}
			}
			else
			{
				getexp11(c);
				c.pushcode(new Variable::ARRAY_PUSH);
			}
			if (!t->getNextIf(','))
			{
				if (t->checkNext() != ']')
				{
					Error(TINA, ',', "object member");
					return;
				}
			}
		}
	}
	void getSuffOp(variable &c)
	{
		while (int n = t->checkNext())
		{
			if (n == Tokenizer::INC)
			{
				t->getNext();
				c.pushcode(new Variable::INC);
			}
			else if (n == Tokenizer::DEC)
			{
				t->getNext();
				c.pushcode(new Variable::DEC);
			}
			else if (n == '('/*')'*/)
			{
				t->getNext();
				if (t->getNextIf(/*'('*/')'))
					c.pushcode(new Variable::PUSH_NULL);
				else
					ParseExpression(c, /*'('*/')');
				c.pushcode(new Variable::CALL);
			}
			else if (n == '[')
			{
				t->getNext();
				ParseExpression(c, ']');
				c.pushcode(new Variable::INDEX);
			}
			else if (n == '.')
			{
				t->getNext();
				if (t->getNextIf(Tokenizer::IDENTIFIER))
					c.pushcode(new Variable::MEMBER(t->nstr));
				else
					Error(TINIA, 0, "member access (.)");
			}
			else if (n == '`')
			{
				t->getNext();
				getexp13(c);
				c.pushcode(new Variable::CALL);
			}
			else break;
		}
	}
	#define PRE_OP(m,o) if(n==m){t->getNext();getTerm(c);c.pushcode(new Variable::o);}
	#define TERM(m,o) if(n==m){t->getNext();c.pushcode(new Variable::o);getSuffOp(c);}
	void getTerm(variable &c)
	{
		int n = t->checkNext();
		if (n == '('/*')'*/)
		{
			t->getNext();
			variable v;
			if (t->getNextIf(/*'('*/')'))
				v.pushcode(new Variable::PUSH_NULL);
			else
				ParseExpression(v, /*'('*/')');
			if (t->getNextIf('{'/*'}'*/))
			{
				v.pushcode(new Variable::ARGUMENT);
				ParseBlock(v, v);
				c.pushcode(new Variable::PUSH_CODE(v));
			}
			else
			{
				if (v.codelength())
				{
					if (c.codelength())
						c.getcode()->push(v.getcode());
					else
						c = v;
					#ifdef PSL_OPTIMIZE_PARENTHESES
					if (t->checkNext() == ',')
					#endif
						c.pushcode(new Variable::PARENTHESES);
				}
			}
			getSuffOp(c);
		}
		else if (n == '[')
		{
			t->getNext();
			ParseTable(c);
			getSuffOp(c);
		}
		else if (n == '@')
		{
			t->getNext();
			int i = 0;
			if (t->getNextIf(Tokenizer::INT))
				i = t->nint;
			c.pushcode(new Variable::LOCALINDEX(i));
			getSuffOp(c);
		}
		else PRE_OP('+', PLUS)
		else PRE_OP('-', MINUS)
		else PRE_OP('!', NOT)
		else PRE_OP('~', COMPL)
		else PRE_OP('*', DEREF)
		else PRE_OP('&', REF)
		else PRE_OP('$', CLOSURE)
		else PRE_OP(Tokenizer::INC, PINC)
		else PRE_OP(Tokenizer::DEC, PDEC)
		else TERM(Tokenizer::IDENTIFIER, VARIABLE(t->nstr))
		else TERM(Tokenizer::INT, PUSH_INT(t->nint))
		else TERM(Tokenizer::HEX, PUSH_HEX(static_cast<hex>(t->nint)))
		else TERM(Tokenizer::NUMBER, PUSH_FLOAT(t->nnum))
		else TERM(Tokenizer::STRING, PUSH_STRING(t->nstr))
		else
		{
			if (n < 0)	Error(UKT);
			else		Error(NOTT, n);
			t->getNext();
		}
	}
	#undef PRE_OP
	#undef TERM
	#define EXP0(m,e,o) if(n==m){t->getNext();get##e(c);c.pushcode(new Variable::o);}
	#define EXP(m,e,o) EXP0(m,exp##e,o)
	void getexp1(variable &c, bool l = false)
	{
		if (!l)
			getTerm(c);
		while (int n = t->checkNext())
		{
			EXP0('*', Term, MUL)
			else EXP0('/', Term, DIV)
			else EXP0('%', Term, MOD)
			else break;
		}
	}
	void getexp2(variable &c, bool l = false)
	{
		getexp1(c, l);
		while (int n = t->checkNext())
		{
			EXP('+', 1, ADD)
			else EXP('-', 1, SUB)
			else break;
		}
	}
	void getexp3(variable &c, bool l = false)
	{
		getexp2(c, l);
		while (int n = t->checkNext())
		{
			EXP(Tokenizer::SHL, 2, SHL)
			else EXP(Tokenizer::SHR, 2, SHR)
			else break;
		}
	}
	void getexp4(variable &c, bool l = false)
	{
		getexp3(c, l);
		while (int n = t->checkNext())
		{
			EXP('&', 3, AND)
			else break;
		}
	}
	void getexp5(variable &c, bool l = false)
	{
		getexp4(c, l);
		while (int n = t->checkNext())
		{
			EXP('^', 4, XOR)
			else break;
		}
	}
	void getexp6(variable &c, bool l = false)
	{
		getexp5(c, l);
		while (int n = t->checkNext())
		{
			EXP('|', 5, OR)
			else break;
		}
	}
	void getexp7(variable &c, bool l = false)
	{
		getexp6(c, l);
		while (int n = t->checkNext())
		{
			EXP('<', 6, LT)
			else EXP(Tokenizer::LE, 6, LE)
			else EXP('>', 6, GT)
			else EXP(Tokenizer::GE, 6, GE)
			else break;
		}
	}
	void getexp8(variable &c, bool l = false)
	{
		getexp7(c, l);
		while (int n = t->checkNext())
		{
			EXP(Tokenizer::EQ, 7, EQ)
			else EXP(Tokenizer::NE, 7, NEQ)
			else break;
		}
	}
	void getexp9(variable &c, bool l = false)
	{
		getexp8(c, l);
		while (int n = t->checkNext())
		{
			#if defined(PSL_OPTIMIZE_BOOL_AND) && defined(PSL_OPTIMIZE_IN_COMPILE)
			if (n==Tokenizer::BAND)
			{
				t->getNext();
				Variable::OpCode *oc = new Variable::JRF(0);
				c.pushcode(oc);
				size_t b = c.codelength();
				getexp8(c);
				c.pushcode(new Variable::JR(1));
				oc->set(static_cast<int>(c.codelength() - b));
				c.pushcode(new Variable::PUSH_NULL);
			}
			#else
			EXP(Tokenizer::BAND, 8, BAND)
			#endif
			else break;
		}
	}
	void getexp10(variable &c, bool l = false)
	{
		getexp9(c, l);
		while (int n = t->checkNext())
		{
			#if defined(PSL_OPTIMIZE_BOOL_AND) && defined(PSL_OPTIMIZE_IN_COMPILE)
			if (n==Tokenizer::BOR)
			{
				t->getNext();
				Variable::OpCode *oc = new Variable::JRT(0);
				c.pushcode(oc);
				size_t b = c.codelength();
				getexp9(c);
				oc->set(static_cast<int>(c.codelength() - b));
			}
			#else
			EXP(Tokenizer::BOR, 9, BOR)
			#endif
			else break;
		}
	}
	void getexp11(variable &c, bool l = false)
	{
		if (!l)
		{
			if (t->checkNext() == Tokenizer::IDENTIFIER)
			{
				Declare d = DECLARE_NONE;
				if (t->nstr == "global")		d = DECLARE_GLOBAL;
				else if (t->nstr == "static")	d = DECLARE_STATIC;
				else if (t->nstr == "local")	d = DECLARE_LOCAL;
				if (d)
				{
					t->getNext();
					string scope = t->nstr;
					if (t->getNextIf(Tokenizer::IDENTIFIER))
					{
						string name = t->nstr;
						variable temp;
						getSuffOp(temp);
						if (t->getNextIf(Tokenizer::IDENTIFIER))
						{
							c.pushcode(new Variable::VARIABLE(name));
							if (temp.codelength())
								c.getcode()->push(temp.getcode());
							c.pushcode(new Variable::INSTANCE);
							Declaration(c, d, t->nstr);
						}
						else
						{
							c.pushcode(new Variable::PUSH_NULL());
							Declaration(c, d, name);
							if (temp.codelength())
								c.getcode()->push(temp.getcode());
						}
					}
					else
					{
						getTerm(c);
						if (t->getNextIf(Tokenizer::IDENTIFIER))
						{
							c.pushcode(new Variable::INSTANCE);
							Declaration(c, d, t->nstr);
						}
						else
						{
							Error(TINIA, 0, scope);
						}
					}
					l = true;
				}
			}
			if (!l)
			{
				getTerm(c);
				if (t->getNextIf(Tokenizer::IDENTIFIER))
				{
					c.pushcode(new Variable::INSTANCE);
					c.pushcode(new Variable::DECLARATION(t->nstr));
					getSuffOp(c);
				}
				l = true;
			}
		}
		getexp10(c, l);
		if (t->getNextIf('?'))
		{
			Variable::OpCode *oc = new Variable::JRF(0);
			c.pushcode(oc);
			size_t b = c.codelength();
			getexp11(c);
			Variable::OpCode *oc2 = new Variable::JR(0);
			c.pushcode(oc2);
			oc->set(static_cast<int>(c.codelength() - b));
			b = c.codelength();
			if (!t->getNextIf(':'))	Error(TINCOT);
			getexp11(c);
			oc2->set(static_cast<int>(c.codelength() - b));
		}
	}
	void getexp12(variable &c, bool l = false)
	{
		getexp11(c, l);
		while (int n = t->checkNext())
		{
			if (n == ',')
			{
				t->getNext();
				n = t->checkNext();
				if (n == /*'('*/')' || n == ';' || n == ']')
					break;
				else
				{
					getexp11(c);
					c.pushcode(new Variable::LIST);
				}
			}
			else break;
		}
	}
	void getexp13(variable &c, bool l = false)
	{
		if (!l && t->checkNext() == Tokenizer::IDENTIFIER && t->nstr == "yield")
		{
			t->getNext();
			if (t->checkNext() == ';')
				c.pushcode(new Variable::PUSH_NULL);
			else
				getexp13(c);
			c.pushcode(new Variable::YIELD);
		}
		else
			getexp12(c, l);
		while (int n = t->checkNext())
		{
			EXP('=', 13, SUBSTITUTION)
			else EXP(Tokenizer::ASSIGN, 13, ASSIGNMENT)
			else EXP(Tokenizer::SADD, 13, SADD)
			else EXP(Tokenizer::SSUB, 13, SSUB)
			else EXP(Tokenizer::SMUL, 13, SMUL)
			else EXP(Tokenizer::SDIV, 13, SDIV)
			else EXP(Tokenizer::SMOD, 13, SMOD)
			else EXP(Tokenizer::SAND, 13, SAND)
			else EXP(Tokenizer::SOR, 13, SOR)
			else EXP(Tokenizer::SXOR, 13, SXOR)
			else EXP(Tokenizer::SSHL, 13, SSHL)
			else EXP(Tokenizer::SSHR, 13, SSHR)
			else break;
		}
	}
	#undef EXP
	#undef EXP0
	void ParseExpression(variable &c, char e, bool l = false)
	{
		getexp13(c, l);
		if (!t->getNextIf(e))
			if (e != ';' || t->checkNext() != /*'{'*/'}')
				Error(TINAE, e, "", t->getPrevLine());
	}
private:
	Tokenizer *t;
	int error;
};
