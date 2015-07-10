class PSLlib
{
public:
	static void Basic(const rsv &r)
	{
		variable v = r;
		v.set("int", 0);
		v.set("float", 0.0);
		v.set("string", "");
		v.set("true", 1);
		v.set("false", 0);
		v.set("nil", NIL);
		v.set("hex", HEX);
		v.set("ref", POINTER);
		v.set("pointer", POINTER);
		v.set("nullptr", POINTER);
		v.set("thread", THREAD);
		v.set("version", version);
		v["object"];
		v["null"];

		v["print"] = Print;
		v["type"] = Type;
		#ifdef PSL_DEBUG
		v["debug"] = Debug;
		#endif
	}
	static void Standard(const rsv &r)
	{
		variable v = r;
		v["range"] = Range;
		v["foreach"] = Foreach;
		v["map"] = Map;
		v["filter"] = Filter;
		v["reduce"] = Reduce;
		v["new"] = New;
		v["eval"] = Eval;
		#ifdef PSL_USE_VARIABLE_MEMORY_MANAGER
		v["GarbageCollection"] = GarbageCollection;
		#endif
		Strlib::set(v["strlib"]);
		Array::set(v["array"]);
		Table::set(v["table"]);
		File::set(v["file"]);
		Binary::set(v["binary"]);
		Time::set(v["time"]);
		Math::set(v["math"]);
	}
private:
	static variable Print(variable &v)
	{
		switch (v.type())
		{
		case NIL:		PSL_PRINTF(("nil"));break;
		case INT:		PSL_PRINTF(("%d", static_cast<int>(v)));break;
		case HEX:		PSL_PRINTF(("%X", static_cast<int>(v)));break;
		case FLOAT:		PSL_PRINTF(("%f", static_cast<double>(v)));break;
		case POINTER:	PSL_PRINTF(("[pointer:%s]", v.toString().c_str()));break;
		case RARRAY:	PSL_PRINTF(("[tuple:%d]", static_cast<int>(v.length())));break;
		case THREAD:	PSL_PRINTF(("[thread:%s]", static_cast<bool>(v) ? "alive" : "dead"));break;
		case BCFUNCTION:PSL_PRINTF(("[bind function]"));break;
		case CCMETHOD:	PSL_PRINTF(("[bind method]"));break;
		default:
			PSL_PRINTF(("%s", v.toString().c_str()));
			break;
		}
		return v;
	}
	static variable Type(variable &v)
	{
		switch (v.type())
		{
		case NIL:		return "nil";
		case INT:		return "int";
		case HEX:		return "hex";
		case FLOAT:		return "float";
		case STRING:	return "string";
		case POINTER:	return "pointer";
		case RARRAY:	return "tuple";
		case OBJECT:	return "object";
		case METHOD:	return "method";
		case CFUNCTION:	return "Cfunction";
		case CMETHOD:	return "Cmethod";
		case CPOINTER:	return "Cpointer";
		case SPOINTER:	return "SmartPointer";
		case THREAD:	return "thread";
		case BCFUNCTION:return "bind function";
		case CCMETHOD:	return "bind method";
		}
		return "";
	}
	#ifdef PSL_DEBUG
	static variable Debug(variable &v)
	{
		v.dump();
		return v;
	}
	#endif
	static variable Range(variable &v)
	{
		variable l;
		if (v.length() == 1)
		{
			int len = v;
			for (int i = len; --i >= 0;)
				l[i] = i;
		}
		else if (v.length() == 2)
		{
			int start = v[0];
			int end = v[1];
			int len = end - start;
			for (int i = len; --i >= 0;)
				l[i] = --end;
		}
		else if (v.length() > 2)
		{
			int start = v[0];
			int end = v[1];
			int step = v[2];
			if (!step)
				step = start < end ? 1 : -1;
			if (step > 0)
				while (start < end)
				{
					l.push(start);
					start += step;
				}
			else
				while (start > end)
				{
					l.push(start);
					start += step;
				}
		}
		return l;
	}
	static variable Foreach(variable &v)
	{
		variable l = v[0];
		variable f = v[1];
		size_t size = l.length();
		if (size)
		{
			for (size_t i = 0; i < size; ++i)
				f(l[i]);
			return size;
		}
		else
		{
			variable k = l.keys();
			size = k.length();
			for (size_t i = 0; i < size; ++i)
				f(k[i], l[k[i]]);
			return k;
		}
	}
	static variable Map(variable &v)
	{
		variable l = v[0];
		variable f = v[1];
		variable n;
		size_t size = l.length();
		for (size_t i = 0; i < size; ++i)
			n[i] = f(l[i]);
		return n;
	}
	static variable Filter(variable &v)
	{
		variable l = v[0];
		variable f = v[1];
		variable n;
		size_t size = l.length();
		for (size_t i = 0; i < size; ++i)
			if (f(l[i]))
				n.push(l[i]);
		return n;
	}
	static variable Reduce(variable &v)
	{
		variable l = v[0];
		variable f = v[1];
		size_t size = l.length();
		if (!size)
			return v;
		variable a = l[0];
		for (size_t i = 1; i < size; ++i)
			a = f(a, l[i]);
		return a;
	}
	static variable New(variable &v)
	{
		variable x = v.instance();
		return x.pointer();
	}
	static variable Eval(variable &v)
	{
		string s = v;
		variable g;
		s += ";";
		Tokenizer t(s.c_str(), "eval", 0);
		Parser p(&t);
		p.Parse(g);
		if (p.getErrorNum())
			return variable();
		return g;
	}
	#ifdef PSL_USE_VARIABLE_MEMORY_MANAGER
	static variable GarbageCollection(variable &v)
	{
		StaticObject::vpool().GarbageCollection();
		return variable();
	}
	#endif
	class Strlib
	{
	public:
		static void set(const rsv &r)
		{
			variable v = r;
			v["char"] = Char;
			v["ctoi"] = ctoi;
			v["length"] = Strlen;
			v["substr"] = Substr;
			v["find"] = Find;
			v["rfind"] = rFind;
			v["split"] = Split;
			#ifdef PSL_USE_CONSOLE
			v["getchar"] = getChar;
			v["gets"] = Gets;
			#endif
		}
	private:
		static variable Char(variable &v)
		{
			char c = v;
			string s = c;
			variable ch = s;
			return ch;
		}
		static variable ctoi(variable &v)	{return v.toString()[0];}
		static variable Strlen(variable &v)	{return v.toString().length();}
		static variable Substr(variable &v)	{return v[0].toString().substr(v[1], v[2]);}
		static variable Find(variable &v)	{return v[0].toString().find(v[1].toString().c_str()[0], v[2]);}
		static variable rFind(variable &v)	{return v[0].toString().rfind(v[1].toString().c_str()[0], v.length() > 2 ? static_cast<int>(v[2]) : -1);}
		static variable Split(variable &v)
		{
			string s = v[0];
			char c = v[1].toString().c_str()[0];
			variable r;
			int l = static_cast<int>(s.length());
			size_t prev = 0;
			for (int i = 0; i < l; ++i)
			{
				if ((i = s.find(c, i)) < 0)
					break;
				size_t p = static_cast<size_t>(i) - prev;
				if (p)		r.push(s.substr(prev, p));
				else		r.push(string());
				prev = static_cast<size_t>(i+1);
			}
			r.push(s.substr(prev));
			return r;
		}
		static variable getChar(variable &v)
		{
			string s;
			using namespace std;
			int c = getchar();
			s = static_cast<char>(c);
			variable ch = s;
			return ch;
		}
		static variable Gets(variable &v)
		{
			string s;
			int c;
			using namespace std;
			while ((c = getc(stdin)) != EOF)
			{
				s += static_cast<char>(c);
				if (c == '\n')
					break;
			}
			return s;
		}
	};
	class Array
	{
	public:
		static void set(const rsv &r)
		{
			variable v = r;
			v["length"] = variable(Length);
			v["push"] = variable(Push);
			v["foreach"] = variable(Foreach);
			v["join"] = variable(Join);
		}
	private:
		static variable Length(variable &this_v, variable &v)
		{
			if (!this_v)	return v.length();
			else		return this_v.length();
		}
		static variable Push(variable &this_v, variable &v)
		{
			if (!this_v)
			{
				v[0].push(v[1]);
				return v[0].length();
			}
			else
			{
				this_v.push(v);
				return this_v.length();
			}
		}
		static variable Foreach(variable &this_v, variable &v)
		{
			size_t size;
			if (!this_v)
			{
				variable array = v[0];
				size = array.length();
				for (size_t i = 0; i < size; ++i)
					v[1](array[i]);
			}
			else
			{
				size = this_v.length();
				for (size_t i = 0; i < size; ++i)
					v(this_v[i]);
			}
			return size;
		}
		static variable Join(variable &this_v, variable &v)
		{
			string r;
			string s;
			if (!this_v)
			{
				variable array = v[0];
				if (v[1])
					s = v[1].toString();
				size_t size = array.length();
				if (size)
					r = array[0].toString();
				for (size_t i = 1; i < size; ++i)
					r += s + array[i].toString();
			}
			else
			{
				if (v)
					s = v.toString();
				size_t size = this_v.length();
				if (size)
					r = this_v[0].toString();
				for (size_t i = 1; i < size; ++i)
					r += s + this_v[i].toString();
			}
			return r;
		}
	};
	class Table
	{
	public:
		static void set(const rsv &r)
		{
			variable v = r;
			v["length"] = variable(Length);
			v["exist"] = variable(Exist);
			v["delete"] = variable(Delete);
			v["keys"] = variable(Keys);
			v["foreach"] = variable(Foreach);
		}
	private:
		static variable Length(variable &this_v, variable &v)
		{
			if (!this_v)	return v.memberLength();
			else			return this_v.memberLength();
		}
		static variable Exist(variable &this_v, variable &v)
		{
			if (!this_v)	return v[0].exist(v[1]);
			else			return this_v.exist(v);
		}
		static variable Delete(variable &this_v, variable &v)
		{
			if (!this_v)	v[0].del(v[1]);
			else			this_v.del(v);
			return variable();	// Žæ‚èœ‚¢‚½•Ï”‚ð•Ô‚·‚Æ‚¢‚¤‚Ì‚àŽèH
		}
		static variable Keys(variable &this_v, variable &v)
		{
			if (!this_v)return v.keys();
			else		return this_v.keys();	// “–‘R‚È‚ª‚çexist‚âkeys‚ªŠÜ‚Ü‚ê‚é‚±‚Æ‚É‚È‚éA‚Ü‚ ‚¢‚¢‚©
		}
		static variable Foreach(variable &this_v, variable &v)
		{
			if (!this_v)
			{
				variable l = v[0];
				variable f = v[1];
				variable k = l.keys();
				size_t s = k.length();
				for (size_t i = 0; i < s; ++i)
				{
					string str = k[i];
					if (!l[str].type(METHOD) && !l[str].type(CMETHOD))
						f(str, l[str]);
				}
				return k;
			}
			else
			{
				variable k = this_v.keys();
				size_t s = k.length();
				for (size_t i = 0; i < s; ++i)
				{
					string str = k[i];
					if (!this_v[str].type(METHOD) && !this_v[str].type(CMETHOD))
						v(str, this_v[str]);
				}
				return k;
			}
		}
	};
	class File
	{
	public:
		static void set(const rsv &r)
		{
			variable v = r;
			variable fc = Close;
			v["open"] = variable(Open);
			v["close"] = fc;
			v["destructor"] = fc;
			v["read"] = variable(Read);
		}
	private:
		static variable Open(variable &this_v, variable &v)
		{
			string name = v.toString();
			using namespace std;
			if (!this_v)
			{
				FILE *fp = fopen(name, "r");
				if (!fp)
					return 0;
				variable f;
				set(f);
				variable r = f.instance();
				r["$$__FILE*fp__$$"] = fp;
				r.set("name", name);
				return r.pointer();
			}
			else
			{
				FILE *fp = this_v["$$__FILE*fp__$$"];
				if (fp)
					fclose(fp);
				this_v["$$__FILE*fp__$$"] = fp = fopen(name, "r");
				if (fp)	this_v["name"] = name;
				else	this_v["name"] = "";
				return this_v.pointer();
			}
		}
		static variable Close(variable &this_v, variable &v)
		{
			using namespace std;
			FILE *fp = this_v["$$__FILE*fp__$$"];
			if (fp)
				fclose(fp);
			this_v["$$__FILE*fp__$$"] = 0;
			this_v["name"] = "";
			return v;
		}
		static variable Read(variable &this_v, variable &v)
		{
			std::FILE *fp = this_v["$$__FILE*fp__$$"];
			size_t size = v;
			if (!fp || !size)
				return "";
			buffer vbuf(size+1);
			char *buf = reinterpret_cast<char*>(vbuf.get());
			vbuf.resize(std::fread(buf, 1, size, fp));
			vbuf.push(0);
			string s = buf;
			return s;
		}
	};
	class Binary
	{
	public:
		static void set(const rsv &r)
		{
			variable v = r;
			variable fc = Close;
			v["open"] = variable(Open);
			v["close"] = fc;
			v["destructor"] = fc;
			v["read"] = variable(Read);
			v["readdw"] = variable(ReadDW);
		}
	private:
		static variable Open(variable &this_v, variable &v)
		{
			string name = v.toString();
			using namespace std;
			if (!this_v)
			{
				FILE *fp = fopen(name, "rb");
				if (!fp)
					return 0;
				variable f;
				set(f);
				variable r = f.instance();
				r["$$__FILE*fp__$$"] = fp;
				r["name"] = name;
				return r.pointer();
			}
			else
			{
				FILE *fp = this_v["$$__FILE*fp__$$"];
				if (fp)
					fclose(fp);
				this_v["$$__FILE*fp__$$"] = fp = fopen(name, "rb");
				if (fp)	this_v["name"] = name;
				else	this_v["name"] = "";
				return this_v.pointer();
			}
		}
		static variable Close(variable &this_v, variable &v)
		{
			using namespace std;
			FILE *fp = this_v["$$__FILE*fp__$$"];
			if (fp)
				fclose(fp);
			this_v["$$__FILE*fp__$$"] = 0;
			this_v["name"] = "";
			return v;
		}
		static variable Read(variable &this_v, variable &v)
		{
			using namespace std;
			FILE *fp = this_v["$$__FILE*fp__$$"];
			int size = v;
			variable r;
			if (!fp || !size)
				return r;
			for (int i = 0; i < size; ++i)
			{
				int c = fgetc(fp);
				if (c == EOF)
				{
					size = i;
					break;
				}
				r.push(static_cast<hex>(c));
			}
			r["length"] = size;
			return r;
		}
		static variable ReadDW(variable &this_v, variable &v)
		{
			using namespace std;
			FILE *fp = this_v["$$__FILE*fp__$$"];
			int size = v;
			variable r;
			if (!fp || !size)
				return r;
			for (int i = 0; i < size; ++i)
			{
				hex h;
				if (!fread(&h, sizeof(hex), 1, fp))
				{
					size = i;
					break;
				}
				r.push(h);
			}
			r["length"] = size;
			return r;
		}
	};
	class Time
	{
	public:
		static void set(const rsv &r)
		{
			variable v = r;
			v["time"] = time;
			v["clock"] = Clock;
			v["CLOCKS_PER_SEC"] = CLOCKS_PER_SEC;
		}
	private:
		static variable time(variable &v)	{return static_cast<int>(std::time(NULL));}
		static variable Clock(variable &v)	{return std::clock();}
	};
	class Math
	{
	public:
		static void set(const rsv &r)
		{
			variable v = r;
			v["abs"] = Abs;
			v["fabs"] = FAbs;
			v["sqrt"] = Sqrt;
			v["pow"] = Pow;
			v["log"] = Log;
			v["sin"] = Sin;
			v["cos"] = Cos;
			v["tan"] = Tan;
			v["asin"] = ASin;
			v["acos"] = ACos;
			v["atan"] = ATan;
			v["atan2"] = ATan2;
		}
	private:
		static variable Abs(variable &v)	{return std::abs(static_cast<int>(v));}
		static variable FAbs(variable &v)	{return std::fabs(static_cast<double>(v));}
		static variable Sqrt(variable &v)	{return std::sqrt(static_cast<double>(v));}
		static variable Pow(variable &v)	{return std::pow(static_cast<double>(v[0]), static_cast<double>(v[1]));}
		static variable Log(variable &v)	{return std::log(static_cast<double>(v));}
		static variable Sin(variable &v)	{return std::sin(static_cast<double>(v));}
		static variable Cos(variable &v)	{return std::cos(static_cast<double>(v));}
		static variable Tan(variable &v)	{return std::tan(static_cast<double>(v));}
		static variable ASin(variable &v)	{return std::asin(static_cast<double>(v));}
		static variable ACos(variable &v)	{return std::acos(static_cast<double>(v));}
		static variable ATan(variable &v)	{return std::atan(static_cast<double>(v));}
		static variable ATan2(variable &v)	{return std::atan2(static_cast<double>(v[0]), static_cast<double>(v[1]));}
	};
};
