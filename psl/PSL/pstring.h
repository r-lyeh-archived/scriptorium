class string
{
public:
	static size_t min_s(size_t x, size_t y)	{return (x < y) ? x : y;}
	static bool empty_s(const char *s){return s == NULL || s[0] == 0;}
private:
	class SharedBuffer
	{
	public:
		PSL_MEMORY_MANAGER(SharedBuffer)
		SharedBuffer(size_t t)	{Init(t);}
		SharedBuffer(const char *s)				{size_t t = std::strlen(s);Init(t, t);fcopy(s, t);}
		SharedBuffer(const char *s, size_t t)	{Init(t, t);copy(s);}
		void finalize()	{if(!--rc)delete this;}
		SharedBuffer *inc()		{++rc;return this;}
		SharedBuffer *clone(size_t t)
		{
			SharedBuffer *c = new SharedBuffer(t);
			c->fcopy(buf, min_s(len, t));
			return c;
		}
		void copy(const char *s)
		{
			size_t t;
			for (t = 0; t < size; ++t)
			{
				if (!s[t])
					break;
				buf[t] = s[t];
			}
			len = t;
		}
		void fcopy(const char *s, size_t m, size_t b = 0)
		{
			std::memcpy(buf+b, s, m);
			len = m+b;
		}
		void fmove(const char *s, size_t m)
		{
			std::memmove(buf, s, m);
			len = m;
		}
		const char *c_str()	{buf[len] = 0;return buf;}
	private:
		~SharedBuffer()			{delete[] buf;}
		void Init(size_t t, size_t l = 0)	{rc = 1;buf = new char[t+1];size = t;len = l;}
		int rc;
		char *buf;
		size_t size;
		size_t len;
	public:
		bool shared()		const{return rc != 1;}
		char *buffer()		const{return buf;}
		char at(size_t t)	const{return buf[t];}
		size_t length()		const{return len;}
		size_t maxlength()	const{return size;}
		void setlen(size_t t)	{len = t;}
	};
	const static char ZERO = '0';
	const static char MINUS = '-';
	const static char SPACE = ' ';
	const static char TAB = '\t';
	const static size_t SPARE = 11;		/* 最低限数値を表現出来る桁数で(>10) */
	const static size_t DOUBLE_L = 64;	/* DOUBLE型の桁数以上の値にする */
public:
	string()	{buf = NULL;}
	string(const string &s)			{buf = s.buf ? s.buf->inc() : NULL;}
	string(const char *s)			{buf = empty_s(s) ? NULL : new SharedBuffer(s);}
	string(const char *s, size_t t)	{buf = (empty_s(s)||t==0) ? NULL : new SharedBuffer(s, t);}
	string(size_t t, char *&s)		{buf = new SharedBuffer(t);buf->setlen(t);s = buf->buffer();}
	string(int i)					{buf = new SharedBuffer(SPARE);setint(i);}
	string(unsigned long i)			{buf = new SharedBuffer(SPARE);sethex(i);}
	string(char c)					{buf = new SharedBuffer(SPARE);buf->buffer()[0] = c;buf->setlen(1);}
	string(double d)
	{
		buf = new SharedBuffer(DOUBLE_L);
		buf->setlen(static_cast<size_t>(std::sprintf(buf->buffer(), "%f", d)));
	};
	~string()	{if (buf)buf->finalize();}

	string &operator=(const string &s)
	{
		SharedBuffer *b = s.buf ? s.buf->inc() : NULL;
		if (buf)	buf->finalize();
		buf = b;
		return *this;
	}
	string &operator+=(const string &s)
	{
		if (s.buf)
		{
			if (!buf)
			{
				buf = s.buf->inc();
			}
			else
			{
				size_t c = buf->length();
				only_and_extend(c + s.buf->length());
				buf->fcopy(s.buf->buffer(), s.buf->length(), c);
			}
		}
		return *this;
	}
	string operator+(const string &s) const
	{
/*		string n = *this; // 実際のとこバッファ共有システムがあるからこれでもそんなにロスがあるわけじゃないと思うけど
		n += s;
		return n;*/
		if (!buf)	return s;
		if (!s.buf)	return *this;
		SharedBuffer *n = new SharedBuffer(buf->length() + s.buf->length());
		n->fcopy(buf->buffer(), buf->length());
		n->fcopy(s.buf->buffer(), s.buf->length(), buf->length());
		return n;
	}
	string &operator=(const char *s)
	{
		if (!buf)
		{
			if (!empty_s(s))
				buf = new SharedBuffer(s);
		}
		else
		{
			if (empty_s(s))
			{
				if (buf->shared())
				{
					buf->finalize();
					buf = NULL;
				}
				else
				{
					buf->setlen(0);
				}
			}
			else
			{
				size_t l = std::strlen(s);
				only_and_extend(l);
				buf->fcopy(s, l);
			}
		}
		return *this;
	}
	string &operator+=(const char *s)
	{
		if (empty_s(s))	return *this;
		if (!buf)
		{
			buf = new SharedBuffer(s);
		}
		else
		{
			size_t l = std::strlen(s);
			size_t c = buf->length();
			only_and_extend(c + l);
			buf->fcopy(s, l, c);
		}
		return *this;
	}
	string operator+(const char *s) const
	{
		if (empty_s(s))	return *this;
		if (!buf)		return new SharedBuffer(s);
		size_t l = std::strlen(s);
		SharedBuffer *n = new SharedBuffer(buf->length() + l);
		n->fcopy(buf->buffer(), buf->length());
		n->fcopy(s, l, buf->length());
		return n;
	}
	string &operator=(int i)
	{
		if (buf && !buf->shared() && buf->length() > SPARE)
		{
			setint(i);
		}
		else
		{
			string s(i);	// 無条件にこっちの方が速いかも…
			*this = s;
		}
		return *this;
	}
	string &operator+=(int i)
	{
		if (!buf)
		{
			buf = new SharedBuffer(SPARE);
			setint(i);
		}
		else
		{
			size_t c = buf->length();
			only_and_extend(c + SPARE);
			setint(i, c);
		}
		return *this;
	}
	string operator+(int i) const
	{
//		if (!buf)	return string(i);	// 余計なことしなくていい気がする
		string s = *this;
		s += i;
		return s;
	}
	string &operator=(char c)
	{
		if (!buf)	buf = new SharedBuffer(SPARE);
		else if (buf->shared())
		{
			buf->finalize();
			buf = new SharedBuffer(SPARE);
		}
		buf->buffer()[0] = c;
		buf->setlen(1);
		return *this;
	}
	string &operator+=(char c)
	{
		if (!buf)
		{
			buf = new SharedBuffer(SPARE);
			buf->buffer()[0] = c;
			buf->setlen(1);
		}
		else
		{
			size_t o = buf->length();
			only_and_extend(o + 1);
			buf->buffer()[o] = c;
			buf->setlen(o + 1);
		}
		return *this;
	}
	string operator+(char c) const
	{
		string s = *this;
		s += c;
		return s;
	}

	operator int() const
	{
		if (!buf)	return 0;
		char *b = buf->buffer();
		size_t l = buf->length();
		bool minus = false;
		size_t c = 0;
		int r = 0;
		for (; c < l; ++c)
		{
			if (b[c] != SPACE && b[c] != TAB)
				break;
		}
		if (b[c] == MINUS)
		{
			minus = true;
			++c;
		}
		for (; c < l; ++c)
		{
			if (!(b[c] >= ZERO && b[c] <= ZERO+9))
				break;
			r *= 10;
			r += b[c] - ZERO;
		}
		if (minus)
			r = -r;
		return r;
	}
	operator char() const
	{
		if (!buf)	return 0;
		return buf->at(0);
	}
	operator const char*() const	{return c_str();}
	const char *c_str() const
	{
		if (buf)	return buf->c_str();
		else		return "";
	}
	string &operator=(double d)
	{
		string s(d);
		*this = s;
		return *this;
	}
	string &operator+=(double d)
	{
		string s(d);
		*this += s;
		return *this;
	}
	string operator+(double d) const
	{
		string s(d);
		return *this + s;
	}
	operator double() const	{return std::strtod(c_str(), NULL);}
	bool operator==(const string &s) const
	{
		if (buf == s.buf)					return true;
		if (!buf || !s.buf)					return false;
		size_t l = buf->length();
		if (l != s.buf->length())			return false;
		return !std::memcmp(buf->buffer(), s.buf->buffer(), l);
	}
	bool operator==(const char *s) const
	{
		if (!buf)
		{
			if (s[0] == 0)	return true;
			else			return false;
		}
		size_t l = buf->length();
		for (size_t t = 0; t < l; ++t)
		{
			if (s[t] == 0)			return false;
			if (buf->at(t) != s[t])	return false;
		}
		if (s[l] != 0)				return false;
		return true;
	}
	bool operator<=(const string &s) const
	{
		if (buf == s.buf)	return true;
		if (!buf)			return true;
		if (!s.buf)			return false;
		size_t max = min_s(buf->length(), s.buf->length());
		for (size_t t = 0; t < max; ++t)
		{
			if (buf->at(t) < s.buf->at(t))	return true;
			if (buf->at(t) > s.buf->at(t))	return false;
		}
		if (buf->length() <= s.buf->length())	return true;
		return false;
	}
	bool operator>=(const string &s) const
	{
		if (buf == s.buf)	return true;
		if (!buf)			return false;
		if (!s.buf)			return true;
		size_t max = min_s(buf->length(), s.buf->length());
		for (size_t t = 0; t < max; ++t)
		{
			if (buf->at(t) < s.buf->at(t))	return false;
			if (buf->at(t) > s.buf->at(t))	return true;
		}
		if (buf->length() >= s.buf->length())	return true;
		return false;
	}
	bool operator!=(const string &s) const	{return !(*this==s);}
	bool operator!=(const char *s) const	{return !(*this==s);}
	bool operator<(const string &s) const	{return !(*this >= s);}
	bool operator>(const string &s) const	{return !(*this <= s);}

	int find(char c, int i = 0) const	/* i位置から検索してcの文字を発見した位置を返す */
	{
		if (i >= 0 && buf)
		{
			for (size_t t = static_cast<size_t>(i); t < buf->length(); ++t)
			{
				if (buf->at(t) == c)
					return static_cast<int>(t);
			}
		}
		return -1;
	}
	int rfind(char c, int i = -1) const	/* i位置から逆方向に検索してcの文字を発見した位置を返す */
	{
		if (buf)
		{
			if (i < 0 || i >= static_cast<int>(buf->length())) i = static_cast<int>(buf->length())-1;
			for (; i >= 0; --i)
			{
				if (buf->at(static_cast<size_t>(i)) == c)
					return i;
			}
		}
		return -1;
	}
	int copy(const char *s, size_t l, size_t i)	/* iからの位置にsをl文字分コピーする */
	{
		if (!buf)	return 0;
		size_t m = buf->length();
		if (i >= m)
			return 0;
		only_and_extend(m);
		if (i + l > m)
			l = m - i;
		buf->fcopy(s, l, i);
		buf->setlen(m);
		return static_cast<int>(l);
	}
	string substr(size_t i = 0, size_t l = 0) const	/* iからl文字を切り出した文字列 */
	{
		if (!buf)	return string();
		size_t m = buf->length();
		if (i >= m)	return string();
		if (!l || (i + l > m))	l = m - i;
		SharedBuffer *n = new SharedBuffer(l);
		n->fcopy(buf->buffer()+i, l);
		return n;
	}
	string &reverse()	/* 反転させる */
	{
		if (buf)
		{
			size_t m = buf->length();
			only_and_extend(m);
			size_t e = m/2;
			char *b = buf->buffer();
			for (size_t t = 0; t < e; ++t)
			{
				char temp = b[t];
				b[t] = b[m-t-1];
				b[m-t-1] = temp;
			}
		}
		return *this;
	}
	string &sprintf(const char *format, ...)
	{
		using namespace std;
		va_list arg;
		va_start(arg, format);
		int size;
		if (buf && !buf->shared())
		{
			size = vsnprintf(buf->buffer(), buf->maxlength(), format, arg);
			if (size <= static_cast<int>(buf->maxlength()))
			{
				va_end(arg);
				return *this;
			}
		}
		else
			size = vsnprintf(NULL, 0, format, arg);
		if (size >= 0)
		{
			SharedBuffer *n = new SharedBuffer(static_cast<size_t>(size));
			n->setlen(static_cast<size_t>(vsnprintf(n->buffer(), static_cast<size_t>(size)+1, format, arg)));
			if (buf)
				buf->finalize();
			buf = n;
		}
		va_end(arg);
		return *this;
	}

	string &operator-=(size_t i)	/* 末尾からi文字を削る */
	{
		if (buf)
		{
			size_t c = buf->length();
			only_and_extend(c);
			if (c < i)
				c = 0;
			else
				c -= i;
			buf->setlen(c);
		}
		return *this;
	}
	string operator-(size_t i) const
	{
		string s = *this;
		s -= i;
		return s;
	}
	string &operator*=(size_t i)	/* i個分の文字列 */
	{
		if (buf)
		{
			size_t c = buf->length();
			only_and_extend(c*i);
			for (size_t x = 0; x < i; ++x)
				buf->fcopy(buf->buffer(), c, c*x);
		}
		return *this;
	}
	string operator*(size_t i) const
	{
		string s = *this;
		s *= i;
		return s;
	}
	string &operator/=(size_t i)	/* 先頭からi文字までの文字列 */
	{
		if (buf)
		{
			size_t c = buf->length();
			if (c > i)
			{
				only_and_extend(c);
				buf->setlen(i);
			}
		}
		return *this;
	}
	string operator/(size_t i) const
	{
		string s = *this;
		s /= i;
		return s;
	}
	string &operator%=(size_t i)	/* 先頭i文字分を削る */
	{
		if (buf)
		{
			size_t c = buf->length();
			if (buf->shared())
			{
				if (i < c)
				{
					SharedBuffer *n = new SharedBuffer(c);
					n->fcopy(buf->buffer()+i, c-i);
					buf->finalize();
					buf = n;
				}
				else
				{
					buf->finalize();
					buf = NULL;
				}
			}
			else
			{
				if (i < c)
					buf->fmove(buf->buffer()+i, c-i);
				else
					buf->setlen(0);
			}
		}
		return *this;
	}
	string operator%(size_t i) const
	{
		string s = *this;
		s %= i;
		return s;
	}

	size_t length() const	{return buf ? buf->length() : 0;}
	bool empty() const		{return !buf || buf->length() == 0;}
	void clear()
	{
		if (!buf)return;
		if (buf->shared())
		{
			buf->finalize();
			buf = NULL;
		}
		else
		{
			buf->setlen(0);
		}
	}
	unsigned long hash() const
	{
		if (!buf)
			return 0;
		unsigned long l = buf->length();
		if (!l)
			return 0;
		unsigned long a = static_cast<unsigned long>(buf->at(0));
		unsigned long b = static_cast<unsigned long>(buf->at(l-1));
		return (b | (a<<8)) ^ (l<<4);
	}
private:
	void setint(int i, size_t c = 0)
	{
		char *b = buf->buffer();
		if (i < 0)
		{
			b[c++] = MINUS;
			i = -i;
		}
		#define C(n) if (i >= n)b[c++] = (char)(i/n%10+ZERO);
		C(1000000000)
		C(100000000)
		C(10000000)
		C(1000000)
		C(100000)
		C(10000)
		C(1000)
		C(100)
		C(10)
		#undef C
		b[c++] = (char)(i%10 + ZERO);
		buf->setlen(c);
	}
	void sethex(unsigned long i, size_t c = 0)
	{
		char *b = buf->buffer();
		#define C(n) if (i >= n){char a = (char)((i/n)&0xF);b[c++] = (char)(a<10 ? a+ZERO : a-10+'A');}
		C(0x10000000)
		C(0x1000000)
		C(0x100000)
		C(0x10000)
		C(0x1000)
		C(0x100)
		C(0x10)
		#undef C
		{char a = (char)(i&0xF);b[c++] = (char)(a<10 ? a+ZERO : a-10+'A');}
		buf->setlen(c);
	}
	void only_and_extend(size_t t)
	{
		if (buf->shared() || (buf->maxlength() < t))
		{
			SharedBuffer *o = buf;
			buf = buf->clone(t+SPARE);
			o->finalize();
		}
	}
	string(SharedBuffer *b)	{buf = b;}
	SharedBuffer *buf;
public:
	class iterator : public std::iterator<std::random_access_iterator_tag, char>
	{
		char *p;
	public:
		iterator(char *x)			{p = x;}
		char &operator*()			{return *p;}
		bool operator==(iterator i)	{return p == i.p;}
		bool operator!=(iterator i)	{return p != i.p;}
		bool operator<(iterator i)	{return p < i.p;}
		iterator &operator++()		{++p;return *this;}
		iterator &operator--()		{--p;return *this;}
		iterator &operator+=(int i)	{p += i;return *this;}
		iterator &operator-=(int i)	{p -= i;return *this;}
		iterator operator+(int i)	{return iterator(p+i);}
		iterator operator-(int i)	{return iterator(p-i);}
		std::ptrdiff_t operator-(iterator i)	{return p - i.p;}
	};
	iterator begin(){if(buf)only_and_extend(length());return iterator(buf->buffer());}
	iterator end()	{return iterator(buf->buffer() + length());}
	class wstring
	{
		class wsbuffer
		{
			~wsbuffer()	{delete[] ws;}
			int rc;
			size_t len;
		public:
			wchar_t *ws;
			wsbuffer(size_t n)
			{
				ws = new wchar_t[n+1];
				rc = 1;
				len = n;
			}
			void finalize()	{if (!--rc)	delete this;}
			wsbuffer *ref()	{++rc;return this;}
			void setlen(size_t l)	{ws[len=l] = 0;}
		} *buf;
	public:
		wstring(const wchar_t *ws)
		{
			using namespace std;
			size_t l = wcslen(ws);
			buf = new wsbuffer(l);
			memcpy(buf->ws, ws, l*sizeof(wchar_t));
			buf->setlen(l);
		}
		wstring(const char *s)
		{
			size_t l = std::strlen(s);
			buf = new wsbuffer(l);
			buf->setlen(std::mbstowcs(buf->ws, s, l));
		}
		wstring(const wstring &s)	{buf = s.buf->ref();}
		wstring &operator=(const wstring &s)	{buf->finalize();buf = s.buf->ref();return *this;}
		~wstring()	{buf->finalize();}
		operator const wchar_t*()	{return buf->ws;}
		const wchar_t *c_str()		{return buf->ws;}
	};
	string(const wchar_t *ws)
	{
		using namespace std;
		size_t l = wcslen(ws)*sizeof(wchar_t);
		buf = new SharedBuffer(l);
		buf->setlen(wcstombs(buf->buffer(), ws, l));
	}
	wstring w_str()	{return buf ? buf->c_str() : "";}
};
