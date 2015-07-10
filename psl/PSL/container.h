class rsv
{
public:
	rsv(Variable *v)	{x = v->ref();}
	rsv(Variable *v,int i)	{x = v;}
	rsv(const variable &v)	{x = v.x->ref();}
#ifdef PSL_NULL_RSV
	rsv()				{x = NULL;}
	rsv(const rsv &v)	{x = v.x ? v.x->ref() : NULL;}
	~rsv()				{if (x) x->finalize();}
	rsv &operator=(const rsv &v)
	{
		if (x) x->finalize();
		x = v.x ? v.x->ref() : NULL;
		return *this;
	}
	void copy(const rsv &v)
	{
		if (x) x->finalize();
		x = v.x ? v.x->clone() : NULL;
	}
	void set(Variable *v)
	{
		if (x) x->finalize();
		x = v;
	}
	Variable *get()	const{if(!x)x = new Variable();return x;}
private:
	mutable Variable *x;
#else
	rsv()				{x = new Variable();}
	rsv(const rsv &v)	{x = v.x->ref();}
	~rsv()				{x->finalize();}
	rsv &operator=(const rsv &v)
	{
		x->finalize();
		x = v.x->ref();
		return *this;
	}
	void copy(const rsv &v)
	{
		x->finalize();
		x = v.x->clone();
	}
	void set(Variable *v)
	{
		x->finalize();
		x = v;
	}
	Variable *get()	const{return x;}
private:
	Variable *x;
#endif
};

#ifdef PSL_USE_STL_VECTOR
	#define PSL_USE_STL_STACK
typedef std::vector<rsv> rlist;
#else
template<class T> class vector
{
public:
	vector()	{res = len = 0;x = NULL;}
	vector(size_t i)	{len = 0;res = i;x = new T[i];}
	~vector()	{delete[] x;}
	void resize(size_t t)
	{
		reserve(t);
		len = t;
	}
	void reserve(size_t t)
	{
		if (t > res)
		{
			res = (res*2 > t) ? res*2: t;
			T *n = new T[res];
			for (size_t i = 0; i < len; ++i)
				n[i] = x[i];
			delete[] x;
			x = n;
		}
	}
	void push_back(const T &v)
	{
		if (len >= res)
			reserve(res*2+1);
		x[len++] = v;
	}
	T &operator[](size_t t) const	{return x[t];}
	size_t size() const	{return len;}
	bool empty() const	{return !len;}
protected:
	T *x;
	size_t res;
	size_t len;
};
typedef vector<rsv> rlist;
#endif

#ifdef PSL_USE_STL_MAP
typedef std::map<string,rsv> table;
#else
class table
{
	typedef unsigned long hash;
	static hash gethash(const string &s, hash max)	{return s.hash() & (max-1);}
	struct data
	{
		PSL_MEMORY_MANAGER(data)
		data(const string &s):first(s)	{}
		data(const string &s, const variable &v):first(s),second(v)	{}
		string first;
		rsv second;
	};
public:
	class iterator
	{
	public:
		iterator(table *t, int i)	{ta = t;n = i;}
		bool operator!=(int i)		{return n != i;}
		data *operator->()			{return ta->d[n];}
		void operator++()
		{
			int max = static_cast<int>(ta->reserve * 2);
			while (++n < max)
				if (ta->d[n])
					return;
			n = -1;
		}
	private:
		table *ta;
		int n;
	};
	friend class iterator;
	table()		{reserve = len = 0;d = NULL;}
	~table()
	{
		size_t max = reserve * 2;
		for (size_t i = 0; i < max; ++i)
			delete d[i];
		delete[] d;
	}
	size_t count(const string &s) const
	{
		if (!reserve)	return 0;
		if (search(s) < 0)
			return 0;
		return 1;
	}
	rsv &operator[](const string &s)
	{
		if (len)
		{
			int i = search(s);
			if (i >= 0)
				return d[i]->second;
		}
		if (len >= reserve)
			resize();
		hash h = gethash(s, reserve);
		size_t i = getnextnull(h);
		d[i] = new data(s);
		++len;
		return d[i]->second;
	}
	void erase(const string &s)
	{
		if (!len)
			return;
		int i = search(s);
		if (i < 0)
			return;
		delete d[i];
		if (i < static_cast<int>(reserve))
			move(static_cast<hash>(i));
		else
			d[i] = NULL;
	}
	bool empty()	const{return !len;}
	size_t size()	const{return len;}
	iterator begin()	{
		if (!len)
			return iterator(this, -1);
		else
		{
			for (size_t i = 0; i < reserve; ++i)
				if (d[i])
					return iterator(this, static_cast<int>(i));
		}
		return iterator(this, -1);
	}
	int end()	{return -1;}
	iterator find(const string &s)
	{
		if (len)
		{
			int i = search(s);
			if (i >= 0)
				return iterator(this, i);
		}
		return iterator(this, -1);
	}

	bool set(const string &s, const variable &v)
	{
		if (len)
		{
			int i = search(s);
			if (i >= 0)
			{
				d[i]->second = v;
				return false;
			}
		}
		if (len >= reserve)
			resize();
		hash h = gethash(s, reserve);
		size_t i = getnextnull(h);
		d[i] = new data(s, v);
		++len;
		return true;
	}
private:
	int search(const string &s) const
	{
		hash h = gethash(s, reserve);
		if (!d[h])
			return -1;
		else if (d[h]->first == s)
			return static_cast<int>(h);

		size_t max = reserve*2;
		for (size_t i = reserve+h; i < max; ++i)
			if (d[i] && d[i]->first == s)
				return static_cast<int>(i);
		max = reserve + h;
		for (size_t i = reserve; i < max; ++i)
			if (d[i] && d[i]->first == s)
				return static_cast<int>(i);
		return -1;
	}
	size_t getnextnull(size_t t) const
	{
		if (!d[t])
			return t;
		size_t max = reserve*2;
		for (size_t i = reserve+t; i < max; ++i)
			if (!d[i])
				return i;
		max = reserve + t;
		for (size_t i = reserve; i < max; ++i)
			if (!d[i])
				return i;
		PSL_PRINTF(("table error:\n"));
		return 0;
	}
	void move(hash h)
	{
		size_t max = reserve*2;
		for (size_t i = reserve; i < max; ++i)
			if (d[i] && h == gethash(d[i]->first, reserve))
			{
				d[h] = d[i];
				d[i] = NULL;
				return;
			}
		d[h] = NULL;
	}
	void resize()
	{
		if (!reserve)
		{
			reserve = 2;
			Reserve();
			return;
		}
		reserve *= 2;
		data **old = Reserve();
		for (size_t i = 0; i < reserve; ++i)
		{
			if (old[i])
			{
				hash h = gethash(old[i]->first, reserve);
				size_t n = getnextnull(h);
				d[n] = old[i];
			}
		}
		delete[] old;
	}
	data **Reserve()
	{
		data **old = d;
		d = new data*[reserve * 2];
		std::memset(d, 0, sizeof(data*) * 2 * reserve);
		return old;
	}
	size_t len;
	size_t reserve;
	data **d;
};
#endif

#ifdef PSL_USE_STL_STACK
typedef std::stack<rsv> rstack;
#else
class rstack : public rlist
{
public:
	#ifdef PSL_CHECKSTACK_PUSH
	void push(const rsv &v)	{push_back(v);}
	#else
	void push(const rsv &v)	{x[len++] = v;}
	rstack():rlist(16){}
	#endif
	rsv &top()	{return x[len-1];}
	#ifdef PSL_POPSTACK_NULL
	rsv pop()
	{
		rsv v = x[--len];
		#ifdef PSL_USE_VARIABLE_MEMORY_MANAGER
		x[len] = StaticObject::rsvnull();
		#else
		PSL_CONST_STATIC rsv null;
		x[len] = null;
		#endif
		return v;
	}
	#else
	rsv &pop()	{return x[--len];}
	void clear()
	{
		for (size_t i = len; i < res; ++i)
		{
			#ifdef PSL_USE_VARIABLE_MEMORY_MANAGER
			x[i] = StaticObject::rsvnull();
			#else
			PSL_CONST_STATIC rsv null;
			x[i] = null;
			#endif
		}
	}
	#endif
};
#endif
