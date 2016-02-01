

#ifndef __SGS_CPPBC_H__
#define __SGS_CPPBC_H__

#ifndef HEADER_SGSCRIPT_H
# define HEADER_SGSCRIPT_H <sgscript.h>
#endif
#include HEADER_SGSCRIPT_H
#include <new>
#include <assert.h>

#ifdef SGS_CPPBC_WITH_STD_STRING
#  include <string>
#endif

#ifdef SGS_CPPBC_WITH_STD_VECTOR
#  include <vector>
#endif


#ifndef SGS_CPPBC_PROCESS
# if __cplusplus >= 201103L
#  define SGS_CPPBC_DEFINIT( x ) = x
# else
#  define SGS_CPPBC_DEFINIT( x )
# endif
struct _sgsInterface
{
	_sgsInterface( const sgs_ObjInterface& src, _sgsInterface* pif = NULL )
	: iface(src), inh_parent(pif), inh_child(NULL), inh_sibling(NULL)
	{
		if( pif )
		{
			inh_sibling = pif->inh_child;
			pif->inh_child = this;
		}
	}
	operator sgs_ObjInterface* (){ return &iface; }
	sgs_ObjInterface* operator -> (){ return &iface; }
	sgs_ObjInterface iface;
	_sgsInterface* inh_parent;
	_sgsInterface* inh_child;
	_sgsInterface* inh_sibling;
};
# define SGS_OBJECT_LITE \
	static int _sgs_destruct( SGS_CTX, sgs_VarObj* obj ); \
	static int _sgs_gcmark( SGS_CTX, sgs_VarObj* obj ); \
	static int _sgs_getindex( SGS_ARGS_GETINDEXFUNC ); \
	static int _sgs_setindex( SGS_ARGS_SETINDEXFUNC ); \
	static int _sgs_dump( SGS_CTX, sgs_VarObj* obj, int depth ); \
	static _sgsInterface _sgs_interface;
# define SGS_OBJECT \
	SGS_OBJECT_LITE \
	sgs_VarObj* m_sgsObject SGS_CPPBC_DEFINIT(nullptr); \
	SGS_CTX SGS_CPPBC_DEFINIT(nullptr);
# define SGS_OBJECT_INHERIT( name ) SGS_OBJECT_LITE
# define SGS_NO_EXPORT
# define SGS_NO_DESTRUCT
# define SGS_METHOD
# define SGS_METHOD_NAMED( name )
# define SGS_MULTRET int
# define SGS_PROPERTY
# define SGS_PROPERTY_FUNC( funcs )
# define SGS_GCREF( mbname )
# define SGS_DUMP( what )
# define SGS_NODUMP( what )
# define SGS_IFUNC( type ) static
# define SGS_ALIAS( func )

#define SGS_DEFAULT_LITE_OBJECT_INTERFACE( name ) \
	template<> inline void sgs_PushVar<name>( SGS_CTX, const name& v ){ sgs_CreateLiteClassFrom( C, NULL, &v ); } \
	template<> struct sgs_GetVar<name> { name operator () ( SGS_CTX, sgs_StkIdx item ){ \
		if( sgs_IsObject( C, item, name::_sgs_interface ) ) return *(name*)sgs_GetObjectData( C, item ); return name(); }};
#endif


#define TOKENPASTE_(x, y) x ## y
#define TOKENPASTE(x, y) TOKENPASTE_(x, y)


template< class T > class _sgsTmpChanger
{
	T& dst;
	T src;
	T bk;
public:
	_sgsTmpChanger( T& d, const T s ) : dst(d), src(s), bk(d){ d = src; }
	~_sgsTmpChanger(){ dst = bk; }
};


class sgsScope
{
public:
	sgsScope( sgs_Context* c ) : C(c), m_stackSize( sgs_StackSize( c ) ){}
	~sgsScope()
	{
		assert( m_stackSize <= sgs_StackSize( C ) );
		sgs_SetStackSize( C, m_stackSize );
	}
	
	bool is_restored(){ return sgs_StackSize( C ) == m_stackSize; }
	
	SGS_CTX;
	sgs_StkIdx m_stackSize;
};
#define SGS_CSCOPE( C ) sgsScope TOKENPASTE( _scp, __LINE__ ) ( C )
#define SGS_SCOPE SGS_CSCOPE( C )


enum EsgsMaybeNot { sgsMaybeNot };

template< class T >
class sgsMaybe /* nullable PODs */
{
public:
	sgsMaybe() : isset(false) {};
	sgsMaybe( EsgsMaybeNot ) : isset(false) {};
	sgsMaybe( const T& val ) : data(val), isset(true) {}
	
	void set( const T& val ){ data = val; isset = true; }
	void unset(){ isset = false; }
	
	bool operator == ( const sgsMaybe<T>& o ) const
	{
		if( isset != o.isset ) return false;
		else if( !isset ) return true;
		else return data == o.data;
	}
	bool operator != ( const sgsMaybe<T>& o ) const { return !( *this == o ); }
	
	T data;
	bool isset;
};


template< class OwningClass >
class sgsArrayIterator
{
public:
	sgsArrayIterator( OwningClass* owner ) : m_owner(owner), m_origsize(owner->size()), m_offset(-1){ sgs_ObjAcquire( m_owner->C, m_owner->m_sgsObject ); }
	sgsArrayIterator( const sgsArrayIterator& other ) : m_owner(other.m_owner), m_origsize(other.m_origsize), m_offset(other.m_offset){ sgs_ObjAcquire( m_owner->C, m_owner->m_sgsObject ); }
	const sgsArrayIterator& operator = ( const sgsArrayIterator& other )
	{
		sgs_ObjRelease( m_owner->C, m_owner->m_sgsObject );
		m_owner = other.m_owner; m_origsize = other.m_origsize; m_offset = other.m_offset;
		sgs_ObjAcquire( m_owner->C, m_owner->m_sgsObject );
	}
	~sgsArrayIterator(){ sgs_ObjRelease( m_owner->C, m_owner->m_sgsObject ); }
	
	void gcmark(){ sgs_ObjGCMark( m_owner->C, m_owner->m_sgsObject ); }
	int convert( SGS_CTX, sgs_VarObj* obj, int to )
	{
		if( to == SGS_CONVOP_TOITER ){ sgs_PushObjectPtr( C, obj ); return SGS_SUCCESS; }
		if( to == SGS_CONVOP_CLONE ){ sgs_PushLiteClassFrom( C, this ); return SGS_SUCCESS; }
		return SGS_ENOTSUP;
	}
	int getnext( SGS_CTX, int mode )
	{
		if( m_origsize != m_owner->size() )
			return SGS_EINPROC;
		if( !mode ) // incr
		{
			m_offset++;
			return m_offset >= 0 && m_offset < m_origsize;
		}
		if( mode & SGS_GETNEXT_KEY ) sgs_PushInt( C, m_offset );
		if( mode & SGS_GETNEXT_VALUE ) sgs_PushVar( C, (*m_owner)[ m_offset ] );
		return SGS_SUCCESS;
	}
	
	static int _sgs_destruct( SGS_CTX, sgs_VarObj* obj ){ ((sgsArrayIterator*)obj->data)->~sgsArrayIterator(); return SGS_SUCCESS; }
	static int _sgs_gcmark( SGS_CTX, sgs_VarObj* obj ){ ((sgsArrayIterator*)obj->data)->gcmark(); return SGS_SUCCESS; }
	static int _sgs_convert( SGS_CTX, sgs_VarObj* obj, int type ){ return ((sgsArrayIterator*)obj->data)->convert( C, obj, type ); }
	static int _sgs_getnext( SGS_CTX, sgs_VarObj* obj, int type ){ return ((sgsArrayIterator*)obj->data)->getnext( C, type ); }
	static sgs_ObjInterface _sgs_interface[1];
	
	OwningClass* m_owner;
	sgs_SizeVal m_origsize, m_offset;
};

template< class OwningClass >
sgs_ObjInterface sgsArrayIterator<OwningClass>::_sgs_interface[1] =
{
	OwningClass::IteratorTypeName,
	sgsArrayIterator::_sgs_destruct, sgsArrayIterator::_sgs_gcmark,
	NULL, NULL,
	sgsArrayIterator::_sgs_convert, NULL, NULL, sgsArrayIterator::_sgs_getnext,
	NULL, NULL
};


// traverse the hierarchy to find the child class in it
// can't do it in reverse for now
// (no way to indicate that sgs_ObjInterface can be downcasted to _sgsInterface)
inline bool _sgsIsChild( sgs_ObjInterface* child, _sgsInterface* parent )
{
	_sgsInterface* p = parent->inh_child;
	while( p )
	{
		if( &p->iface == child )
			return true;
		if( _sgsIsChild( child, p ) )
			return true;
		p = p->inh_sibling;
	}
	return false;
}


template< class T >
class sgsHandle
{
public:
	
	sgsHandle() : object(NULL), C(NULL) {};
	sgsHandle( const sgsHandle& h ) : object(h.object), C(sgs_RootContext(h.C))
	{
		if( object )
			_acquire();
	}
	sgsHandle( sgs_Context* c, sgs_VarObj* obj ) : object(NULL), C(sgs_RootContext(c))
	{
		if( obj && obj->iface == T::_sgs_interface )
		{
			object = obj;
			_acquire();
		}
	}
	sgsHandle( sgs_Context* c, sgs_StkIdx item ) : object(NULL), C(sgs_RootContext(c))
	{
		if( sgs_IsObject( c, item, T::_sgs_interface ) )
		{
			object = sgs_GetObjectStruct( c, item );
			_acquire();
		}
	}
	sgsHandle( sgs_Context* c, sgs_Variable* var, bool cast = false )
	: object(NULL), C(sgs_RootContext(c))
	{
		if( cast )
		{
			if( var->type != SGS_VT_OBJECT )
				return;
			if( _sgsIsChild( var->data.O->iface, &T::_sgs_interface ) )
			{
				object = var->data.O;
				_acquire();
			}
		}
		else if( sgs_IsObjectP( var, T::_sgs_interface ) )
		{
			object = var->data.O;
			_acquire();
		}
	}
	explicit sgsHandle( T* obj ) : object(NULL), C(NULL)
	{
		if( obj )
		{
			object = obj->m_sgsObject;
			C = obj->C;
			_acquire();
		}
	}
	~sgsHandle(){ _release(); }
	
	const sgsHandle& operator = ( const sgsHandle& h )
	{
		if( object != h.object )
		{
			_release();
			if( h.object && h.object->iface == T::_sgs_interface )
			{
				object = h.object;
				C = h.C;
				_acquire();
			}
		}
		return *this;
	}
	
	operator T*(){ return object ? static_cast<T*>( object->data ) : NULL; }
	operator const T*() const { return object ? static_cast<T*>( object->data ) : NULL; }
	
	T* operator -> (){ return object ? static_cast<T*>( object->data ) : NULL; }
	const T* operator -> () const { return object ? static_cast<T*>( object->data ) : NULL; }
	
	bool operator < ( const sgsHandle& h ) const { return object < h.object; }
	bool operator == ( const sgsHandle& h ) const { return object == h.object; }
	bool operator != ( const sgsHandle& h ) const { return object != h.object; }
	
	void gcmark() const { if( object ) sgs_ObjGCMark( C, object ); }
	
	void push( sgs_Context* c ) const { assert( c ); sgs_Variable v;
		v.type = object ? SGS_VT_OBJECT : SGS_VT_NULL; v.data.O = object; sgs_PushVariable( c, v ); }
	bool not_null(){ return !!object; }
	class sgsVariable get_variable();
	
	sgs_VarObj* object;
	
	void _acquire(){ if( object ) sgs_ObjAcquire( C, object ); }
	void _release(){ if( object ){ sgs_ObjRelease( C, object ); object = NULL; } }
	
	sgs_Context* get_ctx() const { return C; }
	void set_ctx( sgs_Context* c ){ C = sgs_RootContext( c ); }
protected: /* prevent mysterious bugs from setting a non-root context */
	SGS_CTX;
};


class sgsString
{
public:
	
	sgsString() : str(NULL), C(NULL) {};
	sgsString( const sgsString& h ) : str(h.str), C(sgs_RootContext(h.C)) { _acquire(); }
	sgsString( sgs_Context* c, sgs_iStr* s ) : str(s), C(sgs_RootContext(c)) { _acquire(); }
	sgsString( sgs_Context* c, sgs_StkIdx item ) : str(NULL), C(sgs_RootContext(c))
	{
		if( sgs_ParseString( c, item, NULL, NULL ) )
		{
			sgs_Variable v;
			sgs_GetStackItem( c, item, &v );
			str = v.data.S;
		}
	}
	sgsString( sgs_Context* c, sgs_Variable* var ) : str(NULL), C(sgs_RootContext(c))
	{
		if( sgs_ParseStringP( c, var, NULL, NULL ) )
		{
			str = var->data.S;
			_acquire();
		}
	}
	sgsString( sgs_Context* c, const char* s, size_t sz ) : str(NULL), C(sgs_RootContext(c))
	{
		assert( sz <= 0x7fffffff );
		sgs_Variable v;
		sgs_InitStringBuf( c, &v, s, (sgs_SizeVal) sz );
		str = v.data.S;
	}
	sgsString( sgs_Context* c, const char* s ) : str(NULL), C(sgs_RootContext(c))
	{
		sgs_Variable v;
		sgs_InitString( c, &v, s );
		str = v.data.S;
	}
	~sgsString(){ _release(); }
	
	const sgsString& operator = ( const sgsString& s )
	{
		if( !same_as( s ) )
		{
			_release();
			if( s.str )
			{
				str = s.str;
				C = s.get_ctx();
				_acquire();
			}
		}
		return *this;
	}
	
	const char* c_str() const { return str ? sgs_str_c_cstr( str ) : NULL; }
	size_t size() const { return str ? (size_t) str->size : 0; }
#ifdef SGS_CPPBC_WITH_STD_STRING
	bool get_string( std::string& out ){ if( str ){ out = std::string( sgs_str_c_cstr( str ), str->size ); return true; } return false; }
#endif
	
	int compare( const sgsString& s ) const
	{
		if( same_as( s ) )
			return 0;
		int null1 = str == NULL;
		int null2 = s.str == NULL;
		if( null1 || null2 )
			return null2 - null1;
		int cmp = memcmp( sgs_str_c_cstr( str ), sgs_str_c_cstr( s.str ), str->size < s.str->size ? str->size : s.str->size );
		if( cmp )
			return cmp;
		return str->size == s.str->size ? 0 : ( str->size < s.str->size ? -1 : 1 );
	}
	bool operator < ( const sgsString& s ) const { return compare( s ) < 0; }
	bool operator == ( const sgsString& s ) const { return same_as( s ); }
	bool operator != ( const sgsString& s ) const { return !same_as( s ); }
	bool same_as( const sgsString& s ) const { return str == s.str; }
	
	bool equals( const char* s ) const { return strcmp( sgs_str_c_cstr( str ), s ) == 0; }
	bool equals( const char* s, size_t sz ) const { return sz == str->size &&
		memcmp( sgs_str_c_cstr( str ), s, sz ) == 0; }
	
	void push( sgs_Context* c ) const { assert( c ); sgs_Variable v;
		v.type = str ? SGS_VT_STRING : SGS_VT_NULL; v.data.S = str; sgs_PushVariable( c, v ); }
	bool not_null(){ return !!str; }
	class sgsVariable get_variable();
	
	sgs_iStr* str;
	
	void _acquire(){ if( str ){ sgs_Variable v; v.type = SGS_VT_STRING; v.data.S = str; sgs_Acquire( C, &v ); } }
	void _release(){ if( str ){ sgs_Variable v; v.type = SGS_VT_STRING; v.data.S = str; sgs_Release( C, &v ); str = NULL; } }
	
	sgs_Context* get_ctx() const { return C; }
	void set_ctx( sgs_Context* c ){ C = sgs_RootContext( c ); }
protected: /* prevent mysterious bugs from setting a non-root context */
	SGS_CTX;
};


class sgsVariable
{
public:
	
	enum EPickAndPop { PickAndPop };
	
	sgsVariable() : C(NULL) { var.type = SGS_VT_NULL; };
	sgsVariable( const sgsVariable& h ) : var(h.var), C(h.C)
	{
		if( h.var.type != SGS_VT_NULL )
			_acquire();
	}
	sgsVariable( sgs_Context* c ) : C(sgs_RootContext(c)) { var.type = SGS_VT_NULL; }
	sgsVariable( sgs_Context* c, sgs_StkIdx item ) : C(sgs_RootContext(c))
	{
		var = sgs_StackItem( c, item );
		_acquire();
	}
	sgsVariable( sgs_Context* c, EPickAndPop ) : C(sgs_RootContext(c))
	{
		var = sgs_StackItem( c, -1 );
		_acquire();
		sgs_Pop( c, 1 );
	}
	sgsVariable( sgs_Context* c, sgs_Variable* v ) : C(sgs_RootContext(c))
	{
		var.type = SGS_VT_NULL;
		if( v && v->type != SGS_VT_NULL )
		{
			var = *v;
			_acquire();
		}
	}
	sgsVariable( const sgsString& s ) : C(sgs_RootContext(s.get_ctx()))
	{
		if( s.str != NULL )
		{
			var.type = SGS_VT_STRING;
			var.data.S = s.str;
			_acquire();
		}
		else
			var.type = SGS_VT_NULL;
	}
	template< class T > sgsVariable( const sgsHandle<T>& h ) : C(h.get_ctx())
	{
		if( h.object != NULL )
		{
			var.type = SGS_VT_OBJECT;
			var.data.O = h.object;
			_acquire();
		}
		else
			var.type = SGS_VT_NULL;
	}
	~sgsVariable(){ _release(); }
	
	const sgsVariable& operator = ( const sgsVariable& h )
	{
		if( *this != h )
		{
			_release();
			if( h.var.type != SGS_VT_NULL )
			{
				var = h.var;
				C = h.get_ctx();
				_acquire();
			}
		}
		return *this;
	}
	
	bool operator < ( const sgsVariable& h ) const
	{
		if( var.type != h.var.type )
			return var.type < h.var.type;
		switch( var.type )
		{
		case SGS_VT_BOOL: return var.data.B < h.var.data.B;
		case SGS_VT_INT: return var.data.I < h.var.data.I;
		case SGS_VT_REAL: return var.data.R < h.var.data.R;
		case SGS_VT_STRING: { uint32_t minsize =
			var.data.S->size < h.var.data.S->size ? var.data.S->size : h.var.data.S->size;
			int diff = memcmp( sgs_var_cstr( &var ), sgs_var_cstr( &h.var ), minsize );
			if( diff ) return diff < 0;
			return var.data.S->size < h.var.data.S->size; }
		case SGS_VT_FUNC: return var.data.F < h.var.data.F;
		case SGS_VT_CFUNC: return var.data.C < h.var.data.C;
		case SGS_VT_OBJECT: return var.data.O < h.var.data.O;
		case SGS_VT_PTR: return var.data.P < h.var.data.P;
		}
		return false;
	}
	bool operator == ( const sgsVariable& h ) const
	{
		if( var.type != h.var.type )
			return false;
		switch( var.type )
		{
		case SGS_VT_BOOL: return var.data.B == h.var.data.B;
		case SGS_VT_INT: return var.data.I == h.var.data.I;
		case SGS_VT_REAL: return var.data.R == h.var.data.R;
		case SGS_VT_STRING:
	#if SGS_STRINGTABLE_MAXLEN >= 0x7fffffff
			return var.data.S == h.var.data.S;
	#else
			if( var.data.S == h.var.data.S ) return true;
			return var.data.S->size == h.var.data.S->size &&
				memcmp( sgs_var_cstr( &var ), sgs_var_cstr( &h.var ), var.data.S->size ) == 0;
	#endif
		case SGS_VT_FUNC: return var.data.F == h.var.data.F;
		case SGS_VT_CFUNC: return var.data.C == h.var.data.C;
		case SGS_VT_OBJECT: return var.data.O == h.var.data.O;
		case SGS_VT_PTR: return var.data.P == h.var.data.P;
		}
		return true;
	}
	bool operator != ( const sgsVariable& h ) const { return !( *this == h ); }
	
	void push( sgs_Context* c ) const { assert( c ); sgs_PushVariable( c, var ); }
	void gcmark() { if( C ) sgs_GCMark( C, &var ); }
	bool not_null() const { return var.type != SGS_VT_NULL; }
	bool is_object( sgs_ObjInterface* iface ){ return !!sgs_IsObjectP( &var, iface ); }
	template< class T > bool is_handle(){ return sgs_IsObjectP( &var, T::_sgs_interface ); }
	template< class T > T* get_object_data(){ return (T*) sgs_GetObjectDataP( &var ); }
	template< class T > sgsHandle<T> get_handle(){ return sgsHandle<T>( C, &var ); }
	template< class T > sgsHandle<T> downcast(){ return sgsHandle<T>( C, &var, true ); }
	int type_id() const { return (int) var.type; }
	bool is_string() const { return var.type == SGS_VT_STRING; }
	sgsString get_string(){ return is_string() ? sgsString( C, var.data.S ) : sgsString(); }
	
	/* indexing */
	sgsVariable getsubitem( sgsVariable key, bool prop )
	{
		sgsVariable out(C);
		if( not_null() )
			sgs_GetIndex( C, var, key.var, &out.var, prop );
		return out;
	}
	sgsVariable getsubitem( const char* key, bool prop )
	{
		if( !not_null() )
		{
			return sgsVariable();
		}
		return getsubitem( sgsString( C, key ).get_variable(), prop );
	}
	sgsVariable getprop( sgsVariable key ){ return getsubitem( key, true ); }
	sgsVariable getindex( sgsVariable key ){ return getsubitem( key, false ); }
	sgsVariable getprop( const char* key ){ return getsubitem( key, true ); }
	sgsVariable getindex( const char* key ){ return getsubitem( key, false ); }
	sgsVariable operator [] ( sgsVariable key ){ return getsubitem( key, false ); }
	sgsVariable operator [] ( const char* key ){ return getsubitem( key, false ); }
	
	bool setsubitem( sgsVariable key, sgsVariable val, bool prop )
	{
		return sgs_SetIndex( C, var, key.var, val.var, prop );
	}
	bool setsubitem( const char* key, sgsVariable val, bool prop )
	{
		if( !not_null() )
		{
			return false;
		}
		return setsubitem( sgsString( C, key ).get_variable(), val, prop );
	}
	bool setprop( sgsVariable key, sgsVariable val ){ return setsubitem( key, val, true ); }
	bool setindex( sgsVariable key, sgsVariable val ){ return setsubitem( key, val, false ); }
	bool setprop( const char* key, sgsVariable val ){ return setsubitem( key, val, true ); }
	bool setindex( const char* key, sgsVariable val ){ return setsubitem( key, val, false ); }
	
	template< class T > T get();
	template< class T > T getdef( const T& def ){ if( not_null() ) return get<T>(); else return def; }
	sgsVariable& set_null(){ _release(); var = sgs_MakeNull(); return *this; }
	sgsVariable& set( bool v ){ _release(); var = sgs_MakeBool( v ); return *this; }
	sgsVariable& set( sgs_Int v ){ _release(); var = sgs_MakeInt( v ); return *this; }
	sgsVariable& set( sgs_Real v ){ _release(); var = sgs_MakeReal( v ); return *this; }
	sgsVariable& set( sgsString v ){ _release(); if( v.not_null() ){ C = v.get_ctx(); var.type = SGS_VT_STRING; var.data.S = v.str; _acquire(); } return *this; }
	sgsVariable& set( sgs_CFunc v ){ _release(); var = sgs_MakeCFunc( v ); return *this; }
	template< class T > sgsVariable& set( sgsHandle< T > v ){ _release(); C = v.object->C; sgs_InitObjectPtr( C, &var, v.object ); return *this; }
	template< class T > sgsVariable& set( T* v ){ _release(); C = v->C; sgs_InitObjectPtr( C, &var, v->m_sgsObject ); return *this; }
	void call( sgs_Context* c, int args = 0, int ret = 0 )
	{
		sgs_Call( c, var, args, ret );
	}
	void thiscall( sgs_Context* c, sgsVariable func, int args = 0, int ret = 0 )
	{
		sgs_InsertVariable( c, -args - 1, var );
		sgs_ThisCall( c, func.var, args, ret );
	}
	void thiscall( sgs_Context* c, const char* key, int args = 0, int ret = 0 )
	{
		thiscall( c, getprop( key ), args, ret );
	}
	
	sgs_Variable var;
	
	void _acquire(){ if( C ){ sgs_Acquire( C, &var ); } }
	void _release(){ if( C ){ sgs_Release( C, &var ); var.type = SGS_VT_NULL; } }
	
	sgs_Context* get_ctx() const { return C; }
	void set_ctx( sgs_Context* c ){ C = sgs_RootContext( c ); }
protected: /* prevent mysterious bugs from setting a non-root context */
	SGS_CTX;
};

template<class T>
inline sgsVariable sgsHandle<T>::get_variable()
{
	sgs_Variable v;
	v.type = object ? SGS_VT_OBJECT : SGS_VT_NULL;
	v.data.O = object;
	return sgsVariable( C, &v );
}

inline sgsVariable sgsString::get_variable()
{
	sgs_Variable v;
	v.type = str ? SGS_VT_STRING : SGS_VT_NULL;
	v.data.S = str;
	return sgsVariable( C, &v );
}


/* GCMark<T> */
template< class T > void sgs_GCMarkVar( SGS_CTX, T& var ){}
template<> inline void sgs_GCMarkVar<sgs_Variable>( SGS_CTX, sgs_Variable& v ){ sgs_GCMark( C, &v ); }
template<> inline void sgs_GCMarkVar<sgsVariable>( SGS_CTX, sgsVariable& v ){ v.gcmark(); }
template< class T > inline void sgs_GCMarkVar( SGS_CTX, sgsHandle<T>& v ){ v.gcmark(); }
#ifdef SGS_CPPBC_WITH_STD_VECTOR
template< class T > inline void sgs_GCMarkVar( SGS_CTX, std::vector<T>& v ){
	for( size_t i = 0; i < v.size(); ++i ) sgs_GCMarkVar( C, v[i] ); }
#endif


/* Dump<T> */
template< class T > sgsString sgs_DumpData( SGS_CTX, const T& var, int depth ){ return sgsString( C, "<unknown>" ); }
template< class T > sgsString sgs_DumpData( SGS_CTX, const T* var, int depth )
{
	if( !var )
		return sgsString( C, "null" );
	return sgs_DumpData( C, *var, depth );
}
template< class T > inline sgsString sgs_DumpDataPushVar( SGS_CTX, const T& var, int depth )
{
	SGS_SCOPE;
	sgs_PushVar( C, var );
	sgs_DumpVar( C, sgs_StackItem( C, -1 ), depth );
	return sgsString( C, -1 );
}
template< class T > inline sgsString sgs_DumpData( SGS_CTX, const sgsMaybe<T>& var, int depth )
{
	SGS_SCOPE;
	if( var.isset )
	{
		sgs_PushString( C, "[set] " );
		sgs_DumpData( C, var.data, depth ).push( C );
		sgs_StringConcat( C, 2 );
		return sgsString( C, -1 );
	}
	return sgsString( C, "[unset]" );
}
template<> inline sgsString sgs_DumpData<sgs_Variable>( SGS_CTX, const sgs_Variable& v, int depth )
{
	SGS_SCOPE;
	sgs_DumpVar( C, v, depth );
	return sgsString( C, -1 );
}
template<> inline sgsString sgs_DumpData<sgsVariable>( SGS_CTX, const sgsVariable& v, int depth ){ return sgs_DumpData( C, v.var, depth ); }
template< class T > inline sgsString sgs_DumpData( SGS_CTX, const sgsHandle<T>& v, int depth ){ return sgs_DumpDataPushVar( C, v, depth ); }
template<> inline sgsString sgs_DumpData<bool>( SGS_CTX, const bool& v, int ){ return sgsString( C, v ? "true" : "false" ); }
#define SGS_DECL_DUMPDATA_INT( type ) \
	template<> inline sgsString sgs_DumpData<type>( SGS_CTX, const type& v, int depth ){ return sgs_DumpDataPushVar( C, v, depth ); }
SGS_DECL_DUMPDATA_INT( signed char );
SGS_DECL_DUMPDATA_INT( unsigned char );
SGS_DECL_DUMPDATA_INT( signed short );
SGS_DECL_DUMPDATA_INT( unsigned short );
SGS_DECL_DUMPDATA_INT( signed int );
SGS_DECL_DUMPDATA_INT( unsigned int );
SGS_DECL_DUMPDATA_INT( signed long );
SGS_DECL_DUMPDATA_INT( unsigned long );
SGS_DECL_DUMPDATA_INT( signed long long );
SGS_DECL_DUMPDATA_INT( unsigned long long );
SGS_DECL_DUMPDATA_INT( float );
SGS_DECL_DUMPDATA_INT( double );
SGS_DECL_DUMPDATA_INT( sgsString );
#ifdef SGS_CPPBC_WITH_STD_STRING
SGS_DECL_DUMPDATA_INT( std::string );
#endif
#ifdef SGS_CPPBC_WITH_STD_VECTOR
template< class T > inline sgsString sgs_DumpData( SGS_CTX, const std::vector<T>& v, int depth )
{
	SGS_SCOPE;
	char bfr[ 32 ];
	bool expand = depth-- > 0;
	sprintf( bfr, "array(std) [%d]%s", (int) v.size(),
		expand ? ( v.size() ? "\n{" : " {}" ) : " ..." );
	sgs_PushString( C, bfr );
	if( expand && v.size() )
	{
		for( size_t i = 0; i < v.size(); ++i )
		{
			sgs_PushString( C, "\n" );
			sgs_DumpData( C, v[i], depth ).push( C );
		}
		sgs_StringConcat( C, (sgs_StkIdx) v.size() * 2 );
		sgs_PadString( C );
		sgs_PushString( C, "\n}" );
		sgs_StringConcat( C, 3 );
	}
	return sgsString( C, -1 );
}
#endif


/* PushVar [stack] */
template< class T > void sgs_PushVar( SGS_CTX, const T& );
template< class T > inline void sgs_PushVar( SGS_CTX, T* v ){ sgs_CreateClass( C, NULL, v ); }
template< class T > inline void sgs_PushVar( SGS_CTX, sgsMaybe<T> v ){ if( v.isset ) sgs_PushVar( C, v.data ); else sgs_PushNull( C ); }
template< class T > inline void sgs_PushVar( SGS_CTX, sgsHandle<T> v ){ v.push( C ); }
template<> inline void sgs_PushVar<sgsVariable>( SGS_CTX, const sgsVariable& v ){ v.push( C ); }
template<> inline void sgs_PushVar( SGS_CTX, void* v ){ sgs_PushPtr( C, v ); }
#define SGS_DECL_PUSHVAR( type, parsefn ) template<> inline void sgs_PushVar<type>( SGS_CTX, const type& v ){ parsefn( C, v ); }
SGS_DECL_PUSHVAR( bool, sgs_PushBool );
#define SGS_DECL_PUSHVAR_INT( type ) template<> inline void sgs_PushVar<type>( SGS_CTX, const type& v ){ sgs_PushInt( C, (sgs_Int) v ); }
SGS_DECL_PUSHVAR_INT( signed char );
SGS_DECL_PUSHVAR_INT( unsigned char );
SGS_DECL_PUSHVAR_INT( signed short );
SGS_DECL_PUSHVAR_INT( unsigned short );
SGS_DECL_PUSHVAR_INT( signed int );
SGS_DECL_PUSHVAR_INT( unsigned int );
SGS_DECL_PUSHVAR_INT( signed long );
SGS_DECL_PUSHVAR_INT( unsigned long );
SGS_DECL_PUSHVAR_INT( signed long long );
SGS_DECL_PUSHVAR_INT( unsigned long long );
SGS_DECL_PUSHVAR( float, sgs_PushReal );
SGS_DECL_PUSHVAR( double, sgs_PushReal );
template<> inline void sgs_PushVar<sgsString>( SGS_CTX, const sgsString& v ){ v.push( C ); }
SGS_DECL_PUSHVAR( sgs_CFunc, sgs_PushCFunc );
#ifdef SGS_CPPBC_WITH_STD_STRING
template<> inline void sgs_PushVar<std::string>( SGS_CTX, const std::string& v ){ sgs_PushStringBuf( C, v.c_str(), (sgs_SizeVal) v.size() ); }
#endif


/* GETVAR [stack] */
template< class T > struct sgs_GetVar {
	T operator () ( SGS_CTX, sgs_StkIdx item );
};
template< class T > struct sgs_GetVarObj { T* operator () ( SGS_CTX, sgs_StkIdx item )
{
	if( sgs_IsObject( C, item, T::_sgs_interface ) )
		return static_cast<T*>( sgs_GetObjectData( C, item ) );
	return NULL;
}};
template<> struct sgs_GetVar<bool> { bool operator () ( SGS_CTX, sgs_StkIdx item )
{
	sgs_Bool v;
	if( sgs_ParseBool( C, item, &v ) )
		return !!v;
	return false;
}};
#define SGS_DECL_GETVAR_INT( type ) \
	template<> struct sgs_GetVar<type> { type operator () ( SGS_CTX, sgs_StkIdx item ){ \
		sgs_Int v; if( sgs_ParseInt( C, item, &v ) ) return (type) v; return 0; }};
SGS_DECL_GETVAR_INT( signed char );
SGS_DECL_GETVAR_INT( unsigned char );
SGS_DECL_GETVAR_INT( signed short );
SGS_DECL_GETVAR_INT( unsigned short );
SGS_DECL_GETVAR_INT( signed int );
SGS_DECL_GETVAR_INT( unsigned int );
SGS_DECL_GETVAR_INT( signed long );
SGS_DECL_GETVAR_INT( unsigned long );
SGS_DECL_GETVAR_INT( signed long long );
template<> struct sgs_GetVar<float> { float operator () ( SGS_CTX, sgs_StkIdx item ){
	sgs_Real v; if( sgs_ParseReal( C, item, &v ) ) return (float) v; return 0; }};
template<> struct sgs_GetVar<double> { double operator () ( SGS_CTX, sgs_StkIdx item ){
	sgs_Real v; if( sgs_ParseReal( C, item, &v ) ) return (double) v; return 0; }};
template<> struct sgs_GetVar<void*> { void* operator () ( SGS_CTX, sgs_StkIdx item ){
	void* v; if( sgs_ParsePtr( C, item, &v ) ) return (void*) v; return 0; }};
template<> struct sgs_GetVar<char*> { char* operator () ( SGS_CTX, sgs_StkIdx item ){
	char* str = NULL; sgs_ParseString( C, item, &str, NULL ); return str; }};
template<> struct sgs_GetVar<sgsString> { sgsString operator () ( SGS_CTX, sgs_StkIdx item ){ return sgsString( C, item ); }};
#ifdef SGS_CPPBC_WITH_STD_STRING
template<> struct sgs_GetVar<std::string> { std::string operator () ( SGS_CTX, sgs_StkIdx item ){
	char* str; sgs_SizeVal size; if( sgs_ParseString( C, item, &str, &size ) )
		return std::string( str, (size_t) size ); return std::string(); }};
#endif
template<> struct sgs_GetVarObj<void> { void* operator () ( SGS_CTX, sgs_StkIdx item )
{
	return sgs_GetVar<void*>()( C, item );
}};
template< class O >
struct sgs_GetVar< sgsMaybe<O> >
{
	sgsMaybe<O> operator () ( SGS_CTX, sgs_StkIdx item ) const
	{
		if( sgs_ItemType( C, item ) != SGS_VT_NULL )
			return sgsMaybe<O>( sgs_GetVar<O>()( C, item ) );
		return sgsMaybe<O>();
	}
};
template< class O >
struct sgs_GetVar< sgsHandle<O> > { sgsHandle<O> operator () ( SGS_CTX, sgs_StkIdx item ) const {
	return sgsHandle<O>( C, item ); } };
template<> struct sgs_GetVar< sgsVariable > { sgsVariable operator () ( SGS_CTX, sgs_StkIdx item ) const {
	return sgsVariable( C, item ); } };


template< class T > T sgsVariable::get()
{
	SGS_SCOPE;
	push( C );
	return sgs_GetVar< T >()( C, -1 );
}


template< class T > void sgs_CreateClass( SGS_CTX, sgs_Variable* out, T* inst )
{
	sgs_CreateObject( C, out, inst, T::_sgs_interface );
	inst->m_sgsObject = out ? sgs_GetObjectStructP( out ) : sgs_GetObjectStruct( C, -1 );
	inst->C = sgs_RootContext( C );
}
template< class T > T* sgs_CreateClassIPA( SGS_CTX, sgs_Variable* out )
{
	T* data = static_cast<T*>( sgs_CreateObjectIPA( C, out, (sgs_SizeVal) sizeof(T), T::_sgs_interface ) );
	data->m_sgsObject = out ? sgs_GetObjectStructP( out ) : sgs_GetObjectStruct( C, -1 );
	data->C = sgs_RootContext( C );
	return data;
}
template< class T> T* sgs_InitCreatedClass( SGS_CTX, sgs_Variable* out, T* inst )
{
	inst->C = sgs_RootContext( C );
	inst->m_sgsObject = out ? sgs_GetObjectStructP( out ) : sgs_GetObjectStruct( C, -1 );
	return inst;
}
#define SGS_CREATECLASS( C, out, name, args ) \
	(sgs_InitCreatedClass( C, out, new (sgs_CreateClassIPA< name >( C, out )) name args ))
	
template< class T > T* sgs_CreateClassFrom( SGS_CTX, sgs_Variable* out, T* inst )
{
	T* data = SGS_CREATECLASS( C, out, T, ( *inst ) );
	return sgs_InitCreatedClass( C, out, data );
}


template< class T > void sgs_CreateLiteClass( SGS_CTX, sgs_Variable* out, T* inst )
{
	sgs_CreateObject( C, out, inst, T::_sgs_interface );
}
template< class T > T* sgs_CreateLiteClassIPA( SGS_CTX, sgs_Variable* out )
{
	return static_cast<T*>( sgs_CreateObjectIPA( C, out, (sgs_SizeVal) sizeof(T), T::_sgs_interface ) );
}
#define SGS_CREATELITECLASS( C, out, name, args ) (new (sgs_CreateLiteClassIPA< name >( C, out )) name args )
template< class T > T* sgs_CreateLiteClassFrom( SGS_CTX, sgs_Variable* out, const T* inst )
{
	T* data = SGS_CREATELITECLASS( C, out, T, ( *inst ) );
	return data;
}


#endif // __SGS_CPPBC_H__

