

#include <errno.h>
#include <math.h>
#ifdef _WIN32
#  undef _WIN32_WINNT
#  define _WIN32_WINNT _WIN32_WINNT_WINXP
#  include <winsock2.h>
#  include <ws2tcpip.h>
#else
#  include <unistd.h>
#  include <sys/ioctl.h>
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  include <netdb.h>
#  define closesocket close
#  define ioctlsocket ioctl
#endif


#ifndef MSG_CONFIRM
#define MSG_CONFIRM 0
#endif
#ifndef MSG_DONTWAIT
#define MSG_DONTWAIT 0
#endif
#ifndef MSG_EOR
#define MSG_EOR 0
#endif
#ifndef MSG_MORE
#define MSG_MORE 0
#endif
#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#endif


#ifndef HEADER_SGSCRIPT_H
# define HEADER_SGSCRIPT_H <sgscript.h>
#endif
#include HEADER_SGSCRIPT_H
#ifndef HEADER_SGS_UTIL_H
# define HEADER_SGS_UTIL_H <sgs_util.h>
#endif
#include HEADER_SGS_UTIL_H

#define DF( x ) { #x, x }
#define STREQ( a, b ) (0==strcmp(a,b))
#define IFN( x ) { sgs_PushCFunc( C, x ); return SGS_SUCCESS; }
#define STDLIB_WARN( warn ) return sgs_Msg( C, SGS_WARNING, warn );
#define SOCK_IBUFSZ 1024


#ifdef _WIN32
#  define SGS_SCKID SOCKET
#  define SGS_NOSOCK INVALID_SOCKET
#  define sgs_sockerror WSAGetLastError()
#  define IOCTLCONV( x ) (int)(x)
#  define IOCTL_VALUE u_long
#  define SOCKDATA_LEN int
#  define SOCKDATA_SLEN int
#  define SOCKADDR_SIZE int
#  define GSO_ARG5TYPE int
#  define sa_family_t int16_t
#else
#  define SGS_SCKID int
#  define SGS_NOSOCK -1
#  define sgs_sockerror errno
#  define IOCTLCONV( x ) x
#  define IOCTL_VALUE int
#  define SOCKDATA_LEN size_t
#  define SOCKDATA_SLEN ssize_t
#  ifdef SGS_PF_ANDROID
#    define SOCKADDR_SIZE socklen_t
#    define GSO_ARG5TYPE socklen_t
#  else
#    define SOCKADDR_SIZE unsigned int
#    define GSO_ARG5TYPE unsigned int
#  endif
#endif


/*
	Socket error handling
*/

#define SCKERRVN "__socket_error"

static int sockassert( SGS_CTX, int test )
{
	int err = test ? 0 : sgs_sockerror;
	sgs_SetGlobalByName( C, SCKERRVN, sgs_MakeInt( err ) );
	return test;
}

static int socket_error( SGS_CTX )
{
	int astext = 0, e = 0;
	SGSFN( "socket_error" );
	if( !sgs_LoadArgs( C, "|b", &astext ) )
		return 0;
	
	if( sgs_PushGlobalByName( C, SCKERRVN ) )
		e = (int) sgs_GetInt( C, -1 );
	else if( !astext )
		sgs_PushInt( C, 0 );
	
	if( !astext )
		return 1;
	
#ifdef _WIN32
	{
		char buf[ 1024 ];
		DWORD numwr = FormatMessageA
		(
			FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, (DWORD) e,
			MAKELANGID( LANG_NEUTRAL, SUBLANG_DEFAULT ),
			buf, 1024, NULL
		);
		if( !numwr )
			STDLIB_WARN( "failed to retrieve error message" )
		sgs_PushStringBuf( C, buf, (sgs_SizeVal) numwr );
	}
#else
	sgs_PushString( C, strerror( e ) );
#endif
	return 1;
}

#define SOCKERR sockassert( C, 1 )
#define SOCKCLEARERR sockassert( C, 0 )


#ifdef _WIN32
#  define AE( x ) #x, (const char*) WSA##x
#else
#  define AE( x ) #x, (const char*) x
#endif
static const char* socket_errno_key_table[] =
{
#ifndef _WIN32
	AE( EBADMSG ), AE( ECANCELED ), AE( EIDRM ), AE( ENODATA ),
	AE( ENOLINK ), AE( ENOMSG ), AE( ENOSR ), AE( ENOSTR ),
	AE( ENOTRECOVERABLE ), AE( ENOTSUP ), AE( EOVERFLOW ),
	AE( EOWNERDEAD ), AE( EPROTO ), AE( ETIME ), AE( ETXTBSY ),
#endif
	
	AE( EADDRINUSE ), AE( EADDRNOTAVAIL ), AE( EAFNOSUPPORT ),
	AE( EALREADY ), AE( ECONNABORTED ), AE( ECONNREFUSED ),
	AE( ECONNRESET ), AE( EDESTADDRREQ ), AE( EHOSTUNREACH ),
	AE( EINPROGRESS ), AE( EISCONN ), AE( ELOOP ), AE( EMSGSIZE ),
	AE( ENETDOWN ), AE( ENETRESET ), AE( ENETUNREACH ), AE( ENOBUFS ),
	AE( ENOPROTOOPT ), AE( ENOTCONN ), AE( ENOTSOCK ),
	AE( EOPNOTSUPP ), AE( EPROTONOSUPPORT ), AE( EPROTOTYPE ),
	AE( ETIMEDOUT ), AE( EWOULDBLOCK ),
	
	NULL,
};
#undef AE

static int socket_geterrnobyname( SGS_CTX )
{
	const char** ekt = socket_errno_key_table;
	char* str;
	
	SGSFN( "socket_geterrnobyname" );
	
	if( !sgs_LoadArgs( C, "s", &str ) )
		return 0;
	
	while( *ekt )
	{
		if( strcmp( *ekt, str ) == 0 )
		{
			sgs_PushInt( C, (int) (size_t) ekt[1] );
			return 1;
		}
		ekt += 2;
	}
	
	sgs_Msg( C, SGS_ERROR, "this socket errno value is unsupported on this platform" );
	return 0;
}


/*
	Socket address object
*/

SGS_DECLARE sgs_ObjInterface sockaddr_iface[1];

#define GET_SAF ((struct sockaddr_storage*)obj->data)->ss_family
#define GET_SAI ((struct sockaddr_in*)obj->data)
#define GET_SAI6 ((struct sockaddr_in6*)obj->data)

static void sockaddr_push_full_addr_string( SGS_CTX, sgs_VarObj* obj )
{
	char addr[ 64 ] = {0};
#ifdef _WIN32
	DWORD ioasz = 64;
	WSAAddressToString( (LPSOCKADDR) obj->data, sizeof(struct sockaddr_storage), NULL, addr, &ioasz );
#else
	if( GET_SAF == AF_INET || GET_SAF == AF_INET6 )
	{
		char pb[ 8 ];
		inet_ntop( GET_SAF, GET_SAF == AF_INET ?
			(void*) &GET_SAI->sin_addr :
			(void*) &GET_SAI6->sin6_addr, addr, 64 );
		sprintf( pb, ":%u", (int) GET_SAF == AF_INET ? GET_SAI->sin_port : GET_SAI6->sin6_port );
		strcat( addr, pb );
	}
#endif
	addr[ 64-1 ] = 0;
	if( *addr )
		sgs_PushString( C, addr );
	else
		sgs_PushString( C, "-" );
}

static int sockaddr_getindex( SGS_ARGS_GETINDEXFUNC )
{
	char* name;
	if( sgs_ParseString( C, 0, &name, NULL ) )
	{
		if( STREQ( name, "family" ) ){ sgs_PushInt( C, GET_SAF ); return SGS_SUCCESS; }
		if( STREQ( name, "port" ) )
		{
			if( GET_SAF == AF_INET ){ sgs_PushInt( C, ntohs( GET_SAI->sin_port ) ); }
			else if( GET_SAF == AF_INET6 ){ sgs_PushInt( C, ntohs( GET_SAI6->sin6_port ) ); }
			else { sgs_PushNull( C ); sgs_Msg( C, SGS_WARNING, "port supported only for AF_INET[6]" ); }
			return SGS_SUCCESS;
		}
		if( STREQ( name, "addr_u32" ) )
		{
			if( GET_SAF == AF_INET ){ sgs_PushInt( C, ntohl( GET_SAI->sin_addr.s_addr ) ); }
			else { sgs_PushNull( C ); sgs_Msg( C, SGS_WARNING, "addr_u32 supported only for AF_INET" ); }
			return SGS_SUCCESS;
		}
		if( STREQ( name, "addr_buf" ) )
		{
			if( GET_SAF == AF_INET ){ sgs_PushStringBuf( C, (char*) &GET_SAI->sin_addr.s_addr, 4 ); }
			else if( GET_SAF == AF_INET6 ){ sgs_PushStringBuf( C, (char*) &GET_SAI6->sin6_addr.s6_addr, 16 ); }
			else { sgs_PushNull( C ); sgs_Msg( C, SGS_WARNING, "addr_buf supported only for AF_INET[6]" ); }
			return SGS_SUCCESS;
		}
		if( STREQ( name, "addr_bytes" ) )
		{
			char* buf = NULL;
			int i, sz = 0;
			if( GET_SAF == AF_INET )
			{
				buf = (char*) &GET_SAI->sin_addr.s_addr;
				sz = 4;
			}
			else if( GET_SAF == AF_INET6 )
			{
				buf = (char*) &GET_SAI6->sin6_addr.s6_addr;
				sz = 16;
			}
			if( buf )
			{
				for( i = 0; i < sz; ++i )
					sgs_PushInt( C, buf[ i ] );
				sgs_CreateArray( C, NULL, sz );
			}
			else { sgs_PushNull( C ); sgs_Msg( C, SGS_WARNING, "addr_bytes supported only for AF_INET[6]" ); }
			return SGS_SUCCESS;
		}
		if( STREQ( name, "addr_string" ) )
		{
			char addr[ 64 ] = {0};
#ifdef _WIN32
			DWORD ioasz = 64;
			WSAAddressToString( (LPSOCKADDR) obj->data, sizeof(struct sockaddr_storage), NULL, addr, &ioasz );
			*strrchr( addr, ':' ) = 0;
#else
			if( GET_SAF == AF_INET )
				inet_ntop( GET_SAF, &GET_SAI->sin_addr, addr, 64 );
			else if( GET_SAF == AF_INET6 )
				inet_ntop( GET_SAF, &GET_SAI6->sin6_addr, addr, 64 );
#endif
			addr[ 64-1 ] = 0;
			if( *addr )
				sgs_PushString( C, addr );
			else
				sgs_PushString( C, "-" );
			return SGS_SUCCESS;
		}
		if( STREQ( name, "full_addr_string" ) )
		{
			sockaddr_push_full_addr_string( C, obj );
			return SGS_SUCCESS;
		}
	}
	return SGS_ENOTFND;
}

static int sockaddr_setindex( SGS_ARGS_SETINDEXFUNC )
{
	char* name;
	if( sgs_ParseString( C, 0, &name, NULL ) )
	{
		if( STREQ( name, "port" ) )
		{
			sgs_Int port;
			if( !sgs_ParseInt( C, 1, &port ) )
				return SGS_EINVAL;
			if( GET_SAF == AF_INET ) GET_SAI->sin_port = htons( (unsigned short) port );
			else if( GET_SAF == AF_INET6 ) GET_SAI6->sin6_port = htons( (unsigned short) port );
			return SGS_SUCCESS;
		}
	}
	return SGS_ENOTFND;
}

static int sockaddr_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	if( type == SGS_VT_STRING )
	{
		sockaddr_push_full_addr_string( C, obj );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int sockaddr_expr( SGS_ARGS_OBJFUNC )
{
	int eop = sgs_ObjectArg( C );
	if( eop == SGS_EOP_COMPARE )
	{
		sgs_Int diff = -1;
		void *data1, *data2;
		if( !sgs_IsObject( C, 0, sockaddr_iface ) ||
			!sgs_IsObject( C, 1, sockaddr_iface ) )
			return SGS_EINVAL;
		data1 = sgs_GetObjectData( C, 0 );
		data2 = sgs_GetObjectData( C, 1 );
		if( ((struct sockaddr_storage*)data1)->ss_family != ((struct sockaddr_storage*)data2)->ss_family )
			diff = ((struct sockaddr_storage*)data1)->ss_family - ((struct sockaddr_storage*)data2)->ss_family;
		else if( ((struct sockaddr_storage*)data1)->ss_family == AF_INET )
		{
			int adiff = 0;
			struct sockaddr_in* sa1 = (struct sockaddr_in*) data1;
			struct sockaddr_in* sa2 = (struct sockaddr_in*) data2;
			adiff = memcmp( &sa1->sin_addr, &sa2->sin_addr, sizeof(sa1->sin_addr) );
			if( !adiff )
				adiff = htons(sa1->sin_port) - htons(sa2->sin_port);
			diff = adiff;
		}
		else if( ((struct sockaddr_storage*)data1)->ss_family == AF_INET6 )
		{
			int adiff = 0;
			struct sockaddr_in6* sa1 = (struct sockaddr_in6*) data1;
			struct sockaddr_in6* sa2 = (struct sockaddr_in6*) data2;
			adiff = memcmp( &sa1->sin6_addr, &sa2->sin6_addr, sizeof(sa1->sin6_addr) );
			if( !adiff )
				adiff = htons(sa1->sin6_port) - htons(sa2->sin6_port);
			diff = adiff;
		}
		sgs_PushInt( C, diff );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int sockaddr_dump( SGS_CTX, sgs_VarObj* obj, int depth )
{
	char buf[ 32 ];
	sprintf( buf, "socket_address [family=%hu] ", (unsigned short) GET_SAF );
	sgs_PushString( C, buf );
	sgs_PushObjectPtr( C, obj );
	sgs_StringConcat( C, 2 );
	return SGS_SUCCESS;
}

static sgs_ObjInterface sockaddr_iface[1] =
{{
	"sockaddr",
	NULL, NULL,
	sockaddr_getindex, sockaddr_setindex,
	sockaddr_convert, NULL, sockaddr_dump, NULL,
	NULL, sockaddr_expr
}};

static void push_sockaddr( SGS_CTX, struct sockaddr_storage* sa, size_t size )
{
	void* ss = sgs_CreateObjectIPA( C, NULL, sizeof(struct sockaddr_storage), sockaddr_iface );
	memset( ss, 0, sizeof(struct sockaddr_storage) );
	memcpy( ss, sa, size );
}


static int sgs_socket_address( SGS_CTX )
{
	struct sockaddr_storage ss;
	char* buf;
	sgs_SizeVal bufsize;
	sgs_Int af;
	uint16_t port = 0;
	
	SGSFN( "socket_address" );
	if( !sgs_LoadArgs( C, "im|+w", &af, &buf, &bufsize, &port ) )
		return 0;
	
	if( af != AF_INET && af != AF_INET6 )
		STDLIB_WARN( "argument 1 (address family)"
			" must be either AF_INET or AF_INET6" )
	
	memset( &ss, 0, sizeof(ss) );
	
	ss.ss_family = (sa_family_t) af;
	port = htons( port );
	{
#ifdef _WIN32
		INT len = sizeof( ss );
		int res = sockassert( C, WSAStringToAddressA( buf, (int16_t) af,
			NULL, (struct sockaddr*) &ss, &len ) == 0 );
#else
		int res = sockassert( C, inet_pton( (int16_t) af, buf, &ss ) == 1 );
#endif
		if( !res )
			STDLIB_WARN( "failed to generate address from string" )
	}
	
	if( af == AF_INET )
	{
		struct sockaddr_in* sai = (struct sockaddr_in*) &ss;
		sai->sin_port = port;
	}
	else if( af == AF_INET6 )
	{
		struct sockaddr_in6* sai = (struct sockaddr_in6*) &ss;
		sai->sin6_port = port;
	}
	else
		STDLIB_WARN( "INTERNAL ERROR (unexpected AF value)" )
	
	push_sockaddr( C, &ss, sizeof(ss) );
	return 1;
}

static int sgs_socket_address_frombytes( SGS_CTX )
{
	struct sockaddr_storage ss = {0};
	char* buf;
	sgs_SizeVal bufsize;
	sgs_Int af;
	uint16_t port = 0;
	
	SGSFN( "socket_address_frombytes" );
	if( !sgs_LoadArgs( C, "im|+w", &af, &buf, &bufsize, &port ) )
		return 0;
	
	if( af != AF_INET && af != AF_INET6 )
		STDLIB_WARN( "argument 1 (address family)"
			" must be either AF_INET or AF_INET6" )
	
	ss.ss_family = (sa_family_t) af;
	port = htons( port );
	if( af == AF_INET )
	{
		struct sockaddr_in* sai = (struct sockaddr_in*) &ss;
		if( bufsize != 4 )
			STDLIB_WARN( "argument 2 (buffer)"
				" must be 4 bytes long for an AF_INET address" )
		sai->sin_port = port;
		memcpy( &sai->sin_addr.s_addr, buf, 4 );
	}
	else if( af == AF_INET6 )
	{
		struct sockaddr_in6* sai = (struct sockaddr_in6*) &ss;
		if( bufsize != 16 )
			STDLIB_WARN( "argument 2 (buffer)"
				" must be 16 bytes long for an AF_INET address" )
		sai->sin6_port = port;
		memcpy( sai->sin6_addr.s6_addr, buf, 16 );
	}
	else
		STDLIB_WARN( "unsupported address family" );
	
	push_sockaddr( C, &ss, sizeof(ss) );
	return 1;
}

static int sgs_socket_getaddrinfo( SGS_CTX )
{
	sgs_StkIdx sz0, sz1;
	struct addrinfo hints, *list = NULL, *pp;
	char *addr, *servport;
	sgs_Int socktype = SOCK_STREAM, af = AF_UNSPEC;
	
	SGSFN( "socket_getaddrinfo" );
	/* address, port, socktype, address_family */
	if( !sgs_LoadArgs( C, "ss|ii", &addr, &servport, &socktype, &af ) )
		return 0;
	
	memset( &hints, 0, sizeof(hints) );
	hints.ai_socktype = (int) socktype;
	hints.ai_family = (int) af;
	
	if( !sockassert( C, getaddrinfo( addr, servport, &hints, &list ) == 0 ) )
		STDLIB_WARN( "failed to get address info" )
	
	pp = list;
	sz0 = sgs_StackSize( C );
	while( pp )
	{
		sz1 = sgs_StackSize( C );
		
		sgs_PushString( C, "flags" );
		sgs_PushInt( C, pp->ai_flags );
		sgs_PushString( C, "family" );
		sgs_PushInt( C, pp->ai_family );
		sgs_PushString( C, "socktype" );
		sgs_PushInt( C, pp->ai_socktype );
		sgs_PushString( C, "protocol" );
		sgs_PushInt( C, pp->ai_protocol );
		sgs_PushString( C, "canonname" );
		if( pp->ai_canonname )
			sgs_PushString( C, pp->ai_canonname );
		else
			sgs_PushNull( C );
		sgs_PushString( C, "addr" );
		push_sockaddr( C, (struct sockaddr_storage*) (void*) pp->ai_addr, pp->ai_addrlen );
		
		sgs_CreateDict( C, NULL, sgs_StackSize( C ) - sz1 );
		pp = pp->ai_next;
	}
	freeaddrinfo( list );
	sgs_CreateArray( C, NULL, sgs_StackSize( C ) - sz0 );
	return 1;
}


static int sgs_socket_gethostname( SGS_CTX )
{
	char buf[ 256 ];
	SGSFN( "socket_gethostname" );
	if( !sgs_LoadArgs( C, "." ) )
		return 0;
	if( !sockassert( C, gethostname( buf, 256 ) == 0 ) )
		STDLIB_WARN( "failed to get host name" )
	buf[ 256-1 ] = 0;
	sgs_PushString( C, buf );
	return 1;
}


/*
	Socket object
*/

SGS_DECLARE sgs_ObjInterface socket_iface[1];
#define SOCK_IHDR( name ) \
	sgs_VarObj* obj; \
	int method_call = sgs_Method( C ); \
	SGSFN( "socket." #name ); \
	if( !sgs_IsObject( C, 0, socket_iface ) ) \
		return sgs_ArgErrorExt( C, 0, method_call, "socket", "" ); \
	obj = sgs_GetObjectStruct( C, 0 );

#define GET_SCK ((SGS_SCKID)(size_t)obj->data)

static int socketI_bind( SGS_CTX )
{
	sgs_Int port;
	struct sockaddr_in sa;
	int ret = 0;
	
	SOCK_IHDR( bind );
	
	if( !sgs_LoadArgs( C, "@>i", &port ) )
		return 0;
	
	memset( &sa, 0, sizeof(sa) );
	sa.sin_family = AF_INET;
	sa.sin_port = htons( (unsigned short) port );
	sa.sin_addr.s_addr = htonl( INADDR_ANY );
	ret = bind( GET_SCK, (struct sockaddr*) &sa, sizeof(sa) );
	
	sgs_PushBool( C, sockassert( C, ret == 0 ) );
	return 1;
}

static int socketI_listen( SGS_CTX )
{
	sgs_Int queuesize;
	
	SOCK_IHDR( listen );
	
	if( !sgs_LoadArgs( C, "@>i", &queuesize ) )
		return 0;
	
	sgs_PushBool( C, sockassert( C, listen( GET_SCK, (int) queuesize ) == 0 ) );
	return 1;
}

static int socketI_accept( SGS_CTX )
{
	SGS_SCKID S;
	struct sockaddr_storage sa = {0};
	SOCKADDR_SIZE sa_size = sizeof( sa );
	
	SOCK_IHDR( accept );
	
	if( !sgs_LoadArgs( C, "@>." ) )
		return 0;
	
	S = accept( GET_SCK, (struct sockaddr*) &sa, &sa_size );
	if( S == SGS_NOSOCK )
	{
		SOCKERR;
		STDLIB_WARN( "failed to accept connection" )
	}
	SOCKCLEARERR;
	sgs_CreateObject( C, NULL, (void*) (size_t) S, socket_iface );
	push_sockaddr( C, &sa, (size_t) sa_size );
	return 2;
}

static int socketI_connect( SGS_CTX )
{
	struct sockaddr* odtdata;
	
	SOCK_IHDR( connect );
	
	if( !sgs_LoadArgs( C, "@>o", sockaddr_iface, &odtdata ) )
		return 0;
	
	sgs_PushBool( C, sockassert( C, connect( GET_SCK,
		odtdata, sizeof(struct sockaddr_storage) ) != -1 ) );
	return 1;
}

static int socketI_send( SGS_CTX )
{
	char* str;
	sgs_SizeVal size;
	sgs_Int flags = 0;
	SOCKDATA_SLEN ret;
	
	SOCK_IHDR( send );
	
	if( !sgs_LoadArgs( C, "@>m|i", &str, &size, &flags ) )
		return 0;
	
	ret = send( GET_SCK, str, (SOCKDATA_LEN) size, (int) flags );
	sockassert( C, ret >= 0 );
	if( ret < 0 )
		sgs_PushBool( C, 0 );
	else
		sgs_PushInt( C, ret );
	return 1;
}

static int socketI_sendto( SGS_CTX )
{
	char* str;
	sgs_SizeVal size;
	sgs_Int flags = 0;
	SOCKDATA_SLEN ret;
	struct sockaddr* odtdata;
	
	SOCK_IHDR( sendto );
	
	if( !sgs_LoadArgs( C, "@>mo|i", &str, &size, sockaddr_iface, &odtdata, &flags ) )
		return 0;
	
	ret = sendto( GET_SCK, str, (SOCKDATA_LEN) size, (int) flags, odtdata, sizeof(struct sockaddr_storage) );
	sockassert( C, ret >= 0 );
	if( ret < 0 )
		sgs_PushBool( C, 0 );
	else
		sgs_PushInt( C, ret );
	return 1;
}

static int socketI_recv( SGS_CTX )
{
	sgs_SizeVal size;
	sgs_Int flags = 0;
	SOCKDATA_SLEN ret;
	char buf[ SOCK_IBUFSZ ], *ptr;
	sgs_MemBuf mb = sgs_membuf_create();
	
	SOCK_IHDR( recv );
	
	if( !sgs_LoadArgs( C, "@>l|i", &size, &flags ) )
		return 0;
	
	if( size > SOCK_IBUFSZ )
	{
		sgs_membuf_resize( &mb, C, (size_t) size );
		ptr = (char*) mb.ptr;
	}
	else
		ptr = buf;
	
	ret = recv( GET_SCK, ptr, (SOCKDATA_LEN) size, (int) flags );
	sockassert( C, ret > 0 );
	if( ret <= 0 )
		sgs_PushBool( C, ret == 0 );
	else
	{
		/* WP: previously provided output limit */
		sgs_PushStringBuf( C, ptr, (sgs_SizeVal) ret );
	}
	sgs_membuf_destroy( &mb, C );
	return 1;
}

static int socketI_recvfrom( SGS_CTX )
{
	struct sockaddr_storage sa = {0};
	SOCKADDR_SIZE sa_size = sizeof( sa );
	
	sgs_SizeVal size;
	sgs_Int flags = 0;
	SOCKDATA_SLEN ret;
	char buf[ SOCK_IBUFSZ ], *ptr;
	sgs_MemBuf mb = sgs_membuf_create();
	
	SOCK_IHDR( recvfrom );
	
	if( !sgs_LoadArgs( C, "@>l|i", &size, &flags ) )
		return 0;
	
	if( size > SOCK_IBUFSZ )
	{
		sgs_membuf_resize( &mb, C, (size_t) size );
		ptr = (char*) mb.ptr;
	}
	else
		ptr = buf;
	
	ret = recvfrom( GET_SCK, ptr, (SOCKDATA_LEN) size, (int) flags,
		(struct sockaddr*) &sa, &sa_size );
	sockassert( C, ret > 0 );
	if( ret < 0 )
	{
		sgs_PushBool( C, 0 );
		return 1;
	}
	else
	{
		/* WP: previously provided output limit */
		sgs_PushStringBuf( C, ptr, (sgs_SizeVal) ret );
		push_sockaddr( C, &sa, (size_t) sa_size );
		return 2;
	}
}

static int socketI_shutdown( SGS_CTX )
{
	sgs_Int flags;
	
	SOCK_IHDR( shutdown );
	
	if( !sgs_LoadArgs( C, "@>i", &flags ) )
		return 0;
	
	sgs_PushBool( C, sockassert( C, shutdown( GET_SCK, (int) flags ) == 0 ) );
	return 1;
}

static int socketI_close( SGS_CTX )
{
	SOCK_IHDR( close );
	
	if( !sgs_LoadArgs( C, "@>." ) )
		return 0;
	
	sgs_PushBool( C, GET_SCK != -1 && closesocket( GET_SCK ) == 0 );
	obj->data = (void*) -1;
	return 1;
}

static int socketI_getpeername( SGS_CTX )
{
	struct sockaddr_storage sa = {0};
	SOCKADDR_SIZE sa_size = sizeof( sa );
	
	SOCK_IHDR( getpeername );
	
	if( !sgs_LoadArgs( C, "@>." ) )
		return 0;
	
	if( getpeername( GET_SCK, (struct sockaddr*) &sa, &sa_size ) != -1 )
	{
		SOCKCLEARERR;
		push_sockaddr( C, &sa, (size_t) sa_size );
		return 1;
	}
	
	SOCKERR;
	STDLIB_WARN( "failed to get peer name" )
}

static int socket_getindex( SGS_ARGS_GETINDEXFUNC )
{
	char* name;
	if( sgs_ParseString( C, 0, &name, NULL ) )
	{
		if( STREQ( name, "bind" ) ) IFN( socketI_bind )
		if( STREQ( name, "listen" ) ) IFN( socketI_listen )
		if( STREQ( name, "accept" ) ) IFN( socketI_accept )
		if( STREQ( name, "connect" ) ) IFN( socketI_connect )
		if( STREQ( name, "send" ) ) IFN( socketI_send )
		if( STREQ( name, "sendto" ) ) IFN( socketI_sendto )
		if( STREQ( name, "recv" ) ) IFN( socketI_recv )
		if( STREQ( name, "recvfrom" ) ) IFN( socketI_recvfrom )
		if( STREQ( name, "shutdown" ) ) IFN( socketI_shutdown )
		if( STREQ( name, "close" ) ) IFN( socketI_close )
		if( STREQ( name, "getpeername" ) ) IFN( socketI_getpeername )
		
		if( STREQ( name, "broadcast" ) )
		{
			int bv;
			GSO_ARG5TYPE bvl = sizeof(int);
			if( !sockassert( C, getsockopt( GET_SCK, SOL_SOCKET, SO_BROADCAST, (char*) &bv, &bvl ) != -1 ) )
			{
				sgs_Msg( C, SGS_WARNING, "failed to retrieve the 'broadcast' property of a socket" );
				sgs_PushNull( C );
			}
			else
				sgs_PushBool( C, bv );
			return SGS_SUCCESS;
		}
		if( STREQ( name, "reuse_addr" ) )
		{
			int bv;
			GSO_ARG5TYPE bvl = sizeof(int);
			if( !sockassert( C, getsockopt( GET_SCK, SOL_SOCKET, SO_REUSEADDR, (char*) &bv, &bvl ) != -1 ) )
			{
				sgs_Msg( C, SGS_WARNING, "failed to retrieve the 'reuse_addr' property of a socket" );
				sgs_PushNull( C );
			}
			else
				sgs_PushBool( C, bv );
			return SGS_SUCCESS;
		}
		if( STREQ( name, "send_timeout" ) )
		{
#ifdef _WIN32
			DWORD tv;
#else
			struct timeval tv;
#endif
			GSO_ARG5TYPE tvl = sizeof(tv);
			if( !sockassert( C, getsockopt( GET_SCK, SOL_SOCKET, SO_SNDTIMEO, (char*) &tv, &tvl ) != -1 ) )
			{
				sgs_Msg( C, SGS_WARNING, "failed to retrieve the 'send_timeout' property of a socket" );
				sgs_PushNull( C );
			}
			else
			{
#ifdef _WIN32
				sgs_PushReal( C, (sgs_Real) 0.001 * (sgs_Real) tv );
#else
				sgs_PushReal( C, (sgs_Real) tv.tv_sec + (sgs_Real) 0.000001 * (sgs_Real) tv.tv_usec );
#endif
			}
			return SGS_SUCCESS;
		}
		if( STREQ( name, "recv_timeout" ) )
		{
#ifdef _WIN32
			DWORD tv;
#else
			struct timeval tv;
#endif
			GSO_ARG5TYPE tvl = sizeof(tv);
			if( !sockassert( C, getsockopt( GET_SCK, SOL_SOCKET, SO_RCVTIMEO, (char*) &tv, &tvl ) != -1 ) )
			{
				sgs_Msg( C, SGS_WARNING, "failed to retrieve the 'recv_timeout' property of a socket" );
				sgs_PushNull( C );
			}
			else
			{
#ifdef _WIN32
				sgs_PushReal( C, (sgs_Real) 0.001 * (sgs_Real) tv );
#else
				sgs_PushReal( C, (sgs_Real) tv.tv_sec + (sgs_Real) 0.000001 * (sgs_Real) tv.tv_usec );
#endif
			}
			return SGS_SUCCESS;
		}
		if( STREQ( name, "error" ) )
		{
			int bv;
			GSO_ARG5TYPE bvl = sizeof(int);
			if( !sockassert( C, getsockopt( GET_SCK, SOL_SOCKET, SO_ERROR, (char*) &bv, &bvl ) != -1 ) )
			{
				sgs_Msg( C, SGS_WARNING, "failed to retrieve the 'error' property of a socket" );
				sgs_PushNull( C );
			}
			else
				sgs_PushBool( C, bv );
			return SGS_SUCCESS;
		}
	}
	return SGS_ENOTFND;
}

static int socket_setindex( SGS_ARGS_SETINDEXFUNC )
{
	char* name;
	if( sgs_ParseString( C, 0, &name, NULL ) )
	{
		if( STREQ( name, "blocking" ) )
		{
			int bv;
			IOCTL_VALUE inbv;
			if( !sgs_ParseBool( C, 1, &bv ) )
				return SGS_EINVAL;
			inbv = !bv;
			if( !sockassert( C, ioctlsocket( GET_SCK, IOCTLCONV( FIONBIO ), &inbv ) != -1 ) )
				sgs_Msg( C, SGS_WARNING, "failed to set the 'blocking' property of a socket" );
			return SGS_SUCCESS;
		}
		if( STREQ( name, "broadcast" ) )
		{
			int bv;
			if( !sgs_ParseBool( C, 1, &bv ) )
				return SGS_EINVAL;
			if( !sockassert( C, setsockopt( GET_SCK, SOL_SOCKET, SO_BROADCAST, (char*) &bv, sizeof(bv) ) != -1 ) )
				sgs_Msg( C, SGS_WARNING, "failed to set the 'broadcast' property of a socket" );
			return SGS_SUCCESS;
		}
		if( STREQ( name, "reuse_addr" ) )
		{
			int bv;
			if( !sgs_ParseBool( C, 1, &bv ) )
				return SGS_EINVAL;
			if( !sockassert( C, setsockopt( GET_SCK, SOL_SOCKET, SO_REUSEADDR, (char*) &bv, sizeof(bv) ) != -1 ) )
				sgs_Msg( C, SGS_WARNING, "failed to set the 'reuse_addr' property of a socket" );
			return SGS_SUCCESS;
		}
		if( STREQ( name, "send_timeout" ) )
		{
			sgs_Real tm = 0.0;
#ifdef _WIN32
			DWORD tv = (DWORD) ( tm * 1000 );
#else
			struct timeval tv;
			tv.tv_sec = (int32_t) floor( tm );
			tv.tv_usec = (int32_t) ( fmod( tm, 1.0f ) * 1000000 );
#endif
			if( !sgs_ParseReal( C, 1, &tm ) )
				return SGS_EINVAL;
			if( !sockassert( C, setsockopt( GET_SCK, SOL_SOCKET, SO_SNDTIMEO, (char*) &tv, sizeof(tv) ) != -1 ) )
				sgs_Msg( C, SGS_WARNING, "failed to set the 'send_timeout' property of a socket" );
			return SGS_SUCCESS;
		}
		if( STREQ( name, "recv_timeout" ) )
		{
			sgs_Real tm = 0.0;
#ifdef _WIN32
			DWORD tv = (DWORD) ( tm * 1000 );
#else
			struct timeval tv;
			tv.tv_sec = (int32_t) floor( tm );
			tv.tv_usec = (int32_t) ( fmod( tm, 1.0f ) * 1000000 );
#endif
			if( !sgs_ParseReal( C, 1, &tm ) )
				return SGS_EINVAL;
			if( !sockassert( C, setsockopt( GET_SCK, SOL_SOCKET, SO_RCVTIMEO, (char*) &tv, sizeof(tv) ) != -1 ) )
				sgs_Msg( C, SGS_WARNING, "failed to set the 'recv_timeout' property of a socket" );
			return SGS_SUCCESS;
		}
	}
	return SGS_ENOTFND;
}

static int socket_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	if( type == SGS_VT_BOOL )
	{
		sgs_PushBool( C, GET_SCK != -1 );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int socket_destruct( SGS_CTX, sgs_VarObj* obj )
{
	if( GET_SCK != -1 )
		closesocket( GET_SCK );
	return SGS_SUCCESS;
}

static sgs_ObjInterface socket_iface[1] =
{{
	"socket",
	socket_destruct, NULL,
	socket_getindex, socket_setindex,
	socket_convert, NULL, NULL, NULL,
	NULL, NULL
}};

static int sgs_socket( SGS_CTX )
{
	SGS_SCKID S;
	sgs_Int addrfamily, type, protocol;
	
	SGSFN( "socket" );
	
	if( !sgs_LoadArgs( C, "iii", &addrfamily, &type, &protocol ) )
		return 0;
	
	S = socket( (int) addrfamily, (int) type, (int) protocol );
	if( S == SGS_NOSOCK )
	{
		SOCKERR;
		STDLIB_WARN( "failed to create socket" )
	}
	SOCKCLEARERR;
	
	sgs_CreateObject( C, NULL, (void*) (size_t) S, socket_iface );
	return 1;
}

static int sgs_socket_tcp( SGS_CTX )
{
	SGS_SCKID S;
	int ipv6 = 0;
	SGSFN( "socket_tcp" );
	
	if( !sgs_LoadArgs( C, "|b", &ipv6 ) )
		return 0;
	
	S = socket( ipv6 ? PF_INET6 : PF_INET, SOCK_STREAM, IPPROTO_TCP );
	if( S == SGS_NOSOCK )
	{
		SOCKERR;
		STDLIB_WARN( "failed to create socket" )
	}
	SOCKCLEARERR;
	
	sgs_CreateObject( C, NULL, (void*) (size_t) S, socket_iface );
	return 1;
}

static int sgs_socket_udp( SGS_CTX )
{
	SGS_SCKID S;
	int ipv6 = 0;
	SGSFN( "socket_udp" );
	
	if( !sgs_LoadArgs( C, "|b", &ipv6 ) )
		return 0;
	
	S = socket( ipv6 ? PF_INET6 : PF_INET, SOCK_DGRAM, IPPROTO_UDP );
	if( S == SGS_NOSOCK )
	{
		SOCKERR;
		STDLIB_WARN( "failed to create socket" )
	}
	SOCKCLEARERR;
	
	sgs_CreateObject( C, NULL, (void*) (size_t) S, socket_iface );
	return 1;
}

static int sgs_socket_select( SGS_CTX )
{
	struct timeval tv;
	sgs_Real timeout = 0;
	sgs_Variable aR, aW, aE;
	sgs_SizeVal szR, szW, szE, i;
	fd_set setR, setW, setE;
	sgs_VarObj* obj;
	SGS_SCKID maxsock = 0;
	int ret;
	
	SGSFN( "socket_select" );
	
	if( !sgs_LoadArgs( C, "aaa|r", &szR, &szW, &szE, &timeout ) )
		return 0;
	
	if( timeout < 0 )
		STDLIB_WARN( "argument 4 (timeout) cannot be negative" )
	
	FD_ZERO( &setR );
	FD_ZERO( &setW );
	FD_ZERO( &setE );
	
	aR = sgs_StackItem( C, 0 );
	aW = sgs_StackItem( C, 1 );
	aE = sgs_StackItem( C, 2 );
	
	for( i = 0; i < szR; ++i )
	{
		sgs_PushNumIndex( C, aR, i );
		if( !sgs_IsObject( C, -1, socket_iface ) )
			return sgs_Msg( C, SGS_WARNING, "item #%d of 'read' array is not a socket", i + 1 );
		obj = sgs_GetObjectStruct( C, -1 );
		if( GET_SCK == -1 )
			return sgs_Msg( C, SGS_WARNING, "item #%d of 'read' array is not an open socket", i + 1 );
		FD_SET( GET_SCK, &setR );
		if( GET_SCK > maxsock )
			maxsock = GET_SCK;
		sgs_Pop( C, 1 );
	}
	
	for( i = 0; i < szW; ++i )
	{
		sgs_PushNumIndex( C, aW, i );
		if( !sgs_IsObject( C, -1, socket_iface ) )
			return sgs_Msg( C, SGS_WARNING, "item #%d of 'write' array is not a socket", i + 1 );
		obj = sgs_GetObjectStruct( C, -1 );
		if( GET_SCK == -1 )
			return sgs_Msg( C, SGS_WARNING, "item #%d of 'write' array is not an open socket", i + 1 );
		FD_SET( GET_SCK, &setW );
		if( GET_SCK > maxsock )
			maxsock = GET_SCK;
		sgs_Pop( C, 1 );
	}
	
	for( i = 0; i < szE; ++i )
	{
		sgs_PushNumIndex( C, aE, i );
		if( !sgs_IsObject( C, -1, socket_iface ) )
			return sgs_Msg( C, SGS_WARNING, "item #%d of 'error' array is not a socket", i + 1 );
		obj = sgs_GetObjectStruct( C, -1 );
		if( GET_SCK == -1 )
			return sgs_Msg( C, SGS_WARNING, "item #%d of 'error' array is not an open socket", i + 1 );
		FD_SET( GET_SCK, &setE );
		if( GET_SCK > maxsock )
			maxsock = GET_SCK;
		sgs_Pop( C, 1 );
	}
	
	tv.tv_sec = (long) floor( timeout );
	tv.tv_usec = (int32_t)( ( timeout - (sgs_Real) tv.tv_sec ) * 1000000 );
	ret = select( (int) maxsock + 1, &setR, &setW, &setE, sgs_StackSize( C ) >= 4 ? &tv : NULL );
	sockassert( C, ret != -1 );
	
	for( i = 0; i < szR; ++i )
	{
		sgs_PushNumIndex( C, aR, i );
		obj = sgs_GetObjectStruct( C, -1 );
		if( !FD_ISSET( GET_SCK, &setR ) )
		{
			sgs_ArrayErase( C, aR, i, 1 );
			i--; szR--;
		}
		sgs_Pop( C, 1 );
	}
	
	for( i = 0; i < szW; ++i )
	{
		sgs_PushNumIndex( C, aW, i );
		obj = sgs_GetObjectStruct( C, -1 );
		if( !FD_ISSET( GET_SCK, &setW ) )
		{
			sgs_ArrayErase( C, aW, i, 1 );
			i--; szW--;
		}
		sgs_Pop( C, 1 );
	}
	
	for( i = 0; i < szE; ++i )
	{
		sgs_PushNumIndex( C, aE, i );
		obj = sgs_GetObjectStruct( C, -1 );
		if( !FD_ISSET( GET_SCK, &setE ) )
		{
			sgs_ArrayErase( C, aE, i, 1 );
			i--; szE--;
		}
		sgs_Pop( C, 1 );
	}
	
	sgs_PushInt( C, ret );
	return 1;
}


static sgs_RegFuncConst f_sock[] =
{
	{ "socket_error", socket_error },
	{ "socket_geterrnobyname", socket_geterrnobyname },
	{ "socket_address", sgs_socket_address },
	{ "socket_address_frombytes", sgs_socket_address_frombytes },
	{ "socket_getaddrinfo", sgs_socket_getaddrinfo },
	{ "socket_gethostname", sgs_socket_gethostname },
	{ "socket", sgs_socket },
	{ "socket_tcp", sgs_socket_tcp },
	{ "socket_udp", sgs_socket_udp },
	{ "socket_select", sgs_socket_select },
};

static sgs_RegIntConst i_sock[] =
{
	DF( PF_INET ), DF( PF_INET6 ), DF( PF_UNIX ), DF( PF_IPX ),
	DF( AF_INET ), DF( AF_INET6 ), DF( AF_UNIX ), DF( AF_IPX ),
	DF( AF_UNSPEC ), DF( AI_PASSIVE ),
	DF( SOCK_STREAM ), DF( SOCK_DGRAM ), DF( SOCK_SEQPACKET ), DF( SOCK_RAW ),
	DF( IPPROTO_TCP ), DF( IPPROTO_UDP ),
	
	DF( MSG_CONFIRM ), DF( MSG_DONTROUTE ), DF( MSG_DONTWAIT ), DF( MSG_EOR ),
	DF( MSG_MORE ), DF( MSG_NOSIGNAL ), DF( MSG_OOB ),
	
	{ "SHUT_RD", 0 }, { "SHUT_WR", 1 }, { "SHUT_RDWR", 2 },
	{ "SD_RECEIVE", 0 }, { "SD_SEND", 1 }, { "SD_BOTH", 2 },
};


#ifdef SGS_COMPILE_MODULE
#  define sockets_module_entry_point sgscript_main
#endif


#ifdef __cplusplus
extern "C"
#endif
#ifdef _WIN32
__declspec(dllexport)
#endif
int sockets_module_entry_point( SGS_CTX )
{
#ifdef _WIN32
	int ret;
	WSADATA wsadata;
#endif
	
	SGS_MODULE_CHECK_VERSION( C );
	
	sgs_SetGlobalByName( C, SCKERRVN, sgs_MakeInt( 0 ) );
	
	sgs_RegisterType( C, "socket", socket_iface );
	sgs_RegisterType( C, "socket_address", sockaddr_iface );
	
	sgs_RegFuncConsts( C, f_sock, SGS_ARRAY_SIZE( f_sock ) );
	sgs_RegIntConsts( C, i_sock, SGS_ARRAY_SIZE( i_sock ) );
	
#ifdef _WIN32
	ret = WSAStartup( MAKEWORD( 2, 0 ), &wsadata );
	if( ret != 0 )
		return SGS_EINPROC;
#endif
	
	return SGS_SUCCESS;
}

/*
	in case someone wants to compile it like a header file...
*/
#undef GET_SCK
#undef GET_SAF
#undef GET_SAI
#undef GET_SAI6
#undef DF
#undef STREQ
#undef IFN
#undef STDLIB_WARN
#undef SOCKADDR_SIZE
#undef SCKERRVN

