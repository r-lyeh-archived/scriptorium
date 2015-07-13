#include "os-curl.h"
#include "../os-binder.h"
#include "curl/curl.h"
#include <curl/easy.h>
#include <curl/multi.h>

#if !defined(LIBCURL_VERSION_NUM) || (LIBCURL_VERSION_NUM < 0x071300)
#  error "Need libcurl version 7.19.0 or greater to compile oscurl."
#endif

#if defined(HAVE_CURL_SSL)
# if defined(HAVE_CURL_OPENSSL)
#  if defined(HAVE_OPENSSL_CRYPTO_H)
#   define OS_CURL_NEED_OPENSSL_TSL
#	pragma message  "libcurl was compiled with OpenSSL support"
#   include <openssl/crypto.h>
#  else
#	 pragma message \
	"libcurl was compiled with OpenSSL support, but configure could not find " \
	"openssl/crypto.h; thus no SSL crypto locking callbacks will be set, which may " \
	"cause random crashes on SSL requests"
#  endif
# elif defined(HAVE_CURL_GNUTLS)
#  if defined(HAVE_GCRYPT_H)
#   define OS_CURL_NEED_GNUTLS_TSL
#	pragma message "libcurl was compiled with GnuTLS support"
#   include <gcrypt.h>
#  else
#	 pragma message \
	"libcurl was compiled with GnuTLS support, but configure could not find " \
	"gcrypt.h; thus no SSL crypto locking callbacks will be set, which may " \
	"cause random crashes on SSL requests"
#  endif
# else
#	 pragma message \
	"libcurl was compiled with SSL support, but configure could not determine which" \
	"library was used; thus no SSL crypto locking callbacks will be set, which may " \
	"cause random crashes on SSL requests"
# endif /* HAVE_CURL_OPENSSL || HAVE_CURL_GNUTLS */
#else
# pragma message("libcurl was compiled without SSL support")
#endif /* HAVE_CURL_SSL */

// 7.21.5
#if LIBCURL_VERSION_NUM < 0x071505
#define CURLE_UNKNOWN_OPTION CURLE_UNKNOWN_TELNET_OPTION
#endif

/* Calculate the number of OBJECTPOINT options we need to store */
#define OPTIONS_SIZE    ((int)CURLOPT_LASTENTRY % 10000)
// #define MOPTIONS_SIZE   ((int)CURLMOPT_LASTENTRY % 10000)

namespace ObjectScript {

	enum ECurlOptionType
	{
		OPT_LONG,
		OPT_BOOL,
		OPT_STRING,
		OPT_LARGE,
		OPT_ARRAY,
		OPT_FILE,
		OPT_FUNC
	};

	struct CurlOptionDesc
	{
		int option;
		const OS_CHAR * name;
		ECurlOptionType type;
		const OS_CHAR * name2;
	};

	static const CurlOptionDesc curl_option_desc[] = {
		{CURLOPT_FILE, OS_TEXT("file"), OPT_FILE, OS_TEXT("writedata")},
		{CURLOPT_URL, OS_TEXT("url"), OPT_STRING},
		{CURLOPT_PORT, OS_TEXT("port")},
		{CURLOPT_PROXY, OS_TEXT("proxy"), OPT_STRING},
		{CURLOPT_USERPWD, OS_TEXT("userpwd")},
		{CURLOPT_PROXYUSERPWD, OS_TEXT("proxyuserpwd"), OPT_STRING},
		{CURLOPT_RANGE, OS_TEXT("range"), OPT_STRING},
		{CURLOPT_READDATA, OS_TEXT("readdata")},
		{CURLOPT_WRITEFUNCTION, OS_TEXT("writefunction"), OPT_FUNC},
		{CURLOPT_READFUNCTION, OS_TEXT("readfunction"), OPT_FUNC},
		{CURLOPT_TIMEOUT, OS_TEXT("timeout")},
		{CURLOPT_INFILESIZE, OS_TEXT("infilesize")},
		{CURLOPT_POSTFIELDS, OS_TEXT("postfields"), OPT_STRING},
		{CURLOPT_REFERER, OS_TEXT("referer"), OPT_STRING},
		{CURLOPT_FTPPORT, OS_TEXT("ftpport"), OPT_STRING},
		{CURLOPT_USERAGENT, OS_TEXT("useragent"), OPT_STRING},
		{CURLOPT_LOW_SPEED_LIMIT, OS_TEXT("low_speed_limit")},
		{CURLOPT_LOW_SPEED_TIME, OS_TEXT("low_speed_time")},
		{CURLOPT_RESUME_FROM, OS_TEXT("resume_from")},
		{CURLOPT_COOKIE, OS_TEXT("cookie"), OPT_STRING},
		{CURLOPT_HTTPHEADER, OS_TEXT("httpheader"), OPT_ARRAY},
		//{CURLOPT_HTTPPOST, OS_TEXT("httppost"), OPT_ARRAY}, // TODO: enable it?
		{CURLOPT_SSLCERT, OS_TEXT("sslcert"), OPT_STRING},
		{CURLOPT_KEYPASSWD, OS_TEXT("keypasswd"), OPT_STRING},
		{CURLOPT_CRLF, OS_TEXT("crlf")},
		{CURLOPT_QUOTE, OS_TEXT("quote"), OPT_ARRAY},
		{CURLOPT_WRITEHEADER, OS_TEXT("writeheader"), OPT_FILE, OS_TEXT("header_file")},
		{CURLOPT_COOKIEFILE, OS_TEXT("cookiefile"), OPT_STRING},
		{CURLOPT_SSLVERSION, OS_TEXT("sslversion")},
		{CURLOPT_TIMECONDITION, OS_TEXT("timecondition")},
		{CURLOPT_TIMEVALUE, OS_TEXT("timevalue")},
		{CURLOPT_CUSTOMREQUEST, OS_TEXT("customrequest"), OPT_STRING},
		{CURLOPT_STDERR, OS_TEXT("stderr")},
		{CURLOPT_POSTQUOTE, OS_TEXT("postquote"), OPT_ARRAY},
		{CURLOPT_VERBOSE, OS_TEXT("verbose"), OPT_BOOL},
		{CURLOPT_HEADER, OS_TEXT("header"), OPT_BOOL},
		{CURLOPT_NOPROGRESS, OS_TEXT("noprogress"), OPT_BOOL},
		{CURLOPT_NOBODY, OS_TEXT("nobody"), OPT_BOOL},
		{CURLOPT_FAILONERROR, OS_TEXT("failonerror"), OPT_BOOL},
		{CURLOPT_UPLOAD, OS_TEXT("upload"), OPT_BOOL},
		{CURLOPT_POST, OS_TEXT("post"), OPT_BOOL},
		{CURLOPT_DIRLISTONLY, OS_TEXT("dirlistonly"), OPT_BOOL},
		{CURLOPT_APPEND, OS_TEXT("append"), OPT_BOOL},
		{CURLOPT_NETRC, OS_TEXT("netrc")},
		{CURLOPT_FOLLOWLOCATION, OS_TEXT("followlocation"), OPT_BOOL},
		{CURLOPT_TRANSFERTEXT, OS_TEXT("transfertext"), OPT_BOOL},
		{CURLOPT_PROGRESSFUNCTION, OS_TEXT("progressfunction"), OPT_FUNC},
		//{CURLOPT_PROGRESSDATA, OS_TEXT("progressdata")},
		{CURLOPT_AUTOREFERER, OS_TEXT("autoreferer"), OPT_BOOL},
		{CURLOPT_PROXYPORT, OS_TEXT("proxyport")},
		{CURLOPT_POSTFIELDSIZE, OS_TEXT("postfieldsize")},
		{CURLOPT_HTTPPROXYTUNNEL, OS_TEXT("httpproxytunnel"), OPT_BOOL},
		{CURLOPT_INTERFACE, OS_TEXT("interface"), OPT_STRING},
		{CURLOPT_KRBLEVEL, OS_TEXT("krblevel"), OPT_STRING},
		{CURLOPT_SSL_VERIFYPEER, OS_TEXT("ssl_verifypeer"), OPT_BOOL},
		{CURLOPT_CAINFO, OS_TEXT("cainfo"), OPT_STRING},
		{CURLOPT_MAXREDIRS, OS_TEXT("maxredirs")},
		{CURLOPT_FILETIME, OS_TEXT("filetime"), OPT_BOOL}, // TODO: bool - correct?
		{CURLOPT_TELNETOPTIONS, OS_TEXT("telnetoptions"), OPT_ARRAY},
		{CURLOPT_MAXCONNECTS, OS_TEXT("maxconnects")},
		{CURLOPT_CLOSEPOLICY, OS_TEXT("closepolicy")},
		{CURLOPT_FRESH_CONNECT, OS_TEXT("fresh_connect"), OPT_BOOL},
		{CURLOPT_FORBID_REUSE, OS_TEXT("forbid_reuse"), OPT_BOOL},
		{CURLOPT_RANDOM_FILE, OS_TEXT("random_file"), OPT_STRING},
		{CURLOPT_EGDSOCKET, OS_TEXT("egdsocket"), OPT_STRING},
		{CURLOPT_CONNECTTIMEOUT, OS_TEXT("connecttimeout")},
		{CURLOPT_HEADERFUNCTION, OS_TEXT("headerfunction"), OPT_FUNC},
		{CURLOPT_HTTPGET, OS_TEXT("httpget"), OPT_BOOL},
		{CURLOPT_SSL_VERIFYHOST, OS_TEXT("ssl_verifyhost")},
		{CURLOPT_COOKIEJAR, OS_TEXT("cookiejar"), OPT_STRING},
		{CURLOPT_SSL_CIPHER_LIST, OS_TEXT("ssl_cipher_list"), OPT_STRING},
		{CURLOPT_HTTP_VERSION, OS_TEXT("http_version")},
		{CURLOPT_FTP_USE_EPSV, OS_TEXT("ftp_use_epsv"), OPT_BOOL},
		{CURLOPT_SSLCERTTYPE, OS_TEXT("sslcerttype"), OPT_STRING},
		{CURLOPT_SSLKEY, OS_TEXT("sslkey"), OPT_STRING},
		{CURLOPT_SSLKEYTYPE, OS_TEXT("sslkeytype"), OPT_STRING},
		{CURLOPT_SSLENGINE, OS_TEXT("sslengine"), OPT_STRING},
		{CURLOPT_SSLENGINE_DEFAULT, OS_TEXT("sslengine_default"), OPT_STRING},
		{CURLOPT_DNS_USE_GLOBAL_CACHE, OS_TEXT("dns_use_global_cache"), OPT_BOOL},
		{CURLOPT_DNS_CACHE_TIMEOUT, OS_TEXT("dns_cache_timeout")},
		{CURLOPT_PREQUOTE, OS_TEXT("prequote"), OPT_ARRAY},
		{CURLOPT_DEBUGFUNCTION, OS_TEXT("debugfunction"), OPT_FUNC},
		//{CURLOPT_DEBUGDATA, OS_TEXT("debugdata")},
		{CURLOPT_COOKIESESSION, OS_TEXT("cookiesession"), OPT_BOOL},
		{CURLOPT_CAPATH, OS_TEXT("capath"), OPT_STRING},
		{CURLOPT_BUFFERSIZE, OS_TEXT("buffersize")},
		{CURLOPT_NOSIGNAL, OS_TEXT("nosignal"), OPT_BOOL},
		{CURLOPT_SHARE, OS_TEXT("share")},
		{CURLOPT_PROXYTYPE, OS_TEXT("proxytype")},
#if (LIBCURL_VERSION_NUM < 0x072106)
		{CURLOPT_ENCODING, OS_TEXT("accept_encoding"), OPT_STRING},
#else
		{CURLOPT_ACCEPT_ENCODING, OS_TEXT("accept_encoding"), OPT_STRING},
#endif
		//{CURLOPT_PRIVATE, OS_TEXT("private")},
		{CURLOPT_HTTP200ALIASES, OS_TEXT("http200aliases"), OPT_ARRAY},
		{CURLOPT_UNRESTRICTED_AUTH, OS_TEXT("unrestricted_auth"), OPT_BOOL},
		{CURLOPT_FTP_USE_EPRT, OS_TEXT("ftp_use_eprt"), OPT_BOOL},
		{CURLOPT_HTTPAUTH, OS_TEXT("httpauth")},
		{CURLOPT_SSL_CTX_FUNCTION, OS_TEXT("ssl_ctx_function"), OPT_FUNC},
		//{CURLOPT_SSL_CTX_DATA, OS_TEXT("ssl_ctx_data")},
		{CURLOPT_PROXYAUTH, OS_TEXT("proxyauth")},
		{CURLOPT_FTP_RESPONSE_TIMEOUT, OS_TEXT("ftp_response_timeout")},
		{CURLOPT_IPRESOLVE, OS_TEXT("ipresolve")},
		{CURLOPT_MAXFILESIZE, OS_TEXT("maxfilesize")},
		{CURLOPT_INFILESIZE_LARGE, OS_TEXT("infilesize_large"), OPT_LARGE},
		{CURLOPT_RESUME_FROM_LARGE, OS_TEXT("resume_from_large"), OPT_LARGE},
		{CURLOPT_MAXFILESIZE_LARGE, OS_TEXT("maxfilesize_large"), OPT_LARGE},
		{CURLOPT_NETRC_FILE, OS_TEXT("netrc_file"), OPT_STRING},
		{CURLOPT_USE_SSL, OS_TEXT("use_ssl")},
		{CURLOPT_POSTFIELDSIZE_LARGE, OS_TEXT("postfieldsize_large"), OPT_LARGE},
		{CURLOPT_TCP_NODELAY, OS_TEXT("tcp_nodelay"), OPT_BOOL},
		{CURLOPT_FTPSSLAUTH, OS_TEXT("ftpsslauth")},
		{CURLOPT_IOCTLFUNCTION, OS_TEXT("ioctlfunction"), OPT_FUNC},
		//{CURLOPT_IOCTLDATA, OS_TEXT("ioctldata")},
		{CURLOPT_FTP_ACCOUNT, OS_TEXT("ftp_account"), OPT_STRING},
		{CURLOPT_COOKIELIST, OS_TEXT("cookielist"), OPT_STRING},
		{CURLOPT_IGNORE_CONTENT_LENGTH, OS_TEXT("ignore_content_length"), OPT_BOOL},
		{CURLOPT_FTP_SKIP_PASV_IP, OS_TEXT("ftp_skip_pasv_ip"), OPT_BOOL},
		{CURLOPT_FTP_FILEMETHOD, OS_TEXT("ftp_filemethod")},
		{CURLOPT_LOCALPORT, OS_TEXT("localport")},
		{CURLOPT_LOCALPORTRANGE, OS_TEXT("localportrange")},
		{CURLOPT_CONNECT_ONLY, OS_TEXT("connect_only"), OPT_BOOL},
		{CURLOPT_CONV_FROM_NETWORK_FUNCTION, OS_TEXT("conv_from_network_function"), OPT_FUNC},
		{CURLOPT_CONV_TO_NETWORK_FUNCTION, OS_TEXT("conv_to_network_function"), OPT_FUNC},
		{CURLOPT_CONV_FROM_UTF8_FUNCTION, OS_TEXT("conv_from_utf8_function"), OPT_FUNC},
		{CURLOPT_MAX_SEND_SPEED_LARGE, OS_TEXT("max_send_speed_large"), OPT_LARGE},
		{CURLOPT_MAX_RECV_SPEED_LARGE, OS_TEXT("max_recv_speed_large"), OPT_LARGE},
		{CURLOPT_FTP_ALTERNATIVE_TO_USER, OS_TEXT("ftp_alternative_to_user"), OPT_STRING},
		{CURLOPT_SOCKOPTFUNCTION, OS_TEXT("sockoptfunction"), OPT_FUNC},
		//{CURLOPT_SOCKOPTDATA, OS_TEXT("sockoptdata")},
		{CURLOPT_SSL_SESSIONID_CACHE, OS_TEXT("ssl_sessionid_cache"), OPT_BOOL},
		{CURLOPT_SSH_AUTH_TYPES, OS_TEXT("ssh_auth_types")},
		{CURLOPT_SSH_PUBLIC_KEYFILE, OS_TEXT("ssh_public_keyfile"), OPT_STRING},
		{CURLOPT_SSH_PRIVATE_KEYFILE, OS_TEXT("ssh_private_keyfile"), OPT_STRING},
		{CURLOPT_FTP_SSL_CCC, OS_TEXT("ftp_ssl_ccc")},
		{CURLOPT_TIMEOUT_MS, OS_TEXT("timeout_ms")},
		{CURLOPT_CONNECTTIMEOUT_MS, OS_TEXT("connecttimeout_ms")},
		{CURLOPT_HTTP_TRANSFER_DECODING, OS_TEXT("http_transfer_decoding"), OPT_BOOL},
		{CURLOPT_HTTP_CONTENT_DECODING, OS_TEXT("http_content_decoding"), OPT_BOOL},
		{CURLOPT_NEW_FILE_PERMS, OS_TEXT("new_file_perms")},
		{CURLOPT_NEW_DIRECTORY_PERMS, OS_TEXT("new_directory_perms")},
		{CURLOPT_POSTREDIR, OS_TEXT("postredir")},
		{CURLOPT_SSH_HOST_PUBLIC_KEY_MD5, OS_TEXT("ssh_host_public_key_md5"), OPT_STRING},
		{CURLOPT_OPENSOCKETFUNCTION, OS_TEXT("opensocketfunction"), OPT_FUNC},
		//{CURLOPT_OPENSOCKETDATA, OS_TEXT("opensocketdata")},
		{CURLOPT_COPYPOSTFIELDS, OS_TEXT("copypostfields"), OPT_STRING},
		{CURLOPT_PROXY_TRANSFER_MODE, OS_TEXT("proxy_transfer_mode"), OPT_BOOL},
		{CURLOPT_SEEKFUNCTION, OS_TEXT("seekfunction"), OPT_FUNC},
		//{CURLOPT_SEEKDATA, OS_TEXT("seekdata")},
		{CURLOPT_CRLFILE, OS_TEXT("crlfile"), OPT_STRING},
		{CURLOPT_ISSUERCERT, OS_TEXT("issuercert"), OPT_STRING},
		{CURLOPT_ADDRESS_SCOPE, OS_TEXT("address_scope")},
#if (LIBCURL_VERSION_NUM >= 0x071301)
		{CURLOPT_CERTINFO, OS_TEXT("certinfo"), OPT_BOOL},
		{CURLOPT_USERNAME, OS_TEXT("username"), OPT_STRING},
		{CURLOPT_PASSWORD, OS_TEXT("password"), OPT_STRING},
		{CURLOPT_PROXYUSERNAME, OS_TEXT("proxyusername"), OPT_STRING},
		{CURLOPT_PROXYPASSWORD, OS_TEXT("proxypassword"), OPT_STRING},
#endif
#if (LIBCURL_VERSION_NUM >= 0x071304)
		{CURLOPT_TFTP_BLKSIZE, OS_TEXT("tftp_blksize")},
		{CURLOPT_NOPROXY, OS_TEXT("noproxy"), OPT_STRING},
		{CURLOPT_PROTOCOLS, OS_TEXT("protocols")},
		{CURLOPT_REDIR_PROTOCOLS, OS_TEXT("redir_protocols")},
		{CURLOPT_SOCKS5_GSSAPI_SERVICE, OS_TEXT("socks5_gssapi_service"), OPT_STRING},
		{CURLOPT_SOCKS5_GSSAPI_NEC, OS_TEXT("socks5_gssapi_nec"), OPT_BOOL},
#endif
#if (LIBCURL_VERSION_NUM >= 0x071306)
		{CURLOPT_SSH_KNOWNHOSTS, OS_TEXT("ssh_knownhosts"), OPT_STRING},
		{CURLOPT_SSH_KEYFUNCTION, OS_TEXT("ssh_keyfunction"), OPT_FUNC},
		//{CURLOPT_SSH_KEYDATA, OS_TEXT("ssh_keydata")},
#endif
#if (LIBCURL_VERSION_NUM >= 0x071400)
		{CURLOPT_MAIL_FROM, OS_TEXT("mail_from"), OPT_STRING},
		{CURLOPT_MAIL_RCPT, OS_TEXT("mail_rcpt"), OPT_ARRAY},
		{CURLOPT_FTP_USE_PRET, OS_TEXT("ftp_use_pret"), OPT_BOOL},
		{CURLOPT_FTP_CREATE_MISSING_DIRS, OS_TEXT("ftp_create_missing_dirs"), OPT_BOOL},
		{CURLOPT_RTSP_REQUEST, OS_TEXT("rtsp_request")},
		{CURLOPT_RTSP_SESSION_ID, OS_TEXT("rtsp_session_id"), OPT_STRING},
		{CURLOPT_RTSP_STREAM_URI, OS_TEXT("rtsp_stream_uri"), OPT_STRING},
		{CURLOPT_RTSP_TRANSPORT, OS_TEXT("rtsp_transport"), OPT_STRING},
		{CURLOPT_RTSP_CLIENT_CSEQ, OS_TEXT("rtsp_client_cseq")},
		{CURLOPT_RTSP_SERVER_CSEQ, OS_TEXT("rtsp_server_cseq")},
		{CURLOPT_INTERLEAVEFUNCTION, OS_TEXT("interleavefunction"), OPT_FUNC},
		//{CURLOPT_INTERLEAVEDATA, OS_TEXT("interleavedata")},
#endif
#if (LIBCURL_VERSION_NUM >= 0x071500)
		{CURLOPT_WILDCARDMATCH, OS_TEXT("wildcardmatch"), OPT_BOOL},
		{CURLOPT_CHUNK_BGN_FUNCTION, OS_TEXT("chunk_bgn_function"), OPT_FUNC},
		{CURLOPT_CHUNK_END_FUNCTION, OS_TEXT("chunk_end_function"), OPT_FUNC},
		{CURLOPT_FNMATCH_FUNCTION, OS_TEXT("fnmatch_function"), OPT_FUNC},
		//{CURLOPT_CHUNK_DATA, OS_TEXT("chunk_data")},
		//{CURLOPT_FNMATCH_DATA, OS_TEXT("fnmatch_data")},
#endif
#if (LIBCURL_VERSION_NUM >= 0x071503)
		{CURLOPT_RESOLVE, OS_TEXT("resolve"), OPT_ARRAY},
#endif
#if (LIBCURL_VERSION_NUM >= 0x071504)
		{CURLOPT_TLSAUTH_USERNAME, OS_TEXT("tlsauth_username"), OPT_STRING},
		{CURLOPT_TLSAUTH_PASSWORD, OS_TEXT("tlsauth_password"), OPT_STRING},
		{CURLOPT_TLSAUTH_TYPE, OS_TEXT("tlsauth_type")},
#endif
#if (LIBCURL_VERSION_NUM >= 0x071506)
		{CURLOPT_TRANSFER_ENCODING, OS_TEXT("transfer_encoding"), OPT_STRING},
#endif
#if (LIBCURL_VERSION_NUM >= 0x071507)
		{CURLOPT_CLOSESOCKETFUNCTION, OS_TEXT("closesocketfunction"), OPT_FUNC},
		//{CURLOPT_CLOSESOCKETDATA, OS_TEXT("closesocketdata")},
#endif
#if (LIBCURL_VERSION_NUM >= 0x071600)
		{CURLOPT_GSSAPI_DELEGATION, OS_TEXT("gssapi_delegation")},
#endif
#if (LIBCURL_VERSION_NUM >= 0x071900)
		{CURLOPT_DNS_SERVERS, OS_TEXT("dns_servers"), OPT_ARRAY},
		{CURLOPT_ACCEPTTIMEOUT_MS, OS_TEXT("accepttimeout_ms")},
		{CURLOPT_TCP_KEEPALIVE, OS_TEXT("tcp_keepalive"), OPT_BOOL},
		{CURLOPT_TCP_KEEPIDLE, OS_TEXT("tcp_keepidle")},
		{CURLOPT_TCP_KEEPINTVL, OS_TEXT("tcp_keepintvl")},
		{CURLOPT_MAIL_AUTH, OS_TEXT("mail_auth"), OPT_STRING},
		{CURLOPT_SSL_OPTIONS, OS_TEXT("ssl_options"), OPT_BOOL},
#endif
	};
	static const int curl_option_count = sizeof(curl_option_desc)/sizeof(curl_option_desc[0]);

	class CurlOS: public OS // get access to protected members
	{
	public:

		class Curl
		{
		public:

			Curl(OS *p_os);
			virtual ~Curl();

			static void initExtension(OS* os);

			bool init();
			void close();
			void reset();

			CURLcode perform();

			bool setOption(CURLoption option, curl_slist * slist);
			bool setOption();

			static size_t writeCallback(char *ptr, size_t size, size_t nmemb, void *userdata);
			static size_t headerCallback(char *ptr, size_t size, size_t nmemb, void *userdata);
			static size_t readCallback(void *ptr, size_t size, size_t nmemb, void *userdata);
			static int progressCallback(void *userdata, double dltotal, double dlnow, double ultotal, double ulnow);
			static int debugCallback(CURL *, curl_infotype type, char *buf, size_t buf_len, void *userdata);
			static curlioerr ioctlCallback(CURL *, int cmd, void *userdata);

			enum EBehavior
			{
				BEHAVIOR_RETURN,
				BEHAVIOR_FILE,
				BEHAVIOR_FUNC,
				BEHAVIOR_STDOUT,
				BEHAVIOR_STDERR,
				BEHAVIOR_IGNORE
			};

			struct CallbackData
			{
				EBehavior behavior;
				int file_id;
				int func_id;
				Core::Buffer * buf;

				CallbackData()
				{
					file_id = 0;
					func_id = 0;
					buf = NULL;
				}

				~CallbackData()
				{
					OS_ASSERT(!file_id && !func_id && !buf);
				}
			};

			enum InfoType
			{
				LONG,
				DOUBLE,
				CHAR_PTR,
				SLIST_PTR,
#if LIBCURL_VERSION_NUM >= 0x071301
				CERTINFO_PTR
#endif
			};

			struct InfoData
			{
				CURLcode result;
				InfoType type;

				long lvalue;
				double dvalue;
				char *cvalue;
				curl_slist *slvalue;
#if LIBCURL_VERSION_NUM >= 0x071301
				curl_certinfo *civalue;
#endif
			};

			InfoData getInfo(CURLINFO info);

			const OS_CHAR * getErrorStr()
			{
				error[sizeof(error)/sizeof(error[0])-1] = 0;
				return error[0] ? error : OS_TEXT("unexpected error");
			}

			void triggerError(int code)
			{
				os->getGlobal(OS_TEXT("CurlException"));
				os->pushGlobals();
				if(code != CURLE_OK){
					os->pushString(getErrorStr());
					os->pushNumber(code);
					os->callFT(2, 1);
				}else{
					os->pushString(OS_TEXT("unexpected error"));
					os->callFT(1, 1);
				}
				os->setException();
				// close();
			}

			void triggerError(const OS_CHAR * message = OS_TEXT("unexpected error"))
			{
				os->getGlobal(OS_TEXT("CurlException"));
				os->pushGlobals();
				os->pushString(message);
				os->callFT(1, 1);
				os->setException();
				// close();
			}

		private:

			OS *os;
			CURL *handle;

			curl_httppost *httppost;
			curl_slist *httpheader;
			curl_slist *http200aliases;
			curl_slist *quote;
			curl_slist *postquote;
			curl_slist *prequote;
			curl_slist *telnetopt;
#if (LIBCURL_VERSION_NUM >= 0x071400)
			curl_slist *mailrcpt;
#endif
#if (LIBCURL_VERSION_NUM >= 0x071503)
			curl_slist *resolve;
#endif
#if (LIBCURL_VERSION_NUM >= 0x071900)
			curl_slist *dnsserver;
#endif

			char error[CURL_ERROR_SIZE];

			CallbackData write;
			CallbackData write_header;
			CallbackData read;
			CallbackData progress;
			CallbackData debug;
			CallbackData ioctl;

			// bool share;

			void resetData();

			// Curl(const Curl &);
			void copyFrom(Curl * other);
		};

		/*
		class CurlShare
		{
			friend class OS;

		public:
			CurlShare(OS *p_os);
			virtual ~CurlShare();

			static void initCurlShareExtension(OS* os);

			static void lockCallback(CURL *handle, curl_lock_data data, curl_lock_access access, void *userptr);
			static void unlockCallback(CURL *handle, curl_lock_data data, void *userptr);

			bool init();
			void close();

			CURLSH * handle() const
			{ return share_handle; }

		private:
			OS *os;
			CURLSH *share_handle;

			CurlShare(const CurlShare &);
			CurlShare & operator = (const CurlShare & other);
		};
		*/

	}; // CurlOS

	template <> struct CtypeName<CurlOS::Curl>{ static const OS_CHAR * getName(){ return OS_TEXT("Curl"); } };
	template <> struct CtypeValue<CurlOS::Curl*>: public CtypeUserClass<CurlOS::Curl*>{};

	// template <> struct CtypeName<CurlOS::CurlShare>{ static const OS_CHAR * getName(){ return OS_TEXT("CurlShare"); } };
	// template <> struct CtypeValue<CurlOS::CurlShare*>: public CtypeUserClass<CurlOS::CurlShare*>{};

	OS_DECL_CTYPE(CURLoption);

	template <>
	struct CtypeValue<CURLoption>
	{
		typedef CURLoption type;

		static bool isValid(type option)
		{
			if (option <= 0)
				return false;
			if (option >= (int)CURLOPTTYPE_OFF_T + OPTIONS_SIZE)
				return false;
			if (option % 10000 >= OPTIONS_SIZE)
				return false;

			return true;
		}

		static type def(ObjectScript::OS * os) { return (type)0; }
		static type getArg(ObjectScript::OS * os, int offs)
		{
			ObjectScript::OS::String str = os->toString(offs);
			for(int i = 0; i < curl_option_count; i++){
				if(str == curl_option_desc[i].name){
					return (type)curl_option_desc[i].option;
				}
				if(curl_option_desc[i].name2 && str == curl_option_desc[i].name){
					return (type)curl_option_desc[i].option;
				}
			}
			return (type)0;
		}

		static void push(ObjectScript::OS * os, type val)
		{
			for(int i = 0; i < curl_option_count; i++){
				if(val == curl_option_desc[i].option){
					return os->pushString(curl_option_desc[i].name);
				}
			}
			os->pushString("unknown");
		}
	};

	OS_DECL_CTYPE(CURLINFO);

	template <>
	struct CtypeValue<CURLINFO>
	{
		typedef CURLINFO type;

		static bool isValid(type info)
		{
			if (info <= 0)
				return false;
			if (info >= (int)CURLINFO_SLIST + CURLINFO_LASTONE)
				return false;
			if (info % CURLINFO_MASK >= CURLINFO_LASTONE)
				return false;

			return true;
		}

		static type def(ObjectScript::OS * os) { return CURLINFO_NONE; }
		static type getArg(ObjectScript::OS * os, int offs)
		{
			ObjectScript::OS::String str = os->toString(offs);
			if(str == "effective_url") return CURLINFO_EFFECTIVE_URL;
			if(str == "total_time") return CURLINFO_TOTAL_TIME;
			if(str == "namelookup_time") return CURLINFO_NAMELOOKUP_TIME;
			if(str == "connect_time") return CURLINFO_CONNECT_TIME;
			if(str == "pretransfer_time") return CURLINFO_PRETRANSFER_TIME;
			if(str == "size_upload") return CURLINFO_SIZE_UPLOAD;
			if(str == "size_download") return CURLINFO_SIZE_DOWNLOAD;
			if(str == "speed_download") return CURLINFO_SPEED_DOWNLOAD;
			if(str == "speed_upload") return CURLINFO_SPEED_UPLOAD;
			if(str == "header_size") return CURLINFO_HEADER_SIZE;
			if(str == "request_size") return CURLINFO_REQUEST_SIZE;
			if(str == "ssl_verifyresult") return CURLINFO_SSL_VERIFYRESULT;
			if(str == "filetime") return CURLINFO_FILETIME;
			if(str == "content_length_download") return CURLINFO_CONTENT_LENGTH_DOWNLOAD;
			if(str == "content_length_upload") return CURLINFO_CONTENT_LENGTH_UPLOAD;
			if(str == "starttransfer_time") return CURLINFO_STARTTRANSFER_TIME;
			if(str == "content_type") return CURLINFO_CONTENT_TYPE;
			if(str == "redirect_time") return CURLINFO_REDIRECT_TIME;
			if(str == "redirect_count") return CURLINFO_REDIRECT_COUNT;
			//if(str == "private") return CURLINFO_PRIVATE;
			if(str == "http_connectcode") return CURLINFO_HTTP_CONNECTCODE;
			if(str == "httpauth_avail") return CURLINFO_HTTPAUTH_AVAIL;
			if(str == "proxyauth_avail") return CURLINFO_PROXYAUTH_AVAIL;
			if(str == "os_errno") return CURLINFO_OS_ERRNO;
			if(str == "num_connects") return CURLINFO_NUM_CONNECTS;
			if(str == "ssl_engines") return CURLINFO_SSL_ENGINES;
			if(str == "cookielist") return CURLINFO_COOKIELIST;
			if(str == "lastsocket") return CURLINFO_LASTSOCKET;
			if(str == "ftp_entry_path") return CURLINFO_FTP_ENTRY_PATH;
			if(str == "redirect_url") return CURLINFO_REDIRECT_URL;
			if(str == "primary_ip") return CURLINFO_PRIMARY_IP;
			if(str == "appconnect_time") return CURLINFO_APPCONNECT_TIME;
#if LIBCURL_VERSION_NUM >= 0x071400
			if(str == "rtsp_session_id") return CURLINFO_RTSP_SESSION_ID;
			if(str == "rtsp_client_cseq") return CURLINFO_RTSP_CLIENT_CSEQ;
			if(str == "rtsp_server_cseq") return CURLINFO_RTSP_SERVER_CSEQ;
			if(str == "rtsp_cseq_recv") return CURLINFO_RTSP_CSEQ_RECV;
#endif
#if LIBCURL_VERSION_NUM >= 0x071301
			if(str == "certinfo") return CURLINFO_CERTINFO;
#endif
#if LIBCURL_VERSION_NUM >= 0x071304
			if(str == "condition_unmet") return CURLINFO_CONDITION_UNMET;
#endif
#if LIBCURL_VERSION_NUM >= 0x071500
			if(str == "primary_port") return CURLINFO_PRIMARY_PORT;
			if(str == "local_ip") return CURLINFO_LOCAL_IP;
			if(str == "local_port") return CURLINFO_LOCAL_PORT;
#endif
// #if LIBCURL_VERSION_NUM >= 0x071900
			if(str == "response_code") return CURLINFO_RESPONSE_CODE;
// #endif
			return CURLINFO_NONE;
		}

		static void push(ObjectScript::OS * os, type val)
		{
			switch(val) {
			case CURLINFO_EFFECTIVE_URL: os->pushString("effective_url"); return;
			case CURLINFO_TOTAL_TIME: os->pushString("total_time"); return;
			case CURLINFO_NAMELOOKUP_TIME: os->pushString("namelookup_time"); return;
			case CURLINFO_CONNECT_TIME: os->pushString("connect_time"); return;
			case CURLINFO_PRETRANSFER_TIME: os->pushString("pretransfer_time"); return;
			case CURLINFO_SIZE_UPLOAD: os->pushString("size_upload"); return;
			case CURLINFO_SIZE_DOWNLOAD: os->pushString("size_download"); return;
			case CURLINFO_SPEED_DOWNLOAD: os->pushString("speed_download"); return;
			case CURLINFO_SPEED_UPLOAD: os->pushString("speed_upload"); return;
			case CURLINFO_HEADER_SIZE: os->pushString("header_size"); return;
			case CURLINFO_REQUEST_SIZE: os->pushString("request_size"); return;
			case CURLINFO_SSL_VERIFYRESULT: os->pushString("ssl_verifyresult"); return;
			case CURLINFO_FILETIME: os->pushString("filetime"); return;
			case CURLINFO_CONTENT_LENGTH_DOWNLOAD: os->pushString("content_length_download"); return;
			case CURLINFO_CONTENT_LENGTH_UPLOAD: os->pushString("content_length_upload"); return;
			case CURLINFO_STARTTRANSFER_TIME: os->pushString("starttransfer_time"); return;
			case CURLINFO_CONTENT_TYPE: os->pushString("content_type"); return;
			case CURLINFO_REDIRECT_TIME: os->pushString("redirect_time"); return;
			case CURLINFO_REDIRECT_COUNT: os->pushString("redirect_count"); return;
			//case CURLINFO_PRIVATE: os->pushString("private"); return;
			case CURLINFO_HTTP_CONNECTCODE: os->pushString("http_connectcode"); return;
			case CURLINFO_HTTPAUTH_AVAIL: os->pushString("httpauth_avail"); return;
			case CURLINFO_PROXYAUTH_AVAIL: os->pushString("proxyauth_avail"); return;
			case CURLINFO_OS_ERRNO: os->pushString("os_errno"); return;
			case CURLINFO_NUM_CONNECTS: os->pushString("num_connects"); return;
			case CURLINFO_SSL_ENGINES: os->pushString("ssl_engines"); return;
			case CURLINFO_COOKIELIST: os->pushString("cookielist"); return;
			case CURLINFO_LASTSOCKET: os->pushString("lastsocket"); return;
			case CURLINFO_FTP_ENTRY_PATH: os->pushString("ftp_entry_path"); return;
			case CURLINFO_REDIRECT_URL: os->pushString("redirect_url"); return;
			case CURLINFO_PRIMARY_IP: os->pushString("primary_ip"); return;
			case CURLINFO_APPCONNECT_TIME: os->pushString("appconnect_time"); return;
#if LIBCURL_VERSION_NUM >= 0x071400
			case CURLINFO_RTSP_SESSION_ID: os->pushString("rtsp_session_id"); return;
			case CURLINFO_RTSP_CLIENT_CSEQ: os->pushString("rtsp_client_cseq"); return;
			case CURLINFO_RTSP_SERVER_CSEQ: os->pushString("rtsp_server_cseq"); return;
			case CURLINFO_RTSP_CSEQ_RECV: os->pushString("rtsp_cseq_recv"); return;
#endif
#if LIBCURL_VERSION_NUM >= 0x071301
			case CURLINFO_CERTINFO: os->pushString("certinfo"); return;
#endif
#if LIBCURL_VERSION_NUM >= 0x071304
			case CURLINFO_CONDITION_UNMET: os->pushString("condition_unmet"); return;
#endif
#if LIBCURL_VERSION_NUM >= 0x071500
			case CURLINFO_PRIMARY_PORT: os->pushString("primary_port"); return;
			case CURLINFO_LOCAL_IP: os->pushString("local_ip"); return;
			case CURLINFO_LOCAL_PORT: os->pushString("local_port"); return;
#endif
// #if LIBCURL_VERSION_NUM >= 0x071900
			case CURLINFO_RESPONSE_CODE: os->pushString("response_code"); return;
// #endif
			}
			os->pushString("unknown");
		}
	};

	CurlOS::Curl::Curl(OS *p_os)
		: os(p_os)
		, handle(NULL)
		, httppost(NULL)
		, httpheader(NULL)
		, http200aliases(NULL)
		, quote(NULL)
		, postquote(NULL)
		, prequote(NULL)
		, telnetopt(NULL)
#if (LIBCURL_VERSION_NUM >= 0x071400)
		, mailrcpt(NULL)
#endif
#if (LIBCURL_VERSION_NUM >= 0x071503)
		, resolve(NULL)
#endif
#if (LIBCURL_VERSION_NUM >= 0x071900)
		, dnsserver(NULL)
#endif
	{
		// share = false;
	}

	CurlOS::Curl::~Curl()
	{
		close();
	}

	void CurlOS::Curl::copyFrom(Curl * other)
	{
		resetData();

		curl_easy_setopt(handle, CURLOPT_ERRORBUFFER, error);
		curl_easy_setopt(handle, CURLOPT_PRIVATE, (char *)os);
		curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, CurlOS::Curl::writeCallback);
		curl_easy_setopt(handle, CURLOPT_WRITEDATA, (void*)this);
		curl_easy_setopt(handle, CURLOPT_HEADERFUNCTION, CurlOS::Curl::headerCallback);
		curl_easy_setopt(handle, CURLOPT_WRITEHEADER, (void*)this);
		curl_easy_setopt(handle, CURLOPT_READFUNCTION, CurlOS::Curl::readCallback);
		curl_easy_setopt(handle, CURLOPT_READDATA, (void*)this);
		curl_easy_setopt(handle, CURLOPT_PROGRESSFUNCTION, CurlOS::Curl::progressCallback);
		curl_easy_setopt(handle, CURLOPT_PROGRESSDATA, (void*)this);
		curl_easy_setopt(handle, CURLOPT_DEBUGDATA, (void*)this);
		curl_easy_setopt(handle, CURLOPT_IOCTLDATA, (void*)this);

		write.behavior = other->write.behavior;
		write.func_id = other->write.func_id;
		write.file_id = other->write.file_id;
		os->retainValueById(write.func_id);
		os->retainValueById(write.file_id);

		write_header.behavior = other->write_header.behavior;
		write_header.func_id = other->write_header.func_id;
		write_header.file_id = other->write_header.file_id;
		os->retainValueById(write_header.func_id);
		os->retainValueById(write_header.file_id);

		read.behavior = other->read.behavior;
		read.func_id = other->read.func_id;
		read.file_id = other->read.file_id;
		os->retainValueById(read.func_id);
		os->retainValueById(read.file_id);

		progress.behavior = other->progress.behavior;
		progress.func_id = other->progress.func_id;
		progress.file_id = other->progress.file_id;
		os->retainValueById(progress.func_id);
		os->retainValueById(progress.file_id);

		curl_slist *slist = other->httpheader;
		while (slist) {
			httpheader = curl_slist_append(httpheader, slist->data);
			slist = slist->next;
		}
		curl_easy_setopt(handle, CURLOPT_HTTPHEADER, httpheader);

		slist = other->http200aliases;
		while (slist) {
			http200aliases = curl_slist_append(http200aliases, slist->data);
			slist = slist->next;
		}
		curl_easy_setopt(handle, CURLOPT_HTTP200ALIASES, http200aliases);

		slist = other->quote;
		while (slist) {
			quote = curl_slist_append(quote, slist->data);
			slist = slist->next;
		}
		curl_easy_setopt(handle, CURLOPT_QUOTE, quote);

		slist = other->postquote;
		while (slist) {
			postquote = curl_slist_append(postquote, slist->data);
			slist = slist->next;
		}
		curl_easy_setopt(handle, CURLOPT_POSTQUOTE, postquote);

		slist = other->prequote;
		while (slist) {
			prequote = curl_slist_append(prequote, slist->data);
			slist = slist->next;
		}
		curl_easy_setopt(handle, CURLOPT_PREQUOTE, prequote);

		slist = other->telnetopt;
		while (slist) {
			telnetopt = curl_slist_append(telnetopt, slist->data);
			slist = slist->next;
		}
		curl_easy_setopt(handle, CURLOPT_TELNETOPTIONS, telnetopt);

#if (LIBCURL_VERSION_NUM >= 0x071400)
		slist = other->mailrcpt;
		while (slist) {
			mailrcpt = curl_slist_append(mailrcpt, slist->data);
			slist = slist->next;
		}
		curl_easy_setopt(handle, CURLOPT_MAIL_RCPT, mailrcpt);
#endif
#if (LIBCURL_VERSION_NUM >= 0x071503)
		slist = other->resolve;
		while (slist) {
			resolve = curl_slist_append(resolve, slist->data);
			slist = slist->next;
		}
		curl_easy_setopt(handle, CURLOPT_RESOLVE, resolve);
#endif
#if (LIBCURL_VERSION_NUM >= 0x071900)
		slist = other->dnsserver;
		while (slist) {
			dnsserver = curl_slist_append(dnsserver, slist->data);
			slist = slist->next;
		}
		curl_easy_setopt(handle, CURLOPT_DNS_SERVERS, dnsserver);
#endif
	}

	bool CurlOS::Curl::init()
	{
		/* Initialize curl handle */
		handle = curl_easy_init();
		if(handle == NULL){
			triggerError(OS::String(os, "error to create curl object"));
			return false;
		}
		CURLcode res = CURLE_OK;

#undef CURL_SETOPT
#define CURL_SETOPT(o,v)			\
	res = curl_easy_setopt(handle,(o),(v));	\
	if(res != CURLE_OK) {			\
		triggerError(res);		\
		return false;			\
	}

		/* Set curl error buffer */
		CURL_SETOPT(CURLOPT_ERRORBUFFER, error);

		/* Set backreference */
		CURL_SETOPT(CURLOPT_PRIVATE, (char *)os);

		/* Enable NOPROGRESS by default, i.e. no progress output */
		CURL_SETOPT(CURLOPT_NOPROGRESS, (long)1);

		/* Disable VERBOSE by default, i.e. no verbose output */
		CURL_SETOPT(CURLOPT_VERBOSE, (long)0);

		/* Set FTP_ACCOUNT to NULL by default */
		CURL_SETOPT(CURLOPT_FTP_ACCOUNT, NULL);

		/* Set default USERAGENT */
		CURL_SETOPT(CURLOPT_USERAGENT, OS_TEXT("OS-CURL-AGENT ") OS_VERSION OS_TEXT(" - ") OS_TEXT(LIBCURL_VERSION));

		/*  */
		CURL_SETOPT(CURLOPT_DNS_USE_GLOBAL_CACHE, 1);

		/*  */
		CURL_SETOPT(CURLOPT_DNS_CACHE_TIMEOUT, 120);

		/*  */
		CURL_SETOPT(CURLOPT_MAXREDIRS, 20); /* prevent infinite redirects */

		/* Set write function callback */
		write.behavior = BEHAVIOR_RETURN;
		// write.buf = new (os->malloc(sizeof(Core::Buffer) OS_DBG_FILEPOS)) Core::Buffer(os);
		CURL_SETOPT(CURLOPT_WRITEFUNCTION, CurlOS::Curl::writeCallback);
		CURL_SETOPT(CURLOPT_WRITEDATA, (void*)this);

		/* Set writeheader function callback */
		write_header.behavior = BEHAVIOR_IGNORE;
		// write_header.buf = new (os->malloc(sizeof(Core::Buffer) OS_DBG_FILEPOS)) Core::Buffer(os);
		CURL_SETOPT(CURLOPT_HEADERFUNCTION, CurlOS::Curl::headerCallback);
		CURL_SETOPT(CURLOPT_WRITEHEADER, (void*)this);

		/* Set read function callback */
		read.behavior = BEHAVIOR_IGNORE;
		// read.buf = new (os->malloc(sizeof(Core::Buffer) OS_DBG_FILEPOS)) Core::Buffer(os);
		CURL_SETOPT(CURLOPT_READFUNCTION, CurlOS::Curl::readCallback);
		CURL_SETOPT(CURLOPT_READDATA, (void*)this);

		/* Set progress function callback */
		progress.behavior = BEHAVIOR_IGNORE;
		// progress.buf = new (os->malloc(sizeof(Core::Buffer) OS_DBG_FILEPOS)) Core::Buffer(os);
		CURL_SETOPT(CURLOPT_PROGRESSFUNCTION, CurlOS::Curl::progressCallback);
		CURL_SETOPT(CURLOPT_PROGRESSDATA, (void*)this);

		/* Set backreference for other callbacks */
		CURL_SETOPT(CURLOPT_DEBUGDATA, (void*)this);
		CURL_SETOPT(CURLOPT_IOCTLDATA, (void*)this);

#undef CURL_SETOPT
		return true;
	}

	void CurlOS::Curl::resetData()
	{
#undef CURL_FORM_FREE
#define CURL_FORM_FREE(v)   if ((v) != NULL) (curl_formfree(v), (v) = NULL)
		CURL_FORM_FREE(httppost);
#undef CURL_FORM_FREE

#define CURL_LIST_FREE(v)   if ((v) != NULL) (curl_slist_free_all(v), (v) = NULL)
		CURL_LIST_FREE(httpheader);
		CURL_LIST_FREE(http200aliases);
		CURL_LIST_FREE(quote);
		CURL_LIST_FREE(postquote);
		CURL_LIST_FREE(prequote);
		CURL_LIST_FREE(telnetopt);
#if (LIBCURL_VERSION_NUM >= 0x071400)
		CURL_LIST_FREE(mailrcpt);
#endif
#if (LIBCURL_VERSION_NUM >= 0x071503)
		CURL_LIST_FREE(resolve);
#endif
#if (LIBCURL_VERSION_NUM >= 0x071900)
		CURL_LIST_FREE(dnsserver);
#endif
#undef CURL_LIST_FREE

		CallbackData * data[] = {&write, &write_header, &read, &progress, &debug, &ioctl};
		for(int i = 0; i < sizeof(data)/sizeof(data[0]); i++){
			if(data[i]->file_id){
				os->releaseValueById(data[i]->file_id);
				data[i]->file_id = 0;
			}
			if(data[i]->func_id){
				os->releaseValueById(data[i]->func_id);
				data[i]->func_id = 0;
			}
			if(data[i]->buf){
#if 1
				os->deleteObj(data[i]->buf);
#else
				data[i]->buf->~Buffer();
				os->free(data[i]->buf);
				data[i]->buf = NULL;
#endif
			}
		}

		/* if(share){
			curl_easy_setopt(handle, CURLOPT_SHARE, NULL);
			// TODO: delete share?
		} */
	}

	void CurlOS::Curl::close()
	{
		if (handle) {
			curl_easy_cleanup(handle);
			handle = NULL;
			resetData();
		}
	}

	void CurlOS::Curl::reset()
	{
		if (handle) {
			curl_easy_reset(handle);
			resetData();
		}
	}

	CURLcode CurlOS::Curl::perform()
	{
		if (handle)
			return curl_easy_perform(handle);
		return CURLE_FAILED_INIT;
	}

	bool CurlOS::Curl::setOption(CURLoption option, curl_slist * slist)
	{
		struct curl_slist ** old_slist = NULL;

		switch (option) {
		case CURLOPT_HTTPPOST: //TODO: возможно будет удобней вынести из этой группы???
			break;

		case CURLOPT_HTTP200ALIASES:
			old_slist = &http200aliases;
			break;

		case CURLOPT_HTTPHEADER:
			old_slist = &httpheader;
			break;

		case CURLOPT_QUOTE:
			old_slist = &quote;
			break;

		case CURLOPT_POSTQUOTE:
			old_slist = &postquote;
			break;

		case CURLOPT_PREQUOTE:
			old_slist = &prequote;
			break;

		case CURLOPT_TELNETOPTIONS:
			old_slist = &telnetopt;
			break;

#if (LIBCURL_VERSION_NUM >= 0x071400)
		case CURLOPT_MAIL_RCPT:
			old_slist = &mailrcpt;
			break;
#endif
#if (LIBCURL_VERSION_NUM >= 0x071503)
		case CURLOPT_RESOLVE:
			old_slist = &resolve;
			break;
#endif
#if (LIBCURL_VERSION_NUM >= 0x071900)
		case CURLOPT_DNS_SERVERS:
			old_slist = &dnsserver;
			break;
#endif
		default:
			CtypeValue<CURLoption>::push(os, option);
			triggerError(OS::String(os, OS_TEXT("curl option \"")) + os->popString() + OS_TEXT("\" must not be array"));
			return false;
		}

		CURLcode code = curl_easy_setopt(handle, option, slist);
		if (code != CURLE_OK) {
			curl_slist_free_all(slist);
			triggerError(code);
			return false;
		}
		curl_slist_free_all(*old_slist);
		*old_slist = slist;
		return true;
	}

	bool CurlOS::Curl::setOption()
	{ 
		CURLcode code = CURLE_OK;
		OS::String name = os->toString(-2);
		for(int i = 0; i < curl_option_count; i++){
			if(name == curl_option_desc[i].name
				|| (curl_option_desc[i].name2 && name == curl_option_desc[i].name2))
			{
				CURLoption option = (CURLoption)curl_option_desc[i].option;
				if(os->isNull()){
					CurlOS::Curl::CallbackData * data = NULL;
					if (option == CURLOPT_WRITEFUNCTION || option == CURLOPT_WRITEDATA)
						data = &this->write;
					else if (option == CURLOPT_HEADERFUNCTION || option == CURLOPT_WRITEHEADER)
						data = &this->write_header;
					else if (option == CURLOPT_READFUNCTION || option == CURLOPT_READDATA)
						data = &this->read;
					else if (option == CURLOPT_PROGRESSFUNCTION)
						data = &this->progress;
					else if (option == CURLOPT_DEBUGFUNCTION) {
						data = &this->debug;
						curl_easy_setopt(handle, option, NULL);
					} else if (option == CURLOPT_IOCTLFUNCTION) {
						data = &this->ioctl;
						curl_easy_setopt(handle, option, NULL);
					} else if (option == CURLOPT_SHARE) {
						// share = false;
						curl_easy_setopt(handle, option, NULL);
						return true;
					} else {
						// os->setException(OS::String(os, OS_TEXT("unsupported value type of curl option \"")) + name + OS_TEXT("\""));
						// return false;
					}
					if(data){
						if (data->func_id) {
							os->releaseValueById(data->func_id);
							data->func_id = 0;
						}
						if (data->file_id) {
							os->releaseValueById(data->file_id);
							data->file_id = 0;
						}
						data->behavior = CurlOS::Curl::BEHAVIOR_RETURN;
					}
					code = CURLE_OK;
					switch(curl_option_desc[i].type){
					case OPT_BOOL:
					case OPT_LONG:
						code = curl_easy_setopt(handle, option, 0);
						break;

					case OPT_LARGE:
						code = curl_easy_setopt(handle, option, (curl_off_t)0);
						break;

					case OPT_STRING:
						code = curl_easy_setopt(handle, option, NULL);
						break;
					}
					if(code != CURLE_OK){
						triggerError(code);
						return false;
					}
					return true;
				}
				if(option == CURLOPT_WRITEDATA || option == CURLOPT_WRITEHEADER){
					CallbackData * write = option == CURLOPT_WRITEDATA ? &this->write : &this->write_header;

					OS::String value = os->toString();
					if(value == OS_TEXT("stdout")){
						if (write->file_id) {
							os->releaseValueById(write->file_id);
							write->file_id = 0;
						}
						if (write->func_id) {
							os->releaseValueById(write->func_id);
							write->func_id = 0;
						}
						write->behavior = CurlOS::Curl::BEHAVIOR_STDOUT;
						return true;
					}
					if(value == OS_TEXT("stderr")){
						if (write->file_id) {
							os->releaseValueById(write->file_id);
							write->file_id = 0;
						}
						if (write->func_id) {
							os->releaseValueById(write->func_id);
							write->func_id = 0;
						}
						write->behavior = CurlOS::Curl::BEHAVIOR_STDERR;
						return true;
					}
					goto file_option;
				}
				code = CURLE_OK;
				switch(curl_option_desc[i].type){
				case OPT_BOOL:
					if(os->getType() != OS_VALUE_TYPE_BOOL){ // !os->isNumber()){
						triggerError(OS::String(os, OS_TEXT("curl option \"")) + name + OS_TEXT("\" must be boolean"));
						return false;
					}
					code = curl_easy_setopt(handle, option, (long)os->toBool()); // TODO: toInt ?
					break;

				case OPT_LONG:
					if(!os->isNumber()){
						triggerError(OS::String(os, OS_TEXT("curl option \"")) + name + OS_TEXT("\" must be number"));
						return false;
					}
					code = curl_easy_setopt(handle, option, (long)os->toInt());
					break;

				case OPT_LARGE:
					if(!os->isNumber()){
						triggerError(OS::String(os, OS_TEXT("curl option \"")) + name + OS_TEXT("\" must be number"));
						return false;
					}
					code = curl_easy_setopt(handle, option, (curl_off_t)os->toInt());
					break;

				case OPT_STRING:
					if(!os->isString()){
						triggerError(OS::String(os, OS_TEXT("curl option \"")) + name + OS_TEXT("\" must be string"));
						return false;
					}
					code = curl_easy_setopt(handle, option, os->toString().toChar());
					break;

				case OPT_ARRAY:
					if(os->isArray()){
						struct curl_slist *slist = NULL;
						int count = os->getLen();
						for(int i = 0; i < count; ++i) {
							os->pushStackValue();
							os->pushNumber(i);
							os->getProperty();

							if (!os->isString()) {
								curl_slist_free_all(slist);
								triggerError(OS::String(os, OS_TEXT("curl option \"")) + name + OS_TEXT("\" must be array of strings"));
								return false;
							}

							slist = curl_slist_append(slist, os->popString().toChar());
						}
						return setOption(option, slist);
					}
					triggerError(OS::String(os, OS_TEXT("curl option \"")) + name + OS_TEXT("\" must be array"));
					return false;

				case OPT_FILE:
file_option:
					// os->getGlobal(OS_TEXT("File"));
					if(os->isObject() || os->isUserdata()){
						CurlOS::Curl::CallbackData * data = NULL;
						if (option == CURLOPT_WRITEDATA)
							data = &this->write;
						else if (option == CURLOPT_WRITEHEADER)
							data = &this->write_header;
						else if (option == CURLOPT_READDATA)
							data = &this->read;
						else if (option == CURLOPT_STDERR) {
							triggerError(OS_TEXT("\"stderr\" is not supported"));
							return false;
						} else if (option == CURLOPT_SHARE) {
							/* if (share) {
								triggerError(OS_TEXT("curl object is already sharing, unshare it first"));
								return false;
							}
							CurlShare * curl_share = CtypeValue<CurlShare*>::getArg(os, -1);
							if(!curl_share){
								triggerError(OS_TEXT("curl share option must be instance of CurlShare"));
								return false;
							}
							code = curl_easy_setopt(handle, option, curl_share->handle());
							share = code == CURLE_OK && curl_share->handle(); */
							break;
						} else {
							triggerError(OS::String(os, OS_TEXT("unsupported value type of curl option \"")) + name + OS_TEXT("\""));
							return false;
						}
						if (data->file_id) {
							os->releaseValueById(data->file_id);
							data->file_id = 0;
						}
						if (data->func_id) {
							os->releaseValueById(data->func_id);
							data->func_id = 0;
						}
						os->retainValueById(data->file_id = os->getValueId());
						data->behavior = CurlOS::Curl::BEHAVIOR_FILE;
						return true;
					}
					triggerError(OS::String(os, OS_TEXT("curl option \"")) + name + OS_TEXT("\" must be file object"));
					return false;

				case OPT_FUNC:
					if(os->isFunction()){
						CurlOS::Curl::CallbackData * data = NULL;
						if (option == CURLOPT_WRITEFUNCTION)
							data = &this->write;
						else if (option == CURLOPT_HEADERFUNCTION)
							data = &this->write_header;
						else if (option == CURLOPT_READFUNCTION)
							data = &this->read;
						else if (option == CURLOPT_PROGRESSFUNCTION)
							data = &this->progress;
						else if (option == CURLOPT_DEBUGFUNCTION) {
							data = &this->debug;
							curl_easy_setopt(handle, option, CurlOS::Curl::debugCallback);
						} else if (option == CURLOPT_IOCTLFUNCTION) {
							data = &this->ioctl;
							curl_easy_setopt(handle, option, CurlOS::Curl::ioctlCallback);
						} else {
							triggerError(OS::String(os, OS_TEXT("unsupported value type of curl option \"")) + name + OS_TEXT("\""));
							return false;
						}
						if (data->func_id) {
							os->releaseValueById(data->func_id);
							data->func_id = 0;
						}
						if (data->file_id) {
							os->releaseValueById(data->file_id);
							data->file_id = 0;
						}
						os->retainValueById(data->func_id = os->getValueId());
						data->behavior = CurlOS::Curl::BEHAVIOR_FUNC;
						return true;
					}
					triggerError(OS::String(os, OS_TEXT("curl option \"")) + name + OS_TEXT("\" must be function"));
					return false;
				}
				if(code != CURLE_OK){
					triggerError(code);
					return false;
				}
				return true;
			}
		}
		triggerError(OS::String(os, OS_TEXT("unknown curl option \"")) + name + OS_TEXT("\""));
		return false;
	}

	CurlOS::Curl::InfoData CurlOS::Curl::getInfo(CURLINFO info)
	{
		CurlOS::Curl::InfoData value;
		value.result = CURLE_UNKNOWN_OPTION;

		switch (info) {
		case CURLINFO_RESPONSE_CODE:
		case CURLINFO_HTTP_CONNECTCODE:
		case CURLINFO_FILETIME:
		case CURLINFO_HEADER_SIZE:
		case CURLINFO_REQUEST_SIZE:
		case CURLINFO_SSL_VERIFYRESULT:
		case CURLINFO_REDIRECT_COUNT:
		case CURLINFO_HTTPAUTH_AVAIL:
		case CURLINFO_PROXYAUTH_AVAIL:
		case CURLINFO_OS_ERRNO:
		case CURLINFO_NUM_CONNECTS:
		case CURLINFO_LASTSOCKET:
#if LIBCURL_VERSION_NUM >= 0x071304
		case CURLINFO_CONDITION_UNMET:
#endif
#if LIBCURL_VERSION_NUM >= 0x071500
		case CURLINFO_PRIMARY_PORT:
		case CURLINFO_LOCAL_PORT:
#endif
#if (LIBCURL_VERSION_NUM >= 0x071400)
		case CURLINFO_RTSP_CLIENT_CSEQ:
		case CURLINFO_RTSP_SERVER_CSEQ:
		case CURLINFO_RTSP_CSEQ_RECV:
#endif
			/* long */
			value.lvalue = 0;
			value.type = LONG;
			value.result = curl_easy_getinfo(handle, info, &value.lvalue);
			break;

		case CURLINFO_CONTENT_TYPE:
		case CURLINFO_EFFECTIVE_URL:
		case CURLINFO_FTP_ENTRY_PATH:
#if LIBCURL_VERSION_NUM >= 0x071500
		case CURLINFO_LOCAL_IP:
#endif
		case CURLINFO_REDIRECT_URL:
		case CURLINFO_PRIMARY_IP:
#if LIBCURL_VERSION_NUM >= 0x071400
		case CURLINFO_RTSP_SESSION_ID:
#endif
			/* string */
			value.cvalue = NULL;
			value.type = CHAR_PTR;
			value.result = curl_easy_getinfo(handle, info, (char **)&value.cvalue);
			break;

		case CURLINFO_TOTAL_TIME:
		case CURLINFO_NAMELOOKUP_TIME:
		case CURLINFO_CONNECT_TIME:
		case CURLINFO_APPCONNECT_TIME:
		case CURLINFO_PRETRANSFER_TIME:
		case CURLINFO_STARTTRANSFER_TIME:
		case CURLINFO_SIZE_UPLOAD:
		case CURLINFO_SIZE_DOWNLOAD:
		case CURLINFO_SPEED_DOWNLOAD:
		case CURLINFO_SPEED_UPLOAD:
		case CURLINFO_CONTENT_LENGTH_DOWNLOAD:
		case CURLINFO_CONTENT_LENGTH_UPLOAD:
		case CURLINFO_REDIRECT_TIME:
			/* double */
			value.dvalue = 0.0;
			value.type = DOUBLE;
			value.result = curl_easy_getinfo(handle, info, &value.dvalue);
			break;

		case CURLINFO_SSL_ENGINES:
		case CURLINFO_COOKIELIST:
			/* slist */
			value.slvalue = NULL;
			value.type = SLIST_PTR;
			value.result = curl_easy_getinfo(handle, info, (curl_slist **)&value.slvalue);
			break;

#if LIBCURL_VERSION_NUM >= 0x071301
		case CURLINFO_CERTINFO:
			value.civalue = NULL;
			value.type = CERTINFO_PTR;
			value.result = curl_easy_getinfo(handle, info, (curl_certinfo **)&value.civalue);
			break;
#endif

		default:
			// triggerError(OS_TEXT("wrong arguments"));
			break;
		}

		return value;
	}

	size_t CurlOS::Curl::writeCallback(char *ptr, size_t size, size_t nmemb, void *userdata)
	{
		Curl *curl = (Curl *)userdata;
		CallbackData *write = &curl->write;
		int length = (int)(size * nmemb);

		OS *os = NULL;
		CURLcode res = curl_easy_getinfo(curl->handle, CURLINFO_PRIVATE, (char **)&os);
		if (res != CURLE_OK || !os || os != curl->os) { // paranoid check
			curl->triggerError(OS_TEXT("unexpected error"));
			return length;
		}

		switch (write->behavior) {
		case BEHAVIOR_STDOUT:
			// return fwrite(ptr, size, nmemb, stdout);
			os->getGlobal(OS_TEXT("echo"));
			os->pushGlobals();
			os->pushString((void*)ptr, length);
			os->callFT(1);
			break;

		case BEHAVIOR_STDERR:
			fwrite(ptr, size, nmemb, stderr);
			break;

		case BEHAVIOR_FILE:
			os->pushValueById(write->file_id);
			os->getProperty(-1, "write");
			os->pushValueById(write->file_id);
			os->pushString((void*)ptr, length);
			os->callFT(1);
			break;

		case BEHAVIOR_RETURN:
			if(length > 0){
				if(!write->buf){
					write->buf = new (os->malloc(sizeof(Core::Buffer) OS_DBG_FILEPOS)) Core::Buffer(os);
				}
				write->buf->append((void*)ptr, length);
			}
			break;

		case BEHAVIOR_FUNC:
			os->pushValueById(write->func_id);
			os->pushString((void*)ptr, length);
			os->callF(1);
			break;

		case BEHAVIOR_IGNORE:
			break;
		}

		return length;
	}

	size_t CurlOS::Curl::headerCallback(char *ptr, size_t size, size_t nmemb, void *userdata)
	{
		Curl *curl = (Curl *)userdata;
		CallbackData *write_header = &curl->write_header;
		int length = (int)(size * nmemb);

		OS *os = NULL;
		CURLcode res = curl_easy_getinfo(curl->handle, CURLINFO_PRIVATE, (char **)&os);
		if (res != CURLE_OK || !os || os != curl->os) { // paranoid check
			curl->triggerError(OS_TEXT("unexpected error"));
			return length;
		}

		switch (write_header->behavior) {
		case BEHAVIOR_STDOUT:
			// return fwrite(ptr, size, nmemb, stdout);
			os->getGlobal(OS_TEXT("echo"));
			os->pushGlobals();
			os->pushString((void*)ptr, length);
			os->callFT(1);
			break;

		case BEHAVIOR_STDERR:
			fwrite(ptr, size, nmemb, stderr);
			break;

		case BEHAVIOR_FILE:
			os->pushValueById(write_header->file_id);
			os->getProperty("write");
			os->pushValueById(write_header->file_id);
			os->pushString((void*)ptr, length);
			os->callFT(1);
			break;

		case BEHAVIOR_FUNC:
			os->pushValueById(write_header->func_id);
			os->pushString((void*)ptr, length);
			os->callF(1);
			break;

		case BEHAVIOR_RETURN:
		case BEHAVIOR_IGNORE:
			break;
		}

		return length;
	}

	size_t CurlOS::Curl::readCallback(void *ptr, size_t size, size_t nmemb, void *userdata)
	{
		Curl *curl = (Curl *)userdata;
		CallbackData *read = &curl->read;
		size_t length = 0;

		OS *os = NULL;
		CURLcode res = curl_easy_getinfo(curl->handle, CURLINFO_PRIVATE, (char **)&os);
		if (res != CURLE_OK || !os || os != curl->os) { // paranoid check
			curl->triggerError(OS_TEXT("unexpected error"));
			return CURLE_OK;
		}

		switch (read->behavior) {
		case BEHAVIOR_FILE: {
			os->pushValueById(read->file_id);
			os->getProperty("read");
			os->pushValueById(read->file_id);
			os->pushNumber(size * nmemb);
			os->callFT(1, 1);
			OS::String ostr = os->popString();
			length = ostr.getDataSize();
			if(length > size * nmemb){
				length = size * nmemb;
			}
			if(length > 0){
				::memcpy(ptr, ostr.toChar(), length);
			}
			break;
							}
		case BEHAVIOR_FUNC: {
			os->pushValueById(read->func_id);
			os->pushNumber(size * nmemb);
			os->callF(1, 1);
			OS::String ostr = os->popString();
			length = ostr.getDataSize();
			if(length > size * nmemb){
				length = size * nmemb;
			}
			if (length > 0){
				::memcpy(ptr, ostr.toChar(), length);
			}
			break;
							}
		case BEHAVIOR_STDOUT:
		case BEHAVIOR_STDERR:
		case BEHAVIOR_RETURN:
		case BEHAVIOR_IGNORE:
			break;
		}

		return length;
	}

	int CurlOS::Curl::progressCallback(void *userdata, double dltotal, double dlnow, double ultotal, double ulnow)
	{
		Curl *curl = (Curl *)userdata;
		CallbackData *progress = &curl->progress;

		OS *os = NULL;
		CURLcode res = curl_easy_getinfo(curl->handle, CURLINFO_PRIVATE, (char **)&os);
		if (res != CURLE_OK || !os || os != curl->os) { // paranoid check
			curl->triggerError(OS_TEXT("unexpected error"));
			return CURLE_OK;
		}

		switch (progress->behavior) {
		case BEHAVIOR_FUNC:
			os->pushValueById(progress->func_id);
			
			os->pushNumber(dltotal);
			os->pushNumber(dlnow);
			os->pushNumber(ultotal);
			os->pushNumber(ulnow);

			os->callF(4);
			break;

		case BEHAVIOR_STDOUT:
		case BEHAVIOR_STDERR:
		case BEHAVIOR_RETURN:
		case BEHAVIOR_IGNORE:
		case BEHAVIOR_FILE:
			break;
		}

		return CURLE_OK;
	}

	int CurlOS::Curl::debugCallback(CURL *, curl_infotype type, char *buf, size_t buf_len, void *userdata)
	{
		Curl *curl = (Curl *)userdata;
		CallbackData *debug = &curl->debug;

		OS *os = NULL;
		CURLcode res = curl_easy_getinfo(curl->handle, CURLINFO_PRIVATE, (char **)&os);
		if (res != CURLE_OK || !os || os != curl->os) { // paranoid check
			curl->triggerError(OS_TEXT("unexpected error"));
			return CURLE_OK;
		}

		switch (debug->behavior) {
		case BEHAVIOR_FUNC:
			os->pushValueById(debug->func_id);
			os->pushNumber(type);
			os->pushString((void*)buf, (int)buf_len);
			os->callF(2);
			break;

		case BEHAVIOR_STDOUT:
		case BEHAVIOR_STDERR:
		case BEHAVIOR_RETURN:
		case BEHAVIOR_IGNORE:
		case BEHAVIOR_FILE:
			break;
		}

		return CURLE_OK;
	}

	curlioerr CurlOS::Curl::ioctlCallback(CURL *, int cmd, void *userdata)
	{
		Curl *curl = (Curl *)userdata;
		CallbackData *ioctl = &curl->ioctl;
		curlioerr ret = CURLIOE_OK;

		OS *os = NULL;
		CURLcode res = curl_easy_getinfo(curl->handle, CURLINFO_PRIVATE, (char **)&os);
		if (res != CURLE_OK || !os || os != curl->os) { // paranoid check
			curl->triggerError(OS_TEXT("unexpected error"));
			return CURLIOE_OK;
		}

		switch (ioctl->behavior) {
		case BEHAVIOR_FUNC:
			os->pushValueById(ioctl->func_id);
			os->pushNumber(cmd);
			os->callF(1, 1);
			ret = (curlioerr)os->popInt(CURLIOE_OK);
			if (ret >= CURLIOE_LAST || ret < 0) {
				curl->triggerError(OS_TEXT("ioctl callback returned invalid value"));
				ret = CURLIOE_FAILRESTART;
			}
			break;

		case BEHAVIOR_STDOUT:
		case BEHAVIOR_STDERR:
		case BEHAVIOR_RETURN:
		case BEHAVIOR_IGNORE:
		case BEHAVIOR_FILE:
			break;
		}

		return ret;
	}

	template <> struct UserDataDestructor<CurlOS::Curl>
	{
		static void dtor(ObjectScript::OS * os, void * data, void * user_param)
		{
			OS_ASSERT(data && dynamic_cast<CurlOS::Curl*>((CurlOS::Curl*)data));
			CurlOS::Curl * buf = (CurlOS::Curl*)data;
			buf->~Curl();
			os->free(buf);
		}
	};

	void initCurlExtension(OS *os)
	{
		CurlOS::Curl::initExtension(os);
	}

	void CurlOS::Curl::initExtension(OS *os)
	{
		struct Lib
		{
			static int __newinstance(OS * os, int params, int, int, void * user_param)
			{
				Curl * self = new (os->malloc(sizeof(Curl) OS_DBG_FILEPOS)) Curl(os);
				if (!self->init()) {
					self->~Curl();
					os->free(self);
					return 0;
				}
				if (params == 1 && os->isObject(-params+0)) {
					while (os->nextIteratorStep()) {
						if(!self->setOption())
							break;
						os->pop(2);
					}
				} else if(params == 2) {
					self->setOption();
				} else if(params != 0) {
					self->triggerError(OS_TEXT("wrong arguments"));
				}
				if(os->isExceptionSet()){
					self->~Curl();
					os->free(self);
					return 0;
				}
				CtypeValue<Curl*>::push(os, self);
				return 1;
			}

			static int clone(OS * os, int params, int, int, void * user_param)
			{
				OS_GET_SELF(Curl*);

				if (params) {
					self->triggerError(OS::String(os, "wrong arguments"));
					return 0;
				}

				Curl * copy = new (os->malloc(sizeof(Curl) OS_DBG_FILEPOS)) Curl(os);
				copy->handle = curl_easy_duphandle(self->handle);
				if (!copy->handle) {
					self->triggerError(OS_TEXT("can't clone curl handle"));
					copy->~Curl();
					os->free(copy);
					return 0;
				}
				copy->copyFrom(self);

				CtypeValue<Curl*>::push(os, copy);
				return 1;
			}

			static int perform(OS * os, int params, int, int, void * user_param)
			{
				OS_GET_SELF(Curl*);

				struct AutoClose {
					Curl * self;
					bool close;

					~AutoClose(){
						if(close){
							self->close();
						}
					}
				} autoClose = {self, params == 0 || !os->toBool(-params+0)};
				(void)autoClose;

				CurlOS::Curl::CallbackData *write = &self->write;

				CURLcode ret = self->perform();
				if (ret != CURLE_OK) { // && ret != CURLE_PARTIAL_FILE) {
					if(!os->isExceptionSet()){
						self->triggerError(ret);
					}
					return 0;
				}

				if (write->behavior == CurlOS::Curl::BEHAVIOR_RETURN) {
					if (write->buf) {
						os->pushString(write->buf->toString());
						write->buf->clear();
					} else {
						os->pushString(OS_TEXT(""));
					}
					return 1;
				} else if (write->behavior == CurlOS::Curl::BEHAVIOR_FILE) {
					//TODO: добавить flush для файлов
					// os->pushNumber(ret);
				} else if (write->behavior == CurlOS::Curl::BEHAVIOR_FUNC   ||
					write->behavior == CurlOS::Curl::BEHAVIOR_STDOUT ||
					write->behavior == CurlOS::Curl::BEHAVIOR_STDERR ||
					write->behavior == CurlOS::Curl::BEHAVIOR_IGNORE) {
						// os->pushNumber(ret);
				}

				return 0;
			}

			static int options(OS * os, int params, int, int, void * user_param)
			{
				OS_GET_SELF(Curl*);
				if (params == 1 && os->isObject(-params+0)) {
					while (os->nextIteratorStep()) {
						if(!self->setOption())
							return 0;
						os->pop(2);
					}
				} else if(params == 2) {
					if(!self->setOption())
						return 0;
				} else {
					self->triggerError(OS_TEXT("wrong arguments"));
					return 0;
				}
				CtypeValue<Curl*>::push(os, self);
				return 1;
			}

			static void pushInfo(OS * os, CurlOS::Curl::InfoData & info)
			{
				switch (info.type) {
				case CurlOS::Curl::LONG:
					os->pushNumber(info.lvalue);
					break;

				case CurlOS::Curl::DOUBLE:
					os->pushNumber(info.dvalue);
					break;

				case CurlOS::Curl::CHAR_PTR:
					if ((char *)info.cvalue)
						os->pushString((char *)info.cvalue);
					else
						os->pushNull();
					break;

				case CurlOS::Curl::SLIST_PTR:
					if ((curl_slist *)info.slvalue) {
						os->newArray();
						while (info.slvalue) {
							os->pushStackValue();
							os->pushString(info.slvalue->data);
							os->addProperty();
							info.slvalue = info.slvalue->next;
						}
						curl_slist_free_all(info.slvalue);
					} else
						os->pushNull();
					break;

#if LIBCURL_VERSION_NUM >= 0x071301
				case CurlOS::Curl::CERTINFO_PTR:
					if ((curl_certinfo *)info.civalue) {
						for (int i = 0; i < info.civalue->num_of_certs; ++i) {
							for(curl_slist *slist = info.civalue->certinfo[i]; slist; slist = slist->next) {
								//TODO: распарсить данные, что бы вернуть Object
								//Так как OpenSLL не поддерживается, не понятно как выглядят данные
							}
						}
						os->pushNull();
					} else
						os->pushNull();
					break;
#endif
				default:
					break;
				}
			}

			static int getInfo(OS * os, int params, int, int, void * user_param)
			{
				CurlOS::Curl::InfoData info;
				OS_GET_SELF(Curl*);

				if (params > 1) {
					self->triggerError(OS::String(os, "wrong arguments number"));
					return 0;
				} else if(params) {
					if (os->isObject(-params+0)) {
						os->newObject();
						int obj_offs = os->getAbsoluteOffs(-1);
						os->pushStackValue(-2);
						int k = 0;
						while (os->nextIteratorStep()) {
							CURLINFO ci = CtypeValue<CURLINFO>::getArg(os, -1);
							os->pushStackValue(obj_offs);
							CtypeValue<CURLINFO>::push(os, ci);
							info = self->getInfo(ci);
							pushInfo(os, info);
							os->addProperty();
							os->pop(2);
						}
						os->pop();
					} else {
						info = self->getInfo(CtypeValue<CURLINFO>::getArg(os, -params+0));
						if (info.result != CURLE_OK)
							return 0;
						pushInfo(os, info);
					}

				} else {
					os->newObject();

#define PUSH_CURLINFO(i)			\
	os->pushStackValue();			\
	CtypeValue<CURLINFO>::push(os, (i));	\
	info = self->getInfo((i));		\
	pushInfo(os, info);			\
	os->setProperty();

					PUSH_CURLINFO(CURLINFO_EFFECTIVE_URL);
					PUSH_CURLINFO(CURLINFO_RESPONSE_CODE);
					PUSH_CURLINFO(CURLINFO_TOTAL_TIME);
					PUSH_CURLINFO(CURLINFO_NAMELOOKUP_TIME);
					PUSH_CURLINFO(CURLINFO_CONNECT_TIME);
					PUSH_CURLINFO(CURLINFO_PRETRANSFER_TIME);
					PUSH_CURLINFO(CURLINFO_SIZE_UPLOAD);
					PUSH_CURLINFO(CURLINFO_SIZE_DOWNLOAD);
					PUSH_CURLINFO(CURLINFO_SPEED_DOWNLOAD);
					PUSH_CURLINFO(CURLINFO_SPEED_UPLOAD);
					PUSH_CURLINFO(CURLINFO_HEADER_SIZE);
					PUSH_CURLINFO(CURLINFO_REQUEST_SIZE);
					PUSH_CURLINFO(CURLINFO_SSL_VERIFYRESULT);
					PUSH_CURLINFO(CURLINFO_FILETIME);
					PUSH_CURLINFO(CURLINFO_CONTENT_LENGTH_DOWNLOAD);
					PUSH_CURLINFO(CURLINFO_CONTENT_LENGTH_UPLOAD);
					PUSH_CURLINFO(CURLINFO_STARTTRANSFER_TIME);
					PUSH_CURLINFO(CURLINFO_CONTENT_TYPE);
					PUSH_CURLINFO(CURLINFO_REDIRECT_TIME);
					PUSH_CURLINFO(CURLINFO_REDIRECT_COUNT);
					PUSH_CURLINFO(CURLINFO_HTTP_CONNECTCODE);
					PUSH_CURLINFO(CURLINFO_HTTPAUTH_AVAIL);
					PUSH_CURLINFO(CURLINFO_PROXYAUTH_AVAIL);
					PUSH_CURLINFO(CURLINFO_OS_ERRNO);
					PUSH_CURLINFO(CURLINFO_NUM_CONNECTS);
					PUSH_CURLINFO(CURLINFO_SSL_ENGINES);
					PUSH_CURLINFO(CURLINFO_COOKIELIST);
					PUSH_CURLINFO(CURLINFO_LASTSOCKET);
					PUSH_CURLINFO(CURLINFO_FTP_ENTRY_PATH);
					PUSH_CURLINFO(CURLINFO_REDIRECT_URL);
					PUSH_CURLINFO(CURLINFO_PRIMARY_IP);
					PUSH_CURLINFO(CURLINFO_APPCONNECT_TIME);
#if LIBCURL_VERSION_NUM >= 0x071400
					PUSH_CURLINFO(CURLINFO_RTSP_SESSION_ID);
					PUSH_CURLINFO(CURLINFO_RTSP_CLIENT_CSEQ);
					PUSH_CURLINFO(CURLINFO_RTSP_SERVER_CSEQ);
					PUSH_CURLINFO(CURLINFO_RTSP_CSEQ_RECV);
#endif
#if LIBCURL_VERSION_NUM >= 0x071301
					PUSH_CURLINFO(CURLINFO_CERTINFO);
#endif
#if LIBCURL_VERSION_NUM >= 0x071304
					PUSH_CURLINFO(CURLINFO_CONDITION_UNMET);
#endif
#if LIBCURL_VERSION_NUM >= 0x071500
					PUSH_CURLINFO(CURLINFO_PRIMARY_PORT);
					PUSH_CURLINFO(CURLINFO_LOCAL_IP);
					PUSH_CURLINFO(CURLINFO_LOCAL_PORT);
#endif
				}
				return 1;
			}

			static int getVersion(OS * os, int params, int, int, void * user_param)
			{
				curl_version_info_data *d = curl_version_info(CURLVERSION_NOW);

				os->newObject();

				os->pushNumber(d->age);
				os->setProperty(-2, OS_TEXT("age"));

				d->version ? os->pushString(d->version) : os->pushNull();
				os->setProperty(-2, OS_TEXT("version"));

				os->pushNumber(d->version_num);
				os->setProperty(-2, OS_TEXT("version_num"));

				d->host ? os->pushString(d->host) : os->pushNull();
				os->setProperty(-2, OS_TEXT("host"));

				os->pushNumber(d->features);
				os->setProperty(-2, OS_TEXT("features"));

				d->ssl_version ? os->pushString(d->ssl_version) : os->pushNull();
				os->setProperty(-2, OS_TEXT("ssl_version"));

				os->pushNumber(d->ssl_version_num);
				os->setProperty(-2, OS_TEXT("ssl_version_num"));

				d->libz_version ? os->pushString(d->libz_version) : os->pushNull();
				os->setProperty(-2, OS_TEXT("libz_version"));

				os->pushNumber(d->iconv_ver_num);
				os->setProperty(-2, OS_TEXT("iconv_ver_num"));

				d->libssh_version ? os->pushString(d->libssh_version) : os->pushNull();
				os->setProperty(-2, OS_TEXT("libssh_version"));

				os->newArray();
				char **p = (char **) d->protocols;
				while (*p != NULL) {
					os->pushStackValue();
					os->pushString(*p);
					os->addProperty();
					p++;
				}
				os->setProperty(-2, OS_TEXT("protocols"));

				return 1;
			}
		};

		OS::NumberDef nums[] = {
#if (LIBCURL_VERSION_NUM >= 0x071304)
			/* numbers are for the CURLOPT_*PROTOCOLS options */
			{OS_TEXT("PROTO_HTTP"), CURLPROTO_HTTP},
			{OS_TEXT("PROTO_HTTPS"), CURLPROTO_HTTPS},
			{OS_TEXT("PROTO_FTP"), CURLPROTO_FTP},
			{OS_TEXT("PROTO_FTPS"), CURLPROTO_FTPS},
			{OS_TEXT("PROTO_SCP"), CURLPROTO_SCP},
			{OS_TEXT("PROTO_SFTP"), CURLPROTO_SFTP},
			{OS_TEXT("PROTO_TELNET"), CURLPROTO_TELNET},
			{OS_TEXT("PROTO_LDAP"), CURLPROTO_LDAP},
			{OS_TEXT("PROTO_LDAPS"), CURLPROTO_LDAPS},
			{OS_TEXT("PROTO_DICT"), CURLPROTO_DICT},
			{OS_TEXT("PROTO_FILE"), CURLPROTO_FILE},
			{OS_TEXT("PROTO_TFTP"), CURLPROTO_TFTP},
			{OS_TEXT("PROTO_IMAP"), CURLPROTO_IMAP},
			{OS_TEXT("PROTO_IMAPS"), CURLPROTO_IMAPS},
			{OS_TEXT("PROTO_POP3"), CURLPROTO_POP3},
			{OS_TEXT("PROTO_POP3S"), CURLPROTO_POP3S},
			{OS_TEXT("PROTO_SMTP"), CURLPROTO_SMTP},
			{OS_TEXT("PROTO_SMTPS"), CURLPROTO_SMTPS},
			{OS_TEXT("PROTO_RTSP"), CURLPROTO_RTSP},
			{OS_TEXT("PROTO_RTMP"), CURLPROTO_RTMP},
			{OS_TEXT("PROTO_RTMPT"), CURLPROTO_RTMPT},
			{OS_TEXT("PROTO_RTMPE"), CURLPROTO_RTMPE},
			{OS_TEXT("PROTO_RTMPTE"), CURLPROTO_RTMPTE},
			{OS_TEXT("PROTO_RTMPS"), CURLPROTO_RTMPS},
			{OS_TEXT("PROTO_RTMPTS"), CURLPROTO_RTMPTS},
//			{OS_TEXT("PROTO_GOPHER"), CURLPROTO_GOPHER},
			{OS_TEXT("PROTO_ALL"), CURLPROTO_ALL},
#endif

			/* numbers are for the CURLOPT_PROXYTYPE option */
			{OS_TEXT("PROXY_HTTP"), CURLPROXY_HTTP},
#if (LIBCURL_VERSION_NUM >= 0x071304)
			{OS_TEXT("PROXY_HTTP_1_0"), CURLPROXY_HTTP_1_0},
#endif
			{OS_TEXT("PROXY_SOCKS4"), CURLPROXY_SOCKS4},
			{OS_TEXT("PROXY_SOCKS5"), CURLPROXY_SOCKS5},
			{OS_TEXT("PROXY_SOCKS4A"), CURLPROXY_SOCKS4A},
			{OS_TEXT("PROXY_SOCKS5_HOSTNAME"), CURLPROXY_SOCKS5_HOSTNAME},


			/* numbers are for use with the CURLOPT_NETRC option */
			{OS_TEXT("NETRC_IGNORED"), CURL_NETRC_IGNORED},
			{OS_TEXT("NETRC_OPTIONAL"), CURL_NETRC_OPTIONAL},
			{OS_TEXT("NETRC_REQUIRED"), CURL_NETRC_REQUIRED},

			/* numbers are for use with the CURLOPT_HTTPAUTH option */
			{OS_TEXT("AUTH_NONE"), CURLAUTH_NONE},
			{OS_TEXT("AUTH_BASIC"), CURLAUTH_BASIC},
			{OS_TEXT("AUTH_DIGEST"), CURLAUTH_DIGEST},
#if (LIBCURL_VERSION_NUM >= 0x071303)
			{OS_TEXT("AUTH_DIGEST_IE"), CURLAUTH_DIGEST_IE},
#endif
			{OS_TEXT("AUTH_GSSNEGOTIATE"), CURLAUTH_GSSNEGOTIATE},
			{OS_TEXT("AUTH_NTLM"), CURLAUTH_NTLM},
#if (LIBCURL_VERSION_NUM >= 0x071600)
			{OS_TEXT("AUTH_NTLM_WB"), CURLAUTH_NTLM_WB},
#endif
			{OS_TEXT("AUTH_DIGEST_IE"), CURLAUTH_DIGEST_IE},
			{OS_TEXT("AUTH_ANY"), CURLAUTH_ANY},
			{OS_TEXT("AUTH_ANYSAFE"), CURLAUTH_ANYSAFE},
#if (LIBCURL_VERSION_NUM >= 0x071503)
			{OS_TEXT("AUTH_ONLY"), CURLAUTH_ONLY},
#endif

			/* numbers are for use with the CURLOPT_POSTREDIR option */
			{OS_TEXT("REDIR_GET_ALL"), CURL_REDIR_GET_ALL},
			{OS_TEXT("REDIR_POST_301"), CURL_REDIR_POST_301},
			{OS_TEXT("REDIR_POST_302"), CURL_REDIR_POST_302},
			//{OS_TEXT("REDIR_POST_303"), CURL_REDIR_POST_303},
			{OS_TEXT("REDIR_POST_ALL"), CURL_REDIR_POST_ALL},

			/* numbers are for use with the CURLOPT_HTTP_VERSION option */
			{OS_TEXT("HTTP_VERSION_NONE"), CURL_HTTP_VERSION_NONE},
			{OS_TEXT("HTTP_VERSION_1_0"), CURL_HTTP_VERSION_1_0},
			{OS_TEXT("HTTP_VERSION_1_1"), CURL_HTTP_VERSION_1_1},
			{OS_TEXT("HTTP_VERSION_LAST"), CURL_HTTP_VERSION_LAST},

			/* numbers are for use with the CURLOPT_FTPSSLAUTH option */
			{OS_TEXT("FTPAUTH_DEFAULT"),  CURLFTPAUTH_DEFAULT},
			{OS_TEXT("FTPAUTH_SSL"), CURLFTPAUTH_SSL},
			{OS_TEXT("FTPAUTH_TLS"), CURLFTPAUTH_TLS},

			/* numbers are for use with the CURLOPT_FTP_SSL_CCC option */
			{OS_TEXT("FTPSSL_CCC_NONE"), CURLFTPSSL_CCC_NONE},
			{OS_TEXT("FTPSSL_CCC_PASSIVE"), CURLFTPSSL_CCC_PASSIVE},
			{OS_TEXT("FTPSSL_CCC_ACTIVE"), CURLFTPSSL_CCC_ACTIVE},

			/* numbers are for use with the CURLOPT_FTP_FILEMETHOD option */
			{OS_TEXT("FTPMETHOD_MULTICWD"), CURLFTPMETHOD_MULTICWD},
			{OS_TEXT("FTPMETHOD_NOCWD"), CURLFTPMETHOD_NOCWD},
			{OS_TEXT("FTPMETHOD_SINGLECWD"), CURLFTPMETHOD_SINGLECWD},


			/* numbers are for use with the CURLOPT_RTSP_REQUEST option */
#if (LIBCURL_VERSION_NUM >= 0x071400)
			{OS_TEXT("RTSPREQ_OPTIONS"), CURL_RTSPREQ_OPTIONS},
			{OS_TEXT("RTSPREQ_DESCRIBE"), CURL_RTSPREQ_DESCRIBE},
			{OS_TEXT("RTSPREQ_ANNOUNCE"), CURL_RTSPREQ_ANNOUNCE},
			{OS_TEXT("RTSPREQ_SETUP"), CURL_RTSPREQ_SETUP},
			{OS_TEXT("RTSPREQ_PLAY"), CURL_RTSPREQ_PLAY},
			{OS_TEXT("RTSPREQ_PAUSE"), CURL_RTSPREQ_PAUSE},
			{OS_TEXT("RTSPREQ_TEARDOWN"), CURL_RTSPREQ_TEARDOWN},
			{OS_TEXT("RTSPREQ_GET_PARAMETER"), CURL_RTSPREQ_GET_PARAMETER},
			{OS_TEXT("RTSPREQ_SET_PARAMETER"), CURL_RTSPREQ_SET_PARAMETER},
			{OS_TEXT("RTSPREQ_RECORD"), CURL_RTSPREQ_RECORD},
			{OS_TEXT("RTSPREQ_RECEIVE"), CURL_RTSPREQ_RECEIVE},
#endif

			/* numbers are for use with the CURLOPT_IPRESOLVE option */
			{OS_TEXT("IPRESOLVE_WHATEVER"), CURL_IPRESOLVE_WHATEVER},
			{OS_TEXT("IPRESOLVE_V4"), CURL_IPRESOLVE_V4},
			{OS_TEXT("IPRESOLVE_V6"), CURL_IPRESOLVE_V6},

			/* numbers are for use with the CURLOPT_USE_SSL option */
			{OS_TEXT("USESSL_NONE"), CURLUSESSL_NONE},
			{OS_TEXT("USESSL_TRY"), CURLUSESSL_TRY},
			{OS_TEXT("USESSL_CONTROL"), CURLUSESSL_CONTROL},
			{OS_TEXT("USESSL_ALL"), CURLUSESSL_ALL},

			/* numbers are for use with the CURLOPT_SSLVERSION option */
			{OS_TEXT("SSLVERSION_DEFAULT"), CURL_SSLVERSION_DEFAULT},
			{OS_TEXT("SSLVERSION_TLSv1"), CURL_SSLVERSION_TLSv1},
			{OS_TEXT("SSLVERSION_SSLv2"), CURL_SSLVERSION_SSLv2},
			{OS_TEXT("SSLVERSION_SSLv3"), CURL_SSLVERSION_SSLv3},

			/* numbers are for use with the CURLOPT_SSH_KEYFUNCTION option */
#if (LIBCURL_VERSION_NUM >= 0x071306)
			{OS_TEXT("KHSTAT_FINE_ADD_TO_FILE"), CURLKHSTAT_FINE_ADD_TO_FILE},
			{OS_TEXT("KHSTAT_FINE"), CURLKHSTAT_FINE},
			{OS_TEXT("KHSTAT_REJECT"), CURLKHSTAT_REJECT},
			{OS_TEXT("KHSTAT_DEFER"), CURLKHSTAT_DEFER},
#endif

			/* debug types */
			{OS_TEXT("INFO_TEXT"), CURLINFO_TEXT},
			{OS_TEXT("INFO_HEADER_IN"), CURLINFO_HEADER_IN},
			{OS_TEXT("INFO_HEADER_OUT"), CURLINFO_HEADER_OUT},
			{OS_TEXT("INFO_DATA_IN"), CURLINFO_DATA_IN},
			{OS_TEXT("INFO_DATA_OUT"), CURLINFO_DATA_OUT},
			{OS_TEXT("INFO_SSL_DATA_IN"), CURLINFO_SSL_DATA_IN},
			{OS_TEXT("INFO_SSL_DATA_OUT"), CURLINFO_SSL_DATA_OUT},

			/* ioctl callback return codes */
			{OS_TEXT("IOE_UNKNOWNCMD"), CURLIOE_UNKNOWNCMD},
			{OS_TEXT("IOE_FAILRESTART"), CURLIOE_FAILRESTART},

			/* all possible error codes from all sorts of curl functions */
			{OS_TEXT("UNSUPPORTED_PROTOCOL"), CURLE_UNSUPPORTED_PROTOCOL},
			{OS_TEXT("FAILED_INIT"), CURLE_FAILED_INIT},
			{OS_TEXT("URL_MALFORMAT"), CURLE_URL_MALFORMAT},
#if (LIBCURL_VERSION_NUM >= 0x071505)
			{OS_TEXT("NOT_BUILT_IN"), CURLE_NOT_BUILT_IN},
#endif
			{OS_TEXT("COULDNT_RESOLVE_PROXY"), CURLE_COULDNT_RESOLVE_PROXY},
			{OS_TEXT("COULDNT_RESOLVE_HOST"), CURLE_COULDNT_RESOLVE_HOST},
			{OS_TEXT("COULDNT_CONNECT"), CURLE_COULDNT_CONNECT},
			{OS_TEXT("FTP_WEIRD_SERVER_REPLY"), CURLE_FTP_WEIRD_SERVER_REPLY},
			{OS_TEXT("REMOTE_ACCESS_DENIED"), CURLE_REMOTE_ACCESS_DENIED},
#if (LIBCURL_VERSION_NUM >= 0x071800)
			{OS_TEXT("FTP_ACCEPT_FAILED"), CURLE_FTP_ACCEPT_FAILED},
			{OS_TEXT("FTP_ACCEPT_TIMEOUT"), CURLE_FTP_ACCEPT_TIMEOUT},
#endif
			{OS_TEXT("FTP_WEIRD_PASS_REPLY"), CURLE_FTP_WEIRD_PASS_REPLY},
			{OS_TEXT("FTP_WEIRD_PASV_REPLY"), CURLE_FTP_WEIRD_PASV_REPLY},
			{OS_TEXT("FTP_WEIRD_227_FORMAT"), CURLE_FTP_WEIRD_227_FORMAT},
			{OS_TEXT("FTP_CANT_GET_HOST"), CURLE_FTP_CANT_GET_HOST},
			{OS_TEXT("FTP_COULDNT_SET_TYPE"), CURLE_FTP_COULDNT_SET_TYPE},
			{OS_TEXT("PARTIAL_FILE"), CURLE_PARTIAL_FILE},
			{OS_TEXT("FTP_COULDNT_RETR_FILE"), CURLE_FTP_COULDNT_RETR_FILE},
			{OS_TEXT("QUOTE_ERROR"), CURLE_QUOTE_ERROR},
			{OS_TEXT("HTTP_RETURNED_ERROR"), CURLE_HTTP_RETURNED_ERROR},
			{OS_TEXT("WRITE_ERROR"), CURLE_WRITE_ERROR},
			{OS_TEXT("UPLOAD_FAILED"), CURLE_UPLOAD_FAILED},
			{OS_TEXT("READ_ERROR"), CURLE_READ_ERROR},
			{OS_TEXT("OUT_OF_MEMORY"), CURLE_OUT_OF_MEMORY},
			{OS_TEXT("OPERATION_TIMEDOUT"), CURLE_OPERATION_TIMEDOUT},
			{OS_TEXT("FTP_PORT_FAILED"), CURLE_FTP_PORT_FAILED},
			{OS_TEXT("FTP_COULDNT_USE_REST"), CURLE_FTP_COULDNT_USE_REST},
			{OS_TEXT("RANGE_ERROR"), CURLE_RANGE_ERROR},
			{OS_TEXT("HTTP_POST_ERROR"), CURLE_HTTP_POST_ERROR},
			{OS_TEXT("SSL_CONNECT_ERROR"), CURLE_SSL_CONNECT_ERROR},
			{OS_TEXT("BAD_DOWNLOAD_RESUME"), CURLE_BAD_DOWNLOAD_RESUME},
			{OS_TEXT("FILE_COULDNT_READ_FILE"), CURLE_FILE_COULDNT_READ_FILE},
			{OS_TEXT("LDAP_CANNOT_BIND"), CURLE_LDAP_CANNOT_BIND},
			{OS_TEXT("LDAP_SEARCH_FAILED"), CURLE_LDAP_SEARCH_FAILED},
			{OS_TEXT("FUNCTION_NOT_FOUND"), CURLE_FUNCTION_NOT_FOUND},
			{OS_TEXT("ABORTED_BY_CALLBACK"), CURLE_ABORTED_BY_CALLBACK},
			{OS_TEXT("BAD_FUNCTION_ARGUMENT"), CURLE_BAD_FUNCTION_ARGUMENT},
			{OS_TEXT("INTERFACE_FAILED"), CURLE_INTERFACE_FAILED},
			{OS_TEXT("TOO_MANY_REDIRECTS"), CURLE_TOO_MANY_REDIRECTS},
			{OS_TEXT("UNKNOWN_OPTION"), CURLE_UNKNOWN_OPTION},
			{OS_TEXT("TELNET_OPTION_SYNTAX"), CURLE_TELNET_OPTION_SYNTAX},
			{OS_TEXT("PEER_FAILED_VERIFICATION"), CURLE_PEER_FAILED_VERIFICATION},
			{OS_TEXT("GOT_NOTHING"), CURLE_GOT_NOTHING},
			{OS_TEXT("SSL_ENGINE_NOTFOUND"), CURLE_SSL_ENGINE_NOTFOUND},
			{OS_TEXT("SSL_ENGINE_SETFAILED"), CURLE_SSL_ENGINE_SETFAILED},
			{OS_TEXT("SEND_ERROR"), CURLE_SEND_ERROR},
			{OS_TEXT("RECV_ERROR"), CURLE_RECV_ERROR},
			{OS_TEXT("SSL_CERTPROBLEM"), CURLE_SSL_CERTPROBLEM},
			{OS_TEXT("SSL_CIPHER"), CURLE_SSL_CIPHER},
			{OS_TEXT("SSL_CACERT"), CURLE_SSL_CACERT},
			{OS_TEXT("BAD_CONTENT_ENCODING"), CURLE_BAD_CONTENT_ENCODING},
			{OS_TEXT("LDAP_INVALID_URL"), CURLE_LDAP_INVALID_URL},
			{OS_TEXT("FILESIZE_EXCEEDED"), CURLE_FILESIZE_EXCEEDED},
			{OS_TEXT("USE_SSL_FAILED"), CURLE_USE_SSL_FAILED},
			{OS_TEXT("SEND_FAIL_REWIND"), CURLE_SEND_FAIL_REWIND},
			{OS_TEXT("SSL_ENGINE_INITFAILED"), CURLE_SSL_ENGINE_INITFAILED},
			{OS_TEXT("LOGIN_DENIED"), CURLE_LOGIN_DENIED},
			{OS_TEXT("TFTP_NOTFOUND"), CURLE_TFTP_NOTFOUND},
			{OS_TEXT("TFTP_PERM"), CURLE_TFTP_PERM},
			{OS_TEXT("REMOTE_DISK_FULL"), CURLE_REMOTE_DISK_FULL},
			{OS_TEXT("TFTP_ILLEGAL"), CURLE_TFTP_ILLEGAL},
			{OS_TEXT("TFTP_UNKNOWNID"), CURLE_TFTP_UNKNOWNID},
			{OS_TEXT("REMOTE_FILE_EXISTS"), CURLE_REMOTE_FILE_EXISTS},
			{OS_TEXT("TFTP_NOSUCHUSER"), CURLE_TFTP_NOSUCHUSER},
			{OS_TEXT("CONV_FAILED"), CURLE_CONV_FAILED},
			{OS_TEXT("CONV_REQD"), CURLE_CONV_REQD},
			{OS_TEXT("SSL_CACERT_BADFILE"), CURLE_SSL_CACERT_BADFILE},
			{OS_TEXT("REMOTE_FILE_NOT_FOUND"), CURLE_REMOTE_FILE_NOT_FOUND},
			{OS_TEXT("SSH"), CURLE_SSH},
			{OS_TEXT("SSL_SHUTDOWN_FAILED"), CURLE_SSL_SHUTDOWN_FAILED},
			{OS_TEXT("AGAIN"), CURLE_AGAIN},
			{OS_TEXT("SSL_CRL_BADFILE"), CURLE_SSL_CRL_BADFILE},
			{OS_TEXT("SSL_ISSUER_ERROR"), CURLE_SSL_ISSUER_ERROR},
#if (LIBCURL_VERSION_NUM >= 0x071400)
			{OS_TEXT("FTP_PRET_FAILED"), CURLE_FTP_PRET_FAILED},
			{OS_TEXT("RTSP_CSEQ_ERROR"), CURLE_RTSP_CSEQ_ERROR},
			{OS_TEXT("RTSP_SESSION_ERROR"), CURLE_RTSP_SESSION_ERROR},
			{OS_TEXT("FTP_BAD_FILE_LIST"), CURLE_FTP_BAD_FILE_LIST},
			{OS_TEXT("CHUNK_FAILED"), CURLE_CHUNK_FAILED},
#endif

			/* version feature bits */
			{OS_TEXT("FEATURE_IPV6"), CURL_VERSION_IPV6},
			{OS_TEXT("FEATURE_KERBEROS4"), CURL_VERSION_KERBEROS4},
			{OS_TEXT("FEATURE_SSL"), CURL_VERSION_SSL},
			{OS_TEXT("FEATURE_LIBZ"), CURL_VERSION_LIBZ},
			{OS_TEXT("FEATURE_NTLM"), CURL_VERSION_NTLM},
			{OS_TEXT("FEATURE_GSSNEGOTIATE"), CURL_VERSION_GSSNEGOTIATE},
			{OS_TEXT("FEATURE_DEBUG"), CURL_VERSION_DEBUG},
			{OS_TEXT("FEATURE_ASYNCHDNS"), CURL_VERSION_ASYNCHDNS},
			{OS_TEXT("FEATURE_SPNEGO"), CURL_VERSION_SPNEGO},
			{OS_TEXT("FEATURE_LARGEFILE"), CURL_VERSION_LARGEFILE},
			{OS_TEXT("FEATURE_IDN"), CURL_VERSION_IDN},
			{OS_TEXT("FEATURE_SSPI"), CURL_VERSION_SSPI},
			{OS_TEXT("FEATURE_CONV"), CURL_VERSION_CONV},
#if (LIBCURL_VERSION_NUM >= 0x071306)
			{OS_TEXT("FEATURE_CURLDEBUG"), CURL_VERSION_CURLDEBUG},
#endif
#if (LIBCURL_VERSION_NUM >= 0x071504)
			{OS_TEXT("FEATURE_TLSAUTH_SRP"), CURL_VERSION_TLSAUTH_SRP},
#endif
#if (LIBCURL_VERSION_NUM >= 0x071600)
			{OS_TEXT("FEATURE_NTLM_WB"), CURL_VERSION_NTLM_WB},
#endif
			{}
		};

		OS::FuncDef funcs[] = {
			{OS_TEXT("__newinstance"), Lib::__newinstance},
			{OS_TEXT("clone"), Lib::clone},
			def(OS_TEXT("close"), &CurlOS::Curl::close),
			def(OS_TEXT("reset"), &CurlOS::Curl::reset),
			{OS_TEXT("perform"), Lib::perform},
			{OS_TEXT("options"), Lib::options},
			{OS_TEXT("getInfo"), Lib::getInfo},
			{OS_TEXT("__get@VERSION"), Lib::getVersion},
			{}
		};

		registerUserClass<Curl>(os, funcs, nums);

#define OS_AUTO_TEXT(exp) OS_TEXT(#exp)
		os->eval(OS_AUTO_TEXT(
			CurlException = extends Exception {
				__construct = function(msg, code){
					super(msg)
					@code = code
				}
			}
		));
	}

	/*===================================================*/
	/* Curl Share Interfase                              */
	/*===================================================*/
	/*
	CurlOS::CurlShare::CurlShare(OS *p_os)
		: os(p_os)
		, share_handle(NULL)
	{
	}

	CurlOS::CurlShare::~CurlShare()
	{
		close();
	}

	bool CurlOS::CurlShare::init()
	{
		share_handle = curl_share_init();
		if (share_handle == NULL)
			return false;

		CURLSHcode res;

#undef CURL_SHARE_SETOPT
#define CURL_SHARE_SETOPT(o,v)				\
	res = curl_share_setopt(share_handle,(o),(v));	\
	if(res != CURLSHE_OK) {				\
	close();					\
	return false;					\
	}

		CURL_SHARE_SETOPT(CURLSHOPT_LOCKFUNC, lockCallback);
		CURL_SHARE_SETOPT(CURLSHOPT_UNLOCKFUNC, unlockCallback);
		CURL_SHARE_SETOPT(CURLSHOPT_USERDATA, (void*)this);

#undef CURL_SHARE_SETOPT

		return true;
	}

	void CurlOS::CurlShare::close()
	{
		if (share_handle) {
			curl_share_cleanup(share_handle);
			share_handle = NULL;

			for (int i = 0; i < CURL_LOCK_DATA_LAST; ++i) {
				//unlock
			}
		}
	}

	void CurlOS::CurlShare::lockCallback(CURL *handle, curl_lock_data data, curl_lock_access access, void *userptr)
	{
		CurlShare *share = (CurlShare *)userptr;
		// lock data
	}

	void CurlOS::CurlShare::unlockCallback(CURL *handle, curl_lock_data data, void *userptr)
	{
		CurlOS::CurlShare *share = (CurlOS::CurlShare *)userptr;
		// unlock data
	}

	OS_DECL_CTYPE(CURLSHoption);

	template <>
	struct CtypeValue<CURLSHoption>
	{
		typedef CURLSHoption type;

		static bool isValid(type option)
		{
			if (option == CURLSHOPT_SHARE || option == CURLSHOPT_UNSHARE)
				return true;
			return false;
		}

		static type def(ObjectScript::OS * os) { return (type)0; }
		static type getArg(ObjectScript::OS * os, int offs)
		{
			ObjectScript::OS::String str = os->toString(offs);
			if(str == "share") return CURLSHOPT_SHARE;
			if(str == "unshare") return CURLSHOPT_UNSHARE;
			return (type)0;
		}

		static void push(ObjectScript::OS * os, type val)
		{
			switch(val) {
			case CURLSHOPT_SHARE: os->pushString("share"); return;
			case CURLSHOPT_UNSHARE: os->pushString("unshare"); return;
			}
			os->pushString("unknown");
		}
	};

	template <> struct UserDataDestructor<CurlOS::CurlShare>
	{
		static void dtor(ObjectScript::OS * os, void * data, void * user_param)
		{
			OS_ASSERT(data && dynamic_cast<CurlOS::CurlShare*>((CurlOS::CurlShare*)data));
			CurlOS::CurlShare * buf = (CurlOS::CurlShare*)data;
			buf->~CurlShare();
			os->free(buf);
		}
	};

	void CurlOS::CurlShare::initCurlShareExtension(OS *os)
	{
		struct Lib
		{
			static int __construct(OS * os, int params, int, int, void * user_param)
			{
				if (params) {
					os->setException("wrong arguments");
					return 0;
				}

				CurlShare * self = new (os->malloc(sizeof(CurlShare) OS_DBG_FILEPOS)) CurlShare(os);
				if (!self->init()) {
					os->setException("could not init curl share object");
					self->~CurlShare();
					os->free(self);
					return 0;
				}

				CtypeValue<CurlShare*>::push(os, self);
				return 1;
			}

			static int errorStr(OS * os, int params, int, int, void * user_param)
			{
				if (params != 1) {
					os->setException("wrong arguments");
					return 0;
				}

				OS_GET_SELF(CurlShare*);
				const char *msg = curl_share_strerror((CURLSHcode)os->popInt());
				msg ? os->pushString(OS::String(os, msg)) : os->pushNull();
				return 1;
			}

			static int options(OS * os, int params, int, int, void * user_param)
			{
				CURLSHcode res = CURLSHE_OK;
				OS_GET_SELF(CurlShare*);

				if (params == 1 && os->isObject(-params+0)) {
					while (os->nextIteratorStep()) {
						CURLSHoption opt = CtypeValue<CURLSHoption>::getArg(os, -2);
						if (CtypeValue<CURLSHoption>::isValid(opt))
							res = curl_share_setopt(self->share_handle, opt, os->toNumber());
						os->pop(2);
					}
				} else if (params == 2) {
					CURLSHoption opt = CtypeValue<CURLSHoption>::getArg(os, -params+0);
					if (CtypeValue<CURLSHoption>::isValid(opt))
						res = curl_share_setopt(self->share_handle, opt, os->toNumber(-params+1));
				} else {
					os->setException("wrong arguments");
					return 0;
				}
				if(res != CURLSHE_OK){
					const char * msg = curl_share_strerror(res);
					os->setException(msg ? msg : "unexpected error");
					return 0;
				}
				CtypeValue<CurlShare*>::push(os, self);
				return 1;
			}
		};

		OS::FuncDef funcs[] = {
			{OS_TEXT("__construct"), Lib::__construct},
			{OS_TEXT("options"), Lib::options},
			{OS_TEXT("errorStr"), Lib::errorStr},
			{}
		};

		OS::NumberDef nums[] = {
			{OS_TEXT("LOCK_COOKIE"), CURL_LOCK_DATA_COOKIE},
			{OS_TEXT("LOCK_DNS"), CURL_LOCK_DATA_DNS},
			{OS_TEXT("LOCK_SSL_SESSION"), CURL_LOCK_DATA_SSL_SESSION},
			{}
		};

		registerUserClass<CurlShare>(os, funcs, nums);
	}
	*/

} // namespace ObjectScript

