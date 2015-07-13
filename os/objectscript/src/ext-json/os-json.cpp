#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "os-json.h"
#include "../objectscript.h"
#include "../os-binder.h"

// #include "JSON_parser.h"

namespace ObjectScript {

#define JSON_PARSER_DEFAULT_DEPTH 512

typedef struct JSON_parser_struct {
	ObjectScript::OS * os;
	int state;
	int depth;
	int top;
	int error_code;
	int* stack;
	int *the_zstack;
	int the_static_zstack[JSON_PARSER_DEFAULT_DEPTH];
} * JSON_parser;

enum error_codes {
	OS_JSON_ERROR_NONE = 0,
	OS_JSON_ERROR_DEPTH, 
	OS_JSON_ERROR_STATE_MISMATCH,  
	OS_JSON_ERROR_CTRL_CHAR,   
	OS_JSON_ERROR_SYNTAX,
	OS_JSON_ERROR_UTF8
};

enum {
	OS_JSON_OBJECT_AS_ARRAY = 1
};

enum {
	// IS_LONG = 1,
	IS_ARRAY = OS_VALUE_TYPE_ARRAY,
	IS_OBJECT = OS_VALUE_TYPE_OBJECT,
	IS_NUMBER = OS_VALUE_TYPE_NUMBER,
	IS_STRING = OS_VALUE_TYPE_STRING,
	IS_BOOL = OS_VALUE_TYPE_BOOL,
	IS_NULL = OS_VALUE_TYPE_NULL
};

#define OS_JSON_HEX_TAG	(1<<0)
#define OS_JSON_HEX_AMP	(1<<1)
#define OS_JSON_HEX_APOS	(1<<2)
#define OS_JSON_HEX_QUOT	(1<<3)
#define OS_JSON_FORCE_OBJECT	(1<<4)
#define OS_JSON_NUMERIC_CHECK	(1<<5)
#define OS_JSON_UNESCAPED_SLASHES	(1<<6)
#define OS_JSON_PRETTY_PRINT	(1<<7)
#define OS_JSON_UNESCAPED_UNICODE	(1<<8)

#define __   -1     /* the universal error code */
#define SUCCESS 0
#define FAILURE -1

/*
Characters are mapped into these 31 character classes. This allows for
a significant reduction in the size of the state transition table.
*/

enum classes {
	C_SPACE,  /* space */
	C_WHITE,  /* other whitespace */
	C_LCURB,  /* {  */
	C_RCURB,  /* } */
	C_LSQRB,  /* [ */
	C_RSQRB,  /* ] */
	C_COLON,  /* : */
	C_COMMA,  /* , */
	C_QUOTE,  /* " */
	C_BACKS,  /* \ */
	C_SLASH,  /* / */
	C_PLUS,   /* + */
	C_MINUS,  /* - */
	C_POINT,  /* . */
	C_ZERO ,  /* 0 */
	C_DIGIT,  /* 123456789 */
	C_LOW_A,  /* a */
	C_LOW_B,  /* b */
	C_LOW_C,  /* c */
	C_LOW_D,  /* d */
	C_LOW_E,  /* e */
	C_LOW_F,  /* f */
	C_LOW_L,  /* l */
	C_LOW_N,  /* n */
	C_LOW_R,  /* r */
	C_LOW_S,  /* s */
	C_LOW_T,  /* t */
	C_LOW_U,  /* u */
	C_ABCDF,  /* ABCDF */
	C_E,      /* E */
	C_ETC,    /* everything else */
	NR_CLASSES
};

static const int ascii_class[128] = {
	/*
	This array maps the 128 ASCII characters into character classes.
	The remaining Unicode characters should be mapped to C_ETC.
	Non-whitespace control characters are errors.
	*/
	__,      __,      __,      __,      __,      __,      __,      __,
	__,      C_WHITE, C_WHITE, __,      __,      C_WHITE, __,      __,
	__,      __,      __,      __,      __,      __,      __,      __,
	__,      __,      __,      __,      __,      __,      __,      __,

	C_SPACE, C_ETC,   C_QUOTE, C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,
	C_ETC,   C_ETC,   C_ETC,   C_PLUS,  C_COMMA, C_MINUS, C_POINT, C_SLASH,
	C_ZERO,  C_DIGIT, C_DIGIT, C_DIGIT, C_DIGIT, C_DIGIT, C_DIGIT, C_DIGIT,
	C_DIGIT, C_DIGIT, C_COLON, C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,

	C_ETC,   C_ABCDF, C_ABCDF, C_ABCDF, C_ABCDF, C_E,     C_ABCDF, C_ETC,
	C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,
	C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,
	C_ETC,   C_ETC,   C_ETC,   C_LSQRB, C_BACKS, C_RSQRB, C_ETC,   C_ETC,

	C_ETC,   C_LOW_A, C_LOW_B, C_LOW_C, C_LOW_D, C_LOW_E, C_LOW_F, C_ETC,
	C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_LOW_L, C_ETC,   C_LOW_N, C_ETC,
	C_ETC,   C_ETC,   C_LOW_R, C_LOW_S, C_LOW_T, C_LOW_U, C_ETC,   C_ETC,
	C_ETC,   C_ETC,   C_ETC,   C_LCURB, C_ETC,   C_RCURB, C_ETC,   C_ETC
};


/*
The state codes.
*/
enum states {
	GO,  /* start    */
	OK,  /* ok       */
	OB,  /* object   */
	KE,  /* key      */
	CO,  /* colon    */
	VA,  /* value    */
	AR,  /* array    */
	ST,  /* string   */
	ES,  /* escape   */
	U1,  /* u1       */
	U2,  /* u2       */
	U3,  /* u3       */
	U4,  /* u4       */
	MI,  /* minus    */
	ZE,  /* zero     */
	IN,  /* integer  */
	FR,  /* fraction */
	E1,  /* e        */
	E2,  /* ex       */
	E3,  /* exp      */
	T1,  /* tr       */
	T2,  /* tru      */
	T3,  /* true     */
	F1,  /* fa       */
	F2,  /* fal      */
	F3,  /* fals     */
	F4,  /* false    */
	N1,  /* nu       */
	N2,  /* nul      */
	N3,  /* null     */
	NR_STATES
};


static const int state_transition_table[NR_STATES][NR_CLASSES] = {
	/*
	The state transition table takes the current state and the current symbol,
	and returns either a new state or an action. An action is represented as a
	negative number. A JSON text is accepted if at the end of the text the
	state is OK and if the mode is MODE_DONE.

	white                                      1-9                                   ABCDF  etc
	space |  {  }  [  ]  :  ,  "  \  /  +  -  .  0  |  a  b  c  d  e  f  l  n  r  s  t  u  |  E  |*/
	/*start  GO*/ {GO,GO,-6,__,-5,__,__,__,ST,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__},
	/*ok     OK*/ {OK,OK,__,-8,__,-7,__,-3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__},
	/*object OB*/ {OB,OB,__,-9,__,__,__,__,ST,__,__,__,__,__,IN,IN,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__},
	/*key    KE*/ {KE,KE,__,__,__,__,__,__,ST,__,__,__,__,__,IN,IN,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__},
	/*colon  CO*/ {CO,CO,__,__,__,__,-2,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__},
	/*value  VA*/ {VA,VA,-6,__,-5,__,__,__,ST,__,__,__,MI,__,ZE,IN,__,__,__,__,__,F1,__,N1,__,__,T1,__,__,__,__},
	/*array  AR*/ {AR,AR,-6,__,-5,-7,__,__,ST,__,__,__,MI,__,ZE,IN,__,__,__,__,__,F1,__,N1,__,__,T1,__,__,__,__},
	/*string ST*/ {ST,__,ST,ST,ST,ST,ST,ST,-4,ES,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST},
	/*escape ES*/ {__,__,__,__,__,__,__,__,ST,ST,ST,__,__,__,__,__,__,ST,__,__,__,ST,__,ST,ST,__,ST,U1,__,__,__},
	/*u1     U1*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,U2,U2,U2,U2,U2,U2,U2,U2,__,__,__,__,__,__,U2,U2,__},
	/*u2     U2*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,U3,U3,U3,U3,U3,U3,U3,U3,__,__,__,__,__,__,U3,U3,__},
	/*u3     U3*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,U4,U4,U4,U4,U4,U4,U4,U4,__,__,__,__,__,__,U4,U4,__},
	/*u4     U4*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,ST,ST,ST,ST,ST,ST,ST,ST,__,__,__,__,__,__,ST,ST,__},
	/*minus  MI*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,ZE,IN,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__},
	/*zero   ZE*/ {OK,OK,__,-8,__,-7,__,-3,__,__,__,__,__,FR,__,__,__,__,__,__,E1,__,__,__,__,__,__,__,__,E1,__},
	/*int    IN*/ {OK,OK,__,-8,__,-7,-10,-3,__,__,__,__,__,FR,IN,IN,__,__,__,__,E1,__,__,__,__,__,__,__,__,E1,__},
	/*frac   FR*/ {OK,OK,__,-8,__,-7,-10,-3,__,__,__,__,__,__,FR,FR,__,__,__,__,E1,__,__,__,__,__,__,__,__,E1,__},
	/*e      E1*/ {__,__,__,__,__,__,__,__,__,__,__,E2,E2,__,E3,E3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__},
	/*ex     E2*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,E3,E3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__},
	/*exp    E3*/ {OK,OK,__,-8,__,-7,__,-3,__,__,__,__,__,__,E3,E3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__},
	/*tr     T1*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,T2,__,__,__,__,__,__},
	/*tru    T2*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,T3,__,__,__},
	/*true   T3*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,OK,__,__,__,__,__,__,__,__,__,__},
	/*fa     F1*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,F2,__,__,__,__,__,__,__,__,__,__,__,__,__,__},
	/*fal    F2*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,F3,__,__,__,__,__,__,__,__},
	/*fals   F3*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,F4,__,__,__,__,__},
	/*false  F4*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,OK,__,__,__,__,__,__,__,__,__,__},
	/*nu     N1*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,N2,__,__,__},
	/*nul    N2*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,N3,__,__,__,__,__,__,__,__},
	/*null   N3*/ {__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,OK,__,__,__,__,__,__,__,__},
};


/*
These modes can be pushed on the stack.
*/
enum modes {
	MODE_ARRAY, 
	MODE_DONE,  
	MODE_KEY,   
	MODE_OBJECT,
};

static const OS_CHAR digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";

class JsonOS: public OS
{
public:

	/*
	Push a mode onto the stack. Return false if there is overflow.
	*/
	static int push(JSON_parser jp, int mode)
	{
		jp->top += 1;
		if (jp->top >= jp->depth) {
			jp->error_code = OS_JSON_ERROR_DEPTH;
			return false;
		}
		jp->stack[jp->top] = mode;
		return true;
	}

	/*
	Pop the stack, assuring that the current mode matches the expectation.
	Return false if there is underflow or if the modes mismatch.
	*/
	static int pop(JSON_parser jp, int mode, bool release)
	{
		if (jp->top < 0 || jp->stack[jp->top] != mode) {
			jp->error_code = OS_JSON_ERROR_STATE_MISMATCH;
			return false;
		}
		if(release){
			jp->os->releaseValueById(jp->the_zstack[jp->top]);
			jp->the_zstack[jp->top] = 0;
		}
		jp->top -= 1;
		return true;
	}

	static JSON_parser newJSONparser(OS * os, int depth)
	{
		JSON_parser jp = (JSON_parser)os->malloc(sizeof(struct JSON_parser_struct) OS_DBG_FILEPOS);
		jp->os = os;
		jp->state = GO;
		jp->depth = depth;
		jp->top = -1;
		jp->error_code = OS_JSON_ERROR_NONE;
		jp->stack = (int*)os->malloc(depth * sizeof(int) OS_DBG_FILEPOS);
		OS_MEMSET(jp->stack, 0, depth * sizeof(int));
		if (depth > JSON_PARSER_DEFAULT_DEPTH) {
			jp->the_zstack = (int*)os->malloc(depth * sizeof(int) OS_DBG_FILEPOS);
		} else {
			jp->the_zstack = &jp->the_static_zstack[0];
		}
		push(jp, MODE_DONE);
		return jp;
	}

	/*
	Delete the JSON_parser object.
	*/
	static int freeJSONparser(JSON_parser jp)
	{
		jp->os->free((void*)jp->stack);
		while(jp->top >= 0){
			jp->os->releaseValueById(jp->the_zstack[jp->top--]);
		}
		if (jp->the_zstack != &jp->the_static_zstack[0]) {
			jp->os->free(jp->the_zstack);
		}
		jp->os->free((void*)jp);
		return false;
	}

	static int dehexChar(char c)
	{
		if (c >= '0' && c <= '9')
		{
			return c - '0';
		}
		else if (c >= 'A' && c <= 'F')
		{
			return c - ('A' - 10);
		}
		else if (c >= 'a' && c <= 'f')
		{
			return c - ('a' - 10);
		}
		else
		{
			return -1;
		}
	}

	static void pushValue(OS * os, Core::Buffer& buf, int type, int options)
	{
		if (type == IS_NUMBER)
		{
			os->pushNumber(Utils::strToFloat(buf.toString().toChar()));
		}
		else if (type == IS_STRING)
		{
			os->pushString(buf.toString());
		}
		else if (type == IS_BOOL)
		{
			os->pushBool((*(buf.buffer.buf) == 't'));
		}
		else /* type == IS_NULL) || type unknown */
		{
			os->pushNull();
		}
	}

	static void popValue(OS * os)
	{
		os->pop();
	}

	static void utf16_to_utf8(Core::Buffer& buf, unsigned short utf16)
	{
		if (utf16 < 0x80)
		{
			buf.append((char)utf16);
		}
		else if (utf16 < 0x800)
		{
			buf.append((char)(0xc0 | (utf16 >> 6)));
			buf.append((char)(0x80 | (utf16 & 0x3f)));
		}
		else if ((utf16 & 0xfc00) == 0xdc00
			&& buf.buffer.count >= 3
			&& ((unsigned char) buf.buffer.buf[buf.buffer.count - 3]) == 0xed
			&& ((unsigned char) buf.buffer.buf[buf.buffer.count - 2] & 0xf0) == 0xa0
			&& ((unsigned char) buf.buffer.buf[buf.buffer.count - 1] & 0xc0) == 0x80)
		{
			/* found surrogate pair */
			unsigned long utf32;

			utf32 = (((buf.buffer.buf[buf.buffer.count - 2] & 0xf) << 16)
				| ((buf.buffer.buf[buf.buffer.count - 1] & 0x3f) << 10)
				| (utf16 & 0x3ff)) + 0x10000;
			buf.buffer.count -= 3;

			buf.append((char)(0xf0 | (utf32 >> 18)));
			buf.append((char)(0x80 | ((utf32 >> 12) & 0x3f)));
			buf.append((char)(0x80 | ((utf32 >> 6) & 0x3f)));
			buf.append((char)(0x80 | (utf32 & 0x3f)));
		}
		else
		{
			buf.append((char)(0xe0 | (utf16 >> 12)));
			buf.append((char)(0x80 | ((utf16 >> 6) & 0x3f)));
			buf.append((char)(0x80 | (utf16 & 0x3f)));
		}
	}

	static void releaseValue(OS * os, int id)
	{
		os->releaseValueById(id);
	}

	static void retainValue(OS * os, int id)
	{
		os->retainValueById(id);
	}

	static void addNextIndexValue(OS * os, int root)
	{
		os->pushValueById(root);
		os->pushStackValue(-2);
		os->addProperty();
		// releaseValue(os, child);
	}

	static void addPropertyValue(OS * os, int root, const char * key)
	{
		os->pushValueById(root);
		os->pushStackValue(-2);
		os->setProperty(key, false);
	}

	static void addValue(OS * os, int root, OS_NUMBER key)
	{
		os->pushValueById(root);
		os->pushNumber(key);
		os->pushStackValue(-3);
		os->setProperty(false);
	}

	static void addAssocValue(OS * os, int root, const char * key)
	{
		addPropertyValue(os, root, key);
		// releaseValue(os, child);
	}

	static void attachValue(JSON_parser jp, int up, int cur, Core::Buffer& key, int key_type, bool assoc)
	{
		int root = jp->the_zstack[up];
		int child =  jp->the_zstack[cur];
		int up_mode = jp->stack[up];

		jp->os->pushValueById(child);
		if (up_mode == MODE_ARRAY)
		{
			addNextIndexValue(jp->os, root);
		}
		else if (up_mode == MODE_OBJECT)
		{
			Core::String str = key.toString();
			if(key_type == IS_NUMBER)
				addValue(jp->os, root, Utils::strToFloat(str.toChar()));
			else if (!assoc)
			{
				addPropertyValue(jp->os, root, str.isEmpty() ? "_empty_" : str.toChar());
				// releaseValue(jp->os, child);
			}
			else
			{
				addAssocValue(jp->os, root, str.toChar());
			}
			key.clear();
		}
		jp->os->pop();
	}

#define FREE_BUFFERS() buf.clear(); key.clear();
#define SWAP_BUFFERS(from, to) from.swap(to)
#define JSON_RESET_TYPE() type = -1; key_type = IS_STRING;

	/*
	The JSON_parser takes a UTF-16 encoded string and determines if it is a
	syntactically correct JSON text. Along the way, it creates a PHP variable.

	It is implemented as a Pushdown Automaton; that means it is a finite state
	machine with a stack.
	*/
	static int parseJSON(JSON_parser jp, int& z, unsigned short utf16_json[], int length, int options)
	{
		int next_char;  /* the next character */
		int next_class;  /* the next character class */
		int next_state;  /* the next state */
		int the_index;
		bool assoc = (options & OS_JSON_OBJECT_AS_ARRAY) != 0;

		Core::Buffer buf(jp->os);
		Core::Buffer key(jp->os);

		unsigned short utf16 = 0;
		int type = -1, key_type = IS_STRING;

		JSON_RESET_TYPE();

		for (the_index = 0; the_index < length; the_index += 1) {
			next_char = utf16_json[the_index];
			if (next_char >= 128) {
				next_class = C_ETC;
			} else {
				next_class = ascii_class[next_char];
				if (next_class <= __) {
					jp->error_code = OS_JSON_ERROR_CTRL_CHAR;
					FREE_BUFFERS();
					return false;
				}
			}
			/*
			Get the next state from the transition table.
			*/
			next_state = state_transition_table[jp->state][next_class];
			if (next_state >= 0) {
				/*
				Change the state and iterate
				*/
				if (type == IS_STRING) {
					if (next_state == ST && jp->state != U4) {
						if (jp->state != ES) {
							utf16_to_utf8(buf, next_char);
						} else {
							switch (next_char) {
							case 'b':
								buf.append('\b');
								break;
							case 't':
								buf.append('\t');
								break;
							case 'n':
								buf.append('\n');
								break;
							case 'f':
								buf.append('\f');
								break;
							case 'r':
								buf.append('\r');
								break;
							default:
								utf16_to_utf8(buf, next_char);
								break;
							}
						}
					} else if (next_state == U2) {
						utf16 = dehexChar(next_char) << 12;
					} else if (next_state == U3) {
						utf16 += dehexChar(next_char) << 8;
					} else if (next_state == U4) {
						utf16 += dehexChar(next_char) << 4;
					} else if (next_state == ST && jp->state == U4) {
						utf16 += dehexChar(next_char);
						utf16_to_utf8(buf, utf16);
					}
				} else if (type < IS_NUMBER && (next_class == C_DIGIT || next_class == C_ZERO)) {
					type = IS_NUMBER;
					buf.append((char)next_char);
				} else if (type == IS_NUMBER && next_state == E1) {
					type = IS_NUMBER;
					buf.append((char)next_char);
				} else if (type < IS_NUMBER && next_class == C_POINT) {
					type = IS_NUMBER;
					buf.append((char)next_char);
				} else if (type < IS_STRING && next_class == C_QUOTE) {
					type = IS_STRING;
				} else if (type < IS_BOOL && ((jp->state == T3 && next_state == OK) || (jp->state == F4 && next_state == OK))) {
					type = IS_BOOL;
				} else if (type < IS_NULL && jp->state == N3 && next_state == OK) {
					type = IS_NULL;
				} else if (type != IS_STRING && next_class > C_WHITE) {
					utf16_to_utf8(buf, next_char);
				}
				jp->state = next_state;
			} else {
				/*
				Perform one of the predefined actions.
				*/
				switch (next_state) {
					/* empty } */
				case -9:
					if (!pop(jp, MODE_KEY, true)) {
						FREE_BUFFERS();
						return false;
					}
					jp->state = OK;
					break;
					/* } */
				case -8:
					if (type != -1 && jp->stack[jp->top] == MODE_OBJECT)
					{
						pushValue(jp->os, buf, type, options);
						Core::String str = key.toString();
						if(key_type == IS_NUMBER)
							addValue(jp->os, jp->the_zstack[jp->top], Utils::strToFloat(str.toChar()));
						else if(!assoc){
							addPropertyValue(jp->os, jp->the_zstack[jp->top], str.isEmpty() ? "_empty_" : str.toChar());
							// releaseValue(jp->os, mval);
						} else {
							addAssocValue(jp->os, jp->the_zstack[jp->top], str.toChar());
						}
						popValue(jp->os);
						key.clear();
						buf.clear();
						JSON_RESET_TYPE();
					}


					if (!pop(jp, MODE_OBJECT, true)) {
						FREE_BUFFERS();
						return false;
					}
					jp->state = OK;
					break;
					/* ] */
				case -7:
					{
						if (type != -1 && jp->stack[jp->top] == MODE_ARRAY)
						{
							pushValue(jp->os, buf, type, options);
							addNextIndexValue(jp->os, jp->the_zstack[jp->top]);
							popValue(jp->os);
							buf.clear();
							JSON_RESET_TYPE();
						}

						if (!pop(jp, MODE_ARRAY, true)) {
							FREE_BUFFERS();
							return false;
						}
						jp->state = OK;
					}
					break;
					/* { */
				case -6:
					if (!push(jp, MODE_KEY)) {
						FREE_BUFFERS();
						return false;
					}

					jp->state = OB;
					if (jp->top > 0) {
						int obj = 0;

						/* if (jp->top == 1) {
						OS_ASSERT(false);
						obj = z;
						} else {
						// ALLOC_INIT_ZVAL(obj);
						} */

						if (!assoc) {
							jp->os->newObject();
							// object_init(obj);
						} else {
							// array_init(obj);
							jp->os->newArray();
						}
						obj = jp->os->getValueId();
						jp->os->retainValueById(obj);
						jp->os->pop();

						if (jp->top == 1) {
							z = obj;
							jp->os->retainValueById(obj);
						}

						jp->os->releaseValueById(jp->the_zstack[jp->top]);
						jp->the_zstack[jp->top] = obj;

						if (jp->top > 1) {
							attachValue(jp, jp->top - 1, jp->top, key, key_type, assoc);
						}

						JSON_RESET_TYPE();
					}

					break;
					/* [ */
				case -5:
					if (!push(jp, MODE_ARRAY)) {
						FREE_BUFFERS();
						return false;
					}
					jp->state = AR;

					if (jp->top > 0) {
						int arr = 0;

						/* if (jp->top == 1) {
						arr = z;
						} else {
						// ALLOC_INIT_ZVAL(arr);
						} */

						jp->os->newArray();
						arr = jp->os->getValueId();
						jp->os->retainValueById(arr);
						jp->os->pop();

						if (jp->top == 1) {
							z = arr;
							jp->os->releaseValueById(arr);
						}

						jp->os->releaseValueById(jp->the_zstack[jp->top]);
						jp->the_zstack[jp->top] = arr;

						if (jp->top > 1) {
							attachValue(jp, jp->top - 1, jp->top, key, key_type, assoc);
						}

						JSON_RESET_TYPE();
					}

					break;

					/* " */
				case -4:
					switch (jp->stack[jp->top]) {
					case MODE_KEY:
						jp->state = CO;
						// smart_str_0(&buf);
						SWAP_BUFFERS(buf, key);
						JSON_RESET_TYPE();
						break;
					case MODE_ARRAY:
					case MODE_OBJECT:
						jp->state = OK;
						break;
					case MODE_DONE:
						if (type == IS_STRING) {
							// smart_str_0(&buf);
							// ZVAL_STRINGL(z, buf.c, buf.len, 1);
							jp->os->pushString(buf.toString());
							z = jp->os->getValueId();
							jp->os->retainValueById(z);
							jp->os->pop();

							jp->state = OK;
							break;
						}
						/* fall through if not IS_STRING */
					default:
						FREE_BUFFERS();
						jp->error_code = OS_JSON_ERROR_SYNTAX;
						return false;
					}
					break;
					/* , */
				case -3:
					{
						if (type != -1 &&
							(jp->stack[jp->top] == MODE_OBJECT ||
							jp->stack[jp->top] == MODE_ARRAY))
						{
							pushValue(jp->os, buf, type, options);
						}else{
							jp->os->pushNull();
						}

						switch (jp->stack[jp->top]) {
						case MODE_OBJECT:
							if (pop(jp, MODE_OBJECT, false) && push(jp, MODE_KEY)) {
								if (type != -1) {
									Core::String str = key.toString();
									if(key_type == IS_NUMBER)
										addValue(jp->os, jp->the_zstack[jp->top], Utils::strToFloat(str.toChar()));
									else if (!assoc) {
										addPropertyValue(jp->os, jp->the_zstack[jp->top], str.isEmpty() ? "_empty_" : str.toChar());
										// releaseValue(jp->os, mval);
									} else {
										addAssocValue(jp->os, jp->the_zstack[jp->top], str.toChar());
									}
									key.clear();
								}
								jp->state = KE;
							}
							break;

						case MODE_ARRAY:
							if (type != -1) {
								addNextIndexValue(jp->os, jp->the_zstack[jp->top]);
							}
							jp->state = VA;
							break;

						default:
							popValue(jp->os);
							FREE_BUFFERS();
							jp->error_code = OS_JSON_ERROR_SYNTAX;
							return false;
						}
						popValue(jp->os);
						buf.clear();
						JSON_RESET_TYPE();
					}
					break;

				case -10: /* : after numeric key */
					switch (jp->stack[jp->top]) {
					case MODE_KEY:
						// jp->state = CO;
						SWAP_BUFFERS(buf, key);
						JSON_RESET_TYPE();
						key_type = IS_NUMBER;
						break;

					default:
						FREE_BUFFERS();
						jp->error_code = OS_JSON_ERROR_SYNTAX;
						return false;
					}
					// no break

					/* : */
				case -2:
					if (pop(jp, MODE_KEY, false) && push(jp, MODE_OBJECT)) {
						jp->state = VA;
						break;
					}
					/*
					syntax error
					*/
				default:
					jp->error_code = OS_JSON_ERROR_SYNTAX;
					FREE_BUFFERS();
					return false;
				}
			}
		}

		FREE_BUFFERS();
		if (jp->state == OK && pop(jp, MODE_DONE, true)) {
			return true;
		}

		jp->error_code = OS_JSON_ERROR_SYNTAX;
		return false;
	}

	static unsigned int next_utf8_char(
		const unsigned char *str,
		size_t str_len,
		size_t *cursor,
		int *status)
	{
		size_t pos = *cursor;
		unsigned int this_char = 0;

		*status = SUCCESS;
		OS_ASSERT(pos <= str_len);

#define MB_FAILURE(pos, advance) do { \
	*cursor = pos + (advance); \
	*status = FAILURE; \
	return 0; \
} while (0)

#define CHECK_LEN(pos, chars_need) ((str_len - (pos)) >= (chars_need))

/* valid as single byte character or leading byte */
#define utf8_lead(c)  ((c) < 0x80 || ((c) >= 0xC2 && (c) <= 0xF4))

/* whether it's actually valid depends on other stuff;
 * this macro cannot check for non-shortest forms, surrogates or
 * code points above 0x10FFFF */
#define utf8_trail(c) ((c) >= 0x80 && (c) <= 0xBF)

		if (!CHECK_LEN(pos, 1))
			MB_FAILURE(pos, 1);

		/* We'll follow strategy 2. from section 3.6.1 of UTR #36:
			* "In a reported illegal byte sequence, do not include any
			*  non-initial byte that encodes a valid character or is a leading
			*  byte for a valid sequence." */
		unsigned char c = str[pos];
		if (c < 0x80) {
			this_char = c;
			pos++;
		} else if (c < 0xc2) {
			MB_FAILURE(pos, 1);
		} else if (c < 0xe0) {
			if (!CHECK_LEN(pos, 2))
				MB_FAILURE(pos, 1);

			if (!utf8_trail(str[pos + 1])) {
				MB_FAILURE(pos, utf8_lead(str[pos + 1]) ? 1 : 2);
			}
			this_char = ((c & 0x1f) << 6) | (str[pos + 1] & 0x3f);
			if (this_char < 0x80) { /* non-shortest form */
				MB_FAILURE(pos, 2);
			}
			pos += 2;
		} else if (c < 0xf0) {
			size_t avail = str_len - pos;

			if (avail < 3 ||
					!utf8_trail(str[pos + 1]) || !utf8_trail(str[pos + 2])) {
				if (avail < 2 || utf8_lead(str[pos + 1]))
					MB_FAILURE(pos, 1);
				else if (avail < 3 || utf8_lead(str[pos + 2]))
					MB_FAILURE(pos, 2);
				else
					MB_FAILURE(pos, 3);
			}

			this_char = ((c & 0x0f) << 12) | ((str[pos + 1] & 0x3f) << 6) | (str[pos + 2] & 0x3f);
			if (this_char < 0x800) { /* non-shortest form */
				MB_FAILURE(pos, 3);
			} else if (this_char >= 0xd800 && this_char <= 0xdfff) { /* surrogate */
				MB_FAILURE(pos, 3);
			}
			pos += 3;
		} else if (c < 0xf5) {
			size_t avail = str_len - pos;

			if (avail < 4 ||
					!utf8_trail(str[pos + 1]) || !utf8_trail(str[pos + 2]) ||
					!utf8_trail(str[pos + 3])) {
				if (avail < 2 || utf8_lead(str[pos + 1]))
					MB_FAILURE(pos, 1);
				else if (avail < 3 || utf8_lead(str[pos + 2]))
					MB_FAILURE(pos, 2);
				else if (avail < 4 || utf8_lead(str[pos + 3]))
					MB_FAILURE(pos, 3);
				else
					MB_FAILURE(pos, 4);
			}
				
			this_char = ((c & 0x07) << 18) | ((str[pos + 1] & 0x3f) << 12) | ((str[pos + 2] & 0x3f) << 6) | (str[pos + 3] & 0x3f);
			if (this_char < 0x10000 || this_char > 0x10FFFF) { /* non-shortest form or outside range */
				MB_FAILURE(pos, 4);
			}
			pos += 4;
		} else {
			MB_FAILURE(pos, 1);
		}
		*cursor = pos;
  		return this_char;
	}

	static int utf8_to_utf16(unsigned short *utf16, const char utf8[], int len)
	{
		size_t pos = 0, us;
		int j, status;

		if (utf16) {
			/* really convert the utf8 string */
			for (j=0 ; (int)pos < len ; j++) {
				us = next_utf8_char((const unsigned char *)utf8, len, &pos, &status);
				if (status != SUCCESS) {
					return -1;
				}
				/* From http://en.wikipedia.org/wiki/UTF16 */
				if (us >= 0x10000) {
					us -= 0x10000;
					utf16[j++] = (unsigned short)((us >> 10) | 0xd800);
					utf16[j] = (unsigned short)((us & 0x3ff) | 0xdc00);
				} else {
					utf16[j] = (unsigned short)us;
				}
			}
		} else {
			/* Only check if utf8 string is valid, and compute utf16 lenght */
			for (j=0 ; (int)pos < len ; j++) {
				us = next_utf8_char((const unsigned char *)utf8, len, &pos, &status);
				if (status != SUCCESS) {
					return -1;
				}
				if (us >= 0x10000) {
					j++;
				}
			}
		}
		return j;
	}

	template <class T>
	static bool parseSimpleDec(const OS_CHAR *& p_str, T& p_val)
	{
		T val = 0, prev_val = 0;
		const OS_CHAR * str = p_str;
		const OS_CHAR * start = str;
		for(; *str >= OS_TEXT('0') && *str <= OS_TEXT('9'); str++){
			val = val * 10 + (T)(*str - OS_TEXT('0'));
			if(prev_val > val){
				p_str = start;
				p_val = 0;
				return false;
			}
			prev_val = val;
		}
		p_val = val;
		p_str = str;
		return str > start;
	}

	static void jsonEscapeString(JsonOS * os, Core::Buffer& buf, const char *s, int len, int options)
	{
		int pos = 0, ulen = 0;
		unsigned short us;
		unsigned short *utf16;
		// size_t newlen;

		if (len == 0) {
			buf.append("\"\"", 2);
			return;
		}

		if (options & OS_JSON_NUMERIC_CHECK) {
			OS_INT64 d;
			// int type;
			// long p;

			const char * str = s;
			if(parseSimpleDec(str, d) && !*str && (OS_INT64)(OS_FLOAT)d == d){
				os->pushNumber(d);
				buf.append(os->popString());
				return;
			}
		}

		utf16 = (options & OS_JSON_UNESCAPED_UNICODE) ? NULL : (unsigned short *) os->malloc(len * sizeof(unsigned short) OS_DBG_FILEPOS);
		ulen = utf8_to_utf16(utf16, s, len);
		if (ulen <= 0) {
			if (utf16) {
				os->free(utf16);
			}
			if (ulen < 0) {
				/* JSON_G(error_code) = OS_JSON_ERROR_UTF8;
				if (!PG(display_errors)) {
					php_error_docref(NULL TSRMLS_CC, E_WARNING, "Invalid UTF-8 sequence in argument");
				} */
				buf.append("null", 4);
			} else {
				buf.append("\"\"", 2);
			}
			return;
		}
		if (!(options & OS_JSON_UNESCAPED_UNICODE)) {
			len = ulen;
		}

		/* pre-allocate for string length plus 2 quotes */
		// smart_str_alloc(buf, len+2, 0);
		buf.append('"');

		while (pos < len)
		{
			us = (options & OS_JSON_UNESCAPED_UNICODE) ? s[pos++] : utf16[pos++];

			switch (us)
			{
				case '"':
					if (options & OS_JSON_HEX_QUOT) {
						buf.append("\\u0022", 6);
					} else {
						buf.append("\\\"", 2);
					}
					break;

				case '\\':
					buf.append("\\\\", 2);
					break;

				case '/':
					if (options & OS_JSON_UNESCAPED_SLASHES) {
						buf.append('/');
					} else {
						buf.append("\\/", 2);
					}
					break;

				case '\b':
					buf.append("\\b", 2);
					break;

				case '\f':
					buf.append("\\f", 2);
					break;

				case '\n':
					buf.append("\\n", 2);
					break;

				case '\r':
					buf.append("\\r", 2);
					break;

				case '\t':
					buf.append("\\t", 2);
					break;

				case '<':
					if (options & OS_JSON_HEX_TAG) {
						buf.append("\\u003C", 6);
					} else {
						buf.append('<');
					}
					break;

				case '>':
					if (options & OS_JSON_HEX_TAG) {
						buf.append("\\u003E", 6);
					} else {
						buf.append((char)'>');
					}
					break;

				case '&':
					if (options & OS_JSON_HEX_AMP) {
						buf.append("\\u0026", 6);
					} else {
						buf.append((char)'&');
					}
					break;

				case '\'':
					if (options & OS_JSON_HEX_APOS) {
						buf.append("\\u0027", 6);
					} else {
						buf.append((char)'\'');
					}
					break;

				default:
					if (us >= ' ' && ((options & OS_JSON_UNESCAPED_UNICODE) || (us & 127) == us)) {
						buf.append((char)(unsigned char) us);
					} else {
						buf.append("\\u", 2);
						buf.append((char)digits[(us & 0xf000) >> 12]);
						buf.append((char)digits[(us & 0xf00)  >> 8]);
						buf.append((char)digits[(us & 0xf0)   >> 4]);
						buf.append((char)digits[(us & 0xf)]);
					}
					break;
			}
		}

		buf.append((char)'"');
		if (utf16) {
			os->free(utf16);
		}
	}

	static int strToJson(OS * os, int params, int, int, void * user_param)
	{
		OS::String self = os->toString(-params-1);
		int options = 0
				| OS_JSON_HEX_TAG
				| OS_JSON_HEX_AMP
				| OS_JSON_HEX_APOS
				| OS_JSON_HEX_QUOT
				// | OS_JSON_FORCE_OBJECT
				// | OS_JSON_NUMERIC_CHECK
				| OS_JSON_UNESCAPED_SLASHES
				// | OS_JSON_PRETTY_PRINT
				// | OS_JSON_UNESCAPED_UNICODE
				;

		Core::Buffer buf(os);
		jsonEscapeString((JsonOS*)os, buf, self.toChar(), self.getLen(), options); 
		os->pushString(buf.toString());
		return 1;
	}

	static int jsonDecode(JsonOS * os, const char *str, int str_len, int options, long depth)
	{
		Core::Buffer buf(os);
		buf.reserveCapacity((str_len+1) * sizeof(unsigned short));
		unsigned short * utf16 = (unsigned short *)buf.buffer.buf;
		int utf16_len = utf8_to_utf16(utf16, str, str_len);
		if (utf16_len <= 0) {
			// JSON_G(error_code) = PHP_JSON_ERROR_UTF8;
			// os->setException("attemp to JSON decode empty sring");
			os->pushNull();
			return 1;
		}

		if (depth <= 0) {
			// php_error_docref(NULL TSRMLS_CC, E_WARNING, "Depth must be greater than zero");
			os->pushNull();
			return 1;
		}

		// ALLOC_INIT_ZVAL(z);
		int z = 0;
		JSON_parser jp = newJSONparser(os, depth);
		if (parseJSON(jp, z, utf16, utf16_len, options)) {
			os->pushValueById(z);
			os->releaseValueById(z);
			freeJSONparser(jp);
			return 1;
		}else{
			freeJSONparser(jp);
#ifdef _MSC_VER
#define strcasecmp _stricmp 
#endif
			if (str_len == 4) {
				if (!strcasecmp(str, "null")) {
					/* We need to explicitly clear the error because its an actual NULL and not an error */
					// jp->error_code = PHP_JSON_ERROR_NONE;
					// RETVAL_NULL();
					os->pushNull();
					return 1;
				} else if (!strcasecmp(str, "true")) {
					// RETVAL_BOOL(1);
					os->pushBool(true);
					return 1;
				}
			} else if (str_len == 5 && !strcasecmp(str, "false")) {
				// RETVAL_BOOL(0);
				os->pushBool(false);
				return 1;
			}

			OS_FLOAT d;
			if(Utils::parseFloat(str, d, Utils::PARSE_FLOAT) && !*str){
				os->pushNumber(d);
				return 1;
			}
		}
		// JSON_G(error_code) = jp->error_code;
		// freeJSONparser(jp);
		os->pushNull();
		return 1;
	}


	/* static int encode(OS * os, int params, int, int, void * user_param)
	{
		if(params > 0){
			return 1;
		}
		return 0;
	} */

	static int decode(OS * os, int params, int, int, void * user_param)
	{
		if(params > 0){
			OS::String str = os->toString(-params+0);
			int options = 0;
			return jsonDecode((JsonOS*)os, str.toChar(), str.getLen(), options, JSON_PARSER_DEFAULT_DEPTH);
		}
		return 0;
	}
};

void initJsonExtension(OS * os)
{
	{
		OS::FuncDef funcs[] = {
			{OS_TEXT("decode"), &JsonOS::decode},
			{}
		};
		os->getModule("json");
		os->setFuncs(funcs);
		os->pop();
	}
	{
		OS::FuncDef funcs[] = {
			{OS_TEXT("toJson"), &JsonOS::strToJson},
			{}
		};
		os->getGlobal("String");
		OS_ASSERT(os->isObject());
		os->setFuncs(funcs);
		os->pop();
	}
}

} // namespace ObjectScript

