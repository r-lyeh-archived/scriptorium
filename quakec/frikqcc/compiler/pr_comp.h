
// this file is shared by quake and qcc

typedef int	func_t;
typedef int	string_t;

typedef enum {ev_void, ev_string, ev_float, ev_vector, ev_entity, ev_field, ev_function, ev_pointer, ev_int, ev_bad = -1} etype_t;


#define	OFS_NULL		0
#define	OFS_RETURN		1
#define	OFS_PARM0		4		// leave 3 ofs for each parm to hold vectors
#define	OFS_PARM1		7
#define	OFS_PARM2		10
#define	OFS_PARM3		13
#define	OFS_PARM4		16
#define	OFS_PARM5		19
#define	OFS_PARM6		22
#define	OFS_PARM7		25
#define	RESERVED_OFS	28


// indices into op table
enum {
	OP_DONE,
	OP_MUL_F,
	OP_MUL_V,
	OP_MUL_FV,
	OP_MUL_VF,
	OP_DIV_F,
	OP_ADD_F,
	OP_ADD_V,

	OP_SUB_F,
	OP_SUB_V,
	
	OP_EQ_F,
	OP_EQ_V,
	OP_EQ_S,
	OP_EQ_E,
	OP_EQ_FNC,
	
	OP_NE_F,
	OP_NE_V,
	OP_NE_S,
	OP_NE_E,
	OP_NE_FNC,
	
	OP_LE,
	OP_GE,
	OP_LT,
	OP_GT,

	OP_LOAD_F,
	OP_LOAD_V,
	OP_LOAD_S,
	OP_LOAD_ENT,
	OP_LOAD_FLD,
	OP_LOAD_FNC,

	OP_ADDRESS,

	OP_STORE_F,
	OP_STORE_V,
	OP_STORE_S,
	OP_STORE_ENT,
	OP_STORE_FLD,
	OP_STORE_FNC,

	OP_STOREP_F,
	OP_STOREP_V,
	OP_STOREP_S,
	OP_STOREP_ENT,
	OP_STOREP_FLD,
	OP_STOREP_FNC,

	OP_RETURN,
	OP_NOT_F,
	OP_NOT_V,
	OP_NOT_S,
	OP_NOT_ENT,
	OP_NOT_FNC,
	OP_IF,
	OP_IFNOT,

	OP_STATE,
	OP_GOTO,
	
	OP_AND,
	OP_AND_EF,
	OP_AND_FE,
	OP_AND_EE,

	OP_OR,
	OP_OR_EF,
	OP_OR_FE,
	OP_OR_EE,

	OP_BITAND,
	OP_BITOR,

	OP_ARRAY_F,
	OP_ARRAY_EF,
	OP_ARRAY_S,
	OP_ARRAY_E,
	OP_ARRAY_SS,
	OP_ARRAY_V,
	OP_ARRAY_FLD,
	OP_ARRAY_FNC,
	OP_ADD_STORE_F,
	OP_ADD_STORE_V,
	OP_ADD_STOREP_F,
	OP_ADD_STOREP_V,

	OP_SUB_STORE_F,
	OP_SUB_STORE_V,
	OP_SUB_STOREP_F,
	OP_SUB_STOREP_V,

	OP_MUL_STORE_F,
	OP_MUL_STOREP_F,

	OP_DIV_STORE_F,
	OP_DIV_STOREP_F,

	OP_AND_STORE_F,
	OP_AND_STOREP_F,

	OP_OR_STORE_F,
	OP_OR_STOREP_F,

	OP_INC_F,
	OP_DEC_F
};

// actual opcodes
enum {
	OP2_DONE,
	OP2_MUL_F,
	OP2_MUL_V,
	OP2_MUL_FV,
	OP2_MUL_VF,
	OP2_DIV_F,
	OP2_ADD_F,
	OP2_ADD_V,
	OP2_SUB_F,
	OP2_SUB_V,
	
	OP2_EQ_F,
	OP2_EQ_V,
	OP2_EQ_S,
	OP2_EQ_E,
	OP2_EQ_FNC,
	
	OP2_NE_F,
	OP2_NE_V,
	OP2_NE_S,
	OP2_NE_E,
	OP2_NE_FNC,
	
	OP2_LE,
	OP2_GE,
	OP2_LT,
	OP2_GT,

	OP2_LOAD_F,
	OP2_LOAD_V,
	OP2_LOAD_S,
	OP2_LOAD_ENT,
	OP2_LOAD_FLD,
	OP2_LOAD_FNC,

	OP2_ADDRESS,

	OP2_STORE_F,
	OP2_STORE_V,
	OP2_STORE_S,
	OP2_STORE_ENT,
	OP2_STORE_FLD,
	OP2_STORE_FNC,

	OP2_STOREP_F,
	OP2_STOREP_V,
	OP2_STOREP_S,
	OP2_STOREP_ENT,
	OP2_STOREP_FLD,
	OP2_STOREP_FNC,

	OP2_RETURN,
	OP2_NOT_F,
	OP2_NOT_V,
	OP2_NOT_S,
	OP2_NOT_ENT,
	OP2_NOT_FNC,
	OP2_IF,
	OP2_IFNOT,
	OP2_CALL0,
	OP2_CALL1,
	OP2_CALL2,
	OP2_CALL3,
	OP2_CALL4,
	OP2_CALL5,
	OP2_CALL6,
	OP2_CALL7,
	OP2_CALL8,
	OP2_STATE,
	OP2_GOTO,
	OP2_AND,
	OP2_OR,
	
	OP2_BITAND,
	OP2_BITOR
};

typedef struct statement_s
{
	unsigned short	op;
	short	a,b,c;
} dstatement_t;

typedef struct
{
	unsigned short	type;		// if DEF_SAVEGLOBAL bit is set
								// the variable needs to be saved in savegames
	unsigned short	ofs;
	int			s_name;
} ddef_t;

#define	DEF_SAVEGLOBAL	(1<<15)

#define	MAX_PARMS	8

typedef struct
{
	int		first_statement;	// negative numbers are builtins
	int		parm_start;
	int		locals;				// total ints of parms + locals
	
	int		profile;		// runtime
	
	int		s_name;
	int		s_file;			// source file defined in
	
	int		numparms;
	byte	parm_size[MAX_PARMS];
} dfunction_t;


#define	PROG_VERSION	6
typedef struct
{
	int		version;
	int		crc;			// check of header file
	
	int		ofs_statements;
	int		numstatements;	// statement 0 is an error

	int		ofs_globaldefs;
	int		numglobaldefs;
	
	int		ofs_fielddefs;
	int		numfielddefs;
	
	int		ofs_functions;
	int		numfunctions;	// function 0 is an empty
	
	int		ofs_strings;
	int		numstrings;		// first string is a null string

	int		ofs_globals;
	int		numglobals;
	
	int		entityfields;
} dprograms_t;

