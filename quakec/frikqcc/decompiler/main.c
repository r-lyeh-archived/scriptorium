#include "qcc.h"


float		pr_globals[MAX_REGS];
int			numpr_globals;

char		strings[MAX_STRINGS];
int			strofs;

dstatement_t	statements[MAX_STATEMENTS];
int			numstatements;
int			statement_linenums[MAX_STATEMENTS];

dfunction_t	functions[MAX_FUNCTIONS];
int			numfunctions;

ddef_t		globals[MAX_GLOBALS];
int			numglobaldefs;

ddef_t		fields[MAX_FIELDS];
int			numfielddefs;

opcode_t pr_opcodes[] =
{
 {"<DONE>", "DONE", -1, false, &def_entity, &def_field, &def_void},

 {"*", "MUL_F", 2, false, &def_float, &def_float, &def_float},
 {"*", "MUL_V", 2, false, &def_vector, &def_vector, &def_float},
 {"*", "MUL_FV", 2, false, &def_float, &def_vector, &def_vector},
 {"*", "MUL_VF", 2, false, &def_vector, &def_float, &def_vector},
 
 {"/", "DIV", 2, false, &def_float, &def_float, &def_float},

 {"+", "ADD_F", 3, false, &def_float, &def_float, &def_float},
 {"+", "ADD_V", 3, false, &def_vector, &def_vector, &def_vector},
  
 {"-", "SUB_F", 3, false, &def_float, &def_float, &def_float},
 {"-", "SUB_V", 3, false, &def_vector, &def_vector, &def_vector},

 {"==", "EQ_F", 4, false, &def_float, &def_float, &def_float},
 {"==", "EQ_V", 4, false, &def_vector, &def_vector, &def_float},
 {"==", "EQ_S", 4, false, &def_string, &def_string, &def_float},
 {"==", "EQ_E", 4, false, &def_entity, &def_entity, &def_float},
 {"==", "EQ_FNC", 4, false, &def_function, &def_function, &def_float},
 
 {"!=", "NE_F", 4, false, &def_float, &def_float, &def_float},
 {"!=", "NE_V", 4, false, &def_vector, &def_vector, &def_float},
 {"!=", "NE_S", 4, false, &def_string, &def_string, &def_float},
 {"!=", "NE_E", 4, false, &def_entity, &def_entity, &def_float},
 {"!=", "NE_FNC", 4, false, &def_function, &def_function, &def_float},
 
 {"<=", "LE", 4, false, &def_float, &def_float, &def_float},
 {">=", "GE", 4, false, &def_float, &def_float, &def_float},
 {"<", "LT", 4, false, &def_float, &def_float, &def_float},
 {">", "GT", 4, false, &def_float, &def_float, &def_float},

 {".", "INDIRECT", 1, false, &def_entity, &def_field, &def_float},
 {".", "INDIRECT", 1, false, &def_entity, &def_field, &def_vector},
 {".", "INDIRECT", 1, false, &def_entity, &def_field, &def_string},
 {".", "INDIRECT", 1, false, &def_entity, &def_field, &def_entity},
 {".", "INDIRECT", 1, false, &def_entity, &def_field, &def_field},
 {".", "INDIRECT", 1, false, &def_entity, &def_field, &def_function},

 {".", "ADDRESS", 1, false, &def_entity, &def_field, &def_pointer},

 {"=", "STORE_F", 5, true, &def_float, &def_float, &def_float},
 {"=", "STORE_V", 5, true, &def_vector, &def_vector, &def_vector},
 {"=", "STORE_S", 5, true, &def_string, &def_string, &def_string},
 {"=", "STORE_ENT", 5, true, &def_entity, &def_entity, &def_entity},
 {"=", "STORE_FLD", 5, true, &def_field, &def_field, &def_field},
 {"=", "STORE_FNC", 5, true, &def_function, &def_function, &def_function},

 {"=", "STOREP_F", 5, true, &def_pointer, &def_float, &def_float},
 {"=", "STOREP_V", 5, true, &def_pointer, &def_vector, &def_vector},
 {"=", "STOREP_S", 5, true, &def_pointer, &def_string, &def_string},
 {"=", "STOREP_ENT", 5, true, &def_pointer, &def_entity, &def_entity},
 {"=", "STOREP_FLD", 5, true, &def_pointer, &def_field, &def_field},
 {"=", "STOREP_FNC", 5, true, &def_pointer, &def_function, &def_function},

 {"<RETURN>", "RETURN", -1, false, &def_void, &def_void, &def_void},
  
 {"!", "NOT_F", -1, false, &def_float, &def_void, &def_float},
 {"!", "NOT_V", -1, false, &def_vector, &def_void, &def_float},
 {"!", "NOT_S", -1, false, &def_vector, &def_void, &def_float},
 {"!", "NOT_ENT", -1, false, &def_entity, &def_void, &def_float},
 {"!", "NOT_FNC", -1, false, &def_function, &def_void, &def_float},
  
  {"<IF>", "IF", -1, false, &def_float, &def_void, &def_void},
  {"<IFNOT>", "IFNOT", -1, false, &def_float, &def_void, &def_void},
  
// calls returns REG_RETURN
 {"<CALL0>", "CALL0", -1, false, &def_function, &def_void, &def_void},
 {"<CALL1>", "CALL1", -1, false, &def_function, &def_void, &def_void},
 {"<CALL2>", "CALL2", -1, false, &def_function, &def_void, &def_void}, 
 {"<CALL3>", "CALL3", -1, false, &def_function, &def_void, &def_void}, 
 {"<CALL4>", "CALL4", -1, false, &def_function, &def_void, &def_void},
 {"<CALL5>", "CALL5", -1, false, &def_function, &def_void, &def_void},
 {"<CALL6>", "CALL6", -1, false, &def_function, &def_void, &def_void},
 {"<CALL7>", "CALL7", -1, false, &def_function, &def_void, &def_void},
 {"<CALL8>", "CALL8", -1, false, &def_function, &def_void, &def_void},
  
 {"<STATE>", "STATE", -1, false, &def_float, &def_float, &def_void},
  
 {"<GOTO>", "GOTO", -1, false, &def_void, &def_void, &def_void},
  
 {"&&", "AND", 6, false, &def_float, &def_float, &def_float},
 {"||", "OR", 6, false, &def_float, &def_float, &def_float},

 {"&", "BITAND", 2, false, &def_float, &def_float, &def_float},
 {"|", "BITOR", 2, false, &def_float, &def_float, &def_float},

 {NULL}
};

// simple types.  function types are dynamically allocated
type_t	type_void = {ev_void, &def_void};
type_t	type_string = {ev_string, &def_string};
type_t	type_float = {ev_float, &def_float};
type_t	type_vector = {ev_vector, &def_vector};
type_t	type_entity = {ev_entity, &def_entity};
type_t	type_field = {ev_field, &def_field};
type_t	type_function = {ev_function, &def_function,NULL,&type_void};
// type_function is a void() function used for state defs
type_t	type_pointer = {ev_pointer, &def_pointer};

type_t	type_floatfield = {ev_field, &def_field, NULL, &type_float};

int		type_size[8] = {1,1,1,3,1,1,1,1};

def_t	def_void = {&type_void, "temp"};
def_t	def_string = {&type_string, "temp"};
def_t	def_float = {&type_float, "temp"};
def_t	def_vector = {&type_vector, "temp"};
def_t	def_entity = {&type_entity, "temp"};
def_t	def_field = {&type_field, "temp"};
def_t	def_function = {&type_function, "temp"};
def_t	def_pointer = {&type_pointer, "temp"};

def_t	def_ret, def_parms[MAX_PARMS];

def_t	*def_for_type[8] = {&def_void, &def_string, &def_float, &def_vector, &def_entity, &def_field, &def_function, &def_pointer};

void DecompileProgsDat(char *name);

int main (int argc, char **argv)
{
	if (argc > 1)
		DecompileProgsDat(argv[1]);
	else
		DecompileProgsDat("progs.dat");
}

