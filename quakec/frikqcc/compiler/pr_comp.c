
#include "qcc.h"
#include "hash.h"

pr_info_t	pr;
boolean		pr_system;
def_t		*pr_global_defs[MAX_REGS];	// to find def for a global variable
int		pr_edict_size;
boolean expectint;

int shortened_store = 0;
char *typenames[] = {"void", "string", "float", "vector", "entity", "field", "function", "pointer", "int", "bad"};


//========================================

def_t		*pr_scope;		// the function being parsed, or NULL
boolean	pr_dumpasm;
string_t	s_file;			// filename for function definition

int		locals_end;		// for tracking local variables vs temps

jmp_buf		pr_parse_abort;		// longjump with this on parse error

void PR_ParseDefs (void);
void PR_ParseState (void);

//========================================

opcode_t pr_opcodes[] =
{
 {"<DONE>", OP2_DONE, -1, false, &def_entity, &def_field, &def_void},

 {"*", OP2_MUL_F, 2, false, &def_float, &def_float, &def_float},
 {"*", OP2_MUL_V, 2, false, &def_vector, &def_vector, &def_float},
 {"*", OP2_MUL_FV, 2, false, &def_float, &def_vector, &def_vector},
 {"*", OP2_MUL_VF, 2, false, &def_vector, &def_float, &def_vector},
 
 {"/", OP2_DIV_F, 2, false, &def_float, &def_float, &def_float},

 {"+", OP2_ADD_F, 3, false, &def_float, &def_float, &def_float},
 {"+", OP2_ADD_V, 3, false, &def_vector, &def_vector, &def_vector},

 {"-", OP2_SUB_F, 3, false, &def_float, &def_float, &def_float},
 {"-", OP2_SUB_V, 3, false, &def_vector, &def_vector, &def_vector},

 {"==", OP2_EQ_F, 4, false, &def_float, &def_float, &def_float},
 {"==", OP2_EQ_V, 4, false, &def_vector, &def_vector, &def_float},
 {"==", OP2_EQ_S, 4, false, &def_string, &def_string, &def_float},
 {"==", OP2_EQ_E, 4, false, &def_entity, &def_entity, &def_float},
 {"==", OP2_EQ_FNC, 4, false, &def_function, &def_function, &def_float},

 {"!=", OP2_NE_F, 4, false, &def_float, &def_float, &def_float},
 {"!=", OP2_NE_V, 4, false, &def_vector, &def_vector, &def_float},
 {"!=", OP2_NE_S, 4, false, &def_string, &def_string, &def_float},
 {"!=", OP2_NE_E, 4, false, &def_entity, &def_entity, &def_float},
 {"!=", OP2_NE_FNC, 4, false, &def_function, &def_function, &def_float},

 {"<=", OP2_LE, 4, false, &def_float, &def_float, &def_float},
 {">=", OP2_GE, 4, false, &def_float, &def_float, &def_float},
 {"<", OP2_LT, 4, false, &def_float, &def_float, &def_float},
 {">", OP2_GT, 4, false, &def_float, &def_float, &def_float},

 {".", OP2_LOAD_F, 1, false, &def_entity, &def_field, &def_float},
 {".", OP2_LOAD_V, 1, false, &def_entity, &def_field, &def_vector},
 {".", OP2_LOAD_S, 1, false, &def_entity, &def_field, &def_string},
 {".", OP2_LOAD_ENT, 1, false, &def_entity, &def_field, &def_entity},
 {".", OP2_LOAD_FLD, 1, false, &def_entity, &def_field, &def_field},
 {".", OP2_LOAD_FNC, 1, false, &def_entity, &def_field, &def_function},

 {".", OP2_ADDRESS, 1, false, &def_entity, &def_field, &def_pointer},

 {"=", OP2_STORE_F, 5, true, &def_float, &def_float, &def_float},
 {"=", OP2_STORE_V, 5, true, &def_vector, &def_vector, &def_vector},
 {"=", OP2_STORE_S, 5, true, &def_string, &def_string, &def_string},
 {"=", OP2_STORE_ENT, 5, true, &def_entity, &def_entity, &def_entity},
 {"=", OP2_STORE_FLD, 5, true, &def_field, &def_field, &def_field},
 {"=", OP2_STORE_FNC, 5, true, &def_function, &def_function, &def_function},

 {"=", OP2_STOREP_F, 5, true, &def_pointer, &def_float, &def_float},
 {"=", OP2_STOREP_V, 5, true, &def_pointer, &def_vector, &def_vector},
 {"=", OP2_STOREP_S, 5, true, &def_pointer, &def_string, &def_string},
 {"=", OP2_STOREP_ENT, 5, true, &def_pointer, &def_entity, &def_entity},
 {"=", OP2_STOREP_FLD, 5, true, &def_pointer, &def_field, &def_field},
 {"=", OP2_STOREP_FNC, 5, true, &def_pointer, &def_function, &def_function},

 {"<RETURN>", OP2_RETURN, -1, false, &def_void, &def_void, &def_void},
 
 {"!", OP2_NOT_F, -1, false, &def_float, &def_void, &def_float},
 {"!", OP2_NOT_V, -1, false, &def_vector, &def_void, &def_float},
 {"!", OP2_NOT_S, -1, false, &def_string, &def_void, &def_float},
 {"!", OP2_NOT_ENT, -1, false, &def_entity, &def_void, &def_float},
 {"!", OP2_NOT_FNC, -1, false, &def_function, &def_void, &def_float},
  
  {"<IF>", OP2_IF, -1, false, &def_float, &def_float, &def_void},
  {"<IFNOT>", OP2_IFNOT, -1, false, &def_float, &def_float, &def_void},
  
 {"<STATE>", OP2_STATE, -1, false, &def_float, &def_float, &def_void},
  
 {"<GOTO>", OP2_GOTO, -1, false, &def_float, &def_void, &def_void},
 // extra conditional crap FrikaC
 // I must be nuts :)
 {"&&", OP2_AND, 6, false, &def_float, &def_float, &def_float},
 {"&&", OP2_AND, 6, false, &def_entity, &def_float, &def_float},
 {"&&", OP2_AND, 6, false, &def_float, &def_entity, &def_float},
 {"&&", OP2_AND, 6, false, &def_entity, &def_entity, &def_float},

 {"||", OP2_OR, 6, false, &def_float, &def_float, &def_float},
 {"||", OP2_OR, 6, false, &def_entity, &def_float, &def_float},
 {"||", OP2_OR, 6, false, &def_float, &def_entity, &def_float},
 {"||", OP2_OR, 6, false, &def_entity, &def_entity, &def_float},

 {"&", OP2_BITAND, 2, false, &def_float, &def_float, &def_float},

 {"|", OP2_BITOR, 2, false, &def_float, &def_float, &def_float},

 {"[", OP2_LOAD_F, 1, false, &def_void, &def_float, &def_float},
 {"[", OP2_LOAD_F, 1, false, &def_entity, &def_float, &def_float},
 {"[", OP2_LOAD_S, 1, false, &def_void, &def_float, &def_string},
 {"[", OP2_LOAD_ENT, 1, false, &def_void, &def_float, &def_entity},
 {"[", OP2_ADD_F, 1, false, &def_string, &def_float, &def_string},
 {"[", OP2_LOAD_V, 1, false, &def_void, &def_float, &def_vector},
 {"[", OP2_LOAD_FLD, 1, false, &def_void, &def_float, &def_field},
 {"[", OP2_LOAD_FNC, 1, false, &def_void, &def_float, &def_function},

 {"+=", OP_ADD_F, 5, true, &def_float, &def_float, &def_float},
 {"+=", OP_ADD_V, 5, true, &def_vector, &def_vector, &def_vector},
 {"+=", OP_ADD_F, 5, true, &def_pointer, &def_float, &def_float},
 {"+=", OP_ADD_V, 5, true, &def_pointer, &def_vector, &def_vector},
 {"-=", OP_SUB_F, 5, true, &def_float, &def_float, &def_float},
 {"-=", OP_SUB_V, 5, true, &def_vector, &def_vector, &def_vector},
 {"-=", OP_SUB_F, 5, true, &def_pointer, &def_float, &def_float},
 {"-=", OP_SUB_V, 5, true, &def_pointer, &def_vector, &def_vector},

 {"*=", OP_MUL_F, 5, true, &def_float, &def_float, &def_float},
 {"*=", OP_MUL_F, 5, true, &def_pointer, &def_float, &def_float},

 {"/=", OP_DIV_F, 5, true, &def_float, &def_float, &def_float},
 {"/=", OP_DIV_F, 5, true, &def_pointer, &def_float, &def_float},
 {"&=", OP_BITAND, 5, true, &def_float, &def_float, &def_float},
 {"&=", OP_BITAND, 5, true, &def_pointer, &def_float, &def_float},
 {"|=", OP_BITOR, 5, true, &def_float, &def_float, &def_float},
 {"|=", OP_BITOR, 5, true, &def_pointer, &def_float, &def_float},

 {"++", OP2_ADD_F, 1, false, &def_float, &def_float, &def_float},
 {"--", OP2_SUB_F, 1, false, &def_float, &def_float, &def_float},

 {NULL}
};


char *opnames[] =
{
"<DONE>", "MUL_F", "MUL_V", "MUL_FV", "MUL_VF", "DIV_F", "ADD_F", "ADD_V", "SUB_F",
 "SUB_V", "EQ_F", "EQ_V", "EQ_S", "EQ_E", "EQ_FNC", "NE_F", "NE_V", "NE_S", "NE_F",
 "NE_FNC", "LE", "GE", "LT", "GT", "LOAD_F", "LOAD_V", "LOAD_S", "LOAD_ENT", "LOAD_FLD",
 "LOAD_FNC",
 "ADDRESS", "STORE_F", "STORE_V", "STORE_S", "STORE_ENT", "STORE_FLD", "STORE_FNC",
 "STOREP_F", "STOREP_V", "STOREP_S", "STOREP_ENT", "STOREP_FLD", "STOREP_FUNC", "RETURN",
 "NOT_F", "NOT_V", "NOT_S", "NOT_ENT", "NOT_FNC", "IF", "IFNOT", "CALL0", "CALL1", "CALL2",
 "CALL3", "CALL4", "CALL5", "CALL6", "CALL7", "CALL8", "STATE", "GOTO", "AND", "OR",
 "BITAND", "BITOR"
};

#define	TOP_PRIORITY	6
#define	NOT_PRIORITY	4

def_t *PR_Expression (int priority, boolean returnused);
//===========================================================================


etype_t PR_GetType (def_t *def)
{
	etype_t type = def->type->type;

	if (def->cast)
	{
		type = def->cast;
		def->cast = ev_void;
	}
	return type;
}

unsigned char used_temps[MAX_REGS];

def_t *GimmeATempFoo (int size)
{
	def_t *d;

	if (!pr_scope)
		PR_ParseError(502, "WTF?! This should be impossible");
	//veri tas
	if (pr_optimize_recycle)
	{
		d = pr_scope->tempnext;

		while (d)
		{
			if (!used_temps[d->ofs])
			{
				if (type_size[d->type->type] == size)
				{
					num_recycled += size;
					return d;
				}
			}
			d = d->tempnext;
		}
	}

	d = (struct def_s *) PR_Malloc (sizeof(def_t));
	d->ofs = numpr_globals;
	d->tempnext = pr_scope->tempnext;
	pr_scope->tempnext = d;
	if (pr_optimize_recycle)
		locals_end = numpr_globals; // bad
	pr_global_defs[numpr_globals] = d;
	numpr_globals += size;
	return d;
}

def_t	*PR_ParseImmediate (void);
def_t *ConstantArithmetic (opcode_t *op, def_t *var_a, def_t *var_b)
{
	def_t *d;
	switch(op->op)
	{
	case OP2_ADD_F:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = G_FLOAT(var_a->ofs) + G_FLOAT(var_b->ofs);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP2_SUB_F:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = G_FLOAT(var_a->ofs) - G_FLOAT(var_b->ofs);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP2_SUB_V:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_vector;
			pr_immediate.vector[0] = G_VECTOR(var_a->ofs, 0) - G_VECTOR(var_b->ofs, 0);
			pr_immediate.vector[1] = G_VECTOR(var_a->ofs, 1) - G_VECTOR(var_b->ofs, 1);
			pr_immediate.vector[2] = G_VECTOR(var_a->ofs, 2) - G_VECTOR(var_b->ofs, 2);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP2_ADD_V:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_vector;
			pr_immediate.vector[0] = G_VECTOR(var_a->ofs, 0) + G_VECTOR(var_b->ofs, 0);
			pr_immediate.vector[1] = G_VECTOR(var_a->ofs, 1) + G_VECTOR(var_b->ofs, 1);
			pr_immediate.vector[2] = G_VECTOR(var_a->ofs, 2) + G_VECTOR(var_b->ofs, 2);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP2_MUL_F:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = G_FLOAT(var_a->ofs) * G_FLOAT(var_b->ofs);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP2_MUL_V:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_float;
			
			pr_immediate._float = G_VECTOR(var_a->ofs, 0) * G_VECTOR(var_b->ofs, 0)+
				G_VECTOR(var_a->ofs, 1) * G_VECTOR(var_b->ofs, 1) +
				G_VECTOR(var_a->ofs, 2) * G_VECTOR(var_b->ofs, 2);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;

	case OP2_MUL_FV:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_vector;
			pr_immediate.vector[0] = G_FLOAT(var_a->ofs) * G_VECTOR(var_b->ofs, 0);
			pr_immediate.vector[1] = G_FLOAT(var_a->ofs) * G_VECTOR(var_b->ofs, 1);
			pr_immediate.vector[2] = G_FLOAT(var_a->ofs) * G_VECTOR(var_b->ofs, 2);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;

	case OP2_MUL_VF:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_vector;
			pr_immediate.vector[0] = G_FLOAT(var_b->ofs) * G_VECTOR(var_a->ofs, 0);
			pr_immediate.vector[1] = G_FLOAT(var_b->ofs) * G_VECTOR(var_a->ofs, 1);
			pr_immediate.vector[2] = G_FLOAT(var_b->ofs) * G_VECTOR(var_a->ofs, 2);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}

		break;
	case OP2_DIV_F:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = G_FLOAT(var_a->ofs) / G_FLOAT(var_b->ofs);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP2_BITAND:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = G_INT(var_a->ofs) & G_INT(var_b->ofs);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP2_BITOR:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = (int)G_FLOAT(var_a->ofs) | (int)G_FLOAT(var_b->ofs);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP2_GE:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = G_FLOAT(var_a->ofs) >= G_FLOAT(var_b->ofs);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP2_LE:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = G_FLOAT(var_a->ofs) <= G_FLOAT(var_b->ofs);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP2_LT:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = G_FLOAT(var_a->ofs) < G_FLOAT(var_b->ofs);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP2_GT:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = G_FLOAT(var_a->ofs) > G_FLOAT(var_b->ofs);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP2_OR:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = G_FLOAT(var_a->ofs) || G_FLOAT(var_b->ofs);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP2_AND:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = G_FLOAT(var_a->ofs) && G_FLOAT(var_b->ofs);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP2_NOT_F:
		if (var_a->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = !G_FLOAT(var_a->ofs);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP2_NOT_V:
		if (var_a->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = !G_VECTOR(var_a->ofs, 0) && !G_VECTOR(var_a->ofs, 1) && !G_VECTOR(var_a->ofs, 2);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP_EQ_F:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = G_FLOAT(var_a->ofs) == G_FLOAT(var_b->ofs);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	case OP_NE_F:
		if (var_a->constant && var_b->constant)
		{
			pr_immediate_type = &type_float;
			pr_immediate._float = G_FLOAT(var_a->ofs) != G_FLOAT(var_b->ofs);
			d = PR_ParseImmediate();
			HashImmediate();
			return d;
		}
		break;
	}
	return NULL;
}


/*
============
PR_Statement

Emits a primitive statement, returning the var it places it's value in
============
*/
dstatement_t *SimpleStatement( int op, int var_a, int var_b, int var_c)
{
	dstatement_t	*statement;

	statement_linenums[numstatements] = pr_source_line;
	statement = &statements[numstatements];

	numstatements++;
	statement->op = op;
	statement->a = var_a;
	statement->b = var_b;
	statement->c = var_c;
	return statement;
}

def_t *PR_Statement ( opcode_t *op, def_t *var_a, def_t *var_b)
{
	dstatement_t	*statement, *sprev;
	def_t			*var_c;
	boolean		templast;

	if (var_a)
		if (var_a->ofs >= 0)
			used_temps[var_a->ofs] = 0;
	if (var_b)
		if (var_b->ofs >= 0)
			used_temps[var_b->ofs] = 0;

	templast = false;

	if (numstatements)
	{
		sprev = &statements[numstatements - 1];
		if (sprev->c >= 0)
			if (!pr_global_defs[sprev->c]->name)
					templast = true;
	}
	if (pr_optimize_constant_arithmetic)
	{
		var_c = ConstantArithmetic(op, var_a, var_b);
		if (var_c)
		{
			num_constant_ops_saved++;
			return var_c;
		}
		if (sprev->op == op->op)
		{
			if (sprev->b >= 0)
			{
				var_c = pr_global_defs[sprev->b];
				if (var_a && var_b && var_c)
				{
					if (var_b->constant && var_c->constant)
					{
						if (sprev->c == var_a->ofs && !var_a->name)
						{
							var_c = ConstantArithmetic(op, var_c, var_b);
							if (var_c)
							{
								sprev->b = var_c->ofs;
								num_constant_ops_saved++;
								return pr_global_defs[sprev->c];
							}
						}
					}
				}
			}
		}
	}

	statement = SimpleStatement(op->op, var_a ? var_a->ofs : 0, var_b ? var_b->ofs : 0, 0);
	shortened_store = 0;

	if (op->type_c == &def_void || op->right_associative)
	{
		var_c = NULL;
		statement->c = 0;			// ifs, gotos, and assignments
									// don't need vars allocated

		if (templast)
		{
			if (pr_optimize_eliminate_temps && ((unsigned) (statement->op - OP2_STORE_F) < 6) && (statement->a == sprev->c))
			{
				sprev->c = statement->b;
				numstatements--;
				num_stores_shortened++;
				shortened_store = 1;
				return var_b;
			}
			else if (pr_optimize_shorten_ifs && (statement->op == OP2_IFNOT))
			{
				if (sprev->op == OP2_NOT_F || sprev->op == OP2_NOT_FNC || sprev->op == OP2_NOT_ENT)
				{
					sprev->op = OP2_IF;
					numstatements--;
					num_ifs_shortened++;
				}
			}
			else if (pr_optimize_shorten_ifs && (statement->op == OP2_IF))
			{
				if (sprev->op == OP2_NOT_F || sprev->op == OP2_NOT_FNC || sprev->op == OP2_NOT_ENT)
				{
					sprev->op = OP2_IFNOT;
					numstatements--;
					num_ifs_shortened++;
				}
			}
		}
	}
	else
	{	// allocate result space
		var_c = GimmeATempFoo(type_size[op->type_c->type->type]);;
		var_c->type = op->type_c->type;
		var_c->cast = ev_void;		
		statement->c = var_c->ofs;

		if (var_c)
			used_temps[var_c->ofs] = 1;
	}


	if (op->right_associative)
		return var_a;
	return var_c;
}

/*
============
PR_GetImmediate

Looks for a preexisting constant; return NULL if none exists
============
*/
def_t *PR_GetImmediate (void)
{
	def_t *cn = NULL;
	struct hash_element *cell = NULL;

	for (cell = htable[pr_immediate_index]; cell != NULL; cell = cell->next) 
	{
		cn = cell->def;
		if (!cn->constant || (cn->type != pr_immediate_type))
			continue;
		if (pr_immediate_type == &type_string)
		{
			if (memcmp(G_STRING(cn->ofs), pr_immediate_string, pr_immediate_strlen))
				continue;
		}
		else if (pr_immediate_type == &type_vector)
		{
			if ((G_FLOAT(cn->ofs) != pr_immediate.vector[0]) || (G_FLOAT(cn->ofs+1) != pr_immediate.vector[1]) || (G_FLOAT(cn->ofs+2) != pr_immediate.vector[2]))
				continue;
		}
		else 
		{
			if (G_INT(cn->ofs) != pr_immediate._int)
				continue;
		}

		return cn;
	}
	return NULL;
}

/*
============
PR_ParseImmediate

Looks for a preexisting constant; allocates a new one if none is found
============
*/
def_t	*PR_ParseImmediate (void)
{
	def_t *cn = NULL;
	struct hash_element *cell = NULL;

	if (cn = PR_GetImmediate())
	{
		return cn;
	}

// allocate a new one
	cn = (struct def_s *) PR_Malloc (sizeof(def_t));

	cn->next = NULL;
	pr.def_tail->next = cn;
	pr.def_tail = cn;

	cell = (struct hash_element *) PR_Malloc (sizeof(struct hash_element));
	cell->next = htable[pr_immediate_index];
	cell->def = cn;
	htable[pr_immediate_index] = cell;
	stats[pr_immediate_index]++;

	cn->type = pr_immediate_type;
	cn->name = "IMMEDIATE";
	cn->constant = 1;
	cn->defined = 1;
	cn->save = 0;
	cn->scope = NULL;		// always share immediates
	cn->cast = ev_void;

// copy the immediate to the global area
	cn->ofs = numpr_globals;
	pr_global_defs[cn->ofs] = cn;
	numpr_globals += type_size[pr_immediate_type->type];
	if (pr_immediate_type == &type_string)
		pr_immediate.string = CopyString (pr_immediate_string, pr_immediate_strlen);
	
	memcpy (pr_globals + cn->ofs, &pr_immediate, 4*type_size[pr_immediate_type->type]);

	return cn;
}

void PrecacheSound (def_t *e, int ch)
{
	char	*n;
	int		i;
	
	if (!e->ofs)
		return;
	n = G_STRING(e->ofs);
	for (i=0 ; i<numsounds ; i++)
		if (!STRCMP(n, precache_sounds[i]))
			return;
	if (numsounds == MAX_SOUNDS)
		Sys_Error("Q608: PrecacheSound: numsounds == MAX_SOUNDS");
	strcpy (precache_sounds[i], n);
	if (ch >= '1'  && ch <= '9')
		precache_sounds_block[i] = ch - '0';
	else
		precache_sounds_block[i] = 1;
	numsounds++;
}

void PrecacheModel (def_t *e, int ch)
{
	char	*n;
	int		i;
	
	if (!e->ofs)
		return;
	n = G_STRING(e->ofs);
	for (i=0 ; i<nummodels ; i++)
		if (!STRCMP(n, precache_models[i]))
			return;
	if (nummodels == MAX_MODELS)
		Sys_Error("Q609: PrecacheModels: nummodels == MAX_MODELS");
	strcpy (precache_models[i], n);
	if (ch >= '1'  && ch <= '9')
		precache_models_block[i] = ch - '0';
	else
		precache_models_block[i] = 1;
	nummodels++;
}

void PrecacheFile (def_t *e, int ch)
{
	char	*n;
	int		i;
	
	if (!e->ofs)
		return;
	n = G_STRING(e->ofs);
	for (i=0 ; i<numfiles ; i++)
		if (!STRCMP(n, precache_files[i]))
			return;
	if (numfiles == MAX_FILES)
		Sys_Error("Q610: PrecacheFile: numfiles == MAX_FILES");
	strcpy (precache_files[i], n);
	if (ch >= '1'  && ch <= '9')
		precache_files_block[i] = ch - '0';
	else
		precache_files_block[i] = 1;
	numfiles++;
}

/*
============
PR_ParseFunctionCall
============
*/

// more than 8 parms keyword - locutus
def_t *extra_parms[MAX_EXTRA_PARMS];

def_t *ecorrection;

def_t *PR_ParseFunctionCall (def_t *func)
{
	def_t		*e, *e2;
	int			arg, numtypes, i, returnused = 0;
	type_t		*t;
	def_t		*parms[MAX_PARMS + MAX_EXTRA_PARMS];
	
	t = func->type;

	if (t->type != ev_function)
		PR_ParseError (503, "%s is not a function", t->def->name);
	
// copy the arguments to the global parameter variables
	arg = 0;
	numtypes = t->num_parms >= 0 ? t->num_parms : -1 - t->num_parms;
	if (!PR_Check(")", tt_punct))
	{
		do
		{
			if (t->num_parms >= 0 && arg >= t->num_parms)
			{
				if (func->name)
					PR_ParseWarning (101, "%s: too many parameters, see definition %s(%i)", func->name, func->s_file, func->line);
				else
					PR_ParseWarning (102, "Indirect function: too many parameters");

			}
			if (t->num_parms < 0 && arg > MAX_PARMS)
				PR_ParseWarning (103, "%s: vararg func cannot have more than %i parms", func->name, MAX_PARMS);

			if (!PR_Check("#", tt_punct))
			{
				if (t->parm_types[arg])
					expectint = t->parm_types[arg]->type == ev_int;
				e = PR_Expression (TOP_PRIORITY, returnused);
				if (ecorrection)
				{
					parms[returnused] = ecorrection;
					ecorrection = NULL;
				}
				if (e->ofs == OFS_RETURN)
					returnused = arg;

				if (arg == 0 && func->name)
				{
				// save information for model and sound caching
					if (!strncmp(func->name,"precache_sound", 14))
						PrecacheSound (e, func->name[14]);
					else if (!strncmp(func->name,"precache_model", 14))
						PrecacheModel (e, func->name[14]);
					else if (!strncmp(func->name,"precache_file", 13))
						PrecacheFile (e, func->name[13]);
				}
				if (e->cast)
				{
					if ((arg < numtypes) && ( e->cast != t->parm_types[arg]->type ) )
					{
						PR_ParseWarning (104, "%s: type mismatch on parm %i. Expected %s, found %s. See definition %s(%i)", 
						func->name, arg, typenames[t->parm_types[arg]->type], typenames[e->cast], func->s_file, func->line);
					}
					e->cast = ev_void;
				}
				else if ((arg < numtypes) && ( e->type != t->parm_types[arg] ) )
				{
					PR_ParseWarning (104, "%s: type mismatch on parm %i. Expected %s, found %s. See definition %s(%i)", 
					func->name, arg, typenames[t->parm_types[arg]->type], typenames[e->type->type], func->s_file, func->line);
				}
				// a vector copy will copy everything
				parms[arg] = e;
				if (arg < MAX_PARMS)
					def_parms[arg].type = t->parm_types[arg];
				else
				{
					if (!extra_parms[arg - MAX_PARMS])
					{
						e2 = (struct def_s *) PR_Malloc (sizeof(def_t));
						e2->ofs = numpr_globals;
						e2->name = "extra parm";
						pr_global_defs[numpr_globals] = e2;
						numpr_globals += 3;
						extra_parms[arg - MAX_PARMS] = e2;
					}
				}
			}
			else
				parms[arg] = NULL;
			arg++;
		}
		while (PR_Check (",", tt_punct));

		PR_Expect (")", tt_punct);
	}
	// FrikaC store all the parms *NOW*
	if (arg > (MAX_PARMS + MAX_EXTRA_PARMS))
		PR_ParseError (504, "%s: more than %i parameters illegal", func->name, MAX_PARMS + MAX_EXTRA_PARMS);
	for (i = 0; i < arg; i++)
	{
		if (!parms[i])
			continue;
		if (i < MAX_PARMS)
		{
			if (pr_optimize_nonvec_parms && t->parm_types[i] != &type_vector)
			{
				PR_Statement (&pr_opcodes[OP_STORE_F], parms[i], &def_parms[i]);
				num_nonvec_parms++;
			}
			else
				PR_Statement (&pr_opcodes[OP_STORE_V], parms[i], &def_parms[i]);
		}
		else
		{
			if (!extra_parms[i - MAX_PARMS])
			{
				e2 = (struct def_s *) PR_Malloc (sizeof(def_t));
				e2->ofs = numpr_globals;
				e2->name = "extra parm";
				pr_global_defs[numpr_globals] = e2;
				numpr_globals += 3;
				extra_parms[i - MAX_PARMS] = e2;
			}
			if (t->parm_types[i] != &type_vector)
				PR_Statement (&pr_opcodes[OP_STORE_F], parms[i], extra_parms[i - MAX_PARMS]);
			else
				PR_Statement (&pr_opcodes[OP_STORE_V], parms[i], extra_parms[i - MAX_PARMS]);
		}
	}
	if (arg < numtypes)
		PR_ParseWarning (105, "%s: Too few parameters. See definition %s(%i)", func->name, func->s_file, func->line);

	SimpleStatement (OP2_CALL0+ (arg > MAX_PARMS ? MAX_PARMS : arg), func->ofs, 0, 0);
	def_ret.type = t->aux_type;
	return &def_ret;
}

/*
============
PR_ParseValue

Returns the global ofs for the current token
============
*/
def_t	*PR_ParseValue (void)
{
	def_t		*d;
	char		*name;
	
// if the token is an immediate, allocate a constant for it
	if (pr_token_type == tt_immediate)
	{
		 d = PR_ParseImmediate ();
		 PR_Lex();
		 return d;
	}

	name = PR_ParseName ();
	
// look through the defs
	d = PR_GetDef (NULL, name, pr_scope, false, 0, 0);
	if (!d)
		PR_ParseError (505, "Unknown value \"%s\"", name);	
	return d;
}

/*
============
PR_Cast
============
*/
def_t *PR_Cast (def_t *e, type_t *type)
{
	def_t *cast;

	cast = (struct def_s *) PR_Malloc (sizeof(def_t));
	memcpy (cast, e, sizeof(def_t));
	cast->type = type;
	return cast;
}

/*
============
PR_Term
============
*/
def_t *PR_Term (void)
{
	def_t	*e, *e2;
	etype_t t;
	type_t *ty;

	if (pr_token_type == tt_punct)
	{
		if (PR_Check("++", tt_punct))
		{

			e = PR_Expression (1, false);
			t = PR_GetType(e);
			if (t == ev_int)
			{
				pr_immediate_type = &type_int;
				pr_immediate._int = 1;
			}
			else
			{
				pr_immediate_type = &type_float;
				pr_immediate._float = 1;
			}
			e2 = PR_ParseImmediate();
			HashImmediate();
			if (t != ev_float && t != ev_int)
				PR_ParseError(506, "Type mismatch for ++, %s not allowed", typenames[t]);
			SimpleStatement(pr_opcodes[OP_INC_F].op, e->ofs, e2->ofs, e->ofs);
			dontcomplain = true;
			return e;
		}
		if (PR_Check("--", tt_punct))
		{
			e = PR_Expression (1, false);
			t = PR_GetType(e);
			if (t == ev_int)
			{
				pr_immediate_type = &type_int;
				pr_immediate._int = 1;
			}
			else
			{
				pr_immediate_type = &type_float;
				pr_immediate._float = 1;
			}
			e2 = PR_ParseImmediate();
			HashImmediate();
			if (t != ev_float && t != ev_int)
				PR_ParseError(507, "Type mismatch for --, %s not allowed", typenames[t]);

			SimpleStatement(pr_opcodes[OP_DEC_F].op, e->ofs, e2->ofs, e->ofs);
			dontcomplain = true;
			return e;
		}

		if (PR_Check ("!", tt_punct))
		{
			e = PR_Expression (NOT_PRIORITY, false);
			t = PR_GetType(e);
			if (t == ev_float || t == ev_int)
				e2 = PR_Statement (&pr_opcodes[OP_NOT_F], e, 0);
			else if (t == ev_string)
				e2 = PR_Statement (&pr_opcodes[OP_NOT_S], e, 0);
			else if (t == ev_entity)
				e2 = PR_Statement (&pr_opcodes[OP_NOT_ENT], e, 0);
			else if (t == ev_vector)
				e2 = PR_Statement (&pr_opcodes[OP_NOT_V], e, 0);
			else if (t == ev_function)
				e2 = PR_Statement (&pr_opcodes[OP_NOT_FNC], e, 0);
			else
			{
				e2 = NULL;		// shut up compiler warning;
				PR_ParseError (508, "type mismatch for \"!\" %s, not allowed", typenames[t]);
			}
			return e2;
		}


		if (PR_Check("&", tt_punct))
		{
			e = PR_Expression(1, false);
			t = e->type->type;
			if (t == ev_string || t == ev_field || t == ev_entity || t == ev_function)
				return PR_Cast(e, &type_float);
				//e->cast = ev_float;
			else if ((t == ev_float) || (t== ev_int))
				return PR_Cast(e, &type_pointer);
				//e->cast = ev_pointer;
			else
				PR_ParseError (509, "Bad type for &. %s not allowed", typenames[t]);
			return e;
		}

		if (PR_Check("*", tt_punct))
		{
			e = PR_Expression(1, false);
			t = e->type->type;
			if ((t == ev_float) || (t== ev_int))
				return PR_Cast(e, &type_entity);
				//e->cast = ev_entity;
			else
				PR_ParseError (510, "bad type for *. %s not allowed", typenames[t]);
			return e;
		}

		if (PR_Check("@", tt_punct))
		{
			e = PR_Expression(1, false);
			t = e->type->type;
			if ((t == ev_float) || (t== ev_int))
				return PR_Cast(e, &type_string);
				//e->cast = ev_string;
			else
				PR_ParseError (511, "bad type for @. %s not allowed", typenames[t]);
			return e;
		}


		if (PR_Check("(", tt_punct))
		{
			// this ought to slow it down...
			if (!STRCMP("float", pr_token) || !STRCMP("vector", pr_token) || !STRCMP("int", pr_token) 
		|| !STRCMP("string", pr_token) || !STRCMP("entity", pr_token) || !STRCMP(".", pr_token) || !STRCMP("void", pr_token))
			{
				ty = PR_ParseType();
				PR_Expect(")", tt_punct);
				e = PR_Expression(1, false);
				return PR_Cast(e, ty);
			}
			e = PR_Expression (TOP_PRIORITY, false);
			PR_Expect (")", tt_punct);
			return e;
		}
	}
	return PR_ParseValue ();
}


/*
==============
PR_Expression
==============
*/

def_t *PR_Expression (int priority, boolean returnused)
{
	opcode_t	*op, *oldop;
	def_t		*e, *e2, *e3, *e4;
	etype_t		type_a, type_b, type_c;
	dstatement_t		*patch1, *patch2;
	int i;
	int index=0;

	if (priority == 0)
		return PR_Term ();
	
	ecorrection = NULL;
	e = PR_Expression (priority-1, returnused);
	if (e->type->type == ev_int)
		expectint = true;
	e4 = e;
	while (1)
	{

		if (priority == 1 && PR_Check ("(", tt_punct) )
		{
			dontcomplain = true;
			e3 = NULL;
			if (returnused)
			{
				e3 = GimmeATempFoo(3);
				e3->type = def_ret.type;
				PR_Statement(&pr_opcodes[OP_STORE_V], &def_ret, e3);
			}
			e = PR_ParseFunctionCall (e);
			
			if (e3 != NULL)
				ecorrection = e3;
			return e;
		}
				// FrikaC : not good CHEDDAR
		if (pr_token[0] == '?' && !pr_token[1])
		{
			if (priority != 6)
				break;
			PR_Lex();
			PR_Statement(&pr_opcodes[OP_IFNOT], e, 0);
			patch1 = &statements[numstatements - 1];
			e = PR_Expression(TOP_PRIORITY, false);
			if (type_size[e->type->type] == 3)
			{
				e2 = GimmeATempFoo(3);
				PR_Statement (&pr_opcodes[OP_STORE_V], e, e2);
			}
			else
			{
				e2 = GimmeATempFoo(1);
				PR_Statement (&pr_opcodes[OP_STORE_F], e, e2);
			}
			PR_Expect(":", tt_punct);
			patch2 = &statements[numstatements];
			SimpleStatement(OP2_GOTO, 0, 0, 0);
			patch1->b = &statements[numstatements] - patch1;
			e = PR_Expression(TOP_PRIORITY, false);
			if (type_size[e->type->type] == 3)
				PR_Statement (&pr_opcodes[OP_STORE_V], e, e2);
			else
				PR_Statement (&pr_opcodes[OP_STORE_F], e, e2);
			patch2->a = &statements[numstatements] - patch2;
			return e2;
			// ugh!
		}


		index = (pr_token[0] + pr_token[1]) & (OPTABLESIZE - 1);
		op = pr_opcodes + optable[index];
		type_c = ev_void;

		if (op->priority != priority || STRCMP(op->name, pr_token))
			break;
		if ((unsigned)(optable[index] - OP_ARRAY_F) < 7)
			expectint = true;
		PR_Lex();

		if (pr_optimize_logicops && conditional)
		{
			if (op->op == OP2_AND)
			{
				patch1 = &statements[numstatements];
				SimpleStatement(OP2_IFNOT, e->ofs,0,0);
				
			}
			else if (op->op == OP2_OR)
			{
				patch1 = &statements[numstatements];
				SimpleStatement(OP2_IF, e->ofs,0,0);
			}
		}
		else if ( optable[index] >= OP_INC_F )
		{
			if (e->constant)
				PR_ParseWarning(106, "Assignment to constant");
			if (e->type->type != ev_float && e->type->type != ev_int)
				PR_ParseError(512, "Type mismatch for \"%s\"", op->name);
			if (e->type->type == ev_int)
			{
				pr_immediate_type = &type_int;
				pr_immediate._int = 1;
				expectint = true;
			}
			else
			{
				pr_immediate_type = &type_float;
				pr_immediate._float = 1;
				expectint = false;
			}
			e2 = PR_ParseImmediate();
			dontcomplain = true;
			SimpleStatement(op->op, e->ofs, e2->ofs, e->ofs);
			return e;
		}

		if ( op->right_associative )
		{
			// if last statement is an indirect, change it to an address of
			if (!shortened_store && ((unsigned)(statements[numstatements-1].op - OP2_LOAD_F) < 6))
			{
				statements[numstatements-1].op = OP2_ADDRESS;
				if (e->type->arraysize)
				{
					def_pointer.type->aux_type = e->type->aux_type;
					e->type = def_pointer.type;
				}
				else
				{
					def_pointer.type->aux_type = e->type;
					e->type = def_pointer.type;
				}
			}
			e2 = PR_Expression (priority, e->ofs == OFS_RETURN);
			if (ecorrection)
			{
				e = ecorrection;
				ecorrection = NULL;
			}
		}
		else
		{
			if ((unsigned)(optable[index] - OP_ARRAY_F) < 7)
			{
				e2 = PR_Expression (TOP_PRIORITY, e->ofs == OFS_RETURN);
				if (e2->type->type != ev_int)
					PR_ParseWarning(107, "Array index should be type int");
				if (ecorrection)
				{
					e = ecorrection;
					ecorrection = NULL;
				}
				PR_Expect("]", tt_punct);
				if (e->type->arraysize)
					type_c = e->type->aux_type->type;
			}
			else
			{
				e2 = PR_Expression (priority-1, e->ofs == OFS_RETURN);
				if (ecorrection)
				{
					e = ecorrection;
					ecorrection = NULL;
				}
			}
		}
		newop:
		// type check
		type_a = PR_GetType(e);
		type_b = PR_GetType(e2);


		// to keep the op count low, we'll lie

		if ((type_a == ev_int && type_b == ev_float) || (type_a == ev_float && type_b == ev_int))
			PR_ParseWarning(108, "Mixed float and int types");
		if (op->type_a->type->type == ev_float && type_a == ev_float && expectint)
			PR_ParseWarning(109, "Expecting int, float a parameter found");
		if (op->type_b->type->type == ev_float && type_b == ev_float && expectint)
			PR_ParseWarning(110, "Expecting int, float b parameter found");

		if (op->name[0] == '.')// field access gets type from field
		{
			if (e2->type->aux_type)
				type_c = e2->type->aux_type->type;
			else
				type_c = ev_bad;	// not a field
		}
		
		if (type_a == ev_int)
		{
			type_a = ev_float;
			expectint = true;
		}
		if (type_b == ev_int)
		{
			type_b = ev_float;
			expectint = true;
		}
		if (type_c == ev_int)
			type_c = ev_float; // blarg

		oldop = op;
		while (type_a != op->type_a->type->type || type_b != op->type_b->type->type || (type_c != ev_void && type_c != op->type_c->type->type) )
		{
			op++;
			if (!op->name || STRCMP(op->name, oldop->name))
				PR_ParseError (513, "Type mismatch for \"%s\". Types %s and %s not allowed", oldop->name, typenames[type_a], typenames[type_b]);
		}
		
		if (type_a == ev_pointer && statements[numstatements-1].op == OP2_ADDRESS && type_b != e->type->aux_type->type)
			PR_ParseError (512, "Type mismatch for \"%s\"", op->name);

		if (optable[index] >= OP_ADD_STORE_F)
		{
			dontcomplain = true;

			//if (e->constant)
			//	PR_ParseWarning(106, "Assignment to constant");

			//SimpleStatement(op->op, e->ofs, e2->ofs, e->ofs);
			
			e = PR_Statement(&pr_opcodes[op->op], e, e2); // hack hack hack
			e2 = e4;
			op = &pr_opcodes[OP_STORE_F];
			index = 0;
		
			goto newop;
		 
		}

		if (pr_optimize_logicops && conditional)
		{
			if (op->op == OP2_AND || op->op == OP2_OR)
			{
				patch1->b = &statements[numstatements] - patch1;
				num_logic_jumps++;
			}
		}

		if (op->right_associative)
		{
			if ((unsigned)(op->op - OP2_STORE_F) < 6)
			{
				if (e->constant)
					PR_ParseWarning(106, "Assignment to constant");
			}
			dontcomplain = true;
			if (conditional)
				PR_ParseWarning(201, "Assignment in conditional");
			e = PR_Statement (op, e2, e);
		}
		else
			e = PR_Statement (op, e, e2);
		if (e4->type->arraysize)
			e->type = e4->type->aux_type;
		else if (type_c != ev_void)	// field access gets type from field
			e->type = e2->type->aux_type;
	
		else if (op->type_c->type->type == ev_float && expectint)
			e->type = &type_int;
		if (e->type && e->type->type == ev_int)
			expectint = true;
		else
			expectint = false;

	}
	expectint = false;

	return e;

}
/* ---------------

FrikaC ---- Label Stuff

------------------- */

int num_continues = 0;
int num_breaks = 0;
int	 pr_continues[32];
int  pr_breaks[32];

extern int PR_FindDefine (char *name);

int GetLabelDef (char *tname, boolean makenew)
{
	dstatement_t		*curpos;
	int i;

	curpos = &statements[numstatements];
	i = PR_FindDefine(tname);

	if (i)
	{
		if(makenew)
		{
			if(pr_defines[i].defined)
				PR_ParseWarning(111, "Redifinition of label %s", tname);
			pr_defines[i].defined = true;
			statements[pr_defines[i].value._int].a = curpos 
				  - &statements[pr_defines[i].value._int] ;
			pr_defines[i].value._int = numstatements;
			return &statements[pr_defines[i].value._int] - curpos;
		}
		else
		{
			if(pr_defines[i].defined)
				return &statements[pr_defines[i].value._int] - curpos;
		}
	}
	num_defines++;
	pr_defines[num_defines].name = (char *)PR_Malloc(strlen(tname) + 1);
	strcpy(pr_defines[num_defines].name, tname);
	macrohash[hash(tname)] = num_defines;
	pr_defines[num_defines].value._int = numstatements;
	pr_defines[num_defines].label = true;
	if (makenew)
		pr_defines[num_defines].defined = true;
	else
		pr_defines[num_defines].defined = false;

	return &statements[pr_defines[num_defines].value._int] - curpos;

}


/*
============
PR_ParseStatement

============
*/

void PR_ParseStatement (void)
{
	def_t				*e;
	dstatement_t		*patch1, *patch2, *patch3;
	int					old_numstatements, numtemp;
	dstatement_t		temp[32];
	int					linenum[32];
	int					breaks = 0, continues = 0;
	int i;
	
	// FrikaC: clean up temps
	memset(used_temps, 0, sizeof(used_temps));

	if (PR_Check ("{", tt_punct))
	{
		while (!PR_Check ("}", tt_punct))
			PR_ParseStatement ();
		return;
	}
	
	if (PR_Check("return", tt_name))
	{
		if (PR_Check (";", tt_punct))
		{
			if(pr_scope->type->aux_type != &type_void)
				PR_ParseWarning(202, "%s must return a value", pr_scope->name);
			PR_Statement (&pr_opcodes[OP_RETURN], 0, 0);
			return;
		}
		e = PR_Expression (TOP_PRIORITY, false);
		PR_Expect (";", tt_punct);
		if(pr_scope->type->aux_type != e->type)
		{
			if (pr_scope->type->aux_type == &type_void)
				PR_ParseWarning(203, "%s does not return a value", pr_scope->name);
			else
			{
				PR_ParseWarning(204, "%s: Type mismatch on return. Expected %s found %s", pr_scope->name, 
				typenames[pr_scope->type->aux_type->type], typenames[e->type->type]);
			}
		}
		PR_Statement (&pr_opcodes[OP_RETURN], e, 0);
		return;		
	}
	
	if (PR_Check("while", tt_name))
	{
		PR_Expect ("(", tt_punct);
		patch2 = &statements[numstatements];
		continues = num_continues;
		breaks = num_breaks;
		conditional = true;
		e = PR_Expression (TOP_PRIORITY, false);
		conditional = false;
		PR_Expect (")", tt_punct);
		PR_Statement (&pr_opcodes[OP_IFNOT], e, 0);
		patch1 = &statements[numstatements-1];
		PR_ParseStatement ();
		SimpleStatement(OP2_GOTO, patch2 - &statements[numstatements], 0, 0);
		patch1->b = &statements[numstatements] - patch1;
		if (breaks != num_breaks)
		{
			for(i = breaks; i < num_breaks; i++)
			{	
				patch1 = &statements[pr_breaks[i]];
				statements[pr_breaks[i]].a = &statements[numstatements] - patch1;
			}
			num_breaks = breaks;
		}
		if (continues != num_continues)
		{
			for(i = continues; i < num_continues; i++)
			{
				statements[pr_continues[i]].a = patch2 - &statements[pr_continues[i]];
			}
			num_continues = continues;
		}
		return;
	}
	if (PR_Check("for", tt_name))
	{
		PR_Expect("(", tt_punct);
		if (!PR_Check(";", tt_punct))
		{
			PR_Expression(TOP_PRIORITY, false);
			PR_Expect(";", tt_punct);
		}

		patch2 = &statements[numstatements];
		conditional = true;
		e = PR_Expression(TOP_PRIORITY, false);
		conditional = false;
		PR_Expect(";", tt_punct);
		// I dunno -- do you know?
		continues = num_continues;
		breaks = num_breaks;

		old_numstatements = numstatements;
		PR_Expression(TOP_PRIORITY, false);
		numtemp = numstatements - old_numstatements;
		if (numtemp > 32)
			PR_ParseError(514, "Update expression too large");
		numstatements = old_numstatements;
		for (i = 0 ; i < numtemp ; i++)
		{
			linenum[i] = statement_linenums[numstatements + i];
			temp[i] = statements[numstatements + i];
		}

		PR_Expect(")", tt_punct);
		PR_Statement(&pr_opcodes[OP_IFNOT], e, 0);
		patch1 = &statements[numstatements-1];
		PR_ParseStatement();
		patch3 = &statements[numstatements];
		for (i = 0 ; i < numtemp ; i++)
		{
			statement_linenums[numstatements] = linenum[i];
			statements[numstatements++] = temp[i];
		}
		SimpleStatement(OP2_GOTO, patch2 - &statements[numstatements], 0, 0);
		patch1->b = &statements[numstatements] - patch1;
		if (breaks != num_breaks)
		{
			for(i = breaks; i < num_breaks; i++)
			{	
				patch1 = &statements[pr_breaks[i]];
				statements[pr_breaks[i]].a = &statements[numstatements] - patch1;
			}
			num_breaks = breaks;
		}
		if (continues != num_continues)
		{
			for(i = continues; i < num_continues; i++)
			{
				patch1 = &statements[pr_continues[i]];
				statements[pr_continues[i]].a = patch3 - patch1;
			}
			num_continues = continues;
		}
		return;
	}
	if (PR_Check("do", tt_name))
	{
		patch1 = &statements[numstatements];
		continues = num_continues;
		breaks = num_breaks;
		PR_ParseStatement ();
		PR_Expect ("while", tt_name);
		PR_Expect ("(", tt_punct);
		conditional = true;
		e = PR_Expression (TOP_PRIORITY, false);
		conditional = false;
		PR_Expect (")", tt_punct);
		PR_Expect (";", tt_punct);
		PR_Statement (&pr_opcodes[OP_IF], e, 0);
		patch2 = &statements[numstatements - 1];
		patch2->b = patch1 - patch2;

		// God I mucked a lot of stuff up...
		if (breaks != num_breaks)
		{
			for(i = breaks; i < num_breaks; i++)
			{
				patch2 = &statements[pr_breaks[i]];
				statements[pr_breaks[i]].a = &statements[numstatements] - patch2;
			}
			num_breaks = breaks;
		}
		if (continues != num_continues)
		{
			for(i = continues; i < num_continues; i++)
			{
				patch2 = &statements[pr_continues[i]];
				statements[pr_continues[i]].a = patch1 - patch2;
			}
			num_continues = continues;
		}
		return;
	}
	
	if (PR_Check("local", tt_name))
	{
	
		PR_ParseDefs ();
		locals_end = numpr_globals;
		return;
	}

	if (PR_Check("goto", tt_name))
	{
		//PR_Lex();
		if (pr_token_type != tt_name)
		{
			PR_ParseError(515, "Invalid label name \"%s\"", pr_token);
			return;
		}
		SimpleStatement(OP2_GOTO, GetLabelDef(pr_token, false), 0, 0);
		PR_Lex();
		PR_Expect(";", tt_punct);
		return;
	}
	if (PR_Check("state", tt_name))
	{
		PR_Expect("[", tt_punct);
		PR_ParseState();
		PR_Expect(";", tt_punct);
		return;
	}
	if (PR_Check(":", tt_punct)) //labels
	{
		if (pr_token_type != tt_name)
		{
			PR_ParseError(515, "invalid label name \"%s\"", pr_token);
			return;
		}
		GetLabelDef(pr_token, true);
		PR_Lex();
		return;
	}
		
	if (PR_Check("if", tt_name))
	{
		PR_Expect ("(", tt_punct);
		conditional = true;
		e = PR_Expression (TOP_PRIORITY, false);
		conditional = false;
		PR_Expect (")", tt_punct);
		
		PR_Statement (&pr_opcodes[OP_IFNOT], e, 0);
		patch1 = &statements[numstatements-1];
		
		PR_ParseStatement ();
		
		if (PR_Check ("else", tt_name))
		{
			patch2 = &statements[numstatements];
			if((&statements[numstatements] - patch1) == 1)
				PR_ParseWarning(112, "Null 'if' statement");
			PR_Statement (&pr_opcodes[OP_GOTO], 0, 0);
			patch1->b = &statements[numstatements] - patch1;
			PR_ParseStatement ();
			patch2->a = &statements[numstatements] - patch2;
			if((&statements[numstatements] - patch2) == 1)
				PR_ParseWarning(113, "Null 'else' statement");

		}
		else
		{
			if((&statements[numstatements] - patch1) == 1)
				PR_ParseWarning(112, "Null 'if' statement");

			patch1->b = &statements[numstatements] - patch1;
		}
		return;
	}
	if (PR_Check("break", tt_name))
	{
		if (num_breaks > 32)
			PR_ParseError(516, "Cannot have more than 32 breaks in a loop");
		pr_breaks[num_breaks] = numstatements;
		PR_Statement (&pr_opcodes[OP_GOTO], 0, 0);
		num_breaks++;
		PR_Expect(";", tt_punct);
		return;
	}
	if (PR_Check("continue", tt_name))
	{
		if (num_continues > 32)
			PR_ParseError(517, "Cannot have more than 32 continues in a loop");
		pr_continues[num_continues] = numstatements;
		PR_Statement (&pr_opcodes[OP_GOTO], 0, 0);
		num_continues++;
		PR_Expect(";", tt_punct);
		return;
	}
	//FrikaC begin


	if (!STRCMP("float", pr_token) || !STRCMP("vector", pr_token) || !STRCMP("int", pr_token) 
		|| !STRCMP("string", pr_token) || !STRCMP("entity", pr_token) || !STRCMP("const", pr_token) || !STRCMP("var", pr_token))
	{

		PR_ParseDefs ();
		locals_end = numpr_globals;
		return;
	}

	// FrikaC end
	dontcomplain = false;
	e = PR_Expression (TOP_PRIORITY, false);
	if ((e->type->type != ev_void) && !dontcomplain)
		PR_ParseWarning(205, "No operation performed");
	PR_Expect (";", tt_punct);
}


/*
==============
PR_ParseState

States are special functions made for convenience.  They automatically
set frame, nextthink (implicitly), and think (allowing forward definitions).

// void() name = [framenum, nextthink] {code}
// expands to:
// function void name ()
// {
//		self.frame=framenum;
//		self.nextthink = time + 0.1;
//		self.think = nextthink
//		<code>
// };
==============
*/
void PR_ParseState (void)
{
	char	*name;
	def_t	*s1, *def;
	
	if (pr_token_type != tt_immediate || pr_immediate_type != &type_float)
		PR_ParseError (518, "State frame must be a number");
	s1 = PR_ParseImmediate ();
	PR_Lex();
	PR_Expect (",", tt_punct);

	name = PR_ParseName ();
	def = PR_GetDef (&type_function, name,0, true, 1, 0);
		
	PR_Expect ("]", tt_punct);
	
	PR_Statement (&pr_opcodes[OP_STATE], s1, def);
}

/*
============
PR_ParseImmediateStatements

Parse a function body
============
*/
function_t *PR_ParseImmediateStatements (type_t *type)
{
	int			i;
	function_t	*f;
	def_t		*defs[MAX_PARMS + MAX_EXTRA_PARMS];
	def_t		*e2;
	f = (struct function_s *) PR_Malloc (sizeof(function_t));

//
// check for builtin function definition #1, #2, etc
//
	if (PR_Check ("#", tt_punct))
	{
		if (pr_token_type != tt_immediate || pr_immediate_type != &type_float || pr_immediate._float != (int)pr_immediate._float)
			PR_ParseError(519, "Bad builtin immediate");
		f->builtin = (int) pr_immediate._float;
		PR_Lex();
		return f;
	}
	
	f->builtin = 0;
//
// define the parms
//
	for (i=0 ; i<type->num_parms ; i++)
	{
		defs[i] = PR_GetDef (type->parm_types[i], pr_parm_names[i], pr_scope, true, -1, -1);
		if (i > MAX_PARMS)
			continue;
		f->parm_ofs[i] = defs[i]->ofs;
		if (i > 0 && f->parm_ofs[i] < f->parm_ofs[i-1])
			Sys_Error("Q611: Bad parm order");
	}
	
	f->code = numstatements;
	if (type->num_parms > MAX_PARMS) // locutus
	{
		for (i = MAX_PARMS; i < type->num_parms; i++)
		{
			if (!extra_parms[i - MAX_PARMS])
			{
				e2 = (struct def_s *) PR_Malloc (sizeof(def_t));
				e2->ofs = numpr_globals;
				e2->name = "extra parm";
				pr_global_defs[numpr_globals] = e2;
				numpr_globals += 3;
				extra_parms[i - MAX_PARMS] = e2;
			}
			if (defs[i]->type->type != ev_vector)
				PR_Statement (&pr_opcodes[OP_STORE_F], extra_parms[i - MAX_PARMS], defs[i]);
			else
				PR_Statement (&pr_opcodes[OP_STORE_V], extra_parms[i - MAX_PARMS], defs[i]);
		}
	}
//
// check for a state opcode
//
	if (PR_Check ("[", tt_punct))
		PR_ParseState ();
		
//
// parse regular statements
//
	PR_Expect ("{", tt_punct);

	while (!PR_Check("}", tt_punct))
		PR_ParseStatement ();
	// this is cheap
	if (type->aux_type != &type_void)
		if (statements[numstatements - 1].op != OP2_RETURN)
			PR_ParseWarning(206, "%s: not all control paths return a value", pr_scope->name );
// emit an end of statements opcode
	PR_Statement (pr_opcodes, 0,0);


	return f;
}


int PR_DefineElements(def_t *def, int save)
{
	def_t	*elem;
	int i, flag = true, start;
	char element[MAX_NAME];
	if (!def->constant)
		def->defined = true;

	start = numpr_globals;
	for (i = 0; i < def->type->arraysize; i++)
	{
		sprintf(element, "%s_%i", def->name, i);
		
		if (def->type->aux_type->arraysize)
		{
			elem = PR_GetDef(def->type->aux_type, element, def->scope, false, -1, save);
			if (elem)
			{
				flag = PR_DefineElements(elem, save);
				start = def->arraystart;
			}
			else
			{
				elem = PR_GetDef(&type_void, element, def->scope, true, -1, save);
				elem->type = def->type->aux_type;
				elem->constant = true;
				elem->defined = true;
				elem->save = false;
				flag = false;
			}
		}
		else
			elem = PR_GetDef(def->type->aux_type, element, def->scope, true, -1, save);
		elem->defined = true;
	}
	def->arraystart = start;
	return flag;
}
void PR_GetArray (def_t *def, int save)
{
	while(!PR_DefineElements(def, save));
};

/*
============
PR_GetDef

If type is NULL, it will match any type
If allocate is true, a new def will be allocated if it can't be found
============
*/

def_t *PR_GetDef (type_t *type, char *name, def_t *scope, boolean allocate, int constant, int save)
{
	def_t	*def, *elem;
	char element[MAX_NAME];

	int index, i, m;
	struct hash_element *cell;

	index = hash(name);
	for (cell = htable[index]; cell != NULL; cell = cell->next) 
	{
		def = cell->def;
		if ( !STRCMP(def->name, name) ) 
		{
			if (def->scope && (def->scope != scope))
			{
				if (!scope && allocate)
					PR_ParseWarning(301, "%s defined as local in %s", def->name, def->scope->name);
				continue;
			}
			if (type && def->type->type != type->type)
			{
				
				if (scope == def->scope)
					PR_ParseError (520, "Type mismatch on redeclaration of %s (see original definition %s(%i))", name, def->s_file, def->line);
				else
				{
					PR_ParseWarning(207, "%s redeclared on different scope (see original definition %s(%i))", name, def->s_file, def->line);
					continue;
				}
			}
			else if (type && (def->type->type == ev_field))
			{
				if (type->aux_type->type != def->type->aux_type->type)
				{	
					if (scope == def->scope)
						PR_ParseWarning (114, "Type mismatch on redeclaration of %s (see original definition %s(%i))", name, def->s_file, def->line);
					else
					{
						PR_ParseWarning(207, "%s redeclared on different scope (see original definition %s(%i))", name, def->s_file, def->line);
						continue;
					}
				}
			}
			else if (type && (def->type->type == ev_function))
			{
				if (type->aux_type->type != def->type->aux_type->type)
						PR_ParseWarning (114, "Type mismatch on redeclaration of %s (see original definition %s(%i))", name, def->s_file, def->line);
				else if (type->num_parms != def->type->num_parms)
						PR_ParseWarning (115, "%s redeclared with different number of parms (see original definition %s(%i))", name, def->s_file, def->line);
				else
				{
					for (i = 0; i < type->num_parms; i++)
						if (type->parm_types[i]->type != def->type->parm_types[i]->type)
							PR_ParseWarning (128, "%s rededeclared with different parms (see original definition %s(%i))", name, def->s_file, def->line);
				}
			}
			if (scope && allocate)
				PR_ParseWarning(116, "Local %s redeclared", name);

 			pr_global_refs[def->ofs]++;
			return def;
		}
	}

	if (!allocate)
		return NULL;
	

// allocate a new def
	def = (struct def_s *) PR_Malloc (sizeof(def_t));
	def->next = NULL;
	pr.def_tail->next = def;
	pr.def_tail = def;

	cell = (struct hash_element *) PR_Malloc (sizeof(struct hash_element));
	cell->next = htable[index];
	cell->def = def;
	htable[index] = cell;
	stats[index]++;

	def->name = (char *) PR_Malloc(strlen(name)+1);
	strcpy(def->name, name);
	def->type = type;


	if (constant == 1)
		def->constant = 1;
	else if (constant == -1)
		def->constant = 0;
	else if (type->arraysize)
		def->constant = 0;
	else if (scope)
		def->constant = 0;
	else if ((type->type == ev_field) || (type->type == ev_function))
		def->constant = 1;
	else
		def->constant = 0;

	if (save == 1)
		def->save = 1;
	else if (save == -1)
		def->save = 0;
	else if (scope)
		def->save = 0;
	else if ((type->type == ev_field) && (def->constant == 1))
		def->save = 1;
	else if (def->constant)
		def->save = 0;
	else
		def->save = 1;

	def->s_file = s_file + strings;
	def->line = pr_source_line;
	def->scope = scope;
	def->tempnext = NULL;

	def->ofs = numpr_globals;
	pr_global_defs[numpr_globals] = def;

	if (!pr_system) // pr_sytem == true means system defs are done
		pr_global_refs[numpr_globals] = 1;
	if(!STRCMP(def->name, "end_sys_fields"))
		pr_system = true;
//
// make automatic defs for the vectors elements
// .origin can be accessed as .origin_x, .origin_y, and .origin_z
//
	if (type->type == ev_vector)
	{		
		sprintf (element, "%s_x",name);
		elem = PR_GetDef (&type_float, element, scope, true, constant, save);
		elem->defined = true; // hack
		pr_global_refs[numpr_globals]++;// FrikaC

		sprintf (element, "%s_y",name);
		elem = PR_GetDef (&type_float, element, scope, true, constant, save);
		elem->defined = true;
		pr_global_refs[numpr_globals]++;// FrikaC

		sprintf (element, "%s_z",name);
		elem = PR_GetDef (&type_float, element, scope, true, constant, save);
		elem->defined = true;
		pr_global_refs[numpr_globals]++;// FrikaC

	}
	else
		numpr_globals += type_size[type->type];

	if ((type->type == ev_field) && def->constant)
	{
		*(int *)&pr_globals[def->ofs] = pr.size_fields;
		def->defined = true;
		
		if (type->aux_type->type == ev_vector)
		{
			sprintf (element, "%s_x",name);
			PR_GetDef (&type_floatfield, element, scope, true, constant, save);
			
			sprintf (element, "%s_y",name);
			PR_GetDef (&type_floatfield, element, scope, true, constant, save);
			
			sprintf (element, "%s_z",name);
			PR_GetDef (&type_floatfield, element, scope, true, constant, save);
		}
		else
			pr.size_fields += type_size[type->aux_type->type];
	}
	else if (type->arraysize)
		PR_GetArray(def, save);
//	if (pr_dumpasm)
//		PR_PrintOfs (def->ofs);
		
	return def;
}

/*
================
PR_ParseDefs

Called at the outer layer and when a local statement is hit
================
*/


void PR_ParseDefs (void)
{
	char		*name;
	type_t		*type, *check;
	def_t		*def, *odef;
	function_t	*f;
	dfunction_t	*df;
	int			i, size;
	int			locals_start, test;
	int constant = 0;
	int save = 0;
	static char	ident[MAX_NAME];
	
	struct hash_element *cell = NULL;
	if (PR_Check("nosave", tt_name))
		save = -1;
	if (PR_Check("var", tt_name))
		constant = -1;
	else if (PR_Check("const", tt_name))
		constant = 1;

	type = PR_ParseType ();
	
	do
	{
		strcpy(ident, PR_ParseName());

		if (PR_Check("(", tt_punct) && type->type != ev_function)
		{
			// HexenC/C/C++ style function declaration
			type = PR_ParseTypeFunction(type);
		}
		if (!STRCMP(pr_token, "="))
		{
			if (constant == 0)
				constant = 1;
			if (type->type == ev_int)
				expectint = true;
		}
		def = PR_GetDef (type, ident, pr_scope, true, constant, save);
// check for an initialization
		if ( PR_Check ("=", tt_punct) || ((type->type == ev_function) && (pr_token[0] == '{')))
		{

			if (def->defined)
				PR_ParseError (521, "%s redeclared (see previous declaration %s(%i))", ident, def->s_file, def->line);
					
			def->s_file = s_file + strings;
			def->line = pr_source_line;
			def->defined = true;

			if (type->type == ev_function)
			{
				num_continues = num_breaks = 0;
				locals_start = locals_end = numpr_globals;
				pr_scope = def;
				test = numstatements;	
				f = PR_ParseImmediateStatements (type);
				pr_scope = NULL;
				G_FUNCTION(def->ofs) = numfunctions;
				f->def = def;
//				if (pr_dumpasm)
//					PR_PrintFunction (def);

		// fill in the dfunction
				df = &functions[numfunctions];
				numfunctions++;
				if (f->builtin)
				{
					if (pr_optimize_function_names)
						df->s_name = f->def->name - strings;
					else
						df->s_name = CopyString (f->def->name, 0);
					num_funcs_saved += strlen(f->def->name) + 1;
					df->first_statement = -f->builtin;
				}
				else
				{	
					df->s_name = CopyString (f->def->name, 0);
					df->first_statement = f->code;
				}

				df->s_file = s_file;
				df->numparms =  f->def->type->num_parms;
				size = 0;
				for (i = 0 ; i < df->numparms ; i++)
				{
					df->parm_size[i] = type_size[f->def->type->parm_types[i]->type];
					size += df->parm_size[i];
				}
				df->locals = locals_end - locals_start;
				df->parm_start = locals_start;

				if (warninglevel >= 3)
				{
					for(i = locals_start; i < locals_end; i++)
					{
						if (pr_global_defs[i] && pr_global_defs[i]->scope == def)
						{
							if (pr_global_refs[i] == 0)
							{
								if (i - locals_start < size) 
									PR_ParseWarning(401, "In function %s parameter %s is unused", f->def->name, pr_global_defs[i]->name);
								else
									PR_ParseWarning(302, "Unreferenced local variable %s from line %i", pr_global_defs[i]->name, pr_global_defs[i]->line);
							}
							else if (pr_global_defs[i]->type->type == ev_vector)
							{
								// vector types are weird..
								if (pr_global_refs[i] + pr_global_refs[i+1] + pr_global_refs[i+2] == 3)
								{
									if (i - locals_start < size) 
										PR_ParseWarning(401, "In function %s parameter is %s unused", f->def->name, pr_global_defs[i]->name);
									else
										PR_ParseWarning(302, "Unreferenced local variable %s from line %i", pr_global_defs[i]->name, pr_global_defs[i]->line);
								}
								i+=2;
							}
						}
					}
				}
				if (num_breaks != 0)				
					PR_ParseError(522, "Illegal break in function %s", strings + df->s_name);
				if (num_continues != 0)
					PR_ParseError(523, "Illegal continue in function %s", strings + df->s_name);

				continue;
			}
			else if (type->arraysize)
			{
				i = 1;
				check = type->aux_type;
				while(check->arraysize)
				{
					i *= check->arraysize;
					check = check->aux_type;
				}
				size = i * def->type->arraysize + def->arraystart;
				if (i == 1)
					i = def->arraystart;
				else
					i += def->arraystart;
				if (pr_immediate_type != check)
					PR_ParseError (524, "Wrong immediate type for %s, Expected %s found %s", ident, typenames[check->type], typenames[pr_immediate_type->type]);
			}
			else if (pr_immediate_type != type)
				PR_ParseError (524, "Wrong immediate type for %s, Expected %s found %s", ident, typenames[type->type], typenames[pr_immediate_type->type]);
			if (type->arraysize)
			{
				
				do
				{
					if (pr_token_type != tt_name)
						if (pr_immediate_type == &type_string)
							pr_immediate.string = CopyString (pr_immediate_string, pr_immediate_strlen);
					memcpy (&(pr_globals[i]), &pr_immediate, 4*type_size[pr_immediate_type->type]);
					PR_Lex();
					i+=type_size[pr_immediate_type->type];
					if (i > size)
						PR_ParseWarning(117, "%s: too many initializers", def->name);
				}
				while (PR_Check(",", tt_punct));
				
			}
			else if (pr_optimize_defs && (odef = PR_GetImmediate()))
			{
				PR_GetImmediate();
				if (def->ofs == numpr_globals - 1)
				{
					numpr_globals--;
					def->ofs = odef->ofs;
					num_defs += 1;

				}
				else
				{
					memcpy (pr_globals + def->ofs, pr_globals + odef->ofs, 4*type_size[pr_immediate_type->type]);
				}
			}
			else
			{
				cell = (struct hash_element *) PR_Malloc (sizeof(struct hash_element));
				cell->next = htable[pr_immediate_index];
				cell->def = def;
				htable[pr_immediate_index] = cell;
				stats[pr_immediate_index]++;
				if (pr_token_type != tt_name)
					if (pr_immediate_type == &type_string)
						pr_immediate.string = CopyString (pr_immediate_string, pr_immediate_strlen);

				memcpy (pr_globals + def->ofs, &pr_immediate, 4*type_size[pr_immediate_type->type]);
			}
			PR_Lex ();
		}
		def->s_file = s_file + strings;
		def->line = pr_source_line;
		expectint = false;

	} while (PR_Check (",", tt_punct));

//	PR_Expect (";", tt_punct);
	PR_Check(";", tt_punct);


}

/*
============
PR_CompileFile

compiles the 0 terminated text, adding defintions to the pr structure
============
*/
char *pr_start;


boolean	PR_CompileFile (char *string, char *filename)
{	
	char *k;
	if (!pr.memory)
		Sys_Error("Q612: PR_CompileFile: Didn't clear");

	PR_ClearGrabMacros ();	// clear the frame macros
	pr_start = pr_file_p = string;
	if (pr_optimize_filenames)
	{
		k = (char *)PR_Malloc(strlen(filename) + 1);
		strcpy(k, filename);
		s_file = k - strings;
		num_files_saved += strlen(filename) + 1;
	}
	else
		s_file = CopyString (filename, 0);

	pr_source_line = 0;
	
	PR_NewLine ();

	PR_Lex ();	// read first token

	while (pr_token_type != tt_eof)
	{
		if (setjmp(pr_parse_abort))
		{
			if (++pr_error_count > MAX_ERRORS)
				return false;
			PR_SkipToSemicolon ();
			if (pr_token_type == tt_eof)
				return false;		
		}

		pr_scope = NULL;	// outside all functions
		
		PR_ParseDefs ();
	}
	
	return (pr_error_count == 0) ? true: false;
}