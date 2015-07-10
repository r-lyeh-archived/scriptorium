#include "qcc.h"
#include "hash.h"


unsigned int hash (char *string)
{
	unsigned int index = 0;
	int count;
	
	for (count = 0; string[count] != '\0' && count < 15; count++)
		index = (index << 1) ^ string[count];
	
	return index & (HSIZE1 - 1);
}

struct hash_element *htable[HSIZE1];
int optable[OPTABLESIZE];

int stats[HSIZE1];

void
inithash()
{
	int i=0;
	int index=0;

	for (i=0; i<HSIZE1; i++) 
	{
		htable[i] = NULL;
		stats[i] = 0;
	}

	// JPG - added optable
	for (i = 0 ; i < OPTABLESIZE ; i++)
		optable[i] = 0;
	for (i = 0 ; pr_opcodes[i].name ; i++)
	{
		if (pr_opcodes[i].priority > 0)
		{
			if (STRCMP(pr_opcodes[i].name, pr_opcodes[i-1].name))
			{
				index = (pr_opcodes[i].name[0] + pr_opcodes[i].name[1]) & (OPTABLESIZE - 1);
				if (optable[index])
				{
					Sys_Error("Q607: optable conflict between %s and %s!\n", pr_opcodes[optable[index]].name, pr_opcodes[i].name);
				}
				optable[index] = i;
			}
		}
	}
}

void HashImmediate (void)
{
	char tmpchar[40];
	if (pr_immediate_type == &type_string) 
		pr_immediate_index = hash(pr_immediate_string);
	else if (pr_immediate_type == &type_float) 
	{
		sprintf(tmpchar, "%d", pr_immediate._int);
		pr_immediate_index = hash(tmpchar);
	} 
	else if (pr_immediate_type == &type_int) 
	{
		sprintf(tmpchar, "%d", pr_immediate._int);
		pr_immediate_index = hash(tmpchar);
	} 
	else if (pr_immediate_type == &type_vector) 
	{
		char tmpchar[80];
		sprintf(tmpchar, "%.4f,%.4f,%.4f", pr_immediate.vector[0], pr_immediate.vector[1], pr_immediate.vector[2]);
		pr_immediate_index = hash(tmpchar);
	} 
	else
		PR_ParseError (501, "invalid immediate type %s", typenames[pr_immediate_type->type]);		
}