#ifndef HASH_H
#define HASH_H

//
// todo:
//
// - Try to reuse temps holding addresses/indirect loads within same function
//

struct hash_element 
{
	struct hash_element *next;
	def_t *def;
};

#define HSIZE1 16384

// JPG
#define OPTABLESIZE 128

extern struct hash_element *htable[HSIZE1];
extern void inithash();
extern int optable[OPTABLESIZE]; // JPG


extern int stats[HSIZE1];

extern unsigned int hash (char *string);

#endif

void HashImmediate (void);