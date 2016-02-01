

#include <math.h>

#include <sgs_int.h>

#include "sgs_prof.h"



typedef struct _mode1item
{
	sgs_StackFrame* frame;
	double startTime;
}
mode1item;

static void mode1hook( void* userdata, SGS_CTX, int evid )
{
	sgs_ProfData* P = (sgs_ProfData*) userdata;
	if( P->hfn )
		P->hfn( P->hctx, C, evid );
	
	if( evid == SGS_HOOK_ENTER || evid == SGS_HOOK_CONT ||
		evid == SGS_HOOK_EXIT || evid == SGS_HOOK_PAUSE )
	{
		double TM = sgs_GetTime();
		sgs_StackFrame* target = NULL, *sf, *lastrec = NULL;
		mode1item* items = (mode1item*) (void*) SGS_ASSUME_ALIGNED( P->frametmp.ptr, 4 );
		size_t i, itemcount = P->frametmp.size / sizeof(mode1item);
		
		if( evid == SGS_HOOK_ENTER || evid == SGS_HOOK_CONT )
			target = C->sf_last;
		else if( evid == SGS_HOOK_EXIT )
			target = C->sf_last->prev;
		/* pause => null */
		
		/* ignore the case when target = current */
		if( itemcount && items[ itemcount - 1 ].frame == target )
			return;
		
		/* find target in stack */
		for( i = 0; i < itemcount; ++i )
		{
			if( items[ i ].frame == target )
				break;
		}
		if( i == itemcount )
		{
			/* find last recorded frame in call stack */
			sf = NULL;
			if( itemcount )
			{
				sf = C->sf_first;
				while( sf )
				{
					if( items[ itemcount - 1 ].frame == sf )
						break;
					sf = sf->next;
				}
			}
			lastrec = sf;
			if( sf == NULL || target == NULL )
				i = 0; /* frame not found/req., remove everything */
			/* otherwise, remove nothing, it's an "ENTER" */
		}
		else
			i++; /* remove starting from next frame */
		
		/* if there is anything to remove */
		if( i < itemcount )
		{
			size_t bki = i;
			sf = C->sf_first;
			/* generate core key (from start to first removed incl.) */
			sgs_membuf_resize( &P->keytmp, C, 0 );
			while( sf && sf != items[ i ].frame )
			{
				const char* fname = "<error>";
				sgs_StackFrameInfo( C, sf, &fname, NULL, NULL );
				sgs_membuf_appbuf( &P->keytmp, C, fname, strlen( fname ) );
				sgs_membuf_appchr( &P->keytmp, C, 0 );
				sf = sf->next;
			}
			
			/* iterate removable frames (assuming frame pointers are valid) */
			for( ; i < itemcount; ++i )
			{
				const char* fname = "<error>";
				sgs_StackFrameInfo( C, items[ i ].frame, &fname, NULL, NULL );
				sgs_membuf_appbuf( &P->keytmp, C, fname, strlen( fname ) );
				sgs_membuf_appchr( &P->keytmp, C, 0 );
				
				/* commit time addition */
				{
					double prevTM = items[ i ].startTime;
					sgs_VHTVar* pair = sgs_vht_get_str( &P->prof->timings, P->keytmp.ptr,
						(uint32_t) P->keytmp.size, sgs_HashFunc( P->keytmp.ptr, P->keytmp.size ) );
					if( pair )
					{
						pair->val.data.R += TM - prevTM;
					}
					else
					{
						sgs_Variable key, val = sgs_MakeReal( TM - prevTM );
						sgs_InitStringBuf( C, &key, P->keytmp.ptr, (sgs_SizeVal) P->keytmp.size );
						sgs_vht_set( &P->prof->timings, C, &key, &val );
						sgs_Release( C, &key );
					}
				}
			}
			
			/* commit size */
			sgs_membuf_resize( &P->frametmp, C, bki * sizeof(mode1item) );
			itemcount = bki;
		}
		
		/* add new frame start times */
		if( lastrec || ( target != NULL && itemcount == 0 ) )
		{
			if( lastrec )
				sf = lastrec->next;
			else
				sf = C->sf_first;
			while( sf )
			{
				mode1item NI = { sf, TM };
				sgs_membuf_appbuf( &P->frametmp, C, &NI, sizeof(NI) );
				if( sf == target )
					break;
				sf = sf->next;
			}
		}
	}
	else if( evid == SGS_HOOK_CREAT )
	{
		sgs_ProfAttach( C, P->prof );
	}
	else if( evid == SGS_HOOK_CFREE )
	{
		sgs_ProfDetach( C, P->prof );
	}
	/* CFORK not supported yet */
}

static void initProfModeData1( sgs_ProfData* P, SGS_CTX )
{
	P->keytmp = sgs_membuf_create();
	P->frametmp = sgs_membuf_create();
	sgs_SetHookFunc( C, mode1hook, P );
}

static void freeProfModeData1( sgs_ProfData* P, SGS_CTX )
{
	sgs_membuf_destroy( &P->frametmp, C );
	sgs_membuf_destroy( &P->keytmp, C );
}

static void initProfMode1( sgs_Prof* P, SGS_CTX )
{
	sgs_vht_init( &P->timings, C, 128, 128 );
}

static void freeProfMode1( sgs_Prof* P, SGS_CTX )
{
	sgs_vht_free( &P->timings, C );
}

static int dpm1sf( const void* p1, const void* p2 )
{
	const sgs_VHTVar* v1 = (const sgs_VHTVar*) p1;
	const sgs_VHTVar* v2 = (const sgs_VHTVar*) p2;
	const sgs_iStr* str1 = v1->key.data.S;
	const sgs_iStr* str2 = v2->key.data.S;
	uint32_t cmpsz = str1->size < str2->size ? str1->size : str2->size;
	int ret = memcmp( sgs_str_c_cstr( str1 ), sgs_str_c_cstr( str2 ), cmpsz );
	if( !ret )
		ret = (int)( str1->size - str2->size );
	return ret;
}


static void dumpProfMode1( sgs_Prof* P, SGS_CTX )
{
	int i;
	sgs_VHTVar* pbuf = sgs_Alloc_n( sgs_VHTVar, (size_t) P->timings.size );
	
	memcpy( pbuf, P->timings.vars, sizeof(sgs_VHTVar) * (size_t) P->timings.size );
	
	qsort( pbuf, (size_t) P->timings.size, sizeof(sgs_VHTVar), dpm1sf );
	
	sgs_Writef( C, "--- Time by call stack frame ---\n" );
	for( i = 0; i < P->timings.size; ++i )
	{
		const char *s, *send;
		sgs_VHTVar* p = pbuf + i;
		s = sgs_var_cstr( &p->key );
		send = s + p->key.data.S->size;
		while( s < send )
		{
			if( s != sgs_var_cstr( &p->key ) )
				sgs_Writef( C, "::" );
			sgs_Writef( C, "%s", s );
			s += strlen( s ) + 1;
		}
		sgs_Writef( C, " - %f\n", p->val.data.R );
	}
	sgs_Writef( C, "---\n" );
	sgs_Free( C, pbuf );
}



#define TOPCNT (SGS_SI_COUNT+1)

static void mode2hook( void* userdata, SGS_CTX, int evid )
{
	sgs_ProfData* P = (sgs_ProfData*) userdata;
	double TM = sgs_GetTime();
	if( P->hfn )
		P->hfn( P->hctx, C, evid );
	
	if( P->instr >= 0 )
	{
		double dif = TM - P->starttime;
		P->prof->ictrs[ P->instr ] += dif >= 0 ? dif : 0;
		P->instr = -1;
	}
	if( evid == SGS_HOOK_STEP )
	{
		sgs_StackFrame* sf = sgs_GetFramePtr( C, NULL, 1 );
		P->instr = SGS_INSTR_GET_OP( *sf->iptr );
		P->prof->iexcs[ P->instr ]++;
		P->starttime = TM;
		P->prev = evid;
	}
	else if( evid == SGS_HOOK_ENTER || evid == SGS_HOOK_CONT )
	{
		P->starttime = TM;
		P->instr = -1;
		P->prev = evid;
	}
	else if( evid == SGS_HOOK_EXIT || evid == SGS_HOOK_PAUSE )
	{
		if( P->prev != SGS_HOOK_STEP )
		{
			/* native function was called */
			double dif = TM - P->starttime;
			P->prof->ictrs[ SGS_SI_COUNT ] += dif >= 0 ? dif : 0;
		}
		P->starttime = TM;
		P->prev = evid;
	}
}

static void initProfModeData2( sgs_ProfData* P, SGS_CTX )
{
	P->instr = -1;
	P->starttime = sgs_GetTime();
	sgs_SetHookFunc( C, mode2hook, P );
}

static void freeProfModeData2( sgs_ProfData* P, SGS_CTX )
{
	SGS_UNUSED( P );
	SGS_UNUSED( C );
}

static void initProfMode2( sgs_Prof* P, SGS_CTX )
{
	int i;
	P->ictrs = sgs_Alloc_n( double, TOPCNT );
	P->iexcs = sgs_Alloc_n( uint32_t, TOPCNT );
	for( i = 0; i < TOPCNT; ++i )
	{
		P->ictrs[ i ] = 0;
		P->iexcs[ i ] = 0;
	}
}

static void freeProfMode2( sgs_Prof* P, SGS_CTX )
{
	sgs_Free( C, P->ictrs );
	sgs_Free( C, P->iexcs );
}

typedef struct { int i; uint32_t c; double t; } icts;
static int its_sort( const void* p1, const void* p2 )
{
	const icts* t1 = (const icts*) p1;
	const icts* t2 = (const icts*) p2;
	if( t1->t != t2->t )
		return t1->t > t2->t ? -1 : 1;
	else if( t1->c != t2->c )
		return t1->c > t2->c ? -1 : 1;
	return 0;
}

static void dumpProfMode2( sgs_Prof* P, SGS_CTX )
{
	int i;
	icts* temp;
	uint32_t totalcnt = 0, c;
	double total = 0, t;
	sgs_Writef( C, "--- Time by VM instruction ---\n" );
	sgs_Writef( C, "|      NAME      |     TIME     |    COUNT    |\n" );
	temp = (icts*) sgs_Malloc( C, sizeof( icts ) * TOPCNT );
	for( i = 0; i < TOPCNT; ++i )
	{
		t = P->ictrs[ i ];
		c = P->iexcs[ i ];
		total += t;
		totalcnt += c;
		temp[ i ].t = t;
		temp[ i ].i = i;
		temp[ i ].c = c;
	}
	qsort( temp, TOPCNT, sizeof( icts ), its_sort );
	for( i = 0; i < TOPCNT; ++i )
	{
		const char* str = temp[ i ].i == SGS_SI_COUNT ?
			"! native code" :
			sgs_CodeString( SGS_CODE_OP, temp[ i ].i );
		sgs_Writef( C, "| %14s - %12f - %11d |\n", str, temp[ i ].t, temp[ i ].c );
	}
	sgs_Free( C, temp );
	sgs_Writef( C, "///\n| %14s - %12f - %11d |\n---\n", "Total", total, totalcnt );
}



typedef struct _mode3item
{
	sgs_StackFrame* frame;
	size_t numallocs;
	size_t numfrees;
	size_t numblocks;
	double szdelta;
}
mode3item;

static void mode3hook( void* userdata, SGS_CTX, int evid )
{
	sgs_ProfData* P = (sgs_ProfData*) userdata;
	if( P->hfn )
		P->hfn( P->hctx, C, evid );
	
	if( evid == SGS_HOOK_ENTER || evid == SGS_HOOK_CONT ||
		evid == SGS_HOOK_EXIT || evid == SGS_HOOK_PAUSE )
	{
		SGS_SHCTX_USE;
		mode3item CD = { NULL, S->numallocs, S->numfrees, S->numblocks, (double) S->memsize };
		
		sgs_StackFrame* target = NULL, *sf, *lastrec = NULL;
		mode3item* items = (mode3item*) (void*) SGS_ASSUME_ALIGNED( P->frametmp.ptr, 4 );
		size_t i, itemcount = P->frametmp.size / sizeof(mode3item);
		
		if( evid == SGS_HOOK_ENTER || evid == SGS_HOOK_CONT )
			target = C->sf_last;
		else if( evid == SGS_HOOK_EXIT )
			target = C->sf_last->prev;
		/* pause => null */
		
		/* ignore the case when target = current */
		if( itemcount && items[ itemcount - 1 ].frame == target )
			return;
		
		/* find target in stack */
		for( i = 0; i < itemcount; ++i )
		{
			if( items[ i ].frame == target )
				break;
		}
		if( i == itemcount )
		{
			/* find last recorded frame in call stack */
			sf = NULL;
			if( itemcount )
			{
				sf = C->sf_first;
				while( sf )
				{
					if( items[ itemcount - 1 ].frame == sf )
						break;
					sf = sf->next;
				}
			}
			lastrec = sf;
			if( sf == NULL || target == NULL )
				i = 0; /* frame not found/req., remove everything */
			/* otherwise, remove nothing, it's an "ENTER" */
		}
		else
			i++; /* remove starting from next frame */
		
		/* if there is anything to remove */
		if( i < itemcount )
		{
			size_t bki = i;
			sf = C->sf_first;
			/* generate core key (from start to first removed incl.) */
			sgs_membuf_resize( &P->keytmp, C, 0 );
			while( sf && sf != items[ i ].frame )
			{
				const char* fname = "<error>";
				sgs_StackFrameInfo( C, sf, &fname, NULL, NULL );
				sgs_membuf_appbuf( &P->keytmp, C, fname, strlen( fname ) );
				sgs_membuf_appchr( &P->keytmp, C, 0 );
				sf = sf->next;
			}
			
			/* iterate removable frames (assuming frame pointers are valid) */
			for( ; i < itemcount; ++i )
			{
				const char* fname = "<error>";
				sgs_StackFrameInfo( C, items[ i ].frame, &fname, NULL, NULL );
				sgs_membuf_appbuf( &P->keytmp, C, fname, strlen( fname ) );
				sgs_membuf_appchr( &P->keytmp, C, 0 );
				
				/* commit memory addition */
				{
					mode3item prevCD = items[ i ], *PD;
					sgs_VHTVar* pair = sgs_vht_get_str( &P->prof->timings, P->keytmp.ptr,
						(uint32_t) P->keytmp.size, sgs_HashFunc( P->keytmp.ptr, P->keytmp.size ) );
					if( pair )
					{
						PD = (mode3item*) pair->val.data.P;
					}
					else
					{
						sgs_Variable key, val;
						val.type = SGS_VT_PTR;
						val.data.P = PD = sgs_Alloc( mode3item );
						memset( PD, 0, sizeof(*PD) );
						sgs_InitStringBuf( C, &key, P->keytmp.ptr, (sgs_SizeVal) P->keytmp.size );
						sgs_vht_set( &P->prof->timings, C, &key, &val );
						sgs_Release( C, &key );
					}
					
					PD->numallocs += CD.numallocs - prevCD.numallocs;
					PD->numfrees += CD.numfrees - prevCD.numfrees;
					PD->numblocks += CD.numblocks - prevCD.numblocks;
					PD->szdelta += CD.szdelta - prevCD.szdelta;
				}
			}
			
			/* commit size */
			sgs_membuf_resize( &P->frametmp, C, bki * sizeof(mode3item) );
			itemcount = bki;
		}
		
		/* add new frame start times */
		if( lastrec || ( target != NULL && itemcount == 0 ) )
		{
			if( lastrec )
				sf = lastrec->next;
			else
				sf = C->sf_first;
			while( sf )
			{
				mode3item NI = CD;
				NI.frame = sf;
				sgs_membuf_appbuf( &P->frametmp, C, &NI, sizeof(NI) );
				if( sf == target )
					break;
				sf = sf->next;
			}
		}
	}
	else if( evid == SGS_HOOK_CREAT )
	{
		sgs_ProfAttach( C, P->prof );
	}
	else if( evid == SGS_HOOK_CFREE )
	{
		sgs_ProfDetach( C, P->prof );
	}
	/* CFORK not supported yet */
}

static void initProfModeData3( sgs_ProfData* P, SGS_CTX )
{
	initProfModeData1( P, C );
	sgs_SetHookFunc( C, mode3hook, P );
}

static void freeProfModeData3( sgs_ProfData* P, SGS_CTX )
{
	freeProfModeData1( P, C );
}

static void initProfMode3( sgs_Prof* P, SGS_CTX )
{
	initProfMode1( P, C );
}

static void freeProfMode3( sgs_Prof* P, SGS_CTX )
{
	sgs_SizeVal i;
	for( i = 0; i < P->timings.size; ++i )
	{
		sgs_Dealloc( P->timings.vars[ i ].val.data.P );
	}
	freeProfMode1( P, C );
}

static int dpm3sf( const void* p1, const void* p2 )
{
	const sgs_VHTVar* v1 = (const sgs_VHTVar*) p1;
	const sgs_VHTVar* v2 = (const sgs_VHTVar*) p2;
	const sgs_iStr* str1 = v1->key.data.S;
	const sgs_iStr* str2 = v2->key.data.S;
	const mode3item* D1 = (const mode3item*) v1->val.data.P;
	const mode3item* D2 = (const mode3item*) v2->val.data.P;
	size_t cmpsz = str1->size < str2->size ? str1->size : str2->size;
	int ret = (int)( ( D2->numallocs + D2->numfrees ) - ( D1->numallocs + D1->numfrees ) );
	if( !ret )
	{
		double abs1 = fabs( D2->szdelta );
		double abs2 = fabs( D1->szdelta );
		ret = abs1 == abs2 ? 0 : ( abs1 < abs2 ? -1 : 1 );
	}
	if( !ret ) ret = memcmp( sgs_str_c_cstr( str1 ), sgs_str_c_cstr( str2 ), cmpsz );
	if( !ret ) ret = (int)( str1->size - str2->size );
	return ret;
}

static void dumpProfMode3( sgs_Prof* P, SGS_CTX )
{
	int i;
	sgs_VHTVar* pbuf = sgs_Alloc_n( sgs_VHTVar, (size_t) P->timings.size );
	
	memcpy( pbuf, P->timings.vars, sizeof(sgs_VHTVar) * (size_t) P->timings.size );
	
	qsort( pbuf, (size_t) P->timings.size, sizeof(sgs_VHTVar), dpm3sf );
	
	sgs_Writef( C, "--- Memory usage by call stack frame ---\n" );
	for( i = 0; i < P->timings.size; ++i )
	{
		const char *s, *send;
		sgs_VHTVar* p = pbuf + i;
		mode3item* PD = (mode3item*) p->val.data.P;
		s = sgs_var_cstr( &p->key );
		send = s + p->key.data.S->size;
		while( s < send )
		{
			if( s != sgs_var_cstr( &p->key ) )
				sgs_Writef( C, "::" );
			sgs_Writef( C, "%s", s );
			s += strlen( s ) + 1;
		}
		sgs_Writef( C, " - %d allocs, %d frees, %d delta blocks, %.3f delta memory (kB)\n",
			PD->numallocs, PD->numfrees, PD->numblocks, PD->szdelta / 1024.0 );
	}
	sgs_Writef( C, "---\n" );
	sgs_Free( C, pbuf );
}


void sgs_ProfInit( SGS_CTX, sgs_Prof* P, int mode )
{
	P->mode = mode;
	sgs_vht_init( &P->ctx2prof, C, 4, 4 );
	
	P->ictrs = NULL;
	P->iexcs = NULL;
	
	if( P->mode == SGS_PROF_FUNCTIME )
		initProfMode1( P, C );
	else if( P->mode == SGS_PROF_OPTIME )
		initProfMode2( P, C );
	else if( P->mode == SGS_PROF_MEMUSAGE )
		initProfMode3( P, C );
	
	sgs_ProfAttach( C, P );
}

void sgs_ProfAttach( SGS_CTX, sgs_Prof* P )
{
	sgs_Variable key = sgs_MakePtr( C );
	sgs_VHTVar* var = sgs_vht_get( &P->ctx2prof, &key );
	if( var )
		return;
	
	sgs_ProfData* PD = sgs_Alloc( sgs_ProfData );
	PD->prof = P;
	PD->hfn = NULL;
	PD->hctx = NULL;
	sgs_GetHookFunc( C, &PD->hfn, &PD->hctx );
	PD->prev = 0;
	
	if( P->mode == SGS_PROF_FUNCTIME )
		initProfModeData1( PD, C );
	else if( P->mode == SGS_PROF_OPTIME )
		initProfModeData2( PD, C );
	else if( P->mode == SGS_PROF_MEMUSAGE )
		initProfModeData3( PD, C );
	
	{
		sgs_Variable val = sgs_MakePtr( PD );
		sgs_vht_set( &P->ctx2prof, C, &key, &val );
	}
}

void sgs_ProfDetach( SGS_CTX, sgs_Prof* P )
{
	sgs_ProfData* PD;
	sgs_Variable key = sgs_MakePtr( C );
	sgs_VHTVar* var = sgs_vht_get( &P->ctx2prof, &key );
	if( var == NULL )
		return;
	
	PD = (sgs_ProfData*) var->val.data.P;
	
	if( P->mode == SGS_PROF_FUNCTIME )
		freeProfModeData1( PD, C );
	else if( P->mode == SGS_PROF_OPTIME )
		freeProfModeData2( PD, C );
	else if( P->mode == SGS_PROF_MEMUSAGE )
		freeProfModeData3( PD, C );
	sgs_SetHookFunc( C, PD->hfn, PD->hctx );
	
	sgs_Dealloc( PD );
	sgs_vht_unset( &P->ctx2prof, C, &key );
}

void sgs_ProfClose( SGS_CTX, sgs_Prof* P )
{
	while( P->ctx2prof.size )
	{
		sgs_ProfDetach( (sgs_Context*) P->ctx2prof.vars[ 0 ].key.data.P, P );
	}
	
	if( P->mode == SGS_PROF_FUNCTIME )
		freeProfMode1( P, C );
	else if( P->mode == SGS_PROF_OPTIME )
		freeProfMode2( P, C );
	else if( P->mode == SGS_PROF_MEMUSAGE )
		freeProfMode3( P, C );
	
	sgs_vht_free( &P->ctx2prof, C );
}

void sgs_ProfDump( SGS_CTX, sgs_Prof* P )
{
	if( P->mode == SGS_PROF_FUNCTIME )
		dumpProfMode1( P, C );
	else if( P->mode == SGS_PROF_OPTIME )
		dumpProfMode2( P, C );
	else if( P->mode == SGS_PROF_MEMUSAGE )
		dumpProfMode3( P, C );
}
