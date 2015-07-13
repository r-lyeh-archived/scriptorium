/*
// Dao Virtual Machine
// http://www.daovm.net
//
// Copyright (c) 2006-2015, Limin Fu
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without modification,
// are permitted provided that the following conditions are met:
//
// * Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright notice,
//   this list of conditions and the following disclaimer in the documentation
//   and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED  BY THE COPYRIGHT HOLDERS AND  CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED  WARRANTIES,  INCLUDING,  BUT NOT LIMITED TO,  THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL  THE COPYRIGHT HOLDER OR CONTRIBUTORS  BE LIABLE FOR ANY DIRECT,
// INDIRECT,  INCIDENTAL, SPECIAL,  EXEMPLARY,  OR CONSEQUENTIAL  DAMAGES (INCLUDING,
// BUT NOT LIMITED TO,  PROCUREMENT OF  SUBSTITUTE  GOODS OR  SERVICES;  LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION)  HOWEVER CAUSED  AND ON ANY THEORY OF
// LIABILITY,  WHETHER IN CONTRACT,  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
// OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
// OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include<wctype.h>

#include"daoRegex.h"
#include"daoValue.h"

#define ALIGN_LEN   sizeof(void*)
#define ALIGN_MASK  (ALIGN_LEN-1)
#define ALIGN( x )  ((x+ALIGN_MASK) & ~ALIGN_MASK)



/* Regex Matching */
enum {
	PAT_NONE ,
	PAT_BEGIN ,
	PAT_STOP ,
	PAT_START ,
	PAT_END ,
	PAT_WBORDER ,
	PAT_SPLIT ,
	PAT_JOIN ,
	PAT_BACKREF ,
	PAT_PATPAIR ,
	PAT_PAIR ,
	PAT_WORD ,
	PAT_SET ,
	PAT_ANY
};

enum DaoRegexConfig
{
	PAT_CONFIG_CASEINS = 1,
	PAT_CONFIG_MIN = 2
};
enum { PAT_ALL_FIXED = 1 };

const char *names[] =
{ "None", "Begin", "Stop", "Start", "End", "Split", "Join",
	"BackRef", "PatPair", "Pair", "Word", "Set", "Any" };

static void PrintRegex( DaoRegex *self, DaoRgxItem *pat )
{
	const char *type = "";
	const char *value = " ";
	char ch1[2] = {0,0};
	int i = 0, j = 0;
	if( pat->type > PAT_ANY ){
		type = "Class";
		ch1[0] = (char)pat->type;
		value = ch1;
	}else if( pat->type == PAT_SET || pat->type == PAT_WORD || pat->type == PAT_PAIR ){
		value = self->wordbuf + pat->word;
	}
	j = pat->word;
	i = pat->length;
	if( pat->type <= PAT_ANY ) type = names[ pat->type ];
	printf( "%6s  %3i,  %2i %2i,  %2i %2i,  %2i %1i,  %2i %2i, %s\n", type, pat->type,
			pat->min, pat->max, pat->next, pat->jump, pat->gid, pat->config, j, i, value );
}

static void SetWord( DaoRegex *self, DaoRgxItem *word, short m, wchar_t ch )
{
	char *mbs;
	if( word->type != PAT_WORD ){
		word->type = PAT_WORD;
		word->length = 0;
		word->word = m;
	}
	mbs = self->wordbuf + word->word;
	mbs[ word->length ] = (char) ch;
	mbs[ ++ word->length ] = 0;
}
static int RegexCompare( DaoRegex *self, DaoRgxItem *p1, DaoRgxItem *p2 )
{
	int res = 0;
	if( p2->type == p1->type && p1->type > PAT_JOIN ){
		if( p1->type >= PAT_ANY ){
			res = 1;
		}else if( p1->length == p2->length ){
			char *mbs = (char*) self->wordbuf;
			res = (strncmp( mbs + p2->word, mbs + p1->word, p1->length ) ==0);
		}
	}
	return res;
}
static void PushWord( DaoRegex *self, DaoRgxItem *word )
{
	DaoRgxItem *patt = self->items + self->count -1;
	if( self->count && RegexCompare( self, word, patt ) ){
		patt->min ++;
		if( patt->max >=0 ) patt->max ++;
		word->type = PAT_NONE;
		return;
	}
	patt = self->items + self->count;
	memcpy( patt, word, sizeof(DaoRgxItem) );
	self->count ++;
	patt->config = self->config;
	patt->min = patt->max = 1;
	word->type = PAT_NONE;
}

static DaoRgxItem* PushRegex( DaoRegex *self, short type )
{
	DaoRgxItem *patt = self->items + self->count;
	patt->type = type;
	patt->gid = 0;
	patt->min = patt->max = patt->next = 1;
	patt->jump = patt->length = 0;
	patt->config = self->config;
	self->count ++;
	return patt;
}
static int ScanInteger( int *output, void *src, daoint pos, daoint end )
{
	char buf[10];
	char *mbs = ((char*)src) + pos;
	daoint i, ch;
	*output = 0;
	if( end <= pos ) return 0;
	end -= pos;
	if( end >= 9 ) end = 9;
	for(i=0; i<end; i++){
		ch = mbs[i];
		if( ! isdigit( ch ) ) break;
		buf[i] = (char) ch;
		buf[i+1] = 0;
	}
	*output = atoi( buf );
	return i;
}
static int SkipSpace( void *src, daoint pos, daoint end )
{
	char *mbs = ((char*)src) + pos;
	daoint i, ch;
	if( end <= pos ) return 0;
	end -= pos;
	for(i=0; i<end; i++){
		ch = mbs[i];
		if( ! isspace( ch ) ) break;
	}
	return i;
}
static int SetRepeat( short *omin, short *omax, void *src, DString *ds, daoint i, daoint end )
{
	short min=0, max=0, offset=1;
	char *mbs = (char*) src;
	int chi, chi2=0;
	daoint pos, pos2;
	int j, k, start = i;
	j = SkipSpace( src, i, end );
	i += j;
	offset += j;
	chi = mbs[i];
	if( i+1<end ) chi2 = mbs[i+1];
	if( chi == '?' ){
		min = 0;
		max = 1;
	}else if( chi == '*' ){
		min = 0;
		max = -1;
	}else if( chi == '+' ){
		min = 1;
		max = -1;
	}else if( chi == '{' && chi2 != '{' ){
		pos = DString_BalancedChar( ds, '}', 0, 0, 0, i+1, end, 0 );
		pos2 = DString_BalancedChar( ds, '{', '{', '}', 0, i+1, pos, 0 );
		if( pos != DAO_NULLPOS && pos2 == DAO_NULLPOS ){
			j = SkipSpace( src, i+1, pos );  i += j+1;
			j = ScanInteger( & k, src, i, pos );  i += j;
			if( j ){
				j = SkipSpace( src, i, pos );  i += j;
				min = max = k;
				chi = mbs[i];
				if( chi == ',' ){
					j = SkipSpace( src, i+1, pos );  i += j+1;
					j = ScanInteger( & k, src, i, pos );
					max = j ? k : -1;
				}
				offset = pos - start + 1;
			}else{
				offset = 0;
			}
		}
	}else{
		offset = 0;
	}
	if( offset ){
		*omin = min;
		*omax = max;
	}
	return offset;
}
static int MakeRegex( DaoRegex *self, DString *ds, void *spatt,
		int start, int end, int grouping )
{
	DaoRgxItem *patt, *patt2, *split=NULL, *patts = self->items;
	DaoRgxItem word = { PAT_NONE };
	DaoRgxItem set = { PAT_NONE };
	DCharState st = { 0, 1, 0 };
	DCharState st2 = { 0, 1, 0 };
	char *buf, *mbs = (char*) spatt;
	int i, j, k, count=0, repeat=0, refgroup=0;
	short type, gid = self->indexed >0 ? self->indexed + 1E4 : self->group;
	int alts = DString_BalancedChar( ds, '|', '(', ')', '%', start, end, 1 );
	daoint pos, pos2;
	int chi, chi2;

	if( alts >0 || grouping >0 ){
		split = PushRegex( self, PAT_SPLIT );
		split->gid = gid;
		count = self->count;
	}
	self->indexed = 0;
	word.next = set.next = 1;
	word.jump = set.jump = 0;
	for(i=start; i<end; i++){
		int verbatim = 0;
		chi = mbs[i];
		chi2 = (i+1) >= end ? 0 : mbs[i+1];
		type = 0;
		k = i;
		/* space chars are ignored, to avoid this, write as [ ][\t][\n] */
		if( isspace( chi ) ){
			if( word.type == PAT_WORD ) PushWord( self, & word );
			continue;
		}
		if( chi == '%' ){
			if( i+1 < end ) chi = mbs[i+1];
			switch( chi ){
			case 's': case 'S': case 'k': case 'K': case 'p': case 'P':
			case 'c': case 'C': case 'a': case 'A': case 'w': case 'W':
			case 'e': case 'E': case 'd': case 'D': case 'x': case 'X':
				type = chi;
				break;
			case 't':
				type = PAT_WORD;
				SetWord( self, & word, i, L'\t' );
				break;
			case 'n':
				type = PAT_WORD;
				SetWord( self, & word, i, L'\n' );
				break;
			case '1': case '2': case '3': case '4': case '5':
			case '6': case '7': case '8': case '9':
				j = ScanInteger( & refgroup, spatt, i+1, end );
				type = PAT_BACKREF;
				i += j-1;
				break;
			case 'b':
				type = PAT_NONE;
				if( i+3 < end ){
					st = DString_DecodeChar( mbs + i + 2, mbs + end );
					st2 = DString_DecodeChar( mbs + i + 2 + st.width, mbs + end );
					chi = st.value;
					chi2 = st2.value;
					if( ! iswspace( chi ) && ! iswspace( chi2 ) ){
						set.type = type = PAT_PAIR;
						set.word = i + 1;
						set.length = st.width + st2.width;
						buf = self->wordbuf + set.word;
						memcpy( buf, mbs + i + 2, set.length * sizeof(char) );
						i += set.length;
					}
				}
				break;
			case 'B':
				type = PAT_NONE;
				chi = mbs[i+2];
				if( chi == '{' ){
					pos = DString_BalancedChar( ds, '}','{','}','%', i+3, end, 0 );
					chi = 0;
					pos2 = DAO_NULLPOS;
					if( pos != DAO_NULLPOS ){
						chi = mbs[pos+1];
						pos2 = DString_BalancedChar( ds, '}','{','}','%', pos+2, end, 0 );
					}
					if( chi == '{' && pos2 != DAO_NULLPOS ){
						short count2, count3;
						count2 = self->count;
						patt = PushRegex( self, PAT_PATPAIR );
						PushRegex( self, PAT_BEGIN );
						MakeRegex( self, ds, spatt, i+3, pos, 0 );
						PushRegex( self, PAT_STOP );
						count3 = self->count;
						PushRegex( self, PAT_BEGIN );
						MakeRegex( self, ds, spatt, pos+2, pos2, 0 );
						PushRegex( self, PAT_STOP );
						patt->next = self->count - count2;
						patt->length = count3 - count2;
						i = pos2;
						i += SetRepeat( & patt->min, & patt->max, spatt, ds, i+1, end );
						continue;
					}
				}
				break;
			default :
				type = (i+1 < end) ? PAT_WORD : PAT_NONE;
				SetWord( self, & word, i, chi );
				break;
			}
			i ++;
		}else if( chi == '{' && chi2 == '{' ){ /* {{text}} */
			verbatim = 1;
			type = PAT_WORD;
			i += 2;
			while( i < end ){
				chi = mbs[i];
				i += 1;
				/* printf( "%c\n", chi ); */
				if( chi == '}' ) break;
				SetWord( self, & word, i-1, chi );
			}
			chi = 0;
			k = i - 1;
			if( i < end ){
				chi = mbs[i];
				k = i;
			}
			if( chi != '}' ) type = PAT_NONE;
		}else if( chi == '(' ){
			if( word.type == PAT_WORD ) PushWord( self, & word );
			pos = DString_BalancedChar( ds, ')', '(', ')', '%', i+1, end, 0 );
			if( pos != DAO_NULLPOS ){
				self->group ++;
				MakeRegex( self, ds, spatt, i+1, pos, 1 );
				i = pos;
				continue;
			}
		}else if( chi == '[' ){
			pos = DString_BalancedChar( ds, ']', 0, 0, '%', i+1, end, 0 );
			pos2 = DString_BalancedChar( ds, '[', '[', ']', '%', i+1, end, 0 );
			if( pos == i+2 && pos2 == DAO_NULLPOS ){
				type = PAT_WORD;
				i ++;
				chi = mbs[i];
				SetWord( self, & word, i, chi );
				i = pos;
			}else if( pos != DAO_NULLPOS && pos2 == DAO_NULLPOS ){
				type = PAT_SET;
				set.type = PAT_SET;
				set.length = pos - i - 1;
				set.word = i + 1;
				buf = self->wordbuf + set.word;
				memcpy( buf, mbs + i + 1, set.length * sizeof(char) );
				buf[ set.length ] = 0;
				i = pos;
			}else{
				type = PAT_NONE;
			}
		}else if( chi == '<' ){
			int k;
			if( word.type == PAT_WORD ) PushWord( self, & word );
			j = SkipSpace( spatt, i+1, end );  i += j+1;
			chi = mbs[i];
			k = i;
			if( isdigit( chi ) ){
				j = ScanInteger( & k, spatt, i, end );  i += j;
				self->indexed = k;
			}else if( islower( chi ) ){
				if( strncmp( mbs + i, "min", 3 ) ==0 ){
					self->config |= PAT_CONFIG_MIN;
					i += 3;
				}
			}else{
				for(; i<end; i++){
					chi = mbs[i];
					if( chi == 'I' ){
						self->config |= PAT_CONFIG_CASEINS;
					}else if( chi == 'C' ){
						self->config &= ~ PAT_CONFIG_CASEINS;
					}else{
						break;
					}
				}
			}
			j = SkipSpace( spatt, i, end );  i += j;
			chi = mbs[i];
			if( chi == '>' && k != i ){
				continue;
			}else{
				type = PAT_NONE;
			}
		}else if( chi == '.' ){
			type = PAT_ANY;
		}else if( chi == '|' ){
			if( word.type == PAT_WORD ) PushWord( self, & word );
			patt2 = PushRegex( self, PAT_JOIN );
			patt = PushRegex( self, PAT_SPLIT );
			patt->gid = patt2->gid = gid;
			if( split ) split->jump = patt - split;
			split = patt;
			continue;
		}else if( chi == '^' ){
			type = PAT_START;
		}else if( chi == '$' ){
			type = PAT_END;
		}else if( chi == '&' ){
			type = PAT_WBORDER;
		}else{
			type = PAT_NONE;
			switch( chi ){
			case ']': case ')': case '{': case '}': case '|':
			case '-': case '?': case '*': case '+': break;
			default : type = PAT_WORD; break;
			}
			if( type == PAT_WORD ) SetWord( self, & word, i, chi );
		}
		if( type != PAT_WORD && word.type == PAT_WORD ) PushWord( self, & word );
		patt = patts + self->count;
		patt->type = type;
		patt->gid = 0;
		patt->min = patt->max = 1;
		patt->next = 1;
		patt->jump = 0;
		if( type == PAT_BACKREF ) patt->gid = refgroup;
		if( type == PAT_NONE ) patt->length = k + 1; /* store the error position */
		if( word.type == PAT_WORD ){
			patt->word = word.word;
			patt->length = word.length;
		}else if( set.type == PAT_SET || set.type == PAT_PAIR ){
			patt->word = set.word;
			patt->length = set.length;
			set.type = PAT_NONE;
		}
		repeat = SetRepeat( & patt->min, & patt->max, spatt, ds, i+1, end );
		i += repeat;
		if( patt->type == PAT_WORD && repeat && verbatim == 0 && patt->length > 1 ){
			/* Handle single character repetition: */
			int offset = patt->word;
			int length = patt->length;
			int min = patt->min;
			int max = patt->max;
			self->count += 1;
			patt->min = patt->max = 1;
			patt->length -= 1;
			patt = PushRegex( self, PAT_WORD );
			patt->min = min;
			patt->max = max;
			patt->word = offset + length - 1;
			patt->length = 1;
			self->count -= 1;
		}
		if( patt->type != PAT_WORD || repeat ){
			patt2 = NULL;
			if( self->count ) patt2 = patts + (self->count-1);
			if( patt2 && RegexCompare( self, patt, patt2 ) ){
				patt2->min += patt->min;
				if( patt2->max >=0 && patt->max >=0 ){
					patt2->max += patt->max;
				}else{
					patt2->max = -1;
				}
			}else{
				patt->config = self->config;
				self->count ++;
			}
			word.type = PAT_NONE;
		}
	}
	if( word.type == PAT_WORD ) PushWord( self, & word );
	if( alts >0 || grouping >0 ){
		patt = PushRegex( self, PAT_JOIN );
		patt->gid = gid;
		for(j=count; j<self->count; j++){
			patt = patts + j;
			if( patt->type == PAT_JOIN && patt->gid == gid )
				patt->next = self->count - j;
			if( patt->gid == gid && j+1 < self->count ){
				/* for PAT_SPLIT added for | in groups,
				 * required for calculate matching length */
				if( patt->type == PAT_SPLIT ) patt->gid = 0;
			}
		}
		split->length = self->count - count;
	}
	return self->count;
}

static const int sizepat = sizeof(DaoRegex);
static const int sizeitm = sizeof(DaoRgxItem);
static const int sizewch = sizeof(wchar_t);

static int InitRegex( DaoRegex *self, DString *ds )
{
	DaoRgxItem *patt;
	void *spatt = (void*)ds->chars;
	daoint i, j, max =0, end = ds->size;
	int fixed = 1;
	self->indexed = self->count = self->group = self->config = self->attrib = 0;
	memset( self->items, 0, self->itemlen );
	memset( self->wordbuf, 0, self->wordlen );
	PushRegex( self, PAT_BEGIN );
	MakeRegex( self, ds, spatt, 0, end, 0 );
	PushRegex( self, PAT_STOP );
#if DEBUG
	if( self->count * sizeof(DaoRgxItem) > (size_t)self->itemlen ){
		printf( "error: allocated memory is not enough for the pattern.\n" );
		printf( "%s\n", ds->chars );
		exit(0);
	}
#endif
	for(i=0; i<self->count; i++){
		patt = self->items + i;
		self->config |= patt->config;
		if( patt->type == PAT_SPLIT && patt->gid > max ) max = patt->gid;
		if( patt->min != patt->max ) fixed = 0;
		if( (patt->type == PAT_SET || patt->type == PAT_WORD) && (patt->config & PAT_CONFIG_CASEINS) ){
			char *w = self->wordbuf + patt->word;
			for(j=0; j<patt->length; j++) w[j] = tolower( w[j] );
		}
	}
	if( max > 10000 ){
		max -= 10000;
		for(i=0; i<self->count; i++){
			patt = self->items + i;
			if( patt->type == PAT_SPLIT || patt->type == PAT_JOIN ){
				if( patt->gid > 10000 ){
					patt->gid -= 10000;
				}else if( patt->gid ){
					patt->gid += max;
				}
			}
		}
		/* self->group += max; */
		self->group = max; /*  restrict capture exporting */
	}
	if( fixed ) self->attrib |= PAT_ALL_FIXED;
	memmove( self->items + self->count, self->wordbuf, self->wordlen );
	self->wordbuf = (char*)(self->items + self->count);
	self->itemlen = self->count * sizeitm;
	self->length = sizepat + self->itemlen + self->wordlen;
	self->length = ALIGN( self->length );
	return self->count;
}

static int MatchOne( DaoRegex *self, DaoRgxItem *patt, daoint pos );

static int MatchWord( DaoRegex *self, DaoRgxItem *patt, daoint pos )
{
	short i;
	char *w, *s;
	if( pos + patt->length > self->end ) return 0;
	w = self->wordbuf + patt->word;
	s = self->source + pos;
	if( patt->config & PAT_CONFIG_CASEINS ){
		/* word is in lowercase */
		for(i=0; i<patt->length; i++) if( tolower( s[i] ) != w[i] ) return 0;
	}else{
		for(i=0; i<patt->length; i++) if( s[i] != w[i] ) return 0;
	}
	patt->offset = patt->length;
	return 1;
}
static DCharState DString_DecodeChar2( char *start, char *end )
{
	DCharState state = DString_DecodeChar( start, end );
	if( sizeof(wchar_t) == 2 && state.value > 0xFFFF ) state.value = 0; /* for isw*(); */
	return state;
}
static int MatchSet( DaoRegex *self, DaoRgxItem *patt, daoint pos )
{
	DCharState ch = { 1, 1, 0 };
	DCharState chi = ch, chi2 = ch, chi3 = ch;
	char *chars = self->wordbuf + patt->word;
	char *end = chars + patt->length;
	char *src = self->source;
	int blmatch = 1;
	int matched = 0;
	short i;

	patt->offset = 0;
	if( pos >= self->end ) return 0;

	ch = DString_DecodeChar( src + pos, src + self->end + 1 );
	if( patt->config & PAT_CONFIG_CASEINS ) ch.value = towlower( ch.value );
	patt->offset = ch.width;
	for(i=0; i<patt->length; ){
		chi3.type = 0;
		chi3.width = 0;
		chi3.value = 0;
		chi = DString_DecodeChar2( chars + i, end );
		chi2 = DString_DecodeChar2( chars + i + chi.width, end );
		if( (i + chi.width + chi2.width) < patt->length ){
			chi3 = DString_DecodeChar2( chars + i + chi.width + chi2.width, end );
		}
		if( i == 0 && chi.value == '^' ){
			blmatch = 0;
		}else if( chi.value == '%' ){
			i += chi2.width;
			switch( chi2.value ){
			case 's': case 'S': case 'k': case 'K': case 'p': case 'P':
			case 'c': case 'C': case 'a': case 'A': case 'w': case 'W':
			case 'e': case 'E': case 'd': case 'D': case 'x': case 'X':
				patt->type = chi2.value;
				matched = (MatchOne( self, patt, pos ) !=0);
				patt->type = PAT_SET;
				break;
			case 't' : matched = (ch.value == '\t'); break;
			case 'n' : matched = (ch.value == '\n'); break;
			default  : matched = (ch.value == chi2.value); break;
			}
		}else if( chi3.type && iswalnum(chi.value) && chi2.value == '-' && iswalnum(chi3.value) ){
			i += chi2.width + chi3.width;
			matched = ( ch.value >= chi.value && ch.value <= chi3.value );
		}else{
			matched = (chi.value == ch.value);
		}
		i += chi.width;
		if( matched ) break;
	}
	if( matched == blmatch ) return 1;
	patt->offset = 0;
	return 0;
}
static int GetGroup( DaoRegex *self, DaoRgxItem *p, int gid, daoint *start, daoint *end )
{
	daoint gp1, gp2;
	gp1 = gp2 = DAO_NULLPOS;
	while( p->from >0 ){
		if( p->type == PAT_SPLIT && p->gid == gid ) gp1 = p->pos;
		if( p->type == PAT_JOIN && p->gid == gid ) gp2 = p->pos;
		p -= p->from;
		if( p->type == PAT_PATPAIR ){
			p = p + p->next - 1;
		}else if( p->type == PAT_BEGIN && p != self->items ){
			p --;
		}
	}
	if( gp1 == DAO_NULLPOS || gp2 == DAO_NULLPOS ) return 0;
	*start = gp1;
	*end = gp2;
	return 1;
}
static int MatchBackRef( DaoRegex *self, DaoRgxItem *patt, daoint pos )
{
	daoint i, n, gp1, gp2, end = self->end;
	short gid = patt->gid;
	char *s, *s2;

	patt->offset = 0;
	if( GetGroup( self, patt, gid, & gp1, & gp2 ) ==0 ) return 0;

	if( pos + (gp2 - gp1) > end ) return 0;
	n = gp2 - gp1;
	s = self->source + pos;
	s2 = self->source + gp1;
	if( self->config & PAT_CONFIG_CASEINS ){
		for(i=0; i<n; i++) if( tolower( s[i] ) != tolower( s2[i] ) ) return 0;
	}else{
		for(i=0; i<n; i++) if( s[i] != s2[i] ) return 0;
	}
	patt->offset = gp2 - gp1;
	return 1;
}
static int DaoRegex_Search( DaoRegex *self, DaoRgxItem *patts, int npatt,
		void *src, daoint size, daoint *start, daoint *end, int fixed );

static int MatchPair( DaoRegex *self, DaoRgxItem *patt, daoint pos )
{
	DCharState st, st2;
	uint_t lc, rc, c;
	char *ps = self->wordbuf + patt->word;
	char *start = self->source + pos;
	char *end = self->source + self->end;
	daoint n = self->end - pos;
	daoint count, i=0;
	int nocase = (patt->config & PAT_CONFIG_CASEINS);

	if( pos + 2 > self->end ) return 0;

	st = DString_DecodeChar( ps, ps + patt->length );
	st2 = DString_DecodeChar( ps + st.width, ps + patt->length );
	lc = st.value;
	rc = st2.value;

	count = 0;
	st = DString_DecodeChar( start, end );
	c = nocase ? towlower( st.value ) : st.value;
	if( c != lc ) return 0;
	for(i=0; i<n; i+=st.width){
		st = DString_DecodeChar( start + i, end );
		c = nocase ? towlower( st.value ) : st.value;
		if( c == lc ){
			count ++;
		}else if( c == rc ){
			count --;
			if( count ==0 ) break;
		}
	}
	if( i >= n ) return 0;
	patt->offset = i + st.width;
	return 1;
}
static int CountRegex( DaoRegex *self, DaoRgxItem *patts, int npatt,
		void *src, daoint size, daoint start, daoint end, daoint start0, daoint end0 )
{
	daoint count = 0, m1 = start, m2 = end;
	while( DaoRegex_Search( self, patts, npatt, self->source, size, & m1, & m2, 0 ) ){
		if( (m2 - m1) == (end0 - start0) ){
			count += strncmp( (char*)src + m1, (char*)src + start0, m2 - m1 + 1 ) == 0;
		}
		m1 = m2 + 1;
		m2 = end;
	}
	return count;
}
static int MatchPatPair( DaoRegex *self, DaoRgxItem *patt, daoint pos )
{
	DaoRgxItem *pl = patt + 1;
	DaoRgxItem *pr = patt + patt->length;
	short itl = patt->length, itr = patt->next - patt->length;
	void *src = self->source;
	daoint end = self->end;
	daoint m1 = pos, m2 = end, m3 = pos, m4 = end, m5, m6;
	int bl;
	int count = 0;
	bl = DaoRegex_Search( self, pl, itl, src, end, & m1, & m2, 1 );
	/* printf( "bl = %i, %i %i\n", bl, m1, m2 ); */
	if( bl == 0 ) return 0;
	m3 = m2 + 1;
	m5 = m1;
	m6 = m2;
	bl = DaoRegex_Search( self, pr, itr, src, end, & m3, & m4, 0 );
	if( bl == 0 )  return 0;
	count = CountRegex( self, pl, itl, src, end, m2, m3, m1, m2 );
	/* printf( "count = %i, %i %i\n", count, m2, m3 ); */
	m2 = m4 + 1;
	while( count >0 ){
		/* reset for back references */
		DaoRegex_Search( self, pl, itl, src, end, & m5, & m6, 1 );
		m3 = m2 + 1;  m4 = end;
		bl = DaoRegex_Search( self, pr, itr, src, end, & m3, & m4, 0 );
		/* printf( "bl2 = %i, %i %i\n", bl, m3, m4 ); */
		if( bl == 0 )  return 0;
		count += CountRegex( self, pl, itl, src, end, m2, m3, m5, m6 ) - 1;
		/* printf( "count = %i, %i %i\n", count, m2, m3 ); */
		m2 = m4 + 1;
	}
	patt->offset = m4 + 1 - pos;
	/* correct backreference */
	m6 ++;
	m4 ++;
	/* printf( "%i  %i;  %i  %i\n", m5, m6, m3, m4 ); */
	DaoRegex_Search( self, pl, itl, src, end, & m5, & m6, 1 );
	DaoRegex_Search( self, pr, itr, src, end, & m3, & m4, 0 );
	return 1;
}
const uint_t dao_cjk_charts[][2] = 
{
	{0x3400, 0x4DBF},   /* Extension A; */
	{0x4E00, 0x9FFF},   /* Basic Block; */
	{0xF900, 0xFAFF},   /* Compatibility; */
	{0x20000, 0x2A6DF}, /* Extension B; */
	{0x2A700, 0x2B73F}, /* Extension C; */
	{0x2B740, 0x2B81F}, /* Extension D; */
	{0x2F800, 0x2FA1F}, /* Compatibility Supplement; */
};
int dao_cjk( uint_t ch )
{
	int i = 0;
	while( i < 7 && dao_cjk_charts[i][1] < ch ) i += 1;
	if( i >= 7 || dao_cjk_charts[i][0] > ch ) return 0;
	return 1;
}

#ifdef WITHOUT_WCTYPE_H
#define iswideogram(c) 0
#define iswphonogram(c) 0
#endif
int dao_character( uint_t ch )
{
#ifdef BSD
	return (ch == '_' || iswalnum(ch) || iswideogram(ch) || iswphonogram(ch) );
#elif defined(LINUX)
	return (ch == '_' || iswalnum(ch));
#else
	uint_t ch2 = ch;
	if( sizeof(wchar_t) == 2 && ch > 0xFFFF ) ch = 0; /* for isw*(); */
	return (ch == '_' || iswalnum(ch) || dao_cjk(ch2));
#endif
}
static int MatchOne( DaoRegex *self, DaoRgxItem *patt, daoint pos )
{
	DCharState st = { 0, 1, 0 };
	DCharState st2 = { 0, 1, 0 };
	uint_t ch, ch2, b1, b2;
	int i;

	patt->offset = 0;
	patt->count ++;
	switch( patt->type ){
	case PAT_WBORDER : 
		if( pos == self->start || pos >= self->end ) return 1;
		for(i=1; i<=4 && (pos-i)>=self->start; ++i){
			st = DString_DecodeChar( self->source + pos-i, self->source + self->end );
			if( i == 1 ) st2 = st;
			if( st.type == i ) break;
		}
		if( st.type == 0 ) st = st2;
		st2 = DString_DecodeChar( self->source + pos, self->source + self->end );
		if( dao_character( st.value ) != dao_character( st2.value ) ) return 1;
		return 0;
	case PAT_ANY :
		if( pos >= self->end ) return 0;
		st = DString_DecodeChar( self->source + pos, self->source + self->end );
		if( pos + st.width >= self->end ) st.width = 1;
		patt->offset = st.width;
		return 1;
	case PAT_SPLIT:
	case PAT_BEGIN: case PAT_JOIN: return 1;
	case PAT_START: return (pos == self->start);
	case PAT_END : return (pos >= self->end);
	case PAT_SET  : return MatchSet(  self, patt, pos );
	case PAT_WORD : return MatchWord( self, patt, pos );
	case PAT_PAIR : return MatchPair( self, patt, pos );
	case PAT_PATPAIR : return MatchPatPair( self, patt, pos );
	case PAT_BACKREF: return MatchBackRef( self, patt, pos );
	default : break;
	}
	if( pos >= self->end ) return 0;

	st = DString_DecodeChar( self->source + pos, self->source + self->end );
	if( st.type == 0 ) return 0;

	patt->offset = st.width;
	ch = ch2 = st.value;
	if( sizeof(wchar_t) == 2 && ch > 0xFFFF ) ch = 0; /* for isw*(); */

	switch( patt->type ){
	case 'a' : return iswalpha( ch );
	case 's' : return iswspace( ch );
	case 'k' : return iswcntrl( ch );
	case 'p' : return iswpunct( ch );
	case 'd' : return iswdigit( ch );
	case 'x' : return iswxdigit( ch );
	case 'e' : return dao_cjk( ch2 );
	case 'w' : return dao_character( ch2 );
	case 'c' : return iswlower( ch );
	case 'A' : return ! iswalpha( ch );
	case 'S' : return ! iswspace( ch );
	case 'K' : return ! iswcntrl( ch );
	case 'P' : return ! iswpunct( ch );
	case 'D' : return ! iswdigit( ch );
	case 'X' : return ! iswxdigit( ch );
	case 'E' : return ! dao_cjk( ch2 );
	case 'W' : return ! dao_character( ch2 );
	case 'C' : return iswupper( ch );
	default : return 0;
	}
	return 0;
}

static int MatchMin( DaoRegex *self, DaoRgxItem *patt, daoint pos )
{
	patt->pos = pos;
	patt->count = 0;
	while( patt->count < patt->min ){
		if( MatchOne( self, patt, pos ) ==0 ) return 0;
		pos += patt->offset;
	}
	patt->offset = pos - patt->pos;
	return 1;
}
static int MatchExpand( DaoRegex *self, DaoRgxItem *patt, daoint pos )
{
	if( patt->max <0 || patt->count < patt->max ){
		daoint offset = patt->offset;
		if( MatchOne( self, patt, pos ) ){
			patt->offset += offset;
			return 1;
		}
	}
	return 0;
}

static int DaoRegex_Search( DaoRegex *self, DaoRgxItem *patts, int npatt,
		void *src, daoint size, daoint *start, daoint *end, int fixed )
{
	DaoRgxItem *patt, *patt2;
	daoint s1 = 0, s2 = size-1;
	daoint pos, sum, max = 0, min = 0x7fffffff, from = 0, to = size;
	daoint oldstart = self->start, oldend = self->end;
	int bl, expand, matched, minmode = ((self->config & PAT_CONFIG_MIN) !=0);
	if( patts == NULL ){
		patts = self->items;
		npatt = self->count;
	}
#if 0
	int i;
	printf( "npatt = %i %i\n", npatt, size );
	for(i=0; i<npatt; i++) printf( "%3i: ", i ), PrintRegex( self, patts + i );
#endif
	/* there is at least PAT_BEGIN and PAT_STOP */
	if( size == 0 || self->count <= 2 ) return 0;
	if( start && *start >= size ) return 0;
	if( start ) from = *start; else start = & s1;
	if( end ) to = *end + 1; else end = & s2;
	if( to > size ) to = size;
	self->source = src;
	self->start = from;
	self->end = to;
	pos = from;
	patt = patts;
	patt->pos = 0;
	patt->from = 0;
	bl = expand = matched = 0;
	while( pos < to+2 ){
		if( expand ){
			bl = MatchExpand( self, patt, pos );
		}else{
			bl = MatchMin( self, patt, pos );
		}
#if 0
		printf( "%4i  %2i, pos = %2i, o = %i,  ch = %c, e = %i, bl = %i, ",
				to, patts->pos, pos, patt->offset, ((char*)src)[pos], expand, bl );
		PrintRegex( self, patt );
#endif
		if( bl ==0 ){
			if( patt->type == PAT_START ) break;
			expand = 0;
			if( patt == patts ){
				pos += 1;
				if( fixed ) break;
			}else{
				pos = patt->pos;
				patt2 = patt - patt->from;
				if( patt2->jump && patt->from == patt2->next ){
					patt = patt2 + patt2->jump;
					patt->from = patt2->jump;
				}else{
					patt = patt2;
					expand = 1;
				}
			}
		}else{
			expand = 0;
			pos = patt->pos + patt->offset;
			patt2 = patt + patt->next;
			patt2->from = patt->next;
			patt = patt2;
		}
		if( matched && patt == patts ){
			break;
		}else if( patt->type == PAT_STOP ){
			patt->pos = pos;
			patt2 = patt;
			sum = pos - patts->pos;
			while( patt2->from >0 ){
				patt2 -= patt2->from;
				if( patt2->type == PAT_JOIN && patt2->gid ) sum += patt2->pos;
				if( patt2->type == PAT_SPLIT && patt2->gid ) sum -= patt2->pos;
			}
			if( patt->type == PAT_STOP && pos == patts->pos ) continue;
			if( ( minmode && sum < min ) || ( ! minmode && sum >= max ) ){
				max = min = sum;
				*start = patts->pos;
				*end = pos - 1;
				patt2 = patt;
				patt2->posave = patt2->pos;
				patt2->fromsave = patt2->from;
				while( patt2->from >0 ){
					patt2 -= patt2->from;
					patt2->posave = patt2->pos;
					patt2->fromsave = patt2->from;
				}
			}
			patt2 = patts + npatt -1;
			patt = patt - patt->from;
			matched = 1;
			expand = 1;
			if( self->attrib & PAT_ALL_FIXED ) break;
		}
	}
	self->start = oldstart;
	self->end = oldend;
	return matched;
}

int DaoRegex_CheckSize( DString *src )
{
	int n = src->size;
	int m = DString_BalancedChar( src, '|', 0,0, '%', 0, n, 1 ) + 4; /* (|||) */
	int size = sizepat + (n+m) * sizeitm + n * (src->chars ? 1 : sizewch) + 4;
	return ALIGN( size );
}
int DaoRegex_CheckSizeChars( const char *src )
{
	DString str = DString_WrapChars( src );
	return DaoRegex_CheckSize( & str );
}

/* assuming "self" pointing to a memory,
 * enough large for the compiled pattern data */
void DaoRegex_Init( DaoRegex *self, DString *src )
{
	int n = src->size;
	int m = DString_BalancedChar( src, '|', 0,0, '%', 0, n, 1 ) + 4; /* (|||) */
	int size = DaoRegex_CheckSize( src );
	self->length = size;
	self->items = (DaoRgxItem*)(((char*)self) + sizepat);
	self->wordbuf = ((char*)self) + sizepat + (n+m) * sizeitm;
	self->itemlen = (n+m) * sizeitm;
	self->wordlen = n * (src->chars ? 1 : sizewch) + 1;
	InitRegex( self, src );
}

int DString_Match( DString *self, const char *pat, daoint *start, daoint *end )
{
	DString *str = DString_New();
	DaoRegex *regex;
	int rc;
	DString_SetChars( str, pat );
	regex = (DaoRegex*) dao_malloc( DaoRegex_CheckSize( str ) );
	DaoRegex_Init( regex, str );
	DString_Delete( str );
	rc = DaoRegex_Match( regex, self, start, end );
	dao_free( regex );
	return rc;
}

int DString_Change( DString *self, const char *pat, const char *target, int index )
{
	DString *str = DString_New();
	DString *tg = DString_New();
	DaoRegex *regex;
	int rc;
	DString_SetChars( str, pat );
	DString_SetChars( tg, target );
	regex = (DaoRegex*) dao_malloc( DaoRegex_CheckSize( str ) );
	DaoRegex_Init( regex, str );
	DString_Delete( str );
	rc = DaoRegex_Change( regex, self, tg, index );
	DString_Delete( tg );
	dao_free( regex );
	return rc;
}
DaoRegex* DaoRegex_New( DString *src )
{
	DaoRegex *self = NULL;
	int size = DaoRegex_CheckSize( src );
	self = (DaoRegex*) dao_malloc( size );
	self->length = size;
	DaoRegex_Init( self, src );
	return self;
}
void DaoRegex_Copy( DaoRegex *self, DaoRegex *src )
{
	memcpy( self, src, src->length );
	self->items = (DaoRgxItem*)(((char*)self) + sizepat);
	self->wordbuf = ((char*)self) + sizepat + self->itemlen;
}
int DaoRegex_Match( DaoRegex *self, DString *src, daoint *start, daoint *end )
{
	return DaoRegex_Search( self, 0,0, src->chars, src->size, start, end, 0 );
}

int DaoRegex_SubMatch( DaoRegex *self, int gid, daoint *start, daoint *end )
{
	DaoRgxItem *p = self->items + self->count-1;
	daoint gp1, gp2;
	if( gid ==0 ){
		*start = self->items->posave;
		*end = p->posave -1;
		return 1;
	}
	gp1 = gp2 = DAO_NULLPOS;
	while( p->fromsave >0 ){
		p -= p->fromsave;
		if( p->type == PAT_SPLIT && p->gid == gid ) gp1 = p->posave;
		if( p->type == PAT_JOIN && p->gid == gid ) gp2 = p->posave -1;
		if( p->type == PAT_PATPAIR ){
			p = p + p->next - 1;
		}else if( p->type == PAT_BEGIN && p != self->items ){
			p --;
		}
	}
	if( gp1 == DAO_NULLPOS || gp2 == DAO_NULLPOS ) return 0;
	*start = gp1;
	*end = gp2;
	return 1;
}
static void Dao_ParseTarget( DString *target, DList *parts, DaoValue *sval )
{
	DString *tmp = sval->xString.value;
	DaoInteger ival = {DAO_INTEGER,0,0,0,0,0};
	daoint i, n = DString_Size( target );
	int ch, ch2;
	DString_Clear( tmp );
	for(i=0; i<n; i++){
		ch = target->chars[i];
		ch2 = target->chars[i+1];
		if( ch == '%' && isdigit( ch2 ) ){
			DList_PushBack( parts, sval );
			DString_Clear( tmp );
			ival.value = ch2 - '0';
			DList_PushBack( parts, (DaoValue*) & ival );
			i ++;
		}else if( ch == '%' ){
			if( i+1 < n ){
				DString_AppendChar( tmp, (char)ch2 );
			}
			i ++;
		}else{
			DString_AppendChar( tmp, (char)ch );
		}
	}
	DList_PushBack( parts, sval );
}
int DaoRegex_ChangeExt( DaoRegex *self, DString *input, DString *output,
		DString *target, int index, daoint *start2, daoint *end2 )
{
	daoint start = start2 ? (daoint) *start2 : 0;
	daoint end = end2 ? (daoint) *end2 : 0;
	daoint i, n=0, p1=start, p2=end, p3, last;
	DaoValue *value = NULL;
	DaoString matched = {DAO_STRING,0,0,0,0,NULL};
	DList *array = DList_New( DAO_DATA_VALUE );
	DString *tmp = DString_New();
	DString *tmp2 = DString_New();

	DString_Reset( output, 0 );
	if( self == NULL || input->size == 0 ) goto DoNothing;

	matched.value = tmp;
	Dao_ParseTarget( target, array, (DaoValue*) & matched );
	if( end == 0 ) end = p2 = DString_Size( input ) - 1;
	n = last = 0;
	while( DaoRegex_Match( self, input, & p1, & p2 ) ){
		n += 1;
		if( index ==0 || n == index ){
			DString_SubString( input, tmp2, last, p1 - last );
			DString_Append( output, tmp2 );
			DString_Clear( tmp );
			for(i=0; i<array->size; i++){
				value = array->items.pValue[i];
				if( value->type == DAO_INTEGER ){
					if( DaoRegex_SubMatch( self, value->xInteger.value, & p1, & p3 ) ){
						DString_SubString( input, tmp2, p1, p3-p1 + 1 );
						DString_Append( tmp, tmp2 );
					}
				}else{
					DString_Append( tmp, value->xString.value );
				}
			}
			DString_Append( output, tmp );
			last = p2 + 1;
		}
		if( start2 ) *start2 = p1;
		if( end2 ) *end2 = p2;
		p1 = p2 + 1;
		p2 = end;
		if( index && n == index ) break;
	}
	DString_SubString( input, tmp2, last, end - last + 1 );
	DString_Append( output, tmp2 );
DoNothing:
	DString_Delete( tmp );
	DString_Delete( tmp2 );
	DList_Delete( array );
	return n;
}
int DaoRegex_Change( DaoRegex *self, DString *source, DString *target, int index )
{
	DString *input = DString_Copy( source );
	int count = DaoRegex_ChangeExt( self, input, source, target, index, NULL, NULL );
	DString_Delete( input );
	return count;
}
