/*
 * RPP -- ROFF Post-Processor
 * By Nils M Holm 1994
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MYNAME	"rpp"
#define MAXLINE	1024
#define TMAC	"util/rp_"
#define PREFIX	"rp_"
#define ROMAN	0
#define BOLD	1
#define ITALIC	2

int	o_asciify = 0,
	o_upcase = 0,
	o_htmlize = 0;

char	*o_prof = "profile";

char	*inits, *bon, *boff, *uon, *uoff, *ion, *ioff;
char	*sBold, *eBold, *sItalics, *eItalics;
int	font = ROMAN;

void error(msg, a1, a2)
char	*msg, *a1, *a2;
{
	fprintf(stderr, "*** %s: ", MYNAME);
	fprintf(stderr, msg, a1, a2);
	fputc('\n', stderr);
}

int setfont(s, i, f)
char	*s;
int	f, i;
{
	if (font == f) return(i);

	s[i] = 0;

	switch (font) {
		case ROMAN:	break;
		case BOLD:	strcat(s, eBold); break;
		case ITALIC:	strcat(s, eItalics); break;
	}

	switch (f) {
		case ROMAN:	break;
		case BOLD:	strcat(s, sBold); break;
		case ITALIC:	strcat(s, sItalics); break;
	}

	font = f;

	return(strlen(s));
}

int copybuf(buf, i, c)
char	*buf;
int	i, c;
{
	if (o_htmlize) {
		switch (c) {
			case '&': strcpy(&buf[i], "&amp;"); return i+5;
			case '<': strcpy(&buf[i], "&lt;");  return i+4;
			case '>': strcpy(&buf[i], "&gt;");  return i+4;
		}
	}
	buf[i] = c;
	return i+1;
}

char *process(s)
char	*s;
{
	static char	obuf[MAXLINE];
	int	i, back;

	if (o_asciify) {
		for (back=i=0; *s; s++)
			if (*s == '\b' && i) {
				i--;
				back = 1;
			}
			else {
				obuf[i++] = (back && o_upcase && islower(*s)?
				toupper(*s): *s);
				back = 0;
			}
		obuf[i] = 0;
		return(obuf);
	}

	for (i=0; *s; s++) {
		if (*s == '\b' && i) {
			if (obuf[--i] == '_') {
				i = setfont(obuf, i, ITALIC);
				for (; *(s-1) == '_' && *s=='\b'; s += 3)
					i = copybuf(obuf, i, *(s+1));
			}
			else {
				i = setfont(obuf, i, BOLD);
				for (; *(s-1) == *(s+1) && *s=='\b'; s += 3)
					i = copybuf(obuf, i, *(s+1));
			}
			s -= 2;
		}
		else {
			i = setfont(obuf, i, ROMAN);
			i = copybuf(obuf, i, *s);
		}
	}
	setfont(obuf, i, ROMAN);
	obuf[i] = 0;

	return(obuf);
}

void rpp(file)
char	*file;
{
	FILE	*in;
	static char	buf[MAXLINE];
	char	*obuf;

	if (file) {
		if ((in = fopen(file, "r")) == NULL) {
			error("no such file: `%s'", file, NULL);
			return;
		}
	}
	else
		in = stdin;

	fgets(buf, MAXLINE, in);
	while (!feof(in)) {
		obuf = process(buf);
		fputs(obuf, stdout);
		fgets(buf, MAXLINE, in);
	}

	if (file) fclose(in);
}

char *strsave(s)
char	*s;
{
	char	*new;

	if ((new = (char *) malloc(strlen(s)+1)) == NULL) {
		error("out of memory", NULL, NULL);
		error("aborting...", NULL, NULL);
		exit(-1);
	}

	strcpy(new, s);
	return(new);
}

int cval(c, r)
int	c, r;
{
	char	*digits;
	int	i;

	digits = "0123456789abcdef";
	c = (isupper(c)? tolower(c): c);
	for (i=0; i<r; i++)
		if (c == digits[i]) return(i);
	return(-1);
}

void set(arg, val, lno)
char	**arg, *val;
int	lno;
{
	char	*p;
	static char	buf[MAXLINE];
	int	i, radix, c, v;

	if ((p = strchr(val, '\t')) == NULL) {
		sprintf(buf, "%d", lno);
		error("missing TAB in profile (line %s)", buf, NULL);
		return;
	}

	while (*p == '\t') p++;
	p[strlen(p)-1] = 0;

	for (i=0; *p; p++)
		if (*p == '\\') switch (*++p) {
			case 0:		--p; break;
			case '!':	break;

			case 'b':
			case 'B':	buf[i++] = '\b'; break;
			case 'e':
			case 'E':	buf[i++] = '\033'; break;
			case 'n':
			case 'N':	buf[i++] = '\n'; break;
			case 'r':
			case 'R':	buf[i++] = '\r'; break;
			case 't':
			case 'T':	buf[i++] = '\t'; break;
			case 'v':
			case 'V':	buf[i++] = '\v'; break;
			case 'f':
			case 'F':	buf[i++] = '\f'; break;

			default:	radix = 8;
					if (isdigit(*p) || *p == 'x' ||
					*p == 'X') {
						if (!isdigit(*p)) {
							p++;
							radix = 16;
						}
						for (v=c=0; v >= 0; p++) {
							v = cval(*p, radix);
							c = v<0?c:(c*radix+v);
						}
						buf[i++] = c;
					}
					else
						buf[i++] = *p;
		}
		else
			buf[i++] = *p;

	buf[i] = 0;

	*arg = strsave(buf);
}

void readprofile()
{
	char	buf[MAXLINE];
	FILE	*pf;
	int	lno = 0;

	if (o_asciify) return;

	inits = bon = boff = uon = uoff = ion = ioff = "";

	sprintf(buf, "%s%s", TMAC, o_prof);
	if ((pf = fopen(buf, "r")) == NULL) {
		sprintf(buf, "%s%s", PREFIX, o_prof);
		if ((pf = fopen(buf, "r")) == NULL) {
			error("profile not found (%s) -- using ASCII-fy mode",
				buf, NULL);
			o_asciify = 1;
			return;
		}
	}

	fgets(buf, MAXLINE, pf);
	while (!feof(pf)) {
		++lno;
		if (buf[0] == '\n' || buf[0] == '#') {
			fgets(buf, MAXLINE, pf);
			continue;
		}
		else if (!strncmp(buf, "init-string", 11))
			set(&inits, buf, lno);
		else if (!strncmp(buf, "boldface-on", 11))
			set(&bon, buf, lno);
		else if (!strncmp(buf, "boldface-off", 12))
			set(&boff, buf, lno);
		else if (!strncmp(buf, "underline-on", 12))
			set(&uon, buf, lno);
		else if (!strncmp(buf, "underline-off", 13))
			set(&uoff, buf, lno);
		else if (!strncmp(buf, "italics-on", 10))
			set(&ion, buf, lno);
		else if (!strncmp(buf, "italics-off", 11))
			set(&ioff, buf, lno);
		else {
			sprintf(buf, "%d", lno);
			error("bad option in profile (line %s)", buf, NULL);
		}
		fgets(buf, MAXLINE, pf);
	}

	fclose(pf);

	sBold = bon;
	eBold = boff;
	sItalics = ion;
	eItalics = ioff;

	fputs(inits, stdout);

	if (!strcmp(o_prof, "html")) o_htmlize = 1;
}

void usage()
{
	fprintf(stderr, "usage: %s [-acu] [-pprofile] [file ...]\n", MYNAME);
	exit(1);
}

int main(argc, argv)
int	argc;
char	**argv;
{
	int	i, p;

	for (p=1; p<argc && argv[p][0] == '-'; p++)
		for (i=1; argv[p][i]; i++) switch(argv[p][i]) {
			case 'a':	o_asciify = 1; break;
			case 'c':	o_asciify = o_upcase = 1; break;
			case 'u':	sItalics = uon;
					eItalics = uoff; break;
			case 'p':	if (argv[p][++i])
						o_prof = &argv[p][i];
					else
						o_prof = argv[++p];
					i = strlen(argv[p])-1;
					break;
			default:	usage();
		}

	readprofile();

	if (p >= argc)
		rpp(NULL);
	else while (p<argc)
		rpp(argv[p++]);

	return(0);
}
