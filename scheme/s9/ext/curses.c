/*
 * Scheme 9 from Empty Space, Curses Interface
 * By Nils M Holm, 2010-2015
 * Placed in the Public Domain
 *
 * A low-level interface to some CURSES(3) routines.
 */

#define S9FES
#include "s9core.h"

/*
 * XXX Because of major C "macro" preprocessor brain damage,
 * the following values have to be *copied* from s9core.h. *Sigh*
 */

#define S9_TRUE  (-2)
#define S9_FALSE (-3)
#define _nl() pr("\n")
#undef nl
#undef TRUE
#undef FALSE

#include <stdlib.h>
#include <curses.h>

int	Running = 0;

cell pp_curs_addch(cell x) {
	if (!Running) return UNSPECIFIC;
	addch(char_value(car(x)));
	return UNSPECIFIC;
}

cell pp_curs_addstr(cell x) {
	if (!Running) return UNSPECIFIC;
	addstr(string(car(x)));
	return UNSPECIFIC;
}

cell pp_curs_attrset(cell x) {
	if (!Running) return UNSPECIFIC;
	attrset(integer_value("curs:attrset", car(x)));
	return UNSPECIFIC;
}

cell pp_curs_beep(cell x) {
	if (!Running) return UNSPECIFIC;
	beep();
	return UNSPECIFIC;
}

cell pp_curs_cbreak(cell x) {
	if (!Running) return UNSPECIFIC;
	cbreak();
	return UNSPECIFIC;
}

cell pp_curs_clear(cell x) {
	if (!Running) return UNSPECIFIC;
	clear();
	return UNSPECIFIC;
}

cell pp_curs_clearok(cell x) {
	if (!Running) return UNSPECIFIC;
	clearok(stdscr, car(x) == S9_TRUE? TRUE: FALSE);
	return UNSPECIFIC;
}

cell pp_curs_clrtobot(cell x) {
	if (!Running) return UNSPECIFIC;
	clrtobot();
	return UNSPECIFIC;
}

cell pp_curs_clrtoeol(cell x) {
	if (!Running) return UNSPECIFIC;
	clrtoeol();
	return UNSPECIFIC;
}

cell pp_curs_cols(cell x) {
	return make_integer(COLS);
}

cell pp_curs_cursoff(cell x) {
	if (!Running) return UNSPECIFIC;
	curs_set(0);
	return UNSPECIFIC;
}

cell pp_curs_curson(cell x) {
	if (!Running) return UNSPECIFIC;
	curs_set(1);
	return UNSPECIFIC;
}

cell pp_curs_delch(cell x) {
	if (!Running) return UNSPECIFIC;
	delch();
	return UNSPECIFIC;
}

cell pp_curs_deleteln(cell x) {
	if (!Running) return UNSPECIFIC;
	deleteln();
	return UNSPECIFIC;
}

cell pp_curs_echo(cell x) {
	if (!Running) return UNSPECIFIC;
	echo();
	return UNSPECIFIC;
}

cell pp_curs_endwin(cell x) {
	if (!Running) return UNSPECIFIC;
	endwin();
	Running = 0;
	return UNSPECIFIC;
}

cell pp_curs_flash(cell x) {
	if (!Running) return UNSPECIFIC;
	flash();
	return UNSPECIFIC;
}

cell pp_curs_flushinp(cell x) {
	if (!Running) return UNSPECIFIC;
	flushinp();
	return UNSPECIFIC;
}

cell pp_curs_get_magic_value(cell x) {
	char	*s = string(car(x));

	if (!strcmp(s, "A_BOLD")) return make_integer(A_BOLD);
	if (!strcmp(s, "A_NORMAL")) return make_integer(A_NORMAL);
	if (!strcmp(s, "A_STANDOUT")) return make_integer(A_STANDOUT);
	if (!strcmp(s, "A_UNDERLINE")) return make_integer(A_UNDERLINE);
	if (!strcmp(s, "KEY_BACKSPACE")) return make_integer(KEY_BACKSPACE);
	if (!strcmp(s, "KEY_DC")) return make_integer(KEY_DC);
	if (!strcmp(s, "KEY_DOWN")) return make_integer(KEY_DOWN);
	if (!strcmp(s, "KEY_END")) return make_integer(KEY_END);
	if (!strcmp(s, "KEY_IC")) return make_integer(KEY_IC);
	if (!strcmp(s, "KEY_HOME")) return make_integer(KEY_HOME);
	if (!strcmp(s, "KEY_LEFT")) return make_integer(KEY_LEFT);
	if (!strcmp(s, "KEY_NPAGE")) return make_integer(KEY_NPAGE);
	if (!strcmp(s, "KEY_PPAGE")) return make_integer(KEY_PPAGE);
	if (!strcmp(s, "KEY_RIGHT")) return make_integer(KEY_RIGHT);
	if (!strcmp(s, "KEY_UP")) return make_integer(KEY_UP);
	return error("curs:get-magic-value: requested value not found",
			car(x));
}

cell pp_curs_getch(cell x) {
	int	c;

	if (!Running) return UNSPECIFIC;
	c = getch();
	if (c == ERR)
		return S9_FALSE;
	return make_integer(c);
}

cell pp_curs_getyx(cell x) {
	int	cx, cy;
	cell	n;

	if (!Running) return UNSPECIFIC;
	getyx(stdscr, cy, cx);
	n = make_integer(cx);
	n = cons(n, NIL);
	save(n);
	n = cons(make_integer(cy), n);
	unsave(1);
	return n;
}

cell pp_curs_idlok(cell x) {
	if (!Running) return UNSPECIFIC;
	idlok(stdscr, car(x) == S9_TRUE? TRUE: FALSE);
	return UNSPECIFIC;
}

cell pp_curs_inch(cell x) {
	if (!Running) return UNSPECIFIC;
	return make_char(inch());
}

cell pp_curs_insch(cell x) {
	if (!Running) return UNSPECIFIC;
	insch(char_value(car(x)));
	return UNSPECIFIC;
}

#ifdef CURSES_COLOR

cell pp_curs_initscr(cell x) {
	int colors[] = { COLOR_BLACK, COLOR_BLUE, COLOR_GREEN,
			 COLOR_CYAN, COLOR_RED, COLOR_MAGENTA,
			 COLOR_YELLOW, COLOR_WHITE };
	int	f, b;

	if (Running) return UNSPECIFIC;
	initscr();
	start_color();
	for (b=0; b<8; b++) {
		for (f=0; f<8; f++) {
			init_pair(b*8+f, colors[f], colors[b]);
		}
	}
	Running = 1;
	return UNSPECIFIC;
}

cell pp_curs_color_set(cell x) {
	int	f, b;
	char	name[] = "curs:color-set";

	f = integer_value(name, car(x));
	b = integer_value(name, cadr(x));
	color_set(b<<3|f, NULL);
	return UNSPECIFIC;
}

cell pp_curs_has_colors(cell x) {
	return has_colors()? S9_TRUE: S9_FALSE;
}

#else /* !CURSES_COLOR */

cell pp_curs_has_colors(cell x) {
	return S9_FALSE;
}
cell pp_curs_initscr(cell x) {
	if (Running) return UNSPECIFIC;
	initscr();
	Running = 1;
	return UNSPECIFIC;
}

#endif /* !CURSES_COLOR */

cell pp_curs_insertln(cell x) {
	if (!Running) return UNSPECIFIC;
	insertln();
	return UNSPECIFIC;
}

cell pp_curs_keypad(cell x) {
	if (!Running) return UNSPECIFIC;
	keypad(stdscr, car(x) == S9_TRUE? TRUE: FALSE);
	return UNSPECIFIC;
}

cell pp_curs_lines(cell x) {
	return make_integer(LINES);
}

cell pp_curs_move(cell x) {
	char	name[] = "curs:move";

	if (!Running) return UNSPECIFIC;
	move(integer_value(name, car(x)), integer_value(name, cadr(x)));
	return UNSPECIFIC;
}

cell pp_curs_mvaddch(cell x) {
	char	name[] = "curs:mvaddch";

	if (!Running) return UNSPECIFIC;
	mvaddch(integer_value(name, car(x)),
		integer_value(name, cadr(x)),
		char_value(caddr(x)));
	return UNSPECIFIC;
}

cell pp_curs_mvaddstr(cell x) {
	char	name[] = "curs:mvaddstr";

	if (!Running) return UNSPECIFIC;
	mvaddstr(integer_value(name, car(x)),
		integer_value(name, cadr(x)),
		string(caddr(x)));
	return UNSPECIFIC;
}

cell pp_curs_mvcur(cell x) {
	char	name[] = "curs:mvcur";

	if (!Running) return UNSPECIFIC;
	if (!integer_p(cadddr(x)))
		return error("curs:mvcur: expected integer, got",
				caddr(cdr(x)));
	mvcur(integer_value(name, car(x)),
		integer_value(name, cadr(x)),
		integer_value(name, caddr(x)),
		integer_value(name, cadddr(x)));
	return UNSPECIFIC;
}

cell pp_curs_mvdelch(cell x) {
	char	name[] = "curs:mvdelch";

	if (!Running) return UNSPECIFIC;
	mvdelch(integer_value(name, car(x)),
		integer_value(name, cadr(x)));
	return UNSPECIFIC;
}

cell pp_curs_mvgetch(cell x) {
	char	name[] = "curs:mvgetch";
	int	c;

	if (!Running) return UNSPECIFIC;
	c = mvgetch(integer_value(name, car(x)),
			integer_value(name, cadr(x)));
	if (c == ERR)
		return S9_FALSE;
	return make_integer(c);
}

cell pp_curs_mvinch(cell x) {
	char	name[] = "curs:mvinch";

	if (!Running) return UNSPECIFIC;
	return make_char((int) mvinch(integer_value(name, car(x)),
			integer_value(name, cadr(x))));
}

cell pp_curs_mvinsch(cell x) {
	char	name[] = "curs:mvinsch";

	if (!Running) return UNSPECIFIC;
	mvinsch(integer_value(name, car(x)),
		integer_value(name, cadr(x)),
		char_value(caddr(x)));
	return UNSPECIFIC;
}

cell pp_curs_nl(cell x) {
	if (!Running) return UNSPECIFIC;
	nl();
	return UNSPECIFIC;
}

cell pp_curs_nocbreak(cell x) {
	if (!Running) return UNSPECIFIC;
	nocbreak();
	return UNSPECIFIC;
}

cell pp_curs_nodelay(cell x) {
	if (!Running) return UNSPECIFIC;
	nodelay(stdscr, car(x) == S9_TRUE? TRUE: FALSE);
	return UNSPECIFIC;
}

cell pp_curs_noecho(cell x) {
	if (!Running) return UNSPECIFIC;
	noecho();
	return UNSPECIFIC;
}

cell pp_curs_nonl(cell x) {
	if (!Running) return UNSPECIFIC;
	nonl();
	return UNSPECIFIC;
}

cell pp_curs_noraw(cell x) {
	if (!Running) return UNSPECIFIC;
	noraw();
	return UNSPECIFIC;
}

cell pp_curs_raw(cell x) {
	if (!Running) return UNSPECIFIC;
	raw();
	return UNSPECIFIC;
}

cell pp_curs_refresh(cell x) {
	if (!Running) return UNSPECIFIC;
	refresh();
	return UNSPECIFIC;
}

cell pp_curs_resetty(cell x) {
	if (!Running) return UNSPECIFIC;
	resetty();
	return UNSPECIFIC;
}

cell pp_curs_savetty(cell x) {
	if (!Running) return UNSPECIFIC;
	savetty();
	return UNSPECIFIC;
}

cell pp_curs_scroll(cell x) {
	if (!Running) return UNSPECIFIC;
	scrl(integer_value("curs:scroll", car(x)));
	return UNSPECIFIC;
}

cell pp_curs_scrollok(cell x) {
	if (!Running) return UNSPECIFIC;
	scrollok(stdscr, car(x) == S9_TRUE? TRUE: FALSE);
	return UNSPECIFIC;
}

cell pp_curs_unctrl(cell x) {
	char	*s;

	if (!Running) return UNSPECIFIC;
	s = (char *) unctrl(integer_value("curs:unctrl", car(x)));
	return make_string(s, strlen(s));
}

cell pp_curs_ungetch(cell x) {
	if (!Running) return UNSPECIFIC;
	ungetch(integer_value("curs:ungetch", car(x)));
	return UNSPECIFIC;
}

PRIM Curs_primitives[] = {
 { "curs:addch",            pp_curs_addch,           1,  1, { CHR,___,___ } },
 { "curs:addstr",           pp_curs_addstr,          1,  1, { STR,___,___ } },
 { "curs:attrset",          pp_curs_attrset,         1,  1, { INT,___,___ } },
 { "curs:beep",             pp_curs_beep,            0,  0, { ___,___,___ } },
 { "curs:cbreak",           pp_curs_cbreak,          0,  0, { ___,___,___ } },
 { "curs:clear",            pp_curs_clear,           0,  0, { ___,___,___ } },
 { "curs:clearok",          pp_curs_clearok,         1,  1, { BOL,___,___ } },
 { "curs:clrtobot",         pp_curs_clrtobot,        0,  0, { ___,___,___ } },
 { "curs:clrtoeol",         pp_curs_clrtoeol,        0,  0, { ___,___,___ } },
#ifdef CURSES_COLOR
 { "curs:color-set",        pp_curs_color_set,       2,  2, { INT,INT,___ } },
#endif /* CURSES_COLOR */
 { "curs:cols",             pp_curs_cols,            0,  0, { ___,___,___ } },
 { "curs:cursoff",          pp_curs_cursoff,         0,  0, { ___,___,___ } },
 { "curs:curson",           pp_curs_curson,          0,  0, { ___,___,___ } },
 { "curs:delch",            pp_curs_delch,           0,  0, { ___,___,___ } },
 { "curs:deleteln",         pp_curs_deleteln,        0,  0, { ___,___,___ } },
 { "curs:echo",             pp_curs_echo,            0,  0, { ___,___,___ } },
 { "curs:endwin",           pp_curs_endwin,          0,  0, { ___,___,___ } },
 { "curs:flash",            pp_curs_flash,           0,  0, { ___,___,___ } },
 { "curs:flushinp",         pp_curs_flushinp,        0,  0, { ___,___,___ } },
 { "curs:get-magic-value",  pp_curs_get_magic_value, 1,  1, { STR,___,___ } },
 { "curs:getch",            pp_curs_getch,           0,  0, { ___,___,___ } },
 { "curs:getyx",            pp_curs_getyx,           0,  0, { ___,___,___ } },
 { "curs:has-colors",       pp_curs_has_colors,      0,  0, { ___,___,___ } },
 { "curs:idlok",            pp_curs_idlok,           1,  1, { BOL,___,___ } },
 { "curs:inch",             pp_curs_inch,            0,  0, { ___,___,___ } },
 { "curs:insch",            pp_curs_insch,           1,  1, { CHR,___,___ } },
 { "curs:initscr",          pp_curs_initscr,         0,  0, { ___,___,___ } },
 { "curs:insertln",         pp_curs_insertln,        0,  0, { ___,___,___ } },
 { "curs:keypad",           pp_curs_keypad,          1,  1, { BOL,___,___ } },
 { "curs:lines",            pp_curs_lines,           0,  0, { ___,___,___ } },
 { "curs:move",             pp_curs_move,            2,  2, { INT,INT,___ } },
 { "curs:mvaddch",          pp_curs_mvaddch,         3,  3, { INT,INT,CHR } },
 { "curs:mvaddstr",         pp_curs_mvaddstr,        3,  3, { INT,INT,STR } },
 { "curs:mvcur",            pp_curs_mvcur,           2,  2, { INT,INT,___ } },
 { "curs:mvdelch",          pp_curs_mvdelch,         2,  2, { INT,INT,___ } },
 { "curs:mvgetch",          pp_curs_mvgetch,         2,  2, { INT,INT,___ } },
 { "curs:mvinch",           pp_curs_mvinch,          2,  2, { INT,INT,___ } },
 { "curs:mvinsch",          pp_curs_mvinsch,         2,  2, { INT,INT,___ } },
 { "curs:nl",               pp_curs_nl,              0,  0, { ___,___,___ } },
 { "curs:nocbreak",         pp_curs_nocbreak,        0,  0, { ___,___,___ } },
 { "curs:nodelay",          pp_curs_nodelay,         1,  1, { BOL,___,___ } },
 { "curs:noecho",           pp_curs_noecho,          0,  0, { ___,___,___ } },
 { "curs:nonl",             pp_curs_nonl,            0,  0, { ___,___,___ } },
 { "curs:noraw",            pp_curs_noraw,           0,  0, { ___,___,___ } },
 { "curs:raw",              pp_curs_raw,             0,  0, { ___,___,___ } },
 { "curs:refresh",          pp_curs_refresh,         0,  0, { ___,___,___ } },
 { "curs:resetty",          pp_curs_resetty,         0,  0, { ___,___,___ } },
 { "curs:savetty",          pp_curs_savetty,         0,  0, { ___,___,___ } },
 { "curs:scroll",           pp_curs_scroll,          1,  1, { INT,___,___ } },
 { "curs:scrollok",         pp_curs_scrollok,        1,  1, { BOL,___,___ } },
 { "curs:unctrl",           pp_curs_unctrl,          1,  1, { INT,___,___ } },
 { "curs:ungetch",          pp_curs_ungetch,         1,  1, { INT,___,___ } },
 { NULL }
};

void curs_init(void) {
	add_primitives("curses", Curs_primitives);
}
