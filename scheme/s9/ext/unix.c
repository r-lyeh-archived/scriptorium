/*
 * Scheme 9 from Empty Space, Unix Interface
 * By Nils M Holm, 2009-2015
 * Placed in the Public Domain
 *
 * A low-level interface to some Unix system services.
 */

/*
 * Make Linux happy.
 */

#ifndef _BSD_SOURCE
 #define _BSD_SOURCE
#endif
#ifndef __FreeBSD__
 #ifndef __NetBSD__
  #ifndef _POSIX_SOURCE
   #define _POSIX_SOURCE
   #define _POSIX_C_SOURCE 200112L
  #endif
  #ifndef _XOPEN_SOURCE
   #define _XOPEN_SOURCE 500
  #endif
 #endif
#endif

#define S9FES
#include "s9core.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/wait.h>
#include <sys/select.h>
#include <pwd.h>
#include <grp.h>
#include <dirent.h>
#include <signal.h>

#ifdef NETWORK
 #include <sys/socket.h>
 #include <netdb.h>
 #include <sys/utsname.h>
 #include <arpa/inet.h>
 #include <netinet/in.h>
#endif /* NETWORK */

/*
 * XXX Shouldn't these be defined in <sys/stat.h> and <signal.h>?
 * Probably just another POSIX hickup. Please someone supply a
 * clean solution.
 */

#ifndef S_ISVTX
 #define S_ISVTX	01000
#endif
#ifndef SIGTRAP
 #define SIGTRAP	5
#endif
#ifndef SIGEMT
 #define SIGEMT		7
#endif
#ifndef SIGSYS
 #define SIGSYS		12
#endif

/*
 *	Allow us at least to write
 *		assign(car(x), cons(foo, bar));
 *	in presence of that fact that C's
 *	order of evaluation messes up
 *		car(x) = cons(foo, bar);
 */
static cell		New_node;
#define assign(n,v)	{ New_node = (v); n = New_node; }

cell	Last_errno = 0;
cell	Catch_errors = 0;

cell sys_error(char *who, cell what) {
	char	buf[256];
	char	*p, *q;
	int	k;

	Last_errno = errno;
	if (who) {
		k = strlen(who);
		strcpy(buf, who);
		strcpy(&buf[k], ": ");
		k += 2;
		q = strerror(errno);
		for (p = &buf[k]; *q && k < 255; k++)
			*p++ = tolower((int) *q++);
		*p = 0;
		if (Catch_errors)
			return FALSE;
		else
			error(buf, what);
	}
	return FALSE;
}

cell sys_ok(void) {
	return Catch_errors? TRUE: UNSPECIFIC;
}

cell pp_sys_access(cell x) {
	return access(string(car(x)),
		integer_value("sys:access", cadr(x))) < 0? FALSE: TRUE;
}

cell pp_sys_catch_errors(cell x) {
	Catch_errors = car(x) == TRUE;
	if (Catch_errors) Last_errno = 0;
	return UNSPECIFIC;
}

cell pp_sys_chdir(cell x) {
	if (chdir(string(car(x))) < 0)
		return sys_error("sys:chdir", x);
	return sys_ok();
}

cell pp_sys_close(cell x) {
	if (close(integer_value("sys:close", car(x))) < 0)
		return sys_error("sys:close", x);
	return sys_ok();
}

cell pp_sys_chmod(cell x) {
	int	r;

	r = chmod(string(car(x)), integer_value("sys:chmod", cadr(x)));
	if (r < 0)
		return sys_error("sys:chmod", x);
	return sys_ok();
}

cell pp_sys_chown(cell x) {
	int	r;

	r = chown(string(car(x)),
		integer_value("sys:chown", cadr(x)),
		integer_value("sys:chown", caddr(x)));
	if (r < 0)
		return sys_error("sys:chown", x);
	return sys_ok();
}

cell pp_sys_command_line(cell x) {
	extern cell	Argv;

	return Argv;
}

cell pp_sys_creat(cell x) {
	int	fd;

	fd = open(string(car(x)), O_CREAT|O_TRUNC|O_WRONLY,
			integer_value("sys:creat", cadr(x)));
	if (fd < 0)
		return sys_error("sys:creat", x);
	return make_integer(fd);
}

cell pp_sys_dup(cell x) {
	int	r;

	r = dup(integer_value("sys:dup", car(x)));
	if (r < 0)
		return sys_error("sys:dup", x);
	return make_integer(r);
}

cell pp_sys_dup2(cell x) {
	int	r;
	char	name[] = "sys:dup2";

	r = dup2(integer_value(name, car(x)),
			integer_value(name, cadr(x)));
	if (r < 0)
		return sys_error("sys:dup2", x);
	return make_integer(r);
}

cell pp_sys_errno(cell x) {
	int	e = Last_errno;

	Last_errno = 0;
	return make_integer(e);
}

cell pp_sys_execv(cell x) {
	char	**argv;
	cell	p;
	int	i;

	for (p = cadr(x); p != NIL; p = cdr(p)) {
		if (!pair_p(p))
			return error(
				"sys:execv: improper list, last element is",
				p);
		if (!string_p(car(p)))
			return error(
				"sys:execv: expected list of string, got",
				car(p));
	}
	argv = malloc((length(cadr(x)) + 2) * sizeof(char *));
	if (argv == NULL)
		return sys_error("sys:execv", NOEXPR);
	argv[0] = string(car(x));
	i = 1;
	for (p = cadr(x); p != NIL; p = cdr(p))
		argv[i++] = string(car(p));
	argv[i] = NULL;
	execv(string(car(x)), argv);
	free(argv);
	return sys_error("sys:execv", NOEXPR);
}

cell pp_sys_exit(cell x) {
	int	r;

	r = integer_value("sys:exit", car(x));
	if (r > 255 || r < 0)
		return error("sys:exit: value out of range", car(x));
	exit(r);
	fatal("exit() failed");
	return sys_ok();
}

cell pp_sys_fileno(cell x) {
	if (!input_port_p(car(x)) && !output_port_p(car(x)))
		return error("sys:fileno: expected port, got", car(x));
	if (Ports[port_no(car(x))] == NULL)
		return sys_error("sys:fileno", x);
	return make_integer(fileno(Ports[port_no(car(x))]));
}

cell pp_sys_flush(cell x) {
	if (fflush(Ports[port_no(car(x))]))
		return sys_error("sys:flush", x);
	return sys_ok();
}

cell pp_sys_fork(cell x) {
	int	pid;

	pid = fork();
	if (pid < 0)
		return sys_error("sys:fork", NOEXPR);
	return make_integer(pid);
}

cell pp_sys_getcwd(cell x) {
	char	*s;
	cell	n;

	s = getcwd(NULL, 1024);
	n = make_string(s, strlen(s));
	free(s);
	return n;
}

cell pp_sys_getenv(cell x) {
	char	*s;

	s = getenv(string(car(x)));
	if (s == NULL)
		return FALSE;
	return make_string(s, strlen(s));
}

cell pp_sys_getgid(cell x) {
	return make_integer(getgid());
}

cell mkgrent(struct group *gr) {
	cell	n, a;

	n = cons(NIL, NIL);
	save(n);
	assign(car(n), cons(symbol_ref("name"), NIL));
	assign(cdar(n), make_string(gr->gr_name, strlen(gr->gr_name)));
	a = cons(NIL, NIL);
	cdr(n) = a;
	assign(car(a), cons(symbol_ref("gid"), NIL));
	assign(cdar(a), make_integer(gr->gr_gid));
	unsave(1);
	return n;
}

cell pp_sys_getgrnam(cell x) {
	struct group	*gr;

	gr = getgrnam(string(car(x)));
	if (gr == NULL)
		return FALSE;
	return mkgrent(gr);
}

cell pp_sys_getgrgid(cell x) {
	struct group	*gr;

	gr = getgrgid(integer_value("sys:getgrgid", car(x)));
	if (gr == NULL)
		return FALSE;
	return mkgrent(gr);
}

cell pp_sys_getpgid(cell x) {
	/* No prototype, neither on FreeBSD 8.2 nor on Debian Lenny? */
	pid_t	getpgid(pid_t);

	return make_integer(getpgid(0));
}

cell pp_sys_getpid(cell x) {
	return make_integer(getpid());
}

cell pp_sys_getpwent(cell x) {
	struct passwd	*pw;
	cell		n, a, pa;

	setpwent();
	n = cons(NIL, NIL);
	save(n);
	a = n;
	pa = n;
	while (1) {
		pw = getpwent();
		if (pw == NULL)
			break;
		pa = a;
		assign(car(a), make_string(pw->pw_name, strlen(pw->pw_name)));
		if (pw != NULL) {
			assign(cdr(a), cons(NIL, NIL));
			a = cdr(a);
		}
	}
	cdr(pa) = NIL;
	endpwent();
	unsave(1);
	return n;
}

cell mkpwent(struct passwd *pw) {
	cell	n, a;

	n = cons(NIL, NIL);
	save(n);
	assign(car(n), cons(symbol_ref("name"), NIL));
	assign(cdar(n), make_string(pw->pw_name, strlen(pw->pw_name)));
	a = cons(NIL, NIL);
	cdr(n) = a;
	assign(car(a), cons(symbol_ref("uid"), NIL));
	assign(cdar(a), make_integer(pw->pw_uid));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("gid"), NIL));
	assign(cdar(a), make_integer(pw->pw_gid));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("gecos"), NIL));
	assign(cdar(a), make_string(pw->pw_gecos, strlen(pw->pw_gecos)));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("home"), NIL));
	assign(cdar(a), make_string(pw->pw_dir, strlen(pw->pw_dir)));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("shell"), NIL));
	assign(cdar(a), make_string(pw->pw_shell, strlen(pw->pw_shell)));
	unsave(1);
	return n;
}

cell pp_sys_getpwnam(cell x) {
	struct passwd	*pw;

	pw = getpwnam(string(car(x)));
	if (pw == NULL)
		return FALSE;
	return mkpwent(pw);
}

cell pp_sys_getpwuid(cell x) {
	struct passwd	*pw;

	pw = getpwuid(integer_value("sys:getpwuid", car(x)));
	if (pw == NULL)
		return FALSE;
	return mkpwent(pw);
}

cell pp_sys_getuid(cell x) {
	return make_integer(getuid());
}

cell pp_sys_kill(cell x) {
	char	name[] = "sys:kill";
	int	sig = integer_value(name, cadr(x));
	int	r;

	r = kill(integer_value(name, car(x)), sig);
	if (r < 0)
		return sys_error("sys:kill", x);
	return sys_ok();
}

cell pp_sys_link(cell x) {
	if (link(string(car(x)), string(cadr(x))) < 0)
		return sys_error("sys:link", x);
	return sys_ok();
}

cell pp_sys_lock(cell x) {
	char	p[256], *s;

	s = string(car(x));
	if (strlen(s) > 248)
		return error("sys:lock: path too long", car(x));
	sprintf(p, "%s.lock", s);
	return (mkdir(p, 0700) < 0)? FALSE: TRUE;
}

cell pp_sys_lseek(cell x) {
	char	name[] = "sys:lseek";
	long	r;

	r = lseek(integer_value(name, car(x)),
		integer_value(name, cadr(x)),
		integer_value(name, caddr(x)));
	if (r < 0L)
		return sys_error("sys:lseek", x);
	return make_integer(r);
}

cell pp_sys_get_magic_value(cell x) {
	char	*s = string(car(x));

	if (!strcmp(s, "F_OK")) return make_integer(F_OK);
	if (!strcmp(s, "X_OK")) return make_integer(X_OK);
	if (!strcmp(s, "W_OK")) return make_integer(W_OK);
	if (!strcmp(s, "R_OK")) return make_integer(R_OK);
	if (!strcmp(s, "O_RDONLY")) return make_integer(O_RDONLY);
	if (!strcmp(s, "O_WRONLY")) return make_integer(O_WRONLY);
	if (!strcmp(s, "O_RDWR")) return make_integer(O_RDWR);
	if (!strcmp(s, "SEEK_SET")) return make_integer(SEEK_SET);
	if (!strcmp(s, "SEEK_CUR")) return make_integer(SEEK_CUR);
	if (!strcmp(s, "SEEK_END")) return make_integer(SEEK_END);
	if (!strcmp(s, "SIGHUP")) return make_integer(SIGHUP);
	if (!strcmp(s, "SIGINT")) return make_integer(SIGINT);
	if (!strcmp(s, "SIGQUIT")) return make_integer(SIGQUIT);
	if (!strcmp(s, "SIGILL")) return make_integer(SIGILL);
	if (!strcmp(s, "SIGTRAP")) return make_integer(SIGTRAP);
	if (!strcmp(s, "SIGABRT")) return make_integer(SIGABRT);
	if (!strcmp(s, "SIGEMT")) return make_integer(SIGEMT);
	if (!strcmp(s, "SIGFPE")) return make_integer(SIGFPE);
	if (!strcmp(s, "SIGKILL")) return make_integer(SIGKILL);
	if (!strcmp(s, "SIGBUS")) return make_integer(SIGBUS);
	if (!strcmp(s, "SIGSEGV")) return make_integer(SIGSEGV);
	if (!strcmp(s, "SIGSYS")) return make_integer(SIGSYS);
	if (!strcmp(s, "SIGPIPE")) return make_integer(SIGPIPE);
	if (!strcmp(s, "SIGALRM")) return make_integer(SIGALRM);
	if (!strcmp(s, "SIGTERM")) return make_integer(SIGTERM);
	if (!strcmp(s, "S_ISUID")) return make_integer(S_ISUID);
	if (!strcmp(s, "S_ISGID")) return make_integer(S_ISGID);
	if (!strcmp(s, "S_ISVTX")) return make_integer(S_ISVTX);
	if (!strcmp(s, "S_IRUSR")) return make_integer(S_IRUSR);
	if (!strcmp(s, "S_IRWXU")) return make_integer(S_IRWXU);
	if (!strcmp(s, "S_IWUSR")) return make_integer(S_IWUSR);
	if (!strcmp(s, "S_IXUSR")) return make_integer(S_IXUSR);
	if (!strcmp(s, "S_IRWXG")) return make_integer(S_IRWXG);
	if (!strcmp(s, "S_IRGRP")) return make_integer(S_IRGRP);
	if (!strcmp(s, "S_IWGRP")) return make_integer(S_IWGRP);
	if (!strcmp(s, "S_IXGRP")) return make_integer(S_IXGRP);
	if (!strcmp(s, "S_IRWXO")) return make_integer(S_IRWXO);
	if (!strcmp(s, "S_IROTH")) return make_integer(S_IROTH);
	if (!strcmp(s, "S_IWOTH")) return make_integer(S_IWOTH);
	if (!strcmp(s, "S_IXOTH")) return make_integer(S_IXOTH);
	else return error("sys:get-magic-value: requested value not found",
			car(x));
}

cell pp_sys_make_input_port(cell x) {
	int	in = new_port();

	if (in < 0)
		return error("sys:make-input-port: out of ports", NOEXPR);
	Ports[in] = fdopen(integer_value("sys:make-input-port", car(x)),
				"r");
	return make_port(in, T_INPUT_PORT);
}

cell pp_sys_make_output_port(cell x) {
	int	out = new_port();

	if (out < 0)
		return error("sys:make-output-port: out of ports", NOEXPR);
	Ports[out] = fdopen(integer_value("sys:make-output-port", car(x)),
				"w");
	return make_port(out, T_OUTPUT_PORT);
}

cell pp_sys_mkdir(cell x) {
	if (mkdir(string(car(x)), integer_value("sys:mkdir", cadr(x))) < 0)
		return sys_error("sys:mkdir", x);
	return sys_ok();
}

cell pp_sys_open(cell x) {
	int	fd;

	fd = open(string(car(x)), integer_value("sys:open", cadr(x)));
	if (fd < 0)
		return sys_error("sys:open", x);
	return make_integer(fd);
}

cell pp_sys_pipe(cell x) {
	int	fd[2];
	cell	n;

	if (pipe(fd) < 0)
		return sys_error("sys:pipe", NOEXPR);
	n = cons(make_integer(fd[1]), NIL);
	save(n);
	n = cons(make_integer(fd[0]), n);
	unsave(1);
	return n;
}

cell pp_sys_read(cell x) {
	cell	buf, buf2;
	int	r, k;
	char	name[] = "sys:read";

	k = integer_value(name, cadr(x));
	buf = make_string("", k);
	r = read(integer_value(name, car(x)), string(buf), k);
	if (r < 0)
		return sys_error("sys:read", x);
	if (r < k) {
		save(buf);
		buf2 = make_string("", r);
		unsave(1);
		strcpy(string(buf2), string(buf));
		buf = buf2;
	}
	return buf;
}

cell pp_sys_readdir(cell x) {
	DIR		*dir;
	struct dirent	*dp;
	cell		n, a, pa;

	dir = opendir(string(car(x)));
	if (dir == NULL)
		return sys_error("sys:readdir", x);
	n = cons(NIL, NIL);
	save(n);
	a = n;
	pa = n;
	while (1) {
		dp = readdir(dir);
		if (dp == NULL)
			break;
		if (	!strcmp(dp->d_name, ".") ||
			!strcmp(dp->d_name, "..")
		)
			continue;
		pa = a;
		assign(car(a), make_string(dp->d_name, strlen(dp->d_name)));
		assign(cdr(a), cons(NIL, NIL));
		a = cdr(a);
	}
	cdr(pa) = NIL;
	if (car(n) == NIL)
		n = NIL;
	closedir(dir);
	unsave(1);
	return n;
}

cell pp_sys_readlink(cell x) {
	char	buf[MAXPATHLEN+1];
	int	k;

	k = readlink(string(car(x)), buf, MAXPATHLEN);
	if (k < 0)
		return sys_error("sys:readlink", x);
	buf[k] = 0;
	return make_string(buf, k);
}

cell pp_sys_rename(cell x) {
	int	r;

	r = rename(string(car(x)), string(cadr(x)));
	if (r < 0)
		return sys_error("sys:rename", x);
	return sys_ok();
}

cell pp_sys_rmdir(cell x) {
	if (rmdir(string(car(x))) < 0)
		return sys_error("sys:rmdir", x);
	return sys_ok();
}

cell pp_sys_select(cell x) {
	cell		p;
	struct timeval	tv, *tvp;
	fd_set		rset, wset;
	char		name[] = "sys:select";
	int		r, k, nfd;
	char		msg[] = "sys:select: expected list of integer, got";

	if (	car(x) != NIL &&
		(!integer_p(caar(x)) ||
		 cdar(x) == NIL ||
		 !integer_p(cadar(x)) ||
		 cddar(x) != NIL)
	) {
		return error(msg, car(x));
	}
	FD_ZERO(&rset);
	nfd = 0;
	for (p = cadr(x); p != NIL; p = cdr(p)) {
		if (!pair_p(p))
			return error("sys:select: improper list", cadr(x));
		if (!integer_p(car(p)))
			return error(msg, cadr(x));
		k = integer_value(name, car(p));
		FD_SET(k, &rset);
		if (k > nfd) nfd = k;
	}
	FD_ZERO(&wset);
	for (p = caddr(x); p != NIL; p = cdr(p)) {
		if (!pair_p(p))
			return error("sys:select: improper list", caddr(x));
		if (!integer_p(car(p)))
			return error(msg, caddr(x));
		k = integer_value(name, car(p));
		FD_SET(k, &wset);
		if (k > nfd) nfd = k;
	}
	nfd++;
	if (car(x) == NIL) {
		tvp = NULL;
	}
	else {
		tv.tv_sec = integer_value(name, caar(x));
		tv.tv_usec = integer_value(name, cadar(x));
		tvp = &tv;
	}
	r = select(nfd, &rset, &wset, NULL, tvp);
	if (r < 0)
		return sys_error(name, x);
	return r==0? FALSE: make_integer(r);
}

cell pp_sys_setgid(cell x) {
	if (setgid(integer_value("sys:setgid", car(x))) < 0)
		return sys_error("sys:setgid", x);
	return sys_ok();
}

cell pp_sys_setpgid(cell x) {
	int	r;

	r = setpgid(0, integer_value("sys:setpgid", car(x)));
	if (r < 0)
		return sys_error("sys:setpgid", x);
	return sys_ok();
}

cell pp_sys_setuid(cell x) {
	if (setuid(integer_value("sys:setgid", car(x))) < 0)
		return sys_error("sys:setuid", x);
	return sys_ok();
}

cell pp_sys_sleep(cell x) {
	if (sleep(integer_value("sys:sleep", car(x))))
		return sys_error("sys:sleep", x);
	return sys_ok();
}

cell sys_stat(int follow, cell x) {
	struct stat	sb;
	cell		n, a;

	if ((follow? stat: lstat)(string(car(x)), &sb) < 0)
		return sys_error(NULL, NOEXPR);
	n = cons(NIL, NIL);
	save(n);
	assign(car(n), cons(symbol_ref("name"), car(x)));
	a = cons(NIL, NIL);
	cdr(n) = a;
	assign(car(a), cons(symbol_ref("size"), NIL));
	assign(cdar(a), make_integer(sb.st_size));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("uid"), NIL));
	assign(cdar(a), make_integer(sb.st_uid));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("gid"), NIL));
	assign(cdar(a), make_integer(sb.st_gid));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("mode"), NIL));
	assign(cdar(a), make_integer(sb.st_mode));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("ctime"), NIL));
	assign(cdar(a), make_integer(sb.st_ctime));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("atime"), NIL));
	assign(cdar(a), make_integer(sb.st_atime));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("mtime"), NIL));
	assign(cdar(a), make_integer(sb.st_mtime));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("dev"), NIL));
	assign(cdar(a), make_integer(sb.st_dev));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("ino"), NIL));
	assign(cdar(a), make_integer(sb.st_ino));
	assign(cdr(a), cons(NIL, NIL));
	a = cdr(a);
	assign(car(a), cons(symbol_ref("nlink"), NIL));
	assign(cdar(a), make_integer(sb.st_nlink));
	unsave(1);
	return n;
}

cell pp_sys_stat(cell x) {
	return sys_stat(1, x);
}

cell pp_sys_lstat(cell x) {
	return sys_stat(0, x);
}

int stat_mode(char *who, int follow, cell x) {
	struct stat	st;

	if ((follow? stat: lstat)(string(car(x)), &st) < 0)
		return sys_error(who, x);
	return st.st_mode;
}

cell pp_sys_stat_block_dev_p(cell x) {
	return S_ISBLK(stat_mode("stat-block-dev?", 1, x))? TRUE: FALSE;
}

cell pp_sys_stat_char_dev_p(cell x) {
	return S_ISCHR(stat_mode("stat-char-dev?", 1, x))? TRUE: FALSE;
}

cell pp_sys_stat_directory_p(cell x) {
	return S_ISDIR(stat_mode("stat-directory?", 1, x))? TRUE: FALSE;
}

cell pp_sys_stat_pipe_p(cell x) {
	return S_ISFIFO(stat_mode("stat-pipe?", 1, x))? TRUE: FALSE;
}

cell pp_sys_stat_regular_p(cell x) {
	return S_ISREG(stat_mode("stat-regular?", 1, x))? TRUE: FALSE;
}

cell pp_sys_stat_socket_p(cell x) {
	return S_ISSOCK(stat_mode("stat-socket?", 1, x))? TRUE: FALSE;
}

cell pp_sys_lstat_block_dev_p(cell x) {
	return S_ISBLK(stat_mode("lstat-block-dev?", 0, x))? TRUE: FALSE;
}

cell pp_sys_lstat_char_dev_p(cell x) {
	return S_ISCHR(stat_mode("lstat-char-dev?", 0, x))? TRUE: FALSE;
}

cell pp_sys_lstat_directory_p(cell x) {
	return S_ISDIR(stat_mode("lstat-directory?", 0, x))? TRUE: FALSE;
}

cell pp_sys_lstat_pipe_p(cell x) {
	return S_ISFIFO(stat_mode("lstat-pipe?", 0, x))? TRUE: FALSE;
}

cell pp_sys_lstat_regular_p(cell x) {
	return S_ISREG(stat_mode("lstat-regular?", 0, x))? TRUE: FALSE;
}

cell pp_sys_lstat_socket_p(cell x) {
	return S_ISSOCK(stat_mode("lstat-socket?", 0, x))? TRUE: FALSE;
}

cell pp_sys_lstat_symlink_p(cell x) {
	return S_ISLNK(stat_mode("lstat-symlink?", 0, x))? TRUE: FALSE;
}

cell pp_sys_strerror(cell x) {
	char	*s = strerror(integer_value("sys:strerror", car(x)));

	return make_string(s, strlen(s));
}

cell pp_sys_symlink(cell x) {
	if (symlink(string(car(x)), string(cadr(x))) < 0)
		return sys_error("sys:symlink", x);
	return sys_ok();
}

cell pp_sys_system(cell x) {
	int	r;

	r = system(string(car(x)));
	if (r < 0 || r > 127)
		return sys_error("sys:system", x);
	return make_integer(r);
}

cell pp_sys_gettimeofday(cell x) {
	struct timeval	t;
	cell		n, m;

	gettimeofday(&t, NULL);
	n = make_integer(t.tv_usec);
	n = cons(n, NIL);
	save(n);
	m = make_integer(t.tv_sec);
	n = cons(m, n);
	unsave(1);
	return n;
}

cell pp_sys_umask(cell x) {
	int	r;

	if (x == NIL)
		umask(r = umask(0));
	else
		r = umask(integer_value("sys:umask", car(x)));
	return make_integer(r);
}

cell pp_sys_unlink(cell x) {
	if (unlink(string(car(x))) < 0)
		return sys_error("sys:unlink", x);
	return sys_ok();
}

cell pp_sys_unlock(cell x) {
	char	p[256], *s;

	s = string(car(x));
	if (strlen(s) > 248)
		return error("sys:unlock: path too long", car(x));
	sprintf(p, "%s.lock", s);
	rmdir(p);
	return sys_ok();
}

cell pp_sys_usleep(cell x) {
#if __FreeBSD__ == 7
	int usleep(useconds_t microseconds);
#endif
	if (usleep(integer_value("sys:usleep", car(x))))
		return sys_error("sys:usleep", x);
	return sys_ok();
}

cell pp_sys_utimes(cell x) {
	if (utimes(string(car(x)), NULL) < 0)
		return sys_error("sys:utimes", x);
	return sys_ok();
}

cell pp_sys_wait(cell x) {
	int	r, status;
	cell	n;

	r = wait(&status);
	if (r < 0)
		return sys_error("sys:wait", NOEXPR);
	n = cons(make_integer(r), NIL);
	save(n);
	n = cons(make_integer(WEXITSTATUS(status)), n);
	unsave(1);
	return n;
}

cell pp_sys_waitpid(cell x) {
	int	r, status;
	char	name[] = "sys:waitpid";

	r = waitpid(integer_value(name, car(x)), &status, WNOHANG);
	if (r < 0)
		return sys_error(name, NOEXPR);
	return r == 0? FALSE: make_integer(WEXITSTATUS(status));
}

cell pp_sys_write(cell x) {
	int	r;

	r = write(integer_value("sys:write", car(x)), string(cadr(x)),
		string_len(cadr(x))-1);
	if (r < 0)
		return sys_error("sys:write", x);
	return make_integer(r);
}

#ifdef NETWORK

cell pp_sys_inet_accept(cell x) {
	int	r;

	r = accept(integer_value("sys:inet-accept", car(x)), NULL, 0);
	if (r < 0)
		return sys_error("sys:inet-accept", x);
	return make_integer(r);
}

cell pp_sys_inet_connect(cell x) {
	struct addrinfo	hints, *res, *rp;
	int 		s;
	int		r;

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	r = getaddrinfo(string(car(x)), string(cadr(x)), &hints, &res);
	if (r != 0)
		return sys_error("sys:inet-connect/getaddrinfo", x);
	s = -1;
	for (rp = res; s < 0 && rp; rp = rp->ai_next) {
		s = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
		if (s < 0)
                           continue;
		if (connect(s, rp->ai_addr, rp->ai_addrlen) < 0) {
			close(s);
			s = -1;
			continue;
		}
	}
	if (s < 0)
		return sys_error("sys:inet-connect", x);
	freeaddrinfo(res);
	return make_integer(s);
}

cell pp_sys_inet_getpeername(cell x) {
	socklen_t		len;
	struct sockaddr_storage addr;
	char			ip[128];
	int			port, fd;
	cell			n, m;

	fd = integer_value("sys:inet-getpeername", car(x));
	len = sizeof addr;
	if (getpeername(fd, (struct sockaddr *) &addr, &len) < 0)
		return sys_error(NULL, NOEXPR);
	if (addr.ss_family == AF_INET6) {
		struct sockaddr_in6 *s = (struct sockaddr_in6 *) &addr;
		port = ntohs(s->sin6_port);
		inet_ntop(AF_INET6, &s->sin6_addr, ip, sizeof ip);
	}
	else {
		struct sockaddr_in *s = (struct sockaddr_in *) &addr;
		port = ntohs(s->sin_port);
		inet_ntop(AF_INET, &s->sin_addr, ip, sizeof ip);
	}
	n = cons(make_integer(port), NIL);
	save(n);
	m = make_string(ip, strlen(ip));
	n = cons(m, n);
	unsave(1);
	return n;
}

cell pp_sys_inet_listen(cell x) {
	struct addrinfo	hints, *res, *rp;
	int		s;
	int		r, maxq;
	char		*host;
	struct utsname	u;

	maxq = integer_value("sys:inet-listen", caddr(x));
	if (string_p(car(x))) {
		host = string(car(x));
	}
	else if (car(x) == TRUE) {
		r = uname(&u);
		if (r < 0)
			return sys_error("sys:inet-listen/uname", x);
		host = u.nodename;
	}
	else {
		return error("sys:inet-listen: expected string or #t, got",
				car(x));
	}
	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags = AI_PASSIVE;
	r = getaddrinfo(host, string(cadr(x)), &hints, &res);
	if (r != 0)
		return sys_error("sys:inet-listen/getaddrinfo", x);
	s = -1;
	for (rp = res; s < 0 && rp; rp = rp->ai_next) {
		s = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
		if (s < 0)
			continue;
		if (bind(s, rp->ai_addr, rp->ai_addrlen) < 0) {
			close(s);
			s = -1;
			continue;
		}
		listen(s, maxq);
	}
	if (s < 0)
		return sys_error("sys:inet-listen", x);
	freeaddrinfo(res);
	return make_integer(s);
}

#endif /* NETWORK */

PRIM Unix_primitives[] = {
 { "sys:access",           pp_sys_access,           2,  2, { STR,INT,___ } },
 { "sys:catch-errors",     pp_sys_catch_errors,     1,  1, { BOL,___,___ } },
 { "sys:chdir",            pp_sys_chdir,            1,  1, { STR,___,___ } },
 { "sys:close",            pp_sys_close,            1,  1, { INT,___,___ } },
 { "sys:chmod",            pp_sys_chmod,            2,  2, { STR,INT,___ } },
 { "sys:chown",            pp_sys_chown,            3,  3, { STR,INT,INT } },
 { "sys:command-line",     pp_sys_command_line,     0,  0, { ___,___,___ } },
 { "sys:creat",            pp_sys_creat,            2,  2, { STR,INT,___ } },
 { "sys:dup",              pp_sys_dup,              1,  1, { INT,___,___ } },
 { "sys:dup2",             pp_sys_dup2,             2,  2, { INT,INT,___ } },
 { "sys:errno",            pp_sys_errno,            0,  0, { ___,___,___ } },
 { "sys:execv",            pp_sys_execv,            2,  2, { STR,LST,___ } },
 { "sys:exit",             pp_sys_exit,             1,  1, { INT,___,___ } },
 { "sys:fileno",           pp_sys_fileno,           1,  1, { ___,___,___ } },
 { "sys:flush",            pp_sys_flush,            1,  1, { OUP,___,___ } },
 { "sys:fork",             pp_sys_fork,             0,  0, { ___,___,___ } },
 { "sys:getcwd",           pp_sys_getcwd,           0,  0, { ___,___,___ } },
 { "sys:getenv",           pp_sys_getenv,           1,  1, { STR,___,___ } },
 { "sys:getgid",           pp_sys_getgid,           0,  0, { ___,___,___ } },
 { "sys:getgrnam",         pp_sys_getgrnam,         1,  1, { STR,___,___ } },
 { "sys:getgrgid",         pp_sys_getgrgid,         1,  1, { INT,___,___ } },
 { "sys:getpgid",          pp_sys_getpgid,          0,  0, { ___,___,___ } },
 { "sys:getpid",           pp_sys_getpid,           0,  0, { ___,___,___ } },
 { "sys:getpwent",         pp_sys_getpwent,         0,  0, { ___,___,___ } },
 { "sys:getpwnam",         pp_sys_getpwnam,         1,  1, { STR,___,___ } },
 { "sys:getpwuid",         pp_sys_getpwuid,         1,  1, { INT,___,___ } },
 { "sys:gettimeofday",     pp_sys_gettimeofday,     0,  0, { ___,___,___ } },
 { "sys:getuid",           pp_sys_getuid,           0,  0, { ___,___,___ } },
 { "sys:kill",             pp_sys_kill,             2,  2, { INT,INT,___ } },
 { "sys:link",             pp_sys_link,             2,  2, { STR,STR,___ } },
 { "sys:lock",             pp_sys_lock,             1,  1, { STR,___,___ } },
 { "sys:lseek",            pp_sys_lseek,            3,  3, { INT,INT,INT } },
 { "sys:lstat",            pp_sys_lstat,            1,  1, { STR,___,___ } },
 { "sys:lstat-block-dev?", pp_sys_lstat_block_dev_p,1,  1, { STR,___,___ } },
 { "sys:lstat-char-dev?",  pp_sys_lstat_char_dev_p, 1,  1, { STR,___,___ } },
 { "sys:lstat-directory?", pp_sys_lstat_directory_p,1,  1, { STR,___,___ } },
 { "sys:lstat-pipe?",      pp_sys_lstat_pipe_p,     1,  1, { STR,___,___ } },
 { "sys:lstat-regular?",   pp_sys_lstat_regular_p,  1,  1, { STR,___,___ } },
 { "sys:lstat-socket?",    pp_sys_lstat_socket_p,   1,  1, { STR,___,___ } },
 { "sys:lstat-symlink?",   pp_sys_lstat_symlink_p,  1,  1, { STR,___,___ } },
 { "sys:get-magic-value",  pp_sys_get_magic_value,  1,  1, { STR,___,___ } },
 { "sys:make-input-port",  pp_sys_make_input_port,  1,  1, { INT,___,___ } },
 { "sys:make-output-port", pp_sys_make_output_port, 1,  1, { INT,___,___ } },
 { "sys:mkdir",            pp_sys_mkdir,            2,  2, { STR,INT,___ } },
 { "sys:open",             pp_sys_open,             2,  2, { STR,INT,___ } },
 { "sys:pipe",             pp_sys_pipe,             0,  0, { ___,___,___ } },
 { "sys:read",             pp_sys_read,             2,  2, { INT,INT,___ } },
 { "sys:readdir",          pp_sys_readdir,          1,  1, { STR,___,___ } },
 { "sys:readlink",         pp_sys_readlink,         1,  1, { STR,___,___ } },
 { "sys:rename",           pp_sys_rename,           2,  2, { STR,STR,___ } },
 { "sys:rmdir",            pp_sys_rmdir,            1,  1, { STR,___,___ } },
 { "sys:setgid",           pp_sys_setgid,           1,  1, { INT,___,___ } },
 { "sys:select",           pp_sys_select,           3,  3, { LST,LST,LST } },
 { "sys:setpgid",          pp_sys_setpgid,          1,  1, { INT,___,___ } },
 { "sys:setuid",           pp_sys_setuid,           1,  1, { INT,___,___ } },
 { "sys:sleep",            pp_sys_sleep,            1,  1, { INT,___,___ } },
 { "sys:stat",             pp_sys_stat,             1,  1, { STR,___,___ } },
 { "sys:stat-block-dev?",  pp_sys_stat_block_dev_p, 1,  1, { STR,___,___ } },
 { "sys:stat-char-dev?",   pp_sys_stat_char_dev_p,  1,  1, { STR,___,___ } },
 { "sys:stat-directory?",  pp_sys_stat_directory_p, 1,  1, { STR,___,___ } },
 { "sys:stat-pipe?",       pp_sys_stat_pipe_p,      1,  1, { STR,___,___ } },
 { "sys:stat-regular?",    pp_sys_stat_regular_p,   1,  1, { STR,___,___ } },
 { "sys:stat-socket?",     pp_sys_stat_socket_p,    1,  1, { STR,___,___ } },
 { "sys:strerror",         pp_sys_strerror,         1,  1, { INT,___,___ } },
 { "sys:symlink",          pp_sys_symlink,          2,  2, { STR,STR,___ } },
 { "sys:system",           pp_sys_system,           1,  1, { STR,___,___ } },
 { "sys:umask",            pp_sys_umask,            0,  1, { INT,___,___ } },
 { "sys:unlink",           pp_sys_unlink,           1,  1, { STR,___,___ } },
 { "sys:unlock",           pp_sys_unlock,           1,  1, { STR,___,___ } },
 { "sys:usleep",           pp_sys_usleep,           1,  1, { INT,___,___ } },
 { "sys:utimes",           pp_sys_utimes,           1,  1, { STR,___,___ } },
 { "sys:wait",             pp_sys_wait,             0,  0, { ___,___,___ } },
 { "sys:waitpid",          pp_sys_waitpid,          1,  1, { INT,___,___ } },
 { "sys:write",            pp_sys_write,            2,  2, { INT,STR,___ } },
#ifdef NETWORK
 { "sys:inet-accept",      pp_sys_inet_accept,      1,  1, { INT,___,___ } },
 { "sys:inet-connect",     pp_sys_inet_connect,     2,  2, { STR,STR,___ } },
 { "sys:inet-getpeername", pp_sys_inet_getpeername, 1,  1, { INT,___,___ } },
 { "sys:inet-listen",      pp_sys_inet_listen,      3,  3, { ___,STR,INT } },
#endif /* NETWORK */
 { NULL }
};

void sys_init(void) {
	signal(SIGPIPE, SIG_IGN);
	add_primitives("sys-unix", Unix_primitives);
#ifdef NETWORK
	add_primitives("network", NULL);
#endif /* NETWORK */
}
