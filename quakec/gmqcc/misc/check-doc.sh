#!/bin/sh
prog=$0

die() {
	echo "$@"
	exit 1
}

want() {
	test -e "$1" && return
	echo "$prog: missing $1"
	echo "$prog: run this script from the top of a gmqcc source tree"
	exit 1
}

for i in opts.def          \
         doc/gmqcc.1       \
         gmqcc.ini.example
do want "$i"; done

# y/_ABCDEFGHIJKLMNOPQRSTUVWXYZ/-abcdefghijklmnopqrstuvwxyz/;
check_opt() {
	opt_def_name=$1
	arg_char=$2

	for i in $(sed -ne \
	'/^#ifdef GMQCC_TYPE_'${opt_def_name}'$/,/^#endif/{
		/GMQCC_DEFINE_FLAG/{
			s/^.*GMQCC_DEFINE_FLAG(\([^,)]*\)[),].*$/\1/;p;
		}
	}' opts.def)
	do
		opt=$(echo "$i" | tr -- '_A-Z' '-a-z')
		grep -qF -- ".It Fl "${arg_char}" Ns Cm $opt" \
			doc/gmqcc.1 || echo "doc/gmqcc.1: missing: -${arg_char}$opt"
		grep -q -- "[^a-zA-Z_]$i[^a-zA-Z_]" \
			gmqcc.ini.example || echo "gmqcc.ini.example: missing: $i"
	done
}

check_opt FLAGS f
check_opt WARNS W
check_opt OPTIMIZATIONS O

# TODO: linux version
if [ "$(uname -s)" != "Linux" ]; then
    for i in doc/*.1;
    do
        mandoc -Tlint -Wall "$i";
    done
fi
