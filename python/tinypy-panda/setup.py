import os
import sys

VARS = {'$CPYTHON':''}
TOPDIR = os.path.abspath(os.path.dirname(__file__))
TEST = False
CLEAN = False
BOOT = False
DEBUG = False
VALGRIND = False
SANDBOX = False
CORE = ['tokenize','parse','encode','py2bc']
MODULES = []

def main():
    chksize()
    if len(sys.argv) < 2:
        print HELP
        return
    
    global TEST,CLEAN,BOOT,DEBUG,VALGRIND,SANDBOX
    TEST = 'test' in sys.argv
    CLEAN = 'clean' in sys.argv
    BOOT = 'boot' in sys.argv
    DEBUG = 'debug' in sys.argv
    VALGRIND = 'valgrind' in sys.argv
    SANDBOX = 'sandbox' in sys.argv
    CLEAN = CLEAN or BOOT
    TEST = TEST or BOOT
        
    get_libs()
    build_mymain()

    build = None
    
    if "linux" in sys.platform:
        vars_linux()
        build = build_gcc
    elif "darwin" in sys.platform:
        vars_osx()
        build = build_gcc
    elif "win" in sys.platform:
        build = build_vs

    #full list of compilers in distutils.ccompiler.show_compilers()
    if "-cunix" in sys.argv:
        build = build_gcc
    elif '-cmsvc' in sys.argv:
        build = build_vs
    elif '-cmingw32' in sys.argv:
        vars_windows()
        build = build_gcc

    if build == None:
        print "couldn't detect OS or incorrect compiler command. defaulting to GCC."
        build = build_gcc
    
    cmd = sys.argv[1]
    if cmd == "tinypy":
        build()
    elif cmd == '64k':
        build_64k()
    elif cmd == 'blob':
        build_blob()
    elif cmd == "build":
        build_blob()
        build_cpython()
    elif cmd == "install":
        install_cpython()
    else:
        print 'invalid command'

HELP = """
python setup.py command [options] [modules]

Commands:
    tinypy - build a vanilla tinypy interpreter binary
    64k - generate a 64k version of the tinypy source
    blob - generate a single tinypy.c and tinypy.h

    build - build CPython module
    install - install CPython module

Options:
    test - run tests during build
    clean - rebuild all .tpc during build
    boot - fully bootstrap and test tinypy
    debug - build with debug options on
    valgrind - run tests through valgrind
    -cCOMPILER - build for a specific platform (-cmingw32, -clinux, -cmsvc, -cunix)
    sandbox - enable sandbox

Modules:
    math - build math module
    random - build random module *
    pygame - build pygame module **
    marshal - build marshal module ***
    jit - build jit module ***
    re - build re module ***
    ??? - build other modules in the modules folder

* coming soon!!
** proof-of-concept included
*** vaporware
"""

def vars_osx():
    VARS['$RM'] = 'rm -f'
    VARS['$VM'] = './vm'
    VARS['$TINYPY'] = './tinypy'
    VARS['$SYS'] = '-osx'
    VARS['$FLAGS'] = ''
    
    VARS['$WFLAGS'] = '-std=c89 -Wall'
    
    if 'pygame' in MODULES:
        VARS['$FLAGS'] += ' `sdl-config --cflags --libs` '

def vars_linux():
    VARS['$RM'] = 'rm -f'
    VARS['$VM'] = './vm'
    VARS['$TINYPY'] = './tinypy'
    VARS['$SYS'] = '-linux'
    VARS['$FLAGS'] = ''
    
    VARS['$WFLAGS'] = '-std=c89 -Wall -Wc++-compat'
    #-Wwrite-strings - i think this is included in -Wc++-compat
    
    if 'pygame' in MODULES:
        VARS['$FLAGS'] += ' `sdl-config --cflags --libs` '

    if SANDBOX:
        VARS['$SYS'] += " -sandbox "
        VARS['$FLAGS'] += " -DTP_SANDBOX "

def vars_windows():
    VARS['$RM'] = 'del'
    VARS['$VM'] = 'vm'
    VARS['$TINYPY'] = 'tinypy'
    VARS['$FLAGS'] = '-lmingw32'
    VARS['$WFLAGS'] = '-Wwrite-strings -Wall'
    VARS['$SYS'] = '-mingw32'
    VARS['$CPYTHON'] = "-c mingw32"

    if 'pygame' in MODULES:
        VARS['$FLAGS'] += ' -Ic:\\mingw\\include\\SDL -lSDLmain -lSDL '

def do_cmd(cmd):
    for k,v in VARS.items():
        cmd = cmd.replace(k,v)
    if '$' in cmd:
        print 'vars_error',cmd
        sys.exit(-1)
    if VALGRIND and (cmd.startswith("./") or cmd.startswith("../")):
        cmd = "valgrind " + cmd
    
    print cmd
    r = os.system(cmd)
    if r:
        print 'exit_status',r
        sys.exit(r)
        
def do_chdir(dest):
    print 'cd',dest
    os.chdir(dest)

def build_bc(opt=False):
    out = []
    for mod in CORE:
        out.append("""unsigned char tp_%s[] = {"""%mod)
        fname = mod+".tpc"
        data = open(fname,'rb').read()
        cols = 16
        for n in xrange(0,len(data),cols):
            out.append(",".join([str(ord(v)) for v in data[n:n+cols]])+',')
        out.append("""};""")
    out.append("")
    f = open('bc.c','wb')
    f.write('\n'.join(out))
    f.close()
    
def open_tinypy(fname,*args):
    return open(os.path.join(TOPDIR,'tinypy',fname),*args)
    
def build_blob():
    mods = CORE[:]
    do_chdir(os.path.join(TOPDIR,'tinypy'))
    for mod in mods: py2bc('python py2bc.py $SRC $DEST',mod)
    do_chdir(os.path.join(TOPDIR))
    
    out = []
    out.append("/*")
    out.extend([v.rstrip() for v in open(os.path.join(TOPDIR,'LICENSE.txt'),'r')])
    out.append("*/")
    out.append("")
    
    out.append("#ifndef TINYPY_H")
    out.append("#define TINYPY_H")
    out.extend([v.rstrip() for v in open_tinypy('tp.h','r')])
    for fname in ['list.c','dict.c','misc.c','string.c','builtins.c',
        'gc.c','ops.c','vm.c','tp.c', 'sandbox.c']:
        for line in open_tinypy(fname,'r'):
            line = line.rstrip()
            if not len(line): continue
            if line[0] == '/': continue
            if line[0] == ' ': continue
            if line[0] == '\t': continue
            if line[-1] != '{': continue
            if 'enum' in line: continue
            if '=' in line: continue
            if '#' in line: continue
            line = line.replace('{',';') 
            
            # Do not include prototypes already defined earlier, or gcc will
            # warn about doubled prototypes in user code.
            if '(' in line:
                line2 = line[:line.find('(') + 1]
                got_already = False
                for already in out:
                    if already.startswith(line2):
                        got_already = True
                        break
                if got_already: continue
            out.append(line)
    out.append("#endif")
    out.append('')
    dest = os.path.join(TOPDIR,'build','tinypy.h')
    print 'writing %s'%dest
    f = open(dest,'w')
    f.write('\n'.join(out))
    f.close()
    
    # we leave all the tinypy.h stuff at the top so that
    # if someone wants to include tinypy.c they don't have to have
    # tinypy.h cluttering up their folder
    
    if not os.path.exists(os.path.join(TOPDIR, 'tinypy', 'bc.c')):
        do_chdir(os.path.join(TOPDIR,'tinypy'))
        build_bc()
        do_chdir(os.path.join(TOPDIR))

    for fname in ['list.c','dict.c','misc.c','string.c','builtins.c',
        'gc.c','ops.c','vm.c','bc.c','tp.c','sandbox.c']:
        for line in open_tinypy(fname,'r'):
            line = line.rstrip()
            if line.find('#include "') != -1: continue
            out.append(line)
    out.append('')
    dest = os.path.join(TOPDIR,'build','tinypy.c')
    print 'writing %s'%dest
    f = open(dest,'w')
    f.write('\n'.join(out))
    f.close()
                
def py2bc(cmd,mod):
    src = '%s.py'%mod
    dest = '%s.tpc'%mod
    if CLEAN or not os.path.exists(dest) or os.stat(src).st_mtime > os.stat(dest).st_mtime:
        cmd = cmd.replace('$SRC',src)
        cmd = cmd.replace('$DEST',dest)
        do_cmd(cmd)
    else:
        print '#',dest,'is up to date'

def build_gcc():
    mods = CORE[:]
    do_chdir(os.path.join(TOPDIR,'tinypy'))
    nopos = ' -nopos '
    if DEBUG: nopos = ''
    if TEST:
        mods.append('tests')
        mods.append('asm')
        mods.append('disasm')
        do_cmd("gcc $WFLAGS -g vmmain.c $FLAGS -lm -o vm")
        if BOOT:
            do_cmd('python tests.py $SYS')
        for mod in mods:
            py2bc('python py2bc.py $SRC $DEST',mod)
    else:
        for mod in mods:
            py2bc('python py2bc.py $SRC $DEST'+nopos,mod)
    if BOOT:
        do_cmd('$VM tests.tpc $SYS')
        for mod in mods: py2bc('$VM py2bc.tpc $SRC $DEST',mod)
        build_bc()
        do_cmd("gcc $WFLAGS -g tpmain.c $FLAGS -lm -o tinypy")
    #second pass - builts optimized binaries and stuff
    if BOOT:
        do_cmd('$TINYPY tests.py $SYS')
        for mod in mods: py2bc('$TINYPY py2bc.py $SRC $DEST'+nopos,mod)
    build_bc(True)
    if BOOT:
        do_cmd("gcc $WFLAGS -O2 tpmain.c $FLAGS -lm -o tinypy")
        do_cmd('$TINYPY tests.py $SYS')
        print("# OK - we'll try -O3 for extra speed ...")
        do_cmd("gcc $WFLAGS -O3 tpmain.c $FLAGS -lm -o tinypy")
        do_cmd('$TINYPY tests.py $SYS')
    if DEBUG:
        do_cmd("gcc $WFLAGS -g mymain.c $FLAGS -lm -o ../build/tinypy")
    else:
        do_cmd("gcc $WFLAGS -O3 mymain.c $FLAGS -lm -o ../build/tinypy")
    if TEST:
        do_cmd(os.path.join('..','build','tinypy')+' tests.py $SYS')
        test_mods(os.path.join('..','build','tinypy')+' $TESTS')
    
    do_chdir('..')
    print("# OK")
    
def get_libs():
    modules = os.listdir('modules')
    for m in modules[:]:
        if m not in sys.argv: modules.remove(m)
    global MODULES
    MODULES = modules

def build_mymain():
    src = os.path.join(TOPDIR,'tinypy','tpmain.c')
    out = open(src,'r').read()
    dest = os.path.join(TOPDIR,'tinypy','mymain.c')
        
    vs = []
    for m in MODULES:
        vs.append('#include "../modules/%s/init.c"'%m)
    out = out.replace('/* INCLUDE */','\n'.join(vs))
    
    vs = []
    for m in MODULES:
        vs.append('%s_init(tp);'%m)
    out = out.replace('/* INIT */','\n'.join(vs))
    
    f = open(dest,'w')
    f.write(out)
    f.close()
    return True
    
def test_mods(cmd):
    for m in MODULES:
        tests = os.path.join('..','modules',m,'tests.py')
        if not os.path.exists(tests): continue
        cmd = cmd.replace('$TESTS',tests)
        do_cmd(cmd)

def build_vs():
    # How to compile on windows with Visual Studio:
    # Call the batch script that sets environement variables for Visual Studio and
    # then run this script.
    # For VS 2005 the script is:
    # "C:\Program Files\Microsoft Visual Studio 8\Common7\Tools\vsvars32.bat"
    # For VS 2008: "C:\Program Files\Microsoft Visual Studio 9.0\Common7\Tools\vsvars32.bat"
    # Doesn't compile with vc6 (no variadic macros)
    # Note: /MD option causes to dynamically link with msvcrt80.dll. This dramatically
    # reduces size (for vm.exe 159k => 49k). Downside is that msvcrt80.dll must be
    # present on the system (and not all windows machine have it). You can either re-distribute
    # msvcrt80.dll or statically link with C runtime by changing /MD to /MT.
    mods = CORE[:]; mods.append('tests')
    os.chdir(os.path.join(TOPDIR,'tinypy'))
    do_cmd('cl vmmain.c /Od /Zi /MD /Fdvm.pdb /Fmvm.map /Fevm.exe')
    do_cmd('python tests.py -win')
    for mod in mods: py2bc('python py2bc.py $SRC $DEST',mod)
    do_cmd('vm.exe tests.tpc -win')
    for mod in mods: py2bc('vm.exe py2bc.tpc $SRC $DEST',mod)
    build_bc()
    do_cmd('cl /Od tpmain.c /Zi /MD /Fdtinypy.pdb /Fmtinypy.map /Fetinypy.exe')
    #second pass - builts optimized binaries and stuff
    do_cmd('tinypy.exe tests.py -win')
    for mod in mods: py2bc('tinypy.exe py2bc.py $SRC $DEST'+nopos,mod)
    build_bc(True)
    do_cmd('cl /Os vmmain.c /D "NDEBUG" /Gy /GL /Zi /MD /Fdvm.pdb /Fmvm.map /Fevm.exe /link /opt:ref /opt:icf')
    do_cmd('cl /Os tpmain.c /D "NDEBUG" /Gy /GL /Zi /MD /Fdtinypy.pdb /Fmtinypy.map /Fetinypy.exe /link /opt:ref,icf /OPT:NOWIN98')
    do_cmd("tinypy.exe tests.py -win")
    do_cmd("dir *.exe")

def shrink(fname):
    f = open(fname,'r'); lines = f.readlines(); f.close()
    out = []
    fixes = [
    'vm','gc','params','STR',
    'int','float','return','free','delete','init',
    'abs','round','system','pow','div','raise','hash','index','printf','main']
    passing = False
    for line in lines:
        #quit if we've already converted
        if '\t' in line: return ''.join(lines)
        
        #change "    " into "\t" and remove blank lines
        if len(line.strip()) == 0: continue
        line = line.rstrip()
        l1,l2 = len(line),len(line.lstrip())
        line = "\t"*((l1-l2)/4)+line.lstrip()
        
        #remove comments
        if '.c' in fname or '.h' in fname:
            #start block comment
            if line.strip()[:2] == '/*':
                passing = True;
            #end block comment
            if line.strip()[-2:] == '*/':
               passing = False;
               continue
            #skip lines inside block comments
            if passing:
                continue
        if '.py' in fname:
            if line.strip()[:1] == '#': continue
        
        #remove the "namespace penalty" from tinypy ...
        for name in fixes:
            line = line.replace('TP_'+name,'t'+name)
            line = line.replace('tp_'+name,'t'+name)
        line = line.replace('TP_','')
        line = line.replace('tp_','')
        
        out.append(line)
    return '\n'.join(out)+'\n'
    
def chksize():
    t1,t2 = 0,0
    for fname in [
        'tokenize.py','parse.py','encode.py','py2bc.py',
        'tp.h','list.c','dict.c','misc.c','string.c','builtins.c',
        'gc.c','ops.c','vm.c','tp.c','tpmain.c',
        ]:
        fname = os.path.join(TOPDIR,'tinypy',fname)
        f = open(fname,'r'); t1 += len(f.read()); f.close()
        txt = shrink(fname)
        t2 += len(txt)
    print "#",t1,t2,t2-65536
    return t2

def build_64k():
    for fname in [
        'tokenize.py','parse.py','encode.py','py2bc.py',
        'tp.h','list.c','dict.c','misc.c','string.c','builtins.c',
        'gc.c','ops.c','vm.c','tp.c','tpmain.c',
        ]:
        src = os.path.join(TOPDIR,'tinypy',fname)
        dest = os.path.join(TOPDIR,'build',fname)
        txt = shrink(src)
        f = open(dest,'w')
        f.write(txt)
        f.close()
        print '%s saved to %s'%(src,dest)

def build_cpython():
    try: from distutils.core import setup, Extension
    except: print "cannot import distutils"

    do_chdir(os.path.join(TOPDIR,'cpython'))
    setup(name = "tinypy",
      version = "0.8",
      description = "tinypy module for CPython",
      url = "http://www.tinypy.org/",
      ext_modules = [Extension("tinypy", ["cpython.c"], define_macros = [('CPYTHON_MOD', None)])])
    
def install_cpython():
    try: from distutils.core import setup, Extension
    except: print "cannot import distutils"

    do_chdir(os.path.join(TOPDIR,'cpython'))
    setup(name = "tinypy",
      version = "0.8",
      description = "tinypy module for CPython",
      url = "http://www.tinypy.org/",
      ext_modules = [Extension("tinypy", ["cpython.c"], define_macros = [('CPYTHON_MOD', None)])])
    
if __name__ == '__main__':
    main()
