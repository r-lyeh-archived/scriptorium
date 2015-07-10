Name:           gmqcc
Version:        0.3.6
Release:        2%{?dist}
Summary:        Improved Quake C Compiler
License:        MIT
URL:            http://graphitemaster.github.io/gmqcc/
Source0:        https://github.com/graphitemaster/%{name}/archive/%{version}.tar.gz#/%{name}-%{version}.tar.gz

# tests fail on big endians
ExclusiveArch:  %{ix86} x86_64 %{arm}

%description
Modern written-from-scratch compiler for the QuakeC language with
support for many common features found in other QC compilers.

%package -n qcvm
Summary:        Standalone QuakeC VM binary executor

%description -n qcvm
Executor for QuakeC VM binary files created using a QC compiler such
as gmqcc or fteqcc. It provides a small set of built-in functions, and
by default executes the main function if there is one. Some options
useful for debugging are available as well.

%package -n gmqpak
Summary:        Standalone Quake PAK file utility

%description -n gmqpak
Standalone Quake PAK file utility supporting the extraction of files,
directories, or whole PAKs, as well as the opposite (creation of PAK files).

%prep
%setup -q
echo '#!/bin/sh' > ./configure
chmod +x ./configure 

# and for all for all of those switches they increase the runtime of the compile
# making compiles of code slower

# we don't need compile time buffer protection, we test with clang's address
# sanatizer and valgrind before releases
%global optflags %(echo %{optflags} | sed 's/-D_FORTIFY_SOURCE=2 //')
# there is no exceptions in C
%global optflags %(echo %{optflags} | sed 's/-fexceptions //')
# same with clangs address sanatizer and valgrind testing
%global optflags %(echo %{optflags} | sed 's/-fstack-protector-strong //')
# buffer overflow protection is unrequired since most (if not all) allocations
# happen dynamically and we have our own memory allocator which checks this
# (with valgrind integration), also clang's address santatizer cathes it as
# for grecord-gcc-switches, that just adds pointless information to the binary
# increasing its size
%global optflags %(echo %{optflags} | sed 's/--param=ssp-buffer-size=4 //')

%build
%configure
make %{?_smp_mflags}

%install
%make_install PREFIX=%{_prefix}

%check
make check

%files
%doc LICENSE README AUTHORS CHANGES TODO
%doc gmqcc.ini.example
%{_mandir}/man1/gmqcc.1*
%{_bindir}/gmqcc

%files -n qcvm
%doc LICENSE README AUTHORS CHANGES TODO
%{_mandir}/man1/qcvm.1*
%{_bindir}/qcvm

%files -n gmqpak
%doc LICENSE README AUTHORS CHANGES TODO
%{_mandir}/man1/gmqpak.1*
%{_bindir}/gmqpak

%changelog
* Sat Nov 16 2013 Dan Horák <dan[at]danny.cz> - 0.3.5-2
- fix build on big endian arches
- use the standard wildcarded filename for man pages
- and make it Exclusive for little endians because tests fail on big endians

* Thu Nov 14 2013 Igor Gnatenko <i.gnatenko.brain@gmail.com> - 0.3.5-1
- 0.3.5 upstream release

* Thu Sep 26 2013 Igor Gnatenko <i.gnatenko.brain@gmail.com> - 0.3.0-2
- Optimizing compile flags

* Fri Sep 20 2013 Igor Gnatenko <i.gnatenko.brain@gmail.com> - 0.3.0-1
- Update to 0.3.0 (improved new package: gmqpak)

* Sat Jul 27 2013 Igor Gnatenko <i.gnatenko.brain@gmail.com> - 0.2.9-1
- Initial release
