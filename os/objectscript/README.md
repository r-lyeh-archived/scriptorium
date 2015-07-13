ObjectScript
============

ObjectScript, OS for short, is a new programming language. It's free, cross-platform, lightweight, embeddable and open-source. It combines the benefits of multiple languages, including: JavaScript, Lua, Ruby, Python and PHP. OS features the syntax of Javascripts, the "multiple results" feature from lua, syntactic shugar from Ruby as well as magic methods from PHP and Ruby - and even more!

The ObjectScript is universal scripting language, there are no compromises any more.

Compile and Install __os-fcgi__ and __os__
=======

Run the following commands after you have cloned this repository:

	mkdir build && cd build
	cmake ..
	make
	make install

If you are on an operating system like Debian or Ubuntu, you can then start __os-fcgi__ by typing:

    service os-fcgi start

Installing the Database layer
-------------------

During the configuration, you may notice that a library named SoCi could not be found. This is the database abstraction which is used by ObjectScript's `ext-odbo` module. You can build SoCi alongside OS by using the following CMake command instead of the one given above:

	cmake .. -DBUILD_SOCI=ON

After that, you should be good to go.

Special note for Apple Mac OS X builds
-------------------
Depending on how you have installed MySQL on your system, you may run into this error by running __os__ or __os-fcgi__ from the build directory or after you have installed it:

```
dyld: Library not loaded: libmysqlclient.18.dylib
  Referenced from: /usr/local/bin/os
  Reason: image not found
```

Or similar. To fix this, do as follows:

```bash
# Navigate back into your build folder if you have previously left it
cd build
../contrib/change_install_name.sh
```

This will update all the binaries within your build folder and make them find the library - hopefuly.

Using ObjectScript in your app
====================

After installing ObjectScript, you also will get the required headers into your system to utilize ObjectScript in your app. This is especially useful to let the user do something within your app. Here is an example:

app.cpp:
```cpp
#include <objectscript.h>

using namespace ObjectScript;

int main(int argc, char** argv) {
	OS* os = OS::create();

	// simulate calling "print()" with given argv
	os->getGlobal("print");
	os->newArray(argc);
	for(int i=0; i<argc; i++) {
		os->pushString(argv[i]);
		os->addProperty(-2);
	}
	os->callF(1);
}
```

Now, compile it just like this:

    g++ app.cpp -o app -lobjectscript

To do so on Windows, copy the resulting `libobjectscript.lib` and `src/objectscript.h` to your project, and compile as:

    cl app.cpp libobjectscript.lib /I. /Fe:app

The library and headers are installed globally into your installation path's `include` folder.

Nginx config example (for __os-fcgi__)
====================

	server {
		listen			80;
		server_name		mydomain.com www.mydomain.com;
		root			/home/myuser/mydomain.com/www;
		error_log		/var/log/nginx/error.mydomain.com.log;
		access_log		off;
		location ~ /\.ht {
			deny all;
		}
		location ~ /\.git {
			deny all;
		}
		location / {
			try_files $uri $uri/ /index.osh /index.os;
		}
		location ~* \.(jpg|jpeg|png|gif|swf|flv|mp4|mov|avi|wmv|m4v|mkv|ico|js|css|txt)$ {
			access_log off;
			expires 7d;
		}
		charset	utf-8;
		location ~ ^.+\.osh? {
			fastcgi_split_path_info	^(.+?\.osh?)(.*)$;
			fastcgi_pass	127.0.0.1:9000;
			fastcgi_index	index.osh;
			include fastcgi_params;
			fastcgi_intercept_errors	on;
			fastcgi_ignore_client_abort	on;
			fastcgi_read_timeout	360;
		}
	}

Apache config example (for __os-fcgi__)
=====================

	<VirtualHost mydomain.com:80>
		ServerAdmin webmaster@mydomain.com
		DocumentRoot "/home/myuser/mydomain.com/www"
		ServerName mydomain.com

		FastCgiExternalServer "/home/myuser/mydomain.com/www" -host 127.0.0.1:9000

		<Directory "/home/myuser/mydomain.com/www">
			# SetHandler fastcgi-script
			AddHandler fastcgi-script .osh
			AddHandler fastcgi-script .os
			Options Indexes FollowSymLinks MultiViews ExecCGI
			AllowOverride all
			Order Deny,Allow
			Deny from all
			Allow from 127.0.0.1
		</Directory>
	</VirtualHost>

## Resources

* [OS2D](https://github.com/unitpoint/os2d) is cross platform engine for 2d mobile games (examples included) made with ObjectScript and Oxygine
* [OS2D binaries](https://github.com/unitpoint/os2d-bin-win) is OS2D binaries and executables examples for Windows
* [CastleMonster](https://github.com/unitpoint/CastleMonster) is opensource old school battle mobile game made with OS2D
* [CastleMonster binaries](https://github.com/unitpoint/CastleMonster-bin-win) is CastleMonster binaries and executables files for Windows
* [eXeXeXeX](https://github.com/unitpoint/eXeXeXeX) is opensource is a 4X genre of strategy-based video game made with OS2D
* [eXeXeXeX binaries](https://github.com/unitpoint/eXeXeXeX-bin-win) is eXeXeXeX binaries and executables files for Windows
* [ObjectScript](https://github.com/unitpoint/objectscript) is ObjectScript language repository of full opensource code
* [ObjectScript binaries](https://github.com/unitpoint/objectscript-bin-win) is ObjectScript binaries and executables files for Windows (os.exe & os-fcgi.exe included)
* [objectscript.org](http://objectscript.org) is ObjectScript site
* [objectscript.org repo](https://github.com/unitpoint/objectscript.org) is ObjectScript site opensource code repository
* [Programming in ObjectScript](https://github.com/unitpoint/objectscript/wiki/Programming-in-ObjectScript)
* [ObjectScript Reference](https://github.com/unitpoint/objectscript/wiki/ObjectScript-Reference)
* [ObjectScript C++ binder generator](https://github.com/unitpoint/objectscript/blob/master/src/os-binder-generator.os)
* [IceTea](https://github.com/IngwiePhoenix/IceTea) is a next-generation build system - a replacement for Make and autotools that works across platforms and is based off ObjectScript.

##Contacts

Please feel free to contact me at anytime, my email is evgeniy.golovin@unitpoint.ru, skype: egolovin

P.S. old files of this repo have been moved to https://github.com/unitpoint/objectscript-old
