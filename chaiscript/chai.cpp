#include <chaiscript/chaiscript.hpp>
#include <chaiscript/chaiscript_stdlib.hpp>
#include <iostream>

std::string helloWorld(const std::string &t_name)
{
  return "Hello " + t_name + "!";
}
 
int main( int argc, const char **argv )
{
	chaiscript::ChaiScript chai(chaiscript::Std_Lib::library());
	if( argc > 1 ) {
		FILE *fp = fopen(argv[1], "rb");
		if( fp ) {
		        fseek(fp, 0L, SEEK_END);
		        size_t size = ftell(fp);
		        fseek(fp, 0L, SEEK_SET);
		        char *buffer = (char *)malloc( size + 1 );		        
		        if( buffer ) {
		                fread( buffer, size, 1, fp );
		                buffer[ size ] = '\0';
		                try {
			                chai.eval(buffer);
		                } catch(const chaiscript::exception::eval_error &e) {
		                	std::cout << e.pretty_print() << std::endl;
		                }
		        }
		        free(buffer);
		        fclose(fp);
		}
	} else {
		chai.add(chaiscript::fun(&helloWorld), 
		       "helloWorld");

		chai.eval("puts(helloWorld(\"Bob\"));");
	}
}
