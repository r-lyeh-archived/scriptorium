// timing - rlyeh, public domain [ref] https://gist.github.com/r-lyeh/07cc318dbeee9b616d5e {
#pragma once
#include <thread>
#include <chrono>
#if !defined(TIMING_USE_OMP) && ( defined(USE_OMP) || defined(_MSC_VER) /*|| defined(__ANDROID_API__)*/ )
#   define TIMING_USE_OMP
#   include <omp.h>
#endif
struct timing {
    static double now() {
#   ifdef TIMING_USE_OMP
        static auto const epoch = omp_get_wtime(); 
        return omp_get_wtime() - epoch;
#   else
        static auto const epoch = std::chrono::steady_clock::now(); // milli ms > micro us > nano ns
        return std::chrono::duration_cast< std::chrono::microseconds >( std::chrono::steady_clock::now() - epoch ).count() / 1000000.0;
#   endif
    }
    template<typename FN>
    static double bench( const FN &fn ) {
        auto took = -now();
        return ( fn(), took + now() );
    }
    static void sleep( double secs ) {
        std::chrono::microseconds duration( (int)(secs * 1000000) );
        std::this_thread::sleep_for( duration );    
    }
};
// } timing

#include <algorithm>
#include <stdlib.h>
#include <iostream>
#include <string>
#include <fstream>
#include <set>
#include <map>
#include <vector>
#include <sstream>

#include <Shlwapi.h>
#pragma comment(lib, "shlwapi.lib")

std::string base( const std::string &app = std::string() ) {
    TCHAR dest[MAX_PATH];
    size_t destSize = MAX_PATH;

    if (!dest) return "./" + app;
    if (MAX_PATH > destSize) return "./" + app;

    DWORD length = GetModuleFileName( NULL, dest, destSize );
    PathRemoveFileSpec(dest);

    for( auto &ch : dest ) {
        if( ch == '\\' ) ch = '/';
    }

    return std::string() + dest + "/" + app;
}

int main( int argc, const char **argv ) {
    int ret = 0;

    // benchmark
    if( argc > 1 )
    {
        int N = 0;
        std::string cmd, sep, desc;
        for( int i = 1; i < argc; ++i ) {
            if( argv[i][0] >= '0' && argv[i][0] <= '9' ) {
                 N = std::strtoul( argv[i], NULL, NULL );
            }
            else
            if( argv[i][0] == '/' && argv[i][1] == '/' ) {
                desc = &argv[i][2];
            }
            else {
                cmd += sep + argv[i];
                sep = ' ';

                if( desc.empty() ) desc = argv[i];
            }
        }
        auto overhead = 0; // measure fib(0)
        auto took = timing::bench( [&]{ 
            // avg time
            for( auto i = 0; i < N - 1; ++i )
            system((cmd+" 1> nul 2> nul").c_str());
            ret = system(cmd.c_str());
        } );
        std::cout << (std::abs(took - overhead) / N) << " s." << std::endl;

        // append to 'bench.csv'
        {
            std::ofstream ofs( base("/bench.csv").c_str(), std::ios::binary | std::ios::app );
            if( ofs.good() ) {
                ofs << desc << "," << ( std::abs(took - overhead) / N ) << std::endl;
            }
        }
    }

    // update 'bench.md'
    {
        auto tokenize = []( const std::string &self, const std::string &delimiters ) -> std::vector< std::string > {
            std::string map( 256, '\0' );
            for( auto &ch : delimiters )
                map[ ch ] = '\1';
            std::vector< std::string > tokens(1);
            for( const unsigned char &ch : self ) {
                /**/ if( !map.at(ch)          ) tokens.back().push_back( ch );
                else if( tokens.back().size() ) tokens.push_back( std::string() );
            }
            while( tokens.size() && !tokens.back().size() ) tokens.pop_back();
            return tokens;
        };

        struct idx {
            std::string name;
            float time;

            float relative_speed( float min, float max ) const {
                float tstep = time / min;
                return 100 / (tstep);
            }

            bool operator <( const idx &other ) const {
                if( time < other.time ) return true;
                return name < other.name;
            }

            bool operator==( const idx &other ) const {
                return name == other.name;
            }
            bool operator!=( const idx &other ) const {
                return name != other.name;
            }
        };

        std::map< std::string, float > unique;
        std::set< idx > sort;

        {
            std::ifstream ifs( base("/bench.csv").c_str(), std::ios::binary);
            std::stringstream ss;
            ss << ifs.rdbuf();
            auto lines = tokenize( ss.str(), "\r\n" );
            for( auto &line : lines ) {
                for( auto &ch : line ) {
                    if( ch == '\\' ) ch = '/';
                }
                auto tokens = tokenize( line, "," );
                if( tokens.size() == 2 ) {
                    if( tokens[0] == "fib" || tokens[0] == "fib.exe" ) tokens[0] = "c/vc";
                    float time = std::strtof( tokens[1].c_str(), NULL );
                    float prev = (unique[ tokens[0] ] = unique[ tokens[0] ]);
                    unique[ tokens[0] ] = prev ? (std::min)( prev, time ) : time;
                }
            }
            for( auto &it : unique ) {
                sort.insert( idx{ it.first, it.second } );
            }
        }

        auto min = float(sort.begin()->time);
        auto max = float(sort.rbegin()->time);

#if 1
        auto cmin = sort.begin(); //find("lua")->time; //float(sort.begin()->time);
        while( cmin != sort.end() && cmin->name.find("[lua]") == std::string::npos ) cmin++;
        min = float(cmin->time);
#endif

        std::ofstream ofs( base("BENCH.md").c_str(), std::ios::binary);
        ofs << "|Language|Time|Relative Lua speed|Score|" << std::endl;
        ofs << "|:-------|---:|:----------------:|----:|" << std::endl;
        for( auto &it : sort ) {
            float speed = it.relative_speed(min, max);
            int score( speed );
            int factor( speed / 100 );
            speed = speed > 100 ? 100 : speed;
            if( factor > 1 ) {
                char buf[16]; sprintf(buf, "%02d", factor);
                ofs << '|' << it.name << "|" << it.time << " s.|![" << speed << "%](http://progressed.io/bar/" << int(speed) << "?title=x" << buf << ")|" << score << " pt|" << std::endl;
            } else {
                ofs << '|' << it.name << "|" << it.time << " s.|![" << speed << "%](http://progressed.io/bar/" << int(speed) << ")|" << score << " pt|" << std::endl;
            }
        }
    }

    return ret;
}
