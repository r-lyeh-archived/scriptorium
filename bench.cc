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

    if( argc <= 2 ) {
        return 0;
    }

    int N = std::strtoul( argv[1], NULL, NULL );

    std::string cmd, sep;
    for( int i = 2; i < argc; ++i ) {
        cmd += sep + argv[i];
        sep = ' ';
    }
    int ret;
    auto overhead = timing::bench( [&]{ 
        // spinning
        for( auto i = 0; i < N; ++i ) 
        ret = system((cmd+"! 1> nul 2> nul").c_str());
    } );
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
            ofs << argv[2] << "," << ( std::abs(took - overhead) / N ) << std::endl;
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
                if( time > other.time ) return false;
                return name < other.name;
            }
        };

        std::set< idx > all;

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
                    all.insert( idx { tokens[0], std::strtof( tokens[1].c_str(), NULL ) } ); 
                }
            }
        }

#if 0
        auto min = float(all.rbegin()->time);
#else
        auto cmin = all.begin(); //find("lua")->time; //float(all.begin()->time);
        while( cmin != all.end() && cmin->name != "lua" ) cmin++;
        auto min = float(cmin->time);
#endif
        auto max = float(all.rbegin()->time);

        std::ofstream ofs( base("BENCH.md").c_str(), std::ios::binary);
        ofs << "|Language|Time|Relative Lua speed|Score|" << std::endl;
        ofs << "|:-------|---:|:----------------:|----:|" << std::endl;
        for( auto &it : all ) {
            float speed = it.relative_speed(min, max);
            int score( speed );
            speed = speed > 100 ? 100 : speed;
            ofs << '|' << it.name << "|" << it.time << " s.|![" << speed << "%](http://progressed.io/bar/" << int(speed) << ")|" << score << " pt|" << std::endl;
        }
    }

    return ret;
}
