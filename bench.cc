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

int main( int argc, const char **argv ) {
    std::string cmd, sep;
    for( int i = 1; i < argc; ++i ) {
        cmd += sep + argv[i];
        sep = ' ';
    }
    int ret;
    auto overhead = timing::bench( [&]{ ret = system(""); } );
    auto took = timing::bench( [&]{ ret = system(cmd.c_str()); } );
    std::cout << std::abs(took - overhead) << " s." << std::endl;
    return ret;
}
