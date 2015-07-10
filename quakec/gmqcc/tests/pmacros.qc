/* will never happen unless blown up */
#ifndef GMQCC
    void() main = { error-gmqcc-is-not-defined; };
#else
    const string standard =
#   ifdef __STD_QCC__
        "qcc"
#   else
#       ifdef __STD_GMQCC__
            "gmqcc"
#       else
#           ifdef __STD_FTEQCC__
                "fteqcc"
#           else
            "unknown"
#           endif
#       endif
#   endif
    ;
    const string verminor = __STD_VERSION_MINOR__;
    const string vermajor = __STD_VERSION_MAJOR__;

#   define ALPHA_a "a"
#   define ALPHA_b "b"
#   define ALPHA_c "c"

#   define ALPHA(SEL) ALPHA_##SEL

#   define ABC ALPHA(a)ALPHA(b)ALPHA(c)

    void() main = {
        if (ABC == "abc")
            print("ABC\n");
        if (standard != "unknown")
            print("123\n");
    };
#endif
