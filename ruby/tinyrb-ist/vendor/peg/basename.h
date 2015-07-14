#include <string.h>
#ifdef WIN32
char *
mybasename(char *path)
{
        char    *s1, *s2;
        s2 = strrchr(path, '\\');
        if ((s1 = strrchr(path, '/')) != 0 || s2 != 0){
                return( (s1>s2?s1:s2) + 1 ); /* don't use max ;)) */
        }
        return(path);
}
#else
char *
mybasename(char *path)
{
        char    *s;
        
        if ((s = strrchr(path, '/')) != 0) {
                return(s + 1);
        }
        return(path);
}
#endif
#define basename mybasename