#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

/*
 * protect some information, not that I care, but this is just to stay
 * safer.
 */
#define SECURITY_BASE  "\
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
#define SECURITY_TOKEN "\
P29hdXRoX2NvbnN1bWVyX2tleT03NnZoM3E0Mmhudm16bTMmb2F1dGhfdG9rZW49\
dzBieHpmMGRmdDhlZGZxJm9hdXRoX3NpZ25hdHVyZV9tZXRob2Q9UExBSU5URVhU\
Jm9hdXRoX3NpZ25hdHVyZT10bWVlY2h0MmNtaDcyeGElMjY5dm9zeDd4OGd5NGtn\
amsmb2F1dGhfdGltZXN0YW1wPSZvYXV0aF9ub25jZT0xMjE2NQo="

int isbase64(char c) {
   return !!(c && strchr(SECURITY_BASE, c) != NULL);
}
char value(char c) {
    const char *load = SECURITY_BASE;
    const char *find = strchr(load, c);

    return (find) ? find - load : 0;
}

int security_decode(unsigned char *dest, const unsigned char *src, int srclen) {
    unsigned char *p;

    if(!*src)
        return 0;

    *dest = 0;
    p = dest;

    do {
        *p++ = (value(src[0]) << 2) | (value(src[1]) >> 4);
        *p++ = (value(src[1]) << 4) | (value(src[2]) >> 2);
        *p++ = (value(src[2]) << 6) | (value(src[3]) >> 0);

        if(!isbase64(src[1])) {
            p -= 2;
            break;
        }
        else if(!isbase64(src[2])) {
            p -= 2;
            break;
        }
        else if(!isbase64(src[3])) {
            p--;
            break;
        }
        src += 4;

        while(*src && (*src == 13 || *src == 10))
        src++;
    } while(srclen-= 4);

    *p = 0;
    return p-dest;
}

#define BASEURL          " https://api-content.dropbox.com/1/files/sandbox/"

/*
 * If more platforms are supported add the entries between the start
 * tag here, and the end tag below. Nothing else needs to be done
 * <tag> (the table needs to match the HTML too)
 */
#define ARCHLINUX_32_REF "%sgmqcc-%c.%c.%c-1-i686.pkg.tar.xz%s"
#define ARCHLINUX_64_REF "%sgmqcc-%c.%c.%c-1-x86_64.pkg.tar.xz%s"
#define DEBIAN_32_REF    "%sgmqcc-%c.%c.%c-i686.deb%s"
#define DEBIAN_64_REF    "%sgmqcc-%c.%c.%c-x86_64.deb%s"
#define WINDOWS_32_REF   "%sgmqcc-%c.%c.%c-win32.zip%s"
#define WINDOWS_64_REF   "%sgmqcc-%c.%c.%c-win64.zip%s"
#define SLACKWARE_32_REF "%sgmqcc-%c.%c.%c-i686.txz%s"
#define SLACKWARE_64_REF "%sgmqcc-%c.%c.%c-x86_64.txz%s"


#define HTML "\
<!doctype html>\
<html>\
<head>\
 <meta charset=\"utf-8\">\
 <meta http-equiv=\"X-UA-Compatible\" content=\"chrome=1\">\
 <title>GMQCC</title>\
 <link rel=\"stylesheet\" href=\"stylesheets/styles.css\">\
 <link rel=\"stylesheet\" href=\"stylesheets/pygment_trac.css\">\
 <script src=\"javascripts/scale.fix.js\"></script>\
 <meta name=\"viewport\" content=\"width=device-width, initial-scale=1, user-scalable=no\">\
 <!--[if lt IE 9]>\
 <script src=\"//html5shiv.googlecode.com/svn/trunk/html5.js\"></script>\
 <![endif]-->\
</head>\
<body>\
 <a href=\"https://github.com/graphitemaster/gmqcc\"><div class=\"fork\"></div></a>\
 <div class=\"wrapper\">\
  <header>\
   <h1 class=\"header\">GMQCC</h1>\
   <p class=\"header\">An Improved Quake C Compiler</p>\
   <ul>\
    <li class=\"buttons\"><a href=index.html>Index</a></li>\
    <li class=\"download\"><a href=\"download.html\">Download</a></li>\
    <li class=\"buttons\"><a href=\"https://github.com/graphitemaster/gmqcc/issues\">Issues</a></li>\
    <li class=\"buttons\"><a href=\"doc.html\">Documentation</a></li>\
    <li class=\"buttons\"><a href=\"https://github.com/graphitemaster/gmqcc\">View On GitHub</a></li>\
   </ul>\
  </header>\
  <section>\
    <table>\
     <tr>\
      <th>Operating System / Distribution</th>\
      <th>x86 Architecture</th>\
      <th>x86_64 Architecture</th>\
     </tr>\
     <tr>\
      <td>Archlinux</td>\
      <td><a href=\"%s\">Download</a></td>\
      <td><a href=\"%s\">Download</a></td>\
     </tr>\
     <tr>\
      <td>Debian</td>\
      <td><a href=\"%s\">Download</a></td>\
      <td><a href=\"%s\">Download</a></td>\
     </tr>\
     <tr>\
      <td>Slackware</td>\
      <td><a href=\"%s\">Download</a></td>\
      <td><a href=\"%s\">Download</a></td>\
     <tr>\
      <td>Windows</td>\
      <td><a href=\"%s\">Download</a></td>\
      <td><a href=\"%s\">Download</a></td>\
    </tr>\
    </table>\
  </section>\
  <footer>\
   <script type=\"text/javascript\" src=\"http://www.ohloh.net/p/602517/widgets/project_partner_badge.js\"></script>\
  </footer>\
 </div>\
 <!--[if !IE]><script>fixScale(document);</script><![endif]-->\
</body>\
</html>\
"

static char build_table[][4096] = {
    ARCHLINUX_32_REF, ARCHLINUX_64_REF,
    DEBIAN_32_REF,    DEBIAN_64_REF,
    SLACKWARE_32_REF, SLACKWARE_64_REF,
    WINDOWS_32_REF,   WINDOWS_64_REF
};
/* </tag> */

#define ISXDIGIT(c) ((c >= 48 && c <= 57) || ((c & ~0x20) >= 65 && (c & ~0x20) <= 70))
typedef struct {
    char        *data;
    unsigned int len;
    unsigned int size;
} url_t;
void escape(url_t *str) {
    char *p, *ptr;
    char hexstr[3];
    unsigned int  i=0;
    unsigned long l=0;

    p = str->data;
    for(i=0; i < str->len; i++) {
        if((p - str->data) >= str->len)
            break;
        if(*p == '%' &&
            ((p - str->data)+2) < str->len &&
            ISXDIGIT(*(p+1)) &&
            ISXDIGIT(*(p+2))
        ) {
            p++;
            hexstr[0] = *p++;
            hexstr[1] = *p++;
            hexstr[2] = 0;
            l = strtoul(hexstr, &ptr, 16);
            str->data[i] = (char)(l & 0x7f);
            continue;
        }
        if(*p == '+') {
            *p = ' ';
        }
        str->data[i] = *p++;
    }
    str->data[i] = 0;
    str->len     = i;
}

void version(const char *directory, char *major, char *minor, char *patch) {
    FILE    *handle;
    char     file[4096];
    size_t   size = 0;
    char    *data = NULL;
    snprintf(file, sizeof(file), "%s/gmqcc.h", directory);

    handle = fopen(file, "r");
    if (!handle) {
        fprintf(stderr, "failed to open %s for reading version (%s)\n",
            file, strerror(errno)
        );
        abort();
    }

    while (getline(&data, &size, handle) != EOF) {

        #define TEST(TYPE, STORE)                               \
            if (strstr(data, "#define GMQCC_VERSION_" TYPE )) { \
                char *get = data;                               \
                while (!isdigit(*get))                          \
                    get++;                                      \
                *STORE = *get;                                  \
            }

        TEST("MAJOR", major)
        TEST("MINOR", minor)
        TEST("PATCH", patch)

        #undef TEST
    }

    free(data);
}

void genhtml() {
    FILE *fp = fopen("download.html", "w");
    if (!fp) {
        fprintf(stderr, "failed to generate HTML: %s\n", strerror(errno));
        abort();
    }

    fprintf(fp, HTML,
        build_table[0], build_table[1],
        build_table[2], build_table[3],
        build_table[4], build_table[5],
        build_table[6], build_table[7]
    );
    fclose (fp);
}

/*
 * Builds a list of download links with the right version and handles the
 * rest of the magic.
 */
void build(const char *directory) {
    /* Figure out version number */
    char   find[3];
    char   decode[4096];
    size_t size;
    version(directory, &find[0], &find[1], &find[2]);

    /*
     * decode the secuity stuff for preparing the URLs which will be used
     * as links.
     */
    memset(decode, 0, sizeof(decode));
    security_decode(decode, SECURITY_TOKEN, strlen(SECURITY_TOKEN));

    for (size = 0; size < sizeof(build_table) / sizeof(*build_table); size++) {
        char *load = strdup(build_table[size]);
        url_t esc  = { NULL, 0 };

        snprintf(build_table[size], 4096, load, BASEURL, find[0], find[1], find[2], decode);
        esc.data = strdup(build_table[size]);
        esc.size = strlen(build_table[size]);
        esc.len  = esc.size;

        /* Yes we also need to escape URLs just incase */
        escape(&esc);
        free(load);
    }

    /*
     * Now generate the HTML file for those download links by asking tinyurl to
     */
    genhtml();
}

int main(int argc, char **argv) {
    size_t itr;

    argc--;
    argv++;
    if (!argc) {
        printf("usage: %s [gmqcc.h location]\n", argv[-1]);
        return 0;
    }

    build(*argv);
}
