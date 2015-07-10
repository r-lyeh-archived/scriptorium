/*
 * Copyright (C) 2012, 2013, 2014, 2015
 *     Dale Weiler
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#define GMQCC_PLATFORM_HEADER
#include <string.h>
#include <stdlib.h>

#include "platform.h"
#include "gmqcc.h"

int platform_vsnprintf(char *buffer, size_t bytes, const char *format, va_list arg) {
    return vsnprintf(buffer, bytes, format, arg);
}

int platform_vsscanf(const char *str, const char *format, va_list arg) {
    return vsscanf(str, format, arg);
}

const struct tm *platform_localtime(const time_t *timer) {
    return localtime(timer);
}

const char *platform_ctime(const time_t *timer) {
    return ctime(timer);
}

char *platform_strncat(char *dest, const char *src, size_t num) {
    return strncat(dest, src, num);
}

const char *platform_getenv(const char *var) {
    return getenv(var);
}

int platform_vasprintf(char **dat, const char *fmt, va_list args) {
    int     ret;
    int     len;
    char   *tmp = NULL;
    char    buf[128];
    va_list cpy;

    va_copy(cpy, args);
    len = vsnprintf(buf, sizeof(buf), fmt, cpy);
    va_end (cpy);

    if (len < 0)
        return len;

    if (len < (int)sizeof(buf)) {
        *dat = util_strdup(buf);
        return len;
    }

    tmp = (char*)mem_a(len + 1);
    if ((ret = vsnprintf(tmp, len + 1, fmt, args)) != len) {
        mem_d(tmp);
        *dat = NULL;
        return -1;
    }

    *dat = tmp;
    return len;
}

char *platform_strcat(char *dest, const char *src) {
    return strcat(dest, src);
}

char *platform_strncpy(char *dest, const char *src, size_t num) {
    return strncpy(dest, src, num);
}

const char *platform_strerror(int err) {
    return strerror(err);
}

FILE *platform_fopen(const char *filename, const char *mode) {
    return fopen(filename, mode);
}

size_t platform_fread(void *ptr, size_t size, size_t count, FILE *stream) {
    return fread(ptr, size, count, stream);
}

size_t platform_fwrite(const void *ptr, size_t size, size_t count, FILE *stream) {
    return fwrite(ptr, size, count, stream);
}

int platform_fflush(FILE *stream) {
    return fflush(stream);
}

int platform_vfprintf(FILE *stream, const char *format, va_list arg) {
    return vfprintf(stream, format, arg);
}

int platform_fclose(FILE *stream) {
    return fclose(stream);
}

int platform_ferror(FILE *stream) {
    return ferror(stream);
}

int platform_fgetc(FILE *stream) {
    return fgetc(stream);
}

int platform_fputs(const char *str, FILE *stream) {
    return fputs(str, stream);
}

int platform_fseek(FILE *stream, long offset, int origin) {
    return fseek(stream, offset, origin);
}

long platform_ftell(FILE *stream) {
    return ftell(stream);
}

int platform_mkdir(const char *path, int mode) {
    /*
     * For some reason mingw32 just doesn't have a correct mkdir impl
     * so we handle that here.
     */
#   ifdef _WIN32
    (void)mode;
    return mkdir(path);
#   else
    return mkdir(path, mode);
#   endif /*!_WIN32*/
}

DIR *platform_opendir(const char *path) {
    return opendir(path);
}

int platform_closedir(DIR *dir) {
    return closedir(dir);
}

struct dirent *platform_readdir(DIR *dir) {
    return readdir(dir);
}

int platform_isatty(int fd) {
    return isatty(fd);
}
