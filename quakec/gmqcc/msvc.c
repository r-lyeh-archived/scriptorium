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

#define CTIME_BUFFER    64
#define GETENV_BUFFER   4096
#define STRERROR_BUFFER 128

static void **platform_mem_pool = NULL;
static void platform_mem_atexit() {
    size_t i;
    for (i = 0; i < vec_size(platform_mem_pool); i++)
        mem_d(platform_mem_pool[i]);
    vec_free(platform_mem_pool);
}

static void *platform_mem_allocate(size_t bytes) {
    void *mem = NULL;
    if (!platform_mem_pool) {
        atexit(&platform_mem_atexit);
        vec_push(platform_mem_pool, NULL);
    }

    mem = mem_a(bytes);
    vec_push(platform_mem_pool, mem);

    return mem;
}

int platform_vsnprintf(char *buffer, size_t bytes, const char *format, va_list arg) {
    return vsnprintf_s(buffer, bytes, bytes, format, arg);
}

int platform_vsscanf(const char *str, const char *format, va_list va) {
    return vsscanf_s(str, format, va);
}

const struct tm *platform_localtime(const time_t *timer) {
    struct tm *t;
    t = (struct tm*)platform_mem_allocate(sizeof(struct tm));
    localtime_s(&t, timer);
    return t;
}

const char *platform_ctime(const time_t *timer) {
    char *buffer = (char *)platform_mem_allocate(CTIME_BUFFER);
    ctime_s(buffer, CTIME_BUFFER, timer);
    return buffer;
}

char *platform_strncat(char *dest, const char *src, size_t num) {
    return strncat_s(dest, num, src, _TRUNCATE);
}

const char *platform_getenv(const char *var) {
    char  *buffer = (char *)platform_mem_allocate(GETENV_BUFFER);
    size_t size;
    getenv_s(&size, buffer, GETENV_BUFFER, var);
    return buffer;
}

/*
 * TODO: this isn't exactly 'accurate' for MSVC but it seems to work,
 * at least to some extent.
 */
int platform_vasprintf(char **dat, const char *fmt, va_list args) {
    int   ret;
    int   len;
    char *tmp = NULL;

    if ((len = _vscprintf(fmt, args)) < 0) {
        *dat = NULL;
        return -1;
    }

    tmp = (char*)mem_a(len + 1);
    if ((ret = _vsnprintf_s(tmp, len+1, len+1, fmt, args)) != len) {
        mem_d(tmp);
        *dat = NULL;
        return -1;
    }
    *dat = tmp;
    return len;
}

char *platform_strcat(char *dest, const char *src) {
    strcat_s(dest, strlen(src), src);
    return dest;
}

char *platform_strncpy(char *dest, const char *src, size_t num) {
    strncpy_s(dest, num, src, num);
    return dest;
}

const char *platform_strerror(int err) {
    char *buffer = (char*)platform_mem_allocate(STRERROR_BUFFER);
    strerror_s(buffer, STRERROR_BUFFER, err);
    return buffer;
}

FILE *platform_fopen(const char *filename, const char *mode) {
    FILE *handle;
    return (fopen_s(&handle, filename, mode) != 0) ? NULL : handle;
}

size_t platform_fread(void *ptr, size_t size, size_t count, FILE *stream) {
    return fread_s(ptr, size * count, size, count, stream);
}

size_t platform_fwrite(const void *ptr, size_t size, size_t count, FILE *stream) {
    return fwrite(ptr, size, count, stream);
}

int platform_fflush(FILE *stream) {
    return fflush(stream);
}

int platform_vfprintf(FILE *stream, const char *format, va_list arg) {
    return vfprintf_s(stream, format, arg);
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
    return mkdir(path, mode);
}

DIR *platform_opendir(const char *path) {
    DIR *dir = (DIR*)mem_a(sizeof(DIR) + strlen(path));
    if (!dir)
        return NULL;

    platform_strncpy(dir->dd_name, path, strlen(path));
    return dir;
}

int platform_closedir(DIR *dir) {
    FindClose((HANDLE)dir->dd_handle);
    mem_d((void*)dir);
    return 0;
}

struct dirent *platform_readdir(DIR *dir) {
    WIN32_FIND_DATA info;
    struct dirent  *data;
    int             ret;

    if (!dir->dd_handle) {
        char *dirname;
        if (*dir->dd_name) {
            size_t n = strlen(dir->dd_name);
            if ((dir = (char*)mem_a(n+5))) {
                platform_strncpy(dirname,     dir->dd_name, n);
                platform_strncpy(dirname + n, "\\*.*",      4);
            }
        } else {
            if (!(dirname = util_strdup("\\*.*")))
                return NULL;
        }

        dir->dd_handle = (long)FindFirstFile(dirname, &info);
        mem_d(dirname);
        ret = !(!dir->dd_handle);
    } else if (dir->dd_handle != -11) {
        ret = FindNextFile((HANDLE)dir->dd_handle, &info);
    } else {
        ret = 0;
    }

    if (!ret)
        return NULL;

    if ((data = (struct dirent*)mem_a(sizeof(struct dirent)))) {
        platform_strncpy(data->d_name, info.cFileName, FILENAME_MAX - 1);
        data->d_name[FILENAME_MAX - 1] = '\0';
        data->d_namelen                = strlen(data->d_name);
    }

    return data;
}

int platform_isatty(int fd) {
    return _isatty(fd);
}
