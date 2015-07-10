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
#include "gmqcc.h"
#include "platform.h"

fs_file_t *fs_file_open(const char *filename, const char *mode) {
    return (fs_file_t*)platform_fopen(filename, mode);
}

size_t fs_file_read(void *buffer, size_t size, size_t count, fs_file_t *fp) {
    return platform_fread(buffer, size, count, (FILE*)fp);
}

int fs_file_printf(fs_file_t *fp, const char *format, ...) {
    int      rt;
    va_list  va;
    va_start(va, format);
    rt = platform_vfprintf((FILE*)fp, format, va);
    va_end  (va);

    return rt;
}

void fs_file_close(fs_file_t *fp) {
    platform_fclose((FILE*)fp);
}

size_t  fs_file_write (
    const void    *buffer,
    size_t         size,
    size_t         count,
    fs_file_t     *fp
) {
    return platform_fwrite(buffer, size, count, (FILE*)fp);
}

int fs_file_error(fs_file_t *fp) {
    return platform_ferror((FILE*)fp);
}

int fs_file_getc(fs_file_t *fp) {
    int get = platform_fgetc((FILE*)fp);
    return (get == EOF) ? FS_FILE_EOF : get;
}

int fs_file_puts(fs_file_t *fp, const char *str) {
    return platform_fputs(str, (FILE*)fp);
}

int fs_file_seek(fs_file_t *fp, long int off, int whence) {
    switch(whence) {
        case FS_FILE_SEEK_CUR: whence = SEEK_CUR; break;
        case FS_FILE_SEEK_SET: whence = SEEK_SET; break;
        case FS_FILE_SEEK_END: whence = SEEK_END; break;
    }
    return platform_fseek((FILE*)fp, off, whence);
}

long int fs_file_tell(fs_file_t *fp) {
    return platform_ftell((FILE*)fp);
}

int fs_file_flush(fs_file_t *fp) {
    return platform_fflush((FILE*)fp);
}

/*
 * Implements libc getline for systems that don't have it, which is
 * assmed all.  This works the same as getline().
 */
int fs_file_getline(char **lineptr, size_t *n, fs_file_t *stream) {
    int   chr;
    int   ret;
    char *pos;

    if (!lineptr || !n || !stream)
        return -1;
    if (!*lineptr) {
        if (!(*lineptr = (char*)mem_a((*n=64))))
            return -1;
    }

    chr = *n;
    pos = *lineptr;

    for (;;) {
        int c = fs_file_getc(stream);

        if (chr < 2) {
            *n += (*n > 16) ? *n : 64;
            chr = *n + *lineptr - pos;
            if (!(*lineptr = (char*)mem_r(*lineptr,*n)))
                return -1;
            pos = *n - chr + *lineptr;
        }

        if (fs_file_error(stream))
            return -1;
        if (c == EOF) {
            if (pos == *lineptr)
                return -1;
            else
                break;
        }

        *pos++ = c;
        chr--;
        if (c == '\n')
            break;
    }
    *pos = '\0';
    return (ret = pos - *lineptr);
}

int fs_dir_make(const char *path) {
    return platform_mkdir(path, 0700);
}

fs_dir_t *fs_dir_open(const char *name) {
    return (fs_dir_t*)platform_opendir(name);
}

int fs_dir_close(fs_dir_t *dir) {
    return platform_closedir((DIR*)dir);
}

fs_dirent_t *fs_dir_read(fs_dir_t *dir) {
    return (fs_dirent_t*)platform_readdir((DIR*)dir);
}
