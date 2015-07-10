/*
 * Copyright (C) 2013, 2014, 2015
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
#include <string.h>
#include <stdlib.h>

#include "gmqcc.h"

/*
 * The PAK format uses a FOURCC concept for storing the magic ident within
 * the header as a uint32_t.
 */
#define PAK_FOURCC ((uint32_t)(((uint8_t)'P'|((uint8_t)'A'<<8)|((uint8_t)'C'<<16)|((uint8_t)'K'<<24))))

typedef struct {
    uint32_t magic;  /* "PACK" */

    /*
     * Offset to first directory entry in PAK file.  It's often
     * best to store the directories at the end of the file opposed
     * to the front, since it allows easy insertion without having
     * to load the entire file into memory again.
     */
    uint32_t diroff;
    uint32_t dirlen;
} pak_header_t;

/*
 * A directory, is sort of a "file entry".  The concept of
 * a directory in Quake world is a "file entry/record". This
 * describes a file (with directories/nested ones too in it's
 * file name).  Hence it can be a file, file with directory, or
 * file with directories.
 */
typedef struct {
    char     name[56];
    uint32_t pos;
    uint32_t len;
} pak_directory_t;

/*
 * Used to get the next token from a string, where the
 * strings themselfs are seperated by chracters from
 * `sep`.  This is essentially strsep.
 */
static char *pak_tree_sep(char **str, const char *sep) {
    char *beg = *str;
    char *end;

    if (!beg)
        return NULL;

    if (*(end = beg + strcspn(beg, sep)))
        * end++ = '\0'; /* null terminate */
    else
          end   = 0;

    *str = end;
    return beg;
}

/*
 * When given a string like "a/b/c/d/e/file"
 * this function will handle the creation of
 * the directory structure, included nested
 * directories.
 */
static void pak_tree_build(const char *entry) {
    char *directory;
    char *elements[28];
    char *pathsplit;
    char *token;

    size_t itr;
    size_t jtr;

    pathsplit = (char *)mem_a(56);
    directory = (char *)mem_a(56);

    memset(pathsplit, 0, 56);

    util_strncpy(directory, entry, 56);
    for (itr = 0; (token = pak_tree_sep(&directory, "/")) != NULL; itr++) {
        elements[itr] = token;
    }

    for (jtr = 0; jtr < itr - 1; jtr++) {
        util_strcat(pathsplit, elements[jtr]);
        util_strcat(pathsplit, "/");

        if (fs_dir_make(pathsplit)) {
            mem_d(pathsplit);
            mem_d(directory);

            /* TODO: undo on fail */

            return;
        }
    }

    mem_d(pathsplit);
    mem_d(directory);
}

typedef struct {
    pak_directory_t *directories;
    pak_header_t     header;
    fs_file_t       *handle;
    bool             insert;
} pak_file_t;

static pak_file_t *pak_open_read(const char *file) {
    pak_file_t *pak;
    size_t      itr;

    if (!(pak = (pak_file_t*)mem_a(sizeof(pak_file_t))))
        return NULL;

    if (!(pak->handle = fs_file_open(file, "rb"))) {
        mem_d(pak);
        return NULL;
    }

    pak->directories = NULL;
    pak->insert      = false; /* read doesn't allow insert */

    memset         (&pak->header, 0, sizeof(pak_header_t));
    fs_file_read   (&pak->header,    sizeof(pak_header_t), 1, pak->handle);

    util_endianswap(&pak->header.magic,  1, sizeof(pak->header.magic));
    util_endianswap(&pak->header.diroff, 1, sizeof(pak->header.diroff));
    util_endianswap(&pak->header.dirlen, 1, sizeof(pak->header.dirlen));

    /*
     * Every PAK file has "PACK" stored as FOURCC data in the
     * header.  If this data cannot compare (as checked here), it's
     * probably not a PAK file.
     */
    if (pak->header.magic != PAK_FOURCC) {
        fs_file_close(pak->handle);
        mem_d        (pak);
        return NULL;
    }

    /*
     * Time to read in the directory handles and prepare the directories
     * vector.  We're going to be reading some the file inwards soon.
     */
    fs_file_seek(pak->handle, pak->header.diroff, FS_FILE_SEEK_SET);

    /*
     * Read in all directories from the PAK file. These are considered
     * to be the "file entries".
     */
    for (itr = 0; itr < pak->header.dirlen / 64; itr++) {
        pak_directory_t dir;
        fs_file_read   (&dir,    sizeof(pak_directory_t), 1, pak->handle);

        /* Don't translate name - it's just an array of bytes. */
        util_endianswap(&dir.pos, 1, sizeof(dir.pos));
        util_endianswap(&dir.len, 1, sizeof(dir.len));

        vec_push(pak->directories, dir);
    }
    return pak;
}

static pak_file_t *pak_open_write(const char *file) {
    pak_file_t *pak;

    if (!(pak = (pak_file_t*)mem_a(sizeof(pak_file_t))))
        return NULL;

    /*
     * Generate the required directory structure / tree for
     * writing this PAK file too.
     */
    pak_tree_build(file);

    if (!(pak->handle = fs_file_open(file, "wb"))) {
        /*
         * The directory tree that was created, needs to be
         * removed entierly if we failed to open a file.
         */
        /* TODO backup directory clean */

        mem_d(pak);
        return NULL;
    }

    memset(&(pak->header), 0, sizeof(pak_header_t));

    /*
     * We're in "insert" mode, we need to do things like header
     * "patching" and writing the directories at the end of the
     * file.
     */
    pak->insert       = true;
    pak->header.magic = PAK_FOURCC;

    /* on BE systems we need to swap the byte order of the FOURCC */
    util_endianswap(&pak->header.magic, 1, sizeof(uint32_t));

    /*
     * We need to write out the header since files will be wrote out to
     * this even with directory entries, and that not wrote.  The header
     * will need to be patched in later with a file_seek, and overwrite,
     * we could use offsets and other trickery.  This is just easier.
     */
    fs_file_write(&(pak->header), sizeof(pak_header_t), 1, pak->handle);

    return pak;
}

static pak_file_t *pak_open(const char *file, const char *mode) {
    if (!file || !mode)
        return NULL;

    switch (*mode) {
        case 'r': return pak_open_read (file);
        case 'w': return pak_open_write(file);
    }

    return NULL;
}

static bool pak_exists(pak_file_t *pak, const char *file, pak_directory_t **dir) {
    size_t itr;

    if (!pak || !file)
        return false;

    for (itr = 0; itr < vec_size(pak->directories); itr++) {
        if (!strcmp(pak->directories[itr].name, file)) {
            /*
             * Store back a pointer to the directory that matches
             * the request if requested (NULL is not allowed).
             */
            if (dir) {
                *dir = &(pak->directories[itr]);
            }
            return true;
        }
    }

    return false;
}

/*
 * Extraction abilities.  These work as you expect them to.
 */
static bool pak_extract_one(pak_file_t *pak, const char *file, const char *outdir) {
    pak_directory_t *dir   = NULL;
    unsigned char   *dat   = NULL;
    char            *local = NULL;
    fs_file_t       *out   = NULL;

    if (!pak_exists(pak, file, &dir)) {
        return false;
    }

    if (!(dat = (unsigned char *)mem_a(dir->len)))
        goto err;

    /*
     * Generate the directory structure / tree that will be required
     * to store the extracted file.
     */
    pak_tree_build(file);

    /* TODO portable path seperators */
    util_asprintf(&local, "%s/%s", outdir, file);

    /*
     * Now create the file, if this operation fails.  Then abort
     * It shouldn't fail though.
     */
    if (!(out = fs_file_open(local, "wb")))
        goto err;

    /* free memory for directory string */
    mem_d(local);

    /* read */
    if (fs_file_seek (pak->handle, dir->pos, FS_FILE_SEEK_SET) != 0)
        goto err;

    fs_file_read (dat, 1, dir->len, pak->handle);
    fs_file_write(dat, 1, dir->len, out);
    fs_file_close(out);

    mem_d(dat);
    return true;

err:
    if (dat) mem_d(dat);
    if (out) fs_file_close(out);
    return false;
}

static bool pak_extract_all(pak_file_t *pak, const char *dir) {
    size_t itr;

    if (!fs_dir_make(dir))
        return false;

    for (itr = 0; itr < vec_size(pak->directories); itr++) {
        if (!pak_extract_one(pak, pak->directories[itr].name, dir))
            return false;
    }

    return true;
}

/*
 * Insertion functions (the opposite of extraction).  Yes for generating
 * PAKs.
 */
static bool pak_insert_one(pak_file_t *pak, const char *file) {
    pak_directory_t dir;
    unsigned char  *dat;
    long            len;
    fs_file_t      *fp;

    /*
     * We don't allow insertion on files that already exist within the
     * pak file.  Weird shit can happen if we allow that ;). We also
     * don't allow insertion if the pak isn't opened in write mode.
     */
    if (!pak || !file || !pak->insert || pak_exists(pak, file, NULL))
        return false;

    if (!(fp = fs_file_open(file, "rb")))
        return false;

    /*
     * Calculate the total file length, since it will be wrote to
     * the directory entry, and the actual contents of the file
     * to the PAK file itself.
     */
    if (fs_file_seek(fp, 0, FS_FILE_SEEK_END) != 0 || ((len = fs_file_tell(fp)) < 0))
        goto err;
    if (fs_file_seek(fp, 0, FS_FILE_SEEK_SET) != 0)
        goto err;

    dir.len = len;
    dir.pos = fs_file_tell(pak->handle);

    /*
     * We're limited to 56 bytes for a file name string, that INCLUDES
     * the directory and '/' seperators.
     */
    if (strlen(file) >= 56)
        goto err;

    util_strncpy(dir.name, file, strlen(file));

    /*
     * Allocate some memory for loading in the data that will be
     * redirected into the PAK file.
     */
    if (!(dat = (unsigned char *)mem_a(dir.len)))
        goto err;

    fs_file_read (dat, dir.len, 1, fp);
    fs_file_close(fp);
    fs_file_write(dat, dir.len, 1, pak->handle);

    /*
     * Now add the directory to the directories vector, so pak_close
     * can actually write it.
     */
    vec_push(pak->directories, dir);

    return true;

err:
    fs_file_close(fp);
    return false;
}

/*
 * Like pak_insert_one, except this collects files in all directories
 * from a root directory, and inserts them all.
 */
#if 0
static bool pak_insert_all(pak_file_t *pak, const char *dir) {
    DIR           *dp;
    struct dirent *dirp;

    if (!(pak->insert))
        return false;

    if (!(dp = fs_dir_open(dir)))
        return false;

    while ((dirp = fs_dir_read(dp))) {
        if (!(pak_insert_one(pak, dirp->d_name))) {
            fs_dir_close(dp);
            return false;
        }
    }

    fs_dir_close(dp);
    return true;
}
#endif /*!if 0 renable when ready to use */

static bool pak_close(pak_file_t *pak) {
    size_t itr;
    long   tell;

    if (!pak)
        return false;

    /*
     * In insert mode we need to patch the header, and write
     * our directory entries at the end of the file.
     */
    if (pak->insert) {
        if ((tell = fs_file_tell(pak->handle)) != 0)
            goto err;

        pak->header.dirlen = vec_size(pak->directories) * 64;
        pak->header.diroff = tell;

        /* patch header */
        if (fs_file_seek (pak->handle, 0, FS_FILE_SEEK_SET) != 0)
            goto err;

        fs_file_write(&(pak->header), sizeof(pak_header_t), 1, pak->handle);

        /* write directories */
        if (fs_file_seek (pak->handle, pak->header.diroff, FS_FILE_SEEK_SET) != 0)
            goto err;

        for (itr = 0; itr < vec_size(pak->directories); itr++)
            fs_file_write(&(pak->directories[itr]), sizeof(pak_directory_t), 1, pak->handle);
    }

    vec_free     (pak->directories);
    fs_file_close(pak->handle);
    mem_d        (pak);

    return true;

err:
    vec_free     (pak->directories);
    fs_file_close(pak->handle);
    mem_d        (pak);

    return false;
}

/*
 * Fancy GCC-like LONG parsing allows things like --opt=param with
 * assignment operator.  This is used for redirecting stdout/stderr
 * console to specific files of your choice.
 */
static bool parsecmd(const char *optname, int *argc_, char ***argv_, char **out, int ds, bool split) {
    int  argc   = *argc_;
    char **argv = *argv_;

    size_t len = strlen(optname);

    if (strncmp(argv[0]+ds, optname, len))
        return false;

    /* it's --optname, check how the parameter is supplied */
    if (argv[0][ds+len] == '=') {
        *out = argv[0]+ds+len+1;
        return true;
    }

    if (!split || argc < ds) /* no parameter was provided, or only single-arg form accepted */
        return false;

    /* using --opt param */
    *out = argv[1];
    --*argc_;
    ++*argv_;
    return true;
}

#include <stdio.h>
int main(int argc, char **argv) {
    bool          extract   = true;
    char         *redirout  = (char*)stdout;
    char         *redirerr  = (char*)stderr;
    char         *file      = NULL;
    char        **files     = NULL;
    pak_file_t   *pak       = NULL;
    size_t        iter      = 0;

    con_init();

    /*
     * Command line option parsing commences now We only need to support
     * a few things in the test suite.
     */
    while (argc > 1) {
        ++argv;
        --argc;

        if (argv[0][0] == '-') {
            if (parsecmd("redirout",  &argc, &argv, &redirout,  1, false))
                continue;
            if (parsecmd("redirerr",  &argc, &argv, &redirerr,  1, false))
                continue;
            if (parsecmd("file",      &argc, &argv, &file,      1, false))
                continue;

            con_change(redirout, redirerr);

            switch (argv[0][1]) {
                case 'e': extract = true;  continue;
                case 'c': extract = false; continue;
            }

            if (!strcmp(argv[0]+1, "debug")) {
                OPTS_OPTION_BOOL(OPTION_DEBUG) = true;
                continue;
            }
            if (!strcmp(argv[0]+1, "memchk")) {
                OPTS_OPTION_BOOL(OPTION_MEMCHK) = true;
                continue;
            }
            if (!strcmp(argv[0]+1, "nocolor")) {
                con_color(0);
                continue;
            }
        }

        vec_push(files, argv[0]);
    }
    con_change(redirout, redirerr);


    if (!file) {
        con_err("-file must be specified for output/input PAK file\n");
        vec_free(files);
        return EXIT_FAILURE;
    }

    if (extract) {
        if (!(pak = pak_open(file, "r"))) {
            con_err("failed to open PAK file %s\n", file);
            vec_free(files);
            return EXIT_FAILURE;
        }

        if (!pak_extract_all(pak, "./")) {
            con_err("failed to extract PAK %s (files may be missing)\n", file);
            pak_close(pak);
            vec_free(files);
            return EXIT_FAILURE;
        }

        /* not possible */
        pak_close(pak);
        vec_free(files);
        stat_info();

        return EXIT_SUCCESS;
    }

    if (!(pak = pak_open(file, "w"))) {
        con_err("failed to open PAK %s for writing\n", file);
        vec_free(files);
        return EXIT_FAILURE;
    }

    for (iter = 0; iter < vec_size(files); iter++) {
        if (!(pak_insert_one(pak, files[iter]))) {
            con_err("failed inserting %s for PAK %s\n", files[iter], file);
            pak_close(pak);
            vec_free(files);
            return EXIT_FAILURE;
        }
    }

    /* not possible */
    pak_close(pak);
    vec_free(files);

    stat_info();
    return EXIT_SUCCESS;
}
