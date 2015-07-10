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

#ifndef GMQCC_PLATFORM_HDR
#define GMQCC_PLATFORM_HDR

#ifndef GMQCC_PLATFORM_HEADER
#   error "This header shouldn't be included!"
#endif

#undef GMQCC_PLATFORM_HEADER
#include <stdarg.h>
#include <time.h>
#include <stdio.h>

#ifdef _WIN32
#   ifndef STDERR_FILENO
#       define STDERR_FILENO 2
#   endif
#   ifndef STDOUT_FILENO
#       define STDOUT_FILENO 1
#   endif
#   ifndef __MINGW32__
#       define _WIN32_LEAN_AND_MEAN
#       include <windows.h>
#       include <io.h>
#       include <fcntl.h>

        struct dirent {
            long               d_ino;
            unsigned short     d_reclen;
            unsigned short     d_namelen;
            char               d_name[FILENAME_MAX];
        };

        typedef struct {
            struct _finddata_t dd_dta;
            struct dirent      dd_dir;
            long               dd_handle;
            int                dd_stat;
            char               dd_name[1];
        } DIR;
#   else
#       include <dirent.h>
#   endif /*!__MINGW32__*/

#   ifndef S_ISDIR
#       define S_ISDIR(X) ((X)&_S_IFDIR)
#   endif
#else
#   include <sys/types.h>
#   include <sys/stat.h>
#   include <unistd.h>
#   include <dirent.h>
#endif /*!_WIN32*/

/*
 * Function: platform_vsnprintf
 *  Write formatted output using a pointer to a lis of arguments.
 *
 * Parameters:
 *  buffer - Storage location for output.
 *  bytes  - Maximum number of characters to write.
 *  format - Format specification.
 *  arg    - Variable argument list.
 *
 * Returns:
 *  The number of characters written if the number of characters to write
 *  is less than or equal to `bytes`; if the number of characters to write
 *  is greater than `bytes`, this function returns -1 indicating that the
 *  output has been truncated. The return value does not include the
 *  terminating null, if one is written.
 *
 * Remarks:
 *  Function takes pointer to an argument list, then formats the data,
 *  and writes up to `bytes` characters to the memory pointed to by
 *  `buffer`. If there is room at the end (that is, if the number of
 *  character to write is less than `bytes`), the buffer will be null-terminated.
 */
int platform_vsnprintf(char *buffer, size_t bytes, const char *format, va_list arg);

/*
 * Function: platform_vsscanf
 *  Reads formatted data from a string.
 *
 * Parameters:
 *  buffer - Stored data to read.
 *  format - Format specification.
 *  arg    - Variable argument list.
 *
 * Returns:
 *  The number of fields that are successfully converted and assigned;
 *  the return value does not include fields that were read but not
 *  assigned. A return vlaue of 0 indicated that no fields were assigned.
 *  The return value if EOF for error or if the end of the string is
 *  reached before the first conversion.
 *
 * Remarks:
 *  Reads data from `buffer` into the locations that are given by each
 *  argument in the `arg` argument list. Every argument in the list must
 *  be a pointer to a variable that has a type that corresponds to a
 *  type specifier in `format`. The `format` argument controls th
 *  interpretation of the input fields and has the same form and function
 *  as the `format` argument for the *scanf* function. If copying takes
 *  place between strings that overlap, the behaviour is undefined.
 */
int platform_vsscanf(const char *buffer, const char *format, va_list arg);

/*
 * Function: platform_localtime
 *  Convert a time value and correct for the local time zone.
 *
 * Parameters
 *  timer - Pointer to stored time.
 *
 * Returns:
 *  A pointer to a structure result, or NULL if the date passed to
 *  the function is before midnight, January 1, 1970.
 */
const struct tm *platform_localtime(const time_t *timer);

/*
 * Function: platform_ctime
 *  Convert a time value to a string and adjust for local time zone
 *  settings.
 *
 * Parameters:
 *  timer - Pointer to stored time.
 *
 * Returns:
 *  Pointer to the character string result. NULL will be returned if time
 *  represents a date before midnight, January 1, 1970, UTC.
 *
 * Remarks:
 *  Converts a time value stored as a `time_t` value into a chracter string.
 *  The `timer` value is usually obtained from a call to *time*, which returns
 *  the number of seconds since midnight, January 1, 1970 UTC. The return
 *  value of the string contains exactly 26 characters. A 24-hour clock is used.
 *  All fields have constant width. The newline chracter and the null character
 *  occupy the last two positions of the string. The converted character string
 *  is also adjusted according to the local time zone settings.
 */
const char *platform_ctime(const time_t *timer);

/*
 * Function: platform_strncat
 *  Append characters of a string.
 *
 * Parameters:
 *  dest - Null terminated destination string
 *  src  - Source string
 *  num  - Number of characters to append
 *
 * Returns:
 *  Pointer to the destination string. No return value is used to indicate
 *  an error.
 *
 * Remarks:
 *  Function appends, at mode, the first `num` characters of `src` to
 *  `dest`. The initial character of `src` overwrites the terminating
 *  null chracter of `dest`. If a null character appears in `src` before
 *  `num` chracters are appended, `platform_strncat` appends all chracters
 *  from `src`, up to the null chracter. If `num` is greater than the
 *  length of `src`, the length of `src` is used in place of `num`.
 */
char *platform_strncat(char *dest, const char *src, size_t num);

/*
 * Function: platform_getenv
 *  Get a value from the current enviroment.
 *
 * Parameters:
 *  var - Enviroment variable name
 *
 * Returns:
 *  A pointer to the enviroment table entry containing `var. It's not
 *  safe to modify the value of the enviroment variable using the returned
 *  pointer. The return value is *NULL* if `var` is not found in the
 *  enviroment table.
 */
const char *platform_getenv(const char *var);

/*
 * Function: platform_vasprintf
 *  Print to allocated string
 *
 * Parameters:
 *  dat  - Pointer to pointer to store allocated data.
 *  fmt  - Format specification.
 *  args - Variable argument list.
 *
 * Returns:
 *  Number of character written, -1 is used to indicate an error.
 *
 * Remarks:
 *  Allocate a string large enough to hold the output including
 *  the terminating null character than write formatted output
 *  to it using format specification.
 */
int platform_vasprintf(char **dat, const char *fmt, va_list args);

/*
 * Function: platform_vfprintf
 *  Write formatted output using a pointer to a list of arguments.
 *
 * Parameters:
 *  stream - Pointer to stream.
 *  format - Format specification.
 *  atrg   - Variable argument list.
 *
 * Returns:
 *  Number of characters written, not including the terminating null
 *  character, or a negitive value if an output error occurs. -1 is
 *  also used to indicate an error.
 *
 * Remarks:
 *  Takes a pointer to an argument list, then formats and writes the
 *  given data to `stream`.
 */
int platform_vfprintf(FILE *stream, const char *format, va_list arg);

/*
 * Function: platform_strcat
 *  Append characters of a string.
 *
 * Parameters:
 *  dest - Null terminated destination string
 *  src  - Source string
 *
 * Returns:
 *  Pointer to the destination string. No return value is used to indicate
 *  an error.
 *
 * Remarks:
 *  Appens `src` to `dest` and terminates with resulting null character.
 *  The initial character of `src` overwrites the terminating null
 *  character of `dest`. The behaviour of platform_strcat is undefined
 *  if the source and destination string overlap.
 */
char *platform_strcat(char *dest, const char *src);

/*
 * Function: platform_strncpy
 *  Copys characters of one string to another.
 *
 * Parameters:
 *  dest - Destination string.
 *  src  - Source string.
 *  num  - Number of characters to be copied.
 *
 * Returns:
 *  `dest`. No return value is reserved to indicate an error.
 *
 * Remarks:
 *  Copies the initial characters of `src` to `dest` and returns `dest`.
 *  If `num` is less than or equal to the length of `src1 a null character
 *  is not appended automatically to the copied string. If `num` is greater
 *  than the length of `src`, the destination string is padded with null
 *  characters up to length `num`. The behaviour of this function is undefined
 *  if the source and destination strings overlap.
 */
char *platform_strncpy(char *dest, const char *src, size_t num);

/*
 * Function: platform_strerror
 *  Get a system error message
 *
 * Parameters:
 *  err - Error number.
 *
 * Returns:
 *  A pointer to the error message
 */
const char *platform_strerror(int err);

/*
 * Function: platform_fopen
 *  Opens a file
 *
 * Parameters:
 *  filename - File name.
 *  mode     - Kind of access that's enabled.
 *
 * Returns:
 *  A pointer to the open file. A null pointer value indicates an error.
 */
FILE *platform_fopen(const char *filename, const char *mode);

/*
 * Function: platform_fread
 *  Reads data from a stream
 *
 * Parameters:
 *  ptr    - Storage location for data.
 *  size   - Item size in bytes.
 *  count  - Maximum number of items to be read.
 *  stream - Pointer to stream
 *
 * Returns:
 *  The number of full items actually read, which may be less than `count`
 *  if an error occurs or if the end of the file is encountered before
 *  reaching `count`. If `size` or `count` is 0, `platform_fread`
 *  returns 0 and the buffer contents are unchanged.
 */
size_t platform_fread(void *ptr, size_t size, size_t count, FILE *stream);

/*
 * Function: platform_fwrite
 *  Writes data to a stream
 *
 * Parameters:
 *  ptr    - Pointer to data to be written.
 *  size   - Item size in bytes.
 *  count  - Maximum number of items to be read.
 *  stream - Pointer to stream
 *
 * Returns:
 *  The number of full items actually written, which may be less than
 *  `count` if an error occurs. Also, if an error occurs, the
 *  file-position indicator cannot be determined.
 *
 * Remarks:
 *  Writes up to `count` items, of `size` length each, from `ptr` to the
 *  output stream. The file pointer associated with stream (if there is one)
 *  is incremented by the number of bytes actually written.
 */
size_t platform_fwrite(const void *ptr, size_t size, size_t count, FILE *stream);

/*
 * Function: platform_fflush
 *  Flushes a stream
 *
 * Parameters:
 *  stream - Pointer to stream
 *
 * Returns:
 *  0 value if the buffer was succesffuly flushed. The value 0 is also
 *  returned in cases in which the specified stream has no buffer or is
 *  open for reading only. A return value of *EOF* indicates an error.
 *
 * Remarks:
 *  Flushes a stream. If the file associated with stream is open for output,
 *  platform_fflush writes to that file the contents of the buffer
 *  associated with the stream. If the stream is open for input,
 *  platform_fflush clears the contents of the buffer. platform_fflush
 *  negates the effect of any prior call to ungetc against stream. Also,
 *  platform_fflush(NULL) flushes all streams opened for output.
 *  The stream remains open after the call. platform_fflush has no effect
 *  on an unbuffered stream.
 */
int platform_fflush(FILE *stream);

/*
 * Function: platform_fclose
 *  Closes a stream.
 *
 * Parameters:
 *  stream - Pointer to stream.
 *
 * Returns:
 *  0 value. *EOF* is used to indicate an error.
 *
 * Remarks:
 *  Closes a stream.
 */
int platform_fclose(FILE *stream);

/*
 * Function: platform_ferror
 *  Tests for an error on a stream.
 *
 * Parameters:
 *  stream - Pointer to stream.
 *
 * Returns:
 *  If not error has occured on `stream`, 0 value is returned, otherwise
 *  it returns a nonzero value.
 *
 * Remarks:
 *  Tests for a reading or writing error on the file associated with `stream`.
 *  If an error has occured, the error indicator for the stream remains set
 *  until the stream is closed or rewound.
 */
int platform_ferror(FILE *stream);

/*
 * Function: platform_fgetc
 *  Read a character from a stream.
 *
 * Parameters:
 *  stream - Pointer to a stream.
 *
 * Returns:
 *  The chracter read as an int or EOF to indicate an error or end-of-file.
 *
 * Remarks:
 *  Reads a single chracter from the current position of the file associated
 *  with `stream`. Than increments that position. If the steam is at the end
 *  of the file, the end-of-file indicator for the stream is set.
 */
int platform_fgetc(FILE *stream);

/*
 * Function: platform_fputs
 *  Write a string to a stream
 *
 * Parameters:
 *  str    - Output string.
 *  stream - Pointer to stream.
 *
 * Returns:
 *  Non-negative value if successful. EOF is used to indicate an error.
 *
 * Remarks:
 *  Copies `str` to the output stream at the current position.
 */
int platform_fputs(const char *str, FILE *stream);

/*
 * Function: platform_fseek
 *  Moves the file pointer to a specified location.
 *
 * Parameters:
 *  stream - Pointer to stream.
 *  offset - Number of bytes from origin to offset.
 *  origin - Initital position.
 *
 * Returns:
 *  0 value, nonzero values are used to indicate an error.
 *
 * Remarks:
 *  Moves the file pointer (if any) associated with stream to a new
 *  location that is offset bytes from origin.
 *  The next operation on the stream takes place at the new location.
 *  On a stream open for update, the next operation can be either a
 *  read or a write.
 */
int platform_fseek(FILE *stream, long offset, int origin);

/*
 * Function: platform_ftell
 *  Gets the current position of a file pointer
 *
 * Parameters:
 *  stream - Pointer to stream
 *
 * Returns:
 *  Current file position. May not reflect physical byte offset, e.g
 *  systems where read-mode does carriage return-linefeed translation.
 *  -1 value is used to indivate an error.
 */
long platform_ftell(FILE *stream);

/*
 * Function: platform_mkdir
 *  Make a directory
 *
 * Parameters:
 *  path    - Path to create
 *  mode    - The mode of the directory (implementation defined)
 *
 * Returns:
 *  0 value. -1 value is used to indicate an error. On error no
 *  directory shall be created.
 *
 * Remarks:
 *  Shall create a new empty directory with with the name path specified
 *  by argument `path.
 */
int platform_mkdir(const char *path, int mode);

/*
 * Function: platform_opendir
 *  Open a directory
 *
 * Parameters:
 *  path - Path to the directory to open
 *
 * Returns:
 *  Pointer to an object of type *DIR*. A null pointer value indicates
 *  an error.
 *
 * Remarks:
 *  Shall open a directory stream corresponding to the directory named by
 *  the `path` argument. The directory stream is positioned at the first entry.
 */
DIR *platform_opendir(const char *path);

/*
 * Function: platform_closedir
 *  Close a directory stream
 *
 * Parameters:
 *  dir - Pointer to directory stream
 *
 * Returns:
 *  0 value. A -1 value indicated an error.
 *
 * Remarks:
 *  Shall close the directory stream referred to by the argument
 *  `dir`. Upon return, the value of `dir` may no longer point to
 *  an accessible object of the type *DIR*.
 */
int platform_closedir(DIR *dir);

/*
 * Function: platform_readdir
 *  Read directory
 *
 * Parameters:
 *  dir - Pointer to directory stream
 *
 * Returns:
 *  Pointer to an object of type `struct dirent`. A null pointer value
 *  indicates an error.
 *
 * Remarks:
 *  When the end of the directory is encountered, a null pointer is
 *  returned.
 */
struct dirent *platform_readdir(DIR *dir);

/*
 * Function: platform_isatty
 *  Determines whether a file descriptor is associated with a character
 *  device.
 *
 * Returns:
 *  A nonzero value if the descriptor is associated with a character
 *  device. Otherwise `platform_isatty` returns 0.
 */
int platform_isatty(int fd);

#endif
