/*  Command-line shell for the "Small" Abstract Machine, using the
 *  Microsoft Windows DLL interface.
 *
 *  Copyright (c) ITB CompuPhase, 1997-2010
 *  Copyright (c) Mark Peter, 1998-1999
 *
 *  Licensed under the Apache License, Version 2.0 (the "License"); you may not
 *  use this file except in compliance with the License. You may obtain a copy
 *  of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 *  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 *  License for the specific language governing permissions and limitations
 *  under the License.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "amx.h"


void *loadprogram(AMX *amx,char *filename)
{
  FILE *fp;
  AMX_HEADER hdr;
  void *program;

  if ( (fp = fopen( filename, "rb" )) != NULL )
  {
    fread(&hdr, sizeof hdr, 1, fp);
    if ( (program = malloc( (int)hdr.stp )) != NULL )
    {
      rewind( fp );
      fread( program, 1, (int)hdr.size, fp );
      fclose( fp );
      memset(amx, 0, sizeof *amx);
      if ( amx_Init( amx, program ) != AMX_ERR_NONE )
      {
         free( program );
         return NULL;
      }

      return program;
    } /* if */
  } /* if */
  return NULL;
}


int main(int argc,char *argv[])
{
  AMX amx;
  cell ret;
  int err;
  void *program;

  if (argc != 2 || (program = loadprogram(&amx,argv[1])) == NULL) {
    printf("Error running program!\n\n"
           "Usage: SRUN <filename>\n\n"
           "The filename must include the extension\n");
    return 1;
  } /* if */

  err = amx_Exec(&amx, &ret, AMX_EXEC_MAIN);

  if (err != AMX_ERR_NONE)
    printf("Run time error %d\n", err);
  else if (ret != 0)
    printf("%s returns %ld\n", argv[1], (long)ret);

  free(program);
  return 0;
}

