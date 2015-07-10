/*  Abstract Machine DLL interface functions
 *
 *  Copyright (c) ITB CompuPhase, 1999-2010
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
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <assert.h>
#include <ctype.h>      /* for isdigit() */
#include <limits.h>
#include <malloc.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>     /* for atoi() */
#include <string.h>

/* redirect amx_Init() to a special version */
#if !defined amx_Init
  #error "amx_Init" must be defined when compiling these sources
#endif
#undef amx_Init /* redirection is over here */

#include "osdefs.h"
#include "balloon.h"
#include "termwin.h"
#include "amx.h"


int AMXAPI amx_InitAMX(AMX *amx, void *program);
static cell AMX_NATIVE_CALL _messagebox(AMX *amx,const cell *params);
static cell AMX_NATIVE_CALL _iswin32(AMX *amx,const cell *params);
static cell AMX_NATIVE_CALL CallDLL(AMX *amx,const cell *params);
static cell AMX_NATIVE_CALL LoadDLL(AMX *amx,const cell *params);
static cell AMX_NATIVE_CALL FreeDLL(AMX *amx,const cell *params);
#if defined __WIN32__ || defined __NT__ || defined _WIN32
  static cell AMX_NATIVE_CALL BalloonSet(AMX *amx,const cell *params);
  static cell AMX_NATIVE_CALL BalloonFont(AMX *amx,const cell *params);
#endif

#if defined _Windows
  static HINSTANCE hinstAMX;
#endif

AMX_NATIVE_INFO dll_Natives[] = {
  { "messagebox", _messagebox },
  { "iswin32", _iswin32 },
  { "calldll", CallDLL },
  { "loaddll", LoadDLL },
  { "freedll", FreeDLL },
#if defined __WIN32__ || defined __NT__ || defined _WIN32
  { "balloon", BalloonSet },
  { "balloonfont", BalloonFont },
#endif
  { NULL, NULL }        /* terminator */
};

int AMXAPI amx_CoreInit(AMX *amx);
int AMXAPI amx_ConsoleInit(AMX *amx);
int AMXAPI amx_FixedInit(AMX *amx);


int AMXAPI amx_Init(AMX *amx, void *program)
{
  int err;

  if ((err=amx_InitAMX(amx,program)) != AMX_ERR_NONE)
    return err;

  /* load standard libraries */
  amx_CoreInit(amx);
  amx_ConsoleInit(amx);
  amx_FixedInit(amx);
  err = amx_Register(amx,dll_Natives,-1);

  return err;
}

#if defined _Windows

/* enum { Ok, Okcancel, okCancel, Yesno, yesNo, Yesnocancel, yesNocancel, yesnoCancel }
 * enum { noicon, information, exclamation, question, stop }
 * messagebox(message[], caption[], buttons=Ok, icons=noicon, timeout=0)
 */
static cell AMX_NATIVE_CALL _messagebox(AMX *amx,const cell *params)
{
static long btn_style[] = { MB_OK, MB_OKCANCEL, MB_YESNO, MB_RETRYCANCEL, MB_ABORTRETRYIGNORE };
static long icon_style[] = { MB_ICONINFORMATION, MB_ICONEXCLAMATION, MB_ICONQUESTION, MB_ICONSTOP };
  char message[256], caption[128];
  long style;
  cell *cptr;
  int len;
  HWND hwnd;

  cptr=amx_Address(amx,params[1]);
  amx_StrLen(cptr,&len);
  amx_GetString(message,cptr,0,sizeof message);

  cptr=amx_Address(amx,params[2]);
  amx_StrLen(cptr,&len);
  amx_GetString(caption,cptr,0,sizeof caption);

  style=0;
  if (params[3]>=0 && params[3]<5)
    style|=btn_style[(int)params[3]];
  if (params[4]>=0 && params[4]<4)
    style|=btn_style[(int)params[4]];

  /* remove previous messagebox */
  if ((hwnd=FindWindow("#32770",caption))!=0)
    EndDialog(hwnd,IDCANCEL);

  return MessageBox(GetFocus(),message,caption,(unsigned)style);
}

#if defined __WIN32__ || defined __NT__ || defined _WIN32
/* balloon(Balloon:&handle, Message[]="", X=0, Y=0, Timeout=-1)
 */
static cell AMX_NATIVE_CALL BalloonSet(AMX *amx,const cell *params)
{
  #if defined __WIN32__ || defined __NT__ || defined _WIN32
    char message[512];
  #else
    char message[256];
  #endif
  cell *cptr;
  int len;
  HWND hwnd;
  POINT pt;

  // get the balloon window handle, or create a new balloon
  cptr=amx_Address(amx,params[1]);
  hwnd=(HWND)*cptr;
  if (hwnd==NULL) {
    hwnd=bm_Create(hinstAMX,NULL);
    *cptr=(cell)hwnd;
    bm_SetAlign(hwnd,BAA_BOTTOM);
    bm_SetTail(hwnd,16);
  } /* if */

  cptr=amx_Address(amx,params[2]);
  amx_StrLen(cptr,&len);
  if (len==0)
    return bm_Set(hwnd,NULL,NULL);
  amx_GetString(message,cptr,0,sizeof message);

  pt.x=params[3];
  pt.y=params[4];

  if (params[5]>=0)
    bm_SetTimeout(hwnd,(DWORD)params[5]);

  return bm_Set(hwnd,message,&pt);
}

/* balloonfont(Balloon:&handle, font[]="", height=16, weight=400, italic=0)
 */
static cell AMX_NATIVE_CALL BalloonFont(AMX *amx,const cell *params)
{
  char name[128];
  cell *cptr;
  int len;
  HWND hwnd;
  HFONT hfont;

  // get the balloon window handle, or create a new balloon
  cptr=amx_Address(amx,params[1]);
  hwnd=(HWND)*cptr;
  if (hwnd==NULL) {
    hwnd=bm_Create(hinstAMX,NULL);
    *cptr=(cell)hwnd;
    bm_SetAlign(hwnd,BAA_BOTTOM);
    bm_SetTail(hwnd,16);
  } /* if */

  cptr=amx_Address(amx,params[2]);
  amx_StrLen(cptr,&len);
  if (len==0)
    return bm_Set(hwnd,NULL,NULL);
  if (len>=sizeof name)
    return FALSE;
  amx_GetString(name,cptr,0,sizeof name);

  hfont=CreateFont((int)params[3], 0, 0, 0, (int)params[4], (int)params[5],
                   0, 0, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS,
                   CLIP_CHARACTER_PRECIS, PROOF_QUALITY, FF_DONTCARE, name);
  hfont=bm_SetFont(hwnd,hfont);
  DeleteObject(hfont);
  return TRUE;
}
#endif

#pragma argsused
static cell AMX_NATIVE_CALL _iswin32(AMX *amx,const cell *params)
{
  #if defined __WIN32__ || defined __NT__ || defined _WIN32
    return 1;
  #else
    return 0;
  #endif
}


/*  push()
**
**  This function is the kind of programming trick that you don't even dare to
**  dream about! With the usual C calling convention, the caller cleans up the
**  stack after calling the function. This allows C functions to be flexible
**  with parameters, both in number and in type.
**  With the Pascal calling convention, used here, the callee (the function)
**  cleans up the stack. But here, function push() doesn't know about any
**  parameters. We neither declare any, nor indicate that the function has no
**  parameters (i.e. the function is not declared having 'void' parameters).
**  When we call function push(), the caller thinks the function cleans up the
**  stack (because of the Pascal calling convention), while the function does
**  not know that it has parameters, so it cannot clean them. As a result,
**  nobody cleans up the stack. Ergo, The parameter you pass to function push()
**  stays on the stack.
*/
static void PASCAL push() { }

FARPROC SearchProcAddress(char *dllname,char *functionname)
{
  HINSTANCE hinst;
  FARPROC lpfn;

  hinst=GetModuleHandle(dllname);
  if (hinst==NULL && strlen(dllname)<128-2) {
    char str[128];
    strcpy(str,dllname);
    #if defined __WIN32__ || defined __NT__ || defined _WIN32
      strcat(str,"32");
    #else
      strcat(str,"16");
    #endif
    hinst=GetModuleHandle(str);
  } /* if */
  if (hinst==NULL)
    return NULL;
  lpfn=GetProcAddress(hinst,functionname);
  #if defined __WIN32__
    if (lpfn==NULL && strlen(functionname)<128-1) {
      char str[128];
      strcpy(str,functionname);
      strcat(str,"A");
      lpfn=GetProcAddress(hinst,str);
    } /* if */
  #endif
  return lpfn;
}

#define MAXPARAMS 32
typedef int (CALLBACK* DLLPROC)();

static cell AMX_NATIVE_CALL CallDLL(AMX *amx,const cell *params)
{
  char dllname[128], functionname[128], typestring[MAXPARAMS];
  cell *cptr,result;
  int len,i,count,j;
  int numparams,paramidx;
  DWORD dllparms[MAXPARAMS];
  DLLPROC DLLproc;

  cptr=amx_Address(amx,params[1]);
  amx_StrLen(cptr,&len);
  if (len>=sizeof dllname)
    return amx_RaiseError(amx, AMX_ERR_NATIVE);
  amx_GetString(dllname,cptr,0,sizeof dllname);

  cptr=amx_Address(amx,params[2]);
  amx_StrLen(cptr,&len);
  if (len>=sizeof functionname)
    return amx_RaiseError(amx, AMX_ERR_NATIVE);
  amx_GetString(functionname,cptr,0,sizeof functionname);

  cptr=amx_Address(amx,params[3]);
  amx_StrLen(cptr,&len);
  if (len>=sizeof typestring)
    return amx_RaiseError(amx, AMX_ERR_NATIVE);
  amx_GetString(typestring,cptr,0,sizeof typestring);

  /* find the function */
  DLLproc=(DLLPROC)SearchProcAddress(dllname,functionname);
  if (DLLproc==NULL)
    return 0;

  /* decode the parameters */
  numparams=0;
  paramidx=0;
  while (typestring[paramidx]!='\0') {
    cptr=amx_Address(amx,params[numparams+4]);
    switch (typestring[paramidx]) {
    case 'w':
      dllparms[paramidx]=(WORD)*cptr;
      break;
    case 'i':
    case 'h':
      dllparms[paramidx]=(int)*cptr;
      break;
    case 'l':
      dllparms[paramidx]=(long)*cptr;
      break;
    case 'I':
    case 'H':
    case 'L':
    case 'W':
      dllparms[paramidx]=(DWORD)cptr;
      break;
    case 'p':
    case 'P':
    case 's':
    case 'S':
      amx_StrLen(cptr,&len);
      dllparms[paramidx]=(DWORD)(LPSTR)malloc(len+1);
      if (dllparms[paramidx]==0L)
        return amx_RaiseError(amx, AMX_ERR_NATIVE);
      amx_GetString((char *)dllparms[paramidx],cptr,0,len+1);
      break;
    default:
      return amx_RaiseError(amx, AMX_ERR_NATIVE);
    } /* switch */
    numparams++;
    paramidx++;
    /* skip digits */
    while (isdigit(typestring[paramidx]))
      paramidx++;
  } /* while */
  if ((params[0]/sizeof(cell)) - 3 != numparams)
    return amx_RaiseError(amx, AMX_ERR_NATIVE); /* format string does not match number of parameters */

  /* push the parameters to the stack (left-to-right in 16-bit; right-to-left
   * in 32-bit)
   */
  #if defined __WIN32__
    for (i=numparams-1; i>=0; i--) {
      /* push always 32-bits */
      push(dllparms[i]);
    } /* for */
  #else
    for (i=paramidx=0; i<numparams; i++) {
      switch (typestring[paramidx]) {
      case 'i':
      case 'h':
      case 'w':
        push((int)dllparms[i]);
        break;
      default:
        push(dllparms[i]);
        break;
      } /* switch */
      /* skip digits */
      paramidx++;
      while (isdigit(typestring[paramidx]))
        paramidx++;
    } /* for */
  #endif

  /* call the function; all parameters are already pushed to the stack (the
   * function should remove the parameters from the stack)
   */
  result=DLLproc();

  /* free allocated memory */
  for (i=paramidx=0; i<numparams; i++) {
    switch (typestring[paramidx]) {
    case 'p':
    case 's':
      free((char *)dllparms[i]);
      break;
    case 'P':
    case 'S':
      cptr=amx_Address(amx,params[i+4]);
      amx_SetString(cptr,(char *)dllparms[i],typestring[paramidx]=='P',0,UNLIMITED);
      free((char *)dllparms[i]);
      break;
#if !defined __WIN32__
    case 'I':
      count=isdigit(typestring[paramidx+1]) ? atoi(&typestring[paramidx+1]) : 1;
      cptr=amx_Address(amx,params[i+4]);
      for (j=count-1; j>=0; j--) {
        if ((j & 1)==0)
          cptr[j] = cptr[j/2];
        else
          cptr[j] = cptr[j/2] >> 16;
        cptr[j] = (long)((short)cptr[j]); /* sign-extent 16-bit integers */
      } /* for */
      break;
    case 'H':
      count=isdigit(typestring[paramidx+1]) ? atoi(&typestring[paramidx+1]) : 1;
      cptr=amx_Address(amx,params[i+4]);
      for (j=count-1; j>=0; j--) {
        if ((j & 1)==0)
          cptr[j] = cptr[j/2];
        else
          cptr[j] = cptr[j/2] >> 16;
        cptr[j] &= 0x0000ffffL;           /* clear high bits of 16-bit words */
      } /* for */
      break;
#endif
    case 'W':
      count=isdigit(typestring[paramidx+1]) ? atoi(&typestring[paramidx+1]) : 1;
      cptr=amx_Address(amx,params[i+4]);
      for (j=count-1; j>=0; j--) {
        if ((j & 1)==0)
          cptr[j] = cptr[j/2];
        else
          cptr[j] = cptr[j/2] >> 16;
        cptr[j] &= 0x0000ffffL;           /* clear high bits of 16-bit words */
      } /* for */
      break;
    } /* switch */
    /* skip digits */
    paramidx++;
    while (isdigit(typestring[paramidx]))
      paramidx++;
  } /* for */

  return result;
}

static HINSTANCE int_LoadLibrary(char *dllname)
{
  HINSTANCE hinstDLL;
  hinstDLL = LoadLibrary(dllname);
  #if !defined __WIN32__
    if ((WORD)hinstDLL<=32)
      hinstDLL=0;
  #endif
  return hinstDLL;
}

static cell AMX_NATIVE_CALL LoadDLL(AMX *amx,const cell *params)
{
  char dllname[128];
  cell *cptr;
  int len;
  HINSTANCE hinstDLL;

  cptr=amx_Address(amx,params[1]);
  amx_StrLen(cptr,&len);
  if (len>=sizeof dllname)
    return amx_RaiseError(amx, AMX_ERR_NATIVE);
  amx_GetString(dllname,cptr,0,sizeof dllname);

  hinstDLL = int_LoadLibrary(dllname);
  if (hinstDLL==0) {
    #if defined __WIN32__ || defined __NT__ || defined _WIN32
      strcat(dllname,"32");
    #else
      strcat(dllname,"16");
    #endif
    hinstDLL = int_LoadLibrary(dllname);
  } /* if */

  return hinstDLL!=0;
}

static cell AMX_NATIVE_CALL FreeDLL(AMX *amx,const cell *params)
{
  char dllname[128];
  cell *cptr;
  int len;
  HINSTANCE hinstDLL;

  cptr=amx_Address(amx,params[1]);
  amx_StrLen(cptr,&len);
  if (len>=sizeof dllname)
    return amx_RaiseError(amx, AMX_ERR_NATIVE);
  amx_GetString(dllname,cptr,0,sizeof dllname);

  hinstDLL=GetModuleHandle(dllname);
  if (hinstDLL==NULL) {
    #if defined __WIN32__ || defined __NT__ || defined _WIN32
      strcat(dllname,"32");
    #else
      strcat(dllname,"16");
    #endif
  } /* if */
  if (hinstDLL==NULL)
    return FALSE;

  #if defined __WIN32__
    return FreeLibrary(hinstDLL);
  #else
    FreeLibrary(hinstDLL);
    return TRUE;
  #endif
}

#endif // _Windows

#if defined(__WIN32__)

  #if defined(__WATCOMC__)
    #define DllMain(h,dw,lp)    LibMain(h,dw,lp)
  #endif

#pragma argsused
BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD dwReason, LPVOID lpRes)
{
  switch (dwReason) {
  case DLL_PROCESS_ATTACH:
    hinstAMX=hinstDLL;
    break;
  case DLL_PROCESS_DETACH:
    break;
  } /* switch */
  return TRUE;
}

#else

#pragma argsused
int FAR PASCAL LibMain(HINSTANCE hinstDLL, WORD wDataSeg, WORD wHeapSize, LPSTR lpszCmdLine)
{
  hinstAMX=hinstDLL;
  return(1);
}

#pragma argsused
int FAR PASCAL _export WEP(int param)
{
  UnregisterClass("AMX_console",hinstAMX);
  return(1);
}

size_t wcslen(const unsigned short *string)
{
  size_t count=0;
  while (*string++!=0)
    count++;
  return count;
}
#endif

