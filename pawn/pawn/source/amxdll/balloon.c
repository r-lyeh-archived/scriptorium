#define STRICT
#include <windows.h>
#include <string.h>
#if defined(CPHASE)
  #include <include/osdefs.h>
  #include <assert/assert.h>
#else
  #include "osdefs.h"
  #include <assert.h>
  #define ASSERT(x)     assert(x)
#endif
#include "balloon.h"

#define BM_MARGINX      6
#define BM_MARGINTOP    2       // the internal leading in the font is on top
#define BM_MARGINBOTTOM 3
#define BM_RADIUS       16      // radius for rounded rectangles (set to 0 for rectangles)
#define BM_OFFSET       10
#define BM_OFFSET_TAIL  0
#define CLASSNAME_TOOLTIP "CP_Balloon"

static ATOM atomAlignStyle;
static ATOM atomTailSize;
static ATOM atomTimeout;
static ATOM atomFont;

LRESULT CALLBACK _export BalloonProc(HWND hWnd, UINT message, WPARAM wParam,
                                     LPARAM lParam);
static HRGN MakeBalloonRegion(int width,int height,int radius,int AlignStyle,int TailSize,
                              int TailX,int TailY);


HWND bm_Create(HINSTANCE hInstance, HWND hwndParent)
{
static BOOL is_init=FALSE;
  WNDCLASS wc;

  if (!is_init) {
    // Fill in the window class structure that describes the "tool tip"
    wc.style = CS_GLOBALCLASS;
    wc.lpfnWndProc = BalloonProc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.hInstance = hInstance;
    wc.hIcon = NULL;
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = 0;
    wc.lpszMenuName = NULL;
    wc.lpszClassName = CLASSNAME_TOOLTIP;
    if (!RegisterClass(&wc))
      return FALSE;
    // register atoms for window properties
    atomAlignStyle=GlobalAddAtom("ttip:AlignStyle");
    atomTailSize=GlobalAddAtom("ttip:TailSize");
    atomTimeout=GlobalAddAtom("ttip:atomTimeout");
    atomFont=GlobalAddAtom("ttip:Font");
  } /* if */
  is_init=TRUE;

  return CreateWindow(CLASSNAME_TOOLTIP, "", WS_POPUP,
                      0, 0, 0, 0, hwndParent, NULL, hInstance, NULL);
}

void bm_Cleanup(HINSTANCE hInstance)
{
  GlobalDeleteAtom(atomAlignStyle);
  GlobalDeleteAtom(atomTailSize);
  GlobalDeleteAtom(atomTimeout);
  GlobalDeleteAtom(atomFont);
  UnregisterClass(CLASSNAME_TOOLTIP,hInstance);
}

LRESULT CALLBACK _export BalloonProc(HWND hwnd, UINT message, WPARAM wParam,
                                     LPARAM lParam)
{
static DWORD lasttime;  // ??? should be a property
static POINT ttpos;
  RECT rect;
  POINT point;
  int x, y, width, height;
  SIZE size;
  LPSTR ptr;
  char text[256];

  switch (message) {
  case WM_CREATE: {
    HFONT hfont = CreateFont(-11, 0, 0, 0, FW_NORMAL, FALSE, 0, 0, DEFAULT_CHARSET,
                             OUT_DEFAULT_PRECIS, CLIP_CHARACTER_PRECIS,
                             PROOF_QUALITY, FF_DONTCARE, "MS Sans Serif");
    SetProp(hwnd,MAKEINTRESOURCE(atomFont),hfont);
    break;
  } /* case */

  case WM_DESTROY: {
    HFONT hfont = (HFONT)GetProp(hwnd,MAKEINTRESOURCE(atomFont));
    if (hfont)
      DeleteObject(hfont);
    RemoveProp(hwnd,MAKEINTRESOURCE(atomAlignStyle));
    RemoveProp(hwnd,MAKEINTRESOURCE(atomTailSize));
    RemoveProp(hwnd,MAKEINTRESOURCE(atomTimeout));
    RemoveProp(hwnd,MAKEINTRESOURCE(atomFont));
    break;
  } /* case */

  case WM_ERASEBKGND:
    return 0L;

  case WM_LBUTTONDOWN:
  case WM_RBUTTONDOWN:
  case WM_LBUTTONUP:
  case WM_RBUTTONUP:
    ShowWindow(hwnd, SW_HIDE);
    if (IsWindow(GetParent(hwnd)))
      PostMessage(GetParent(hwnd), BAN_VISIBLE, (WPARAM)hwnd, 0L);
    break;

  case WM_WINDOWPOSCHANGING:
  case WM_WINDOWPOSCHANGED:
    return 0L;

  case WM_PAINT: {
    PAINTSTRUCT ps;
    COLORREF clrBackgr;
    HBRUSH hbrBackgr;
    HRGN hrgn;
    int AlignStyle=(int)GetProp(hwnd,MAKEINTRESOURCE(atomAlignStyle));
    HFONT hfont = (HFONT)GetProp(hwnd,MAKEINTRESOURCE(atomFont));

    HDC hdc = BeginPaint(hwnd, &ps);
    GetClientRect(hwnd, &rect);

    clrBackgr=GetNearestColor(hdc, RGB(255,255,192));
    hbrBackgr = CreateSolidBrush(clrBackgr);
    #if defined __WIN32__
      hrgn=CreateRectRgn(0,0,1,1);
      GetWindowRgn(hwnd,hrgn);      /* Win32-only */
    #else
      #if BM_RADIUS==0
        hrgn=CreateRectRgn(rect.left, rect.top, rect.right, rect.bottom);
      #else
        hrgn=CreateRoundRectRgn(rect.left, rect.top, rect.right, rect.bottom, BM_RADIUS, BM_RADIUS);
      #endif
    #endif
    FillRgn(hdc,hrgn,hbrBackgr);
    FrameRgn(hdc,hrgn,GetStockObject(BLACK_BRUSH),1,1);
    DeleteObject(hbrBackgr);

    hfont = SelectObject(hdc, hfont);
    GetWindowText(hwnd, text, sizeof text);
    GetTextExtentPoint(hdc, text, lstrlen(text), &size);
    SetBkMode(hdc, TRANSPARENT);
    y=BM_MARGINTOP;
    if (AlignStyle==BAA_TOP) {
      int TailSize=(int)GetProp(hwnd,MAKEINTRESOURCE(atomTailSize));
      y+=TailSize;
    } /* if */
    for (ptr=strtok(text,"\n"); ptr!=NULL; ptr=strtok(NULL,"\n"), y+=size.cy) {
      GetTextExtentPoint(hdc, ptr, lstrlen(ptr), &size);
      x = ((rect.right-rect.left) - size.cx) / 2;
      TextOut(hdc, x, y, ptr, lstrlen(ptr));
    } /* for */
    SelectObject(hdc, hfont);

    EndPaint(hwnd, &ps);
    break;
  } /* case */

  case WM_TIMER: {
    DWORD timeout=(DWORD)GetProp(hwnd,MAKEINTRESOURCE(atomTimeout));
    DWORD curtime=GetTickCount();
    GetCursorPos(&point);
    if (timeout==0)
      break;
    if (curtime >= lasttime+timeout)
      SendMessage(hwnd, BAM_SHOW, 0, 0L);
    break;
  } /* if */

  case BAM_SETPOS:
    ttpos=*(LPPOINT)lParam;
    break;

  case BAM_SHOW:
    if (wParam) {
      HDC hdc;
      HRGN hrgn;
      int AlignStyle=(int)GetProp(hwnd,MAKEINTRESOURCE(atomAlignStyle));
      int TailSize=(int)GetProp(hwnd,MAKEINTRESOURCE(atomTailSize));
      HFONT hfont=(HFONT)GetProp(hwnd,MAKEINTRESOURCE(atomFont));
      ASSERT(lParam!=0);
      lstrcpy(text, (LPSTR)lParam);
      SetWindowText(hwnd, text);
      InvalidateRect(hwnd, NULL, FALSE);
      hdc = GetDC(hwnd);
      hfont = SelectObject(hdc, hfont);
      for (ptr=strtok(text,"\n"), width=height=0; ptr!=NULL; ptr=strtok(NULL,"\n")) {
        GetTextExtentPoint(hdc, ptr, lstrlen(ptr), &size);
        width = max(width, size.cx);
        height += size.cy;
      } /* for */
      SelectObject(hdc, hfont);
      ReleaseDC(hwnd, hdc);
      width += 2*BM_MARGINX;
      height += BM_MARGINTOP + BM_MARGINBOTTOM;
      #if 0
        // round width up to multiple of 16 pixels and height up to a multiple
        // of 8 pixels
        width+=16-1;
        width=width - (width%16);
        height+=8-1;
        height=height - (height%8);
      #endif
      // also add the tail size to the height
      height+=TailSize;
      // check alignment
      switch (AlignStyle) {
      case BAA_TOP:
        x = ttpos.x - width/4;
        y = ttpos.y;
        break;
      case BAA_BOTTOM:
        x = ttpos.x - width/4;
        y = ttpos.y - height;
        break;
      } /* switch */
      // check screen position against the screen size (width)
      if (x < 0)
        x = 0;
      else if (x+width >= GetSystemMetrics(SM_CXSCREEN)-1)
        x = GetSystemMetrics(SM_CXSCREEN) - width - 1;
      if (y < 0)
        y = 0;
      else if (y+height >= GetSystemMetrics(SM_CYSCREEN)-1)
        y = GetSystemMetrics(SM_CYSCREEN) - height - 1;
      GetWindowRect(hwnd,&rect);  // get first, check later (for new region)
      SetWindowPos(hwnd, HWND_TOPMOST, x, y, width, height, SWP_SHOWWINDOW|SWP_NOACTIVATE);
      hrgn=MakeBalloonRegion(width+1,height+1,BM_RADIUS,AlignStyle,TailSize,ttpos.x-x,ttpos.y-y);
      ASSERT(hrgn!=NULL);
      SetWindowRgn(hwnd, hrgn, TRUE);
      UpdateWindow(hwnd);
      lasttime=GetTickCount();
    } else {
      ShowWindow(hwnd, SW_HIDE);
      lasttime=GetTickCount();
    } /* if */
    break;

  default:
    return DefWindowProc(hwnd, message, wParam, lParam);
  } /* switch */
  return 0L;
}

BOOL bm_Set(HWND hwndBalloon,LPSTR Text,LPPOINT Pointer)
{
  POINT pt;

  if (Pointer==NULL) {
    // apply an offset to the mouse position, depending on the alignment and
    // the presence of the tail
    int AlignStyle=(int)GetProp(hwndBalloon,MAKEINTRESOURCE(atomAlignStyle));
    int TailSize=(int)GetProp(hwndBalloon,MAKEINTRESOURCE(atomTailSize));
    int offset= (TailSize>0) ? BM_OFFSET_TAIL : BM_OFFSET;
    GetCursorPos(&pt);  // get mouse position (in screen coordinates)
    switch (AlignStyle) {
    case BAA_TOP:
      pt.y += offset;
      break;
    case BAA_BOTTOM:
      pt.y -= offset;
      break;
    } /* switch */
  } else {
    pt=*Pointer;
  } /* if */

  if (Text!=NULL && lstrlen(Text)>0) {
    SendMessage(hwndBalloon,BAM_SETPOS,0,(LPARAM)(LPVOID)&pt);
    SendMessage(hwndBalloon,BAM_SHOW,TRUE,(LPARAM)Text);
  } else {
    SendMessage(hwndBalloon,BAM_SHOW,FALSE,0);
  } /* if */
  return TRUE;
}

int bm_SetAlign(HWND hwndBalloon, int style)
{
  int prev = (int)GetProp(hwndBalloon,MAKEINTRESOURCE(atomAlignStyle));
  SetProp(hwndBalloon,MAKEINTRESOURCE(atomAlignStyle),(HANDLE)style);
  return prev;
}

int bm_SetTail(HWND hwndBalloon, int height)
{
  int prev = (int)GetProp(hwndBalloon,MAKEINTRESOURCE(atomTailSize));
  SetProp(hwndBalloon,MAKEINTRESOURCE(atomTailSize),(HANDLE)height);
  return prev;
}

DWORD bm_SetTimeout(HWND hwndBalloon, DWORD milliseconds)
{
  DWORD prev = (DWORD)GetProp(hwndBalloon,MAKEINTRESOURCE(atomTimeout));
  SetProp(hwndBalloon,MAKEINTRESOURCE(atomTimeout),(HANDLE)milliseconds);
  if (milliseconds!=0)
    SetTimer(hwndBalloon,1,100,NULL);
  else
    KillTimer(hwndBalloon,1);
  return prev;
}

HFONT bm_SetFont(HWND hwndBalloon, HFONT hfont)
{
  HFONT prev = GetProp(hwndBalloon,MAKEINTRESOURCE(atomFont));
  SetProp(hwndBalloon,MAKEINTRESOURCE(atomFont),hfont);
  return prev;
}

static HRGN MakeBalloonRegion(int width,int height,int radius,int AlignStyle,
                              int TailSize,int TailX,int TailY)
{
  POINT Tail[3];
  HRGN hrgnRect,hrgnTail;
  int top,bottom,rad;

  // attach the tail to the balloon
  switch (AlignStyle) {
  case BAA_TOP:
    top=TailSize;
    bottom=height;
    Tail[0].y=Tail[2].y=top;
    break;
  case BAA_BOTTOM:
    top=0;
    bottom=height-TailSize;
    Tail[0].y=Tail[2].y=bottom-1;
    break;
  } /* switch */
  // point tail to left/right, depending on its position
  rad=(radius+1)/2;                     // ellipse diameter to radius (round up)
  Tail[0].x= (TailX>width/2+8) ? TailX-TailSize-4 : TailX+TailSize;
  if (Tail[0].x>width-rad-TailSize)     // clamp base of tail inside the balloon
    Tail[0].x=width-rad-TailSize;
  if (Tail[0].x<rad)
    Tail[0].x=rad;
  Tail[2].x=Tail[0].x+TailSize;
  if (Tail[2].x>width-rad)              // adjust for very narrow balloons
    Tail[2].x=width-rad;
  Tail[1].x=TailX;
  Tail[1].y=TailY;

   // create combined region
   if (radius<=0)
     hrgnRect=CreateRectRgn(0,top,width,bottom);
   else
     hrgnRect=CreateRoundRectRgn(0,top,width,bottom,radius,radius);
   hrgnTail=CreatePolygonRgn(Tail,3,ALTERNATE);
   CombineRgn(hrgnRect,hrgnRect,hrgnTail,RGN_OR);

   DeleteObject(hrgnTail);
   return hrgnRect;
}
