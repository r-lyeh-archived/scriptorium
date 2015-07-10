#if !defined _BALLOON_H
#define _BALLOON_H

#if defined __cplusplus
  extern "C" {
#endif

#define BAM_SHOW        (WM_USER+0x110) // show or hide
#define BAM_SETPOS      (WM_USER+0x111) // set tooltip position
#define BAN_QUERY       (WM_USER+0x112) // ToolTip notify, tooltip has been shown for some time now
#define BAN_VISIBLE     (WM_USER+0x113) // notify: window was hidden by clicking into it


/* Creates a hidden tooltip window. You may call this function multiple times,
 * to get multiple tooltip windows.
 */
HWND bm_Create(HINSTANCE hInstance, HWND hwndParent);


/* It is usually unnecessary to call this function explicitly. If used, call
 * this function after removing the last tooltip control. It removes the
 * window class and global atoms.
 */
void bm_Cleanup(HINSTANCE hInstance);


/* hwndBalloon  - the handle returned by InitToolTipControl()
 * Text         - the text to set (and show the tooltip), or NULL to hide the tooltip
 * Pointer      - position of the tooltip in screen coordinates; or NULL to use the mouse cursor position
 */
BOOL bm_Set(HWND hwndBalloon, LPSTR Text, LPPOINT Pointer);


/* bm_SetAlign() returns the previous style value. To query the current
 * style without changing it, set "style" to -1.
 * The alignment refers to the position if the "tail" relative to the window.
 * In a normal operation, the tail points to the mouse cursor, or the object
 * that the tooltip refers to.
 */
enum {                  /* Tooltip align styles (default = BAA_TOP) */
  BAA_TOP,
  BAA_BOTTOM,
};
int bm_SetAlign(HWND hwndBalloon, int style);

int bm_SetTail(HWND hwndBalloon, int height);

DWORD bm_SetTimeout(HWND hwndBalloon, DWORD milliseconds);

/* bm_SetFont() returns the previous font handle (which you may want to
 * delete). The new font handle that you pass in to this function should NOT
 * be deleted as long as the tooltip window exists.
 */
HFONT bm_SetFont(HWND hwndBalloon, HFONT hfont);

#if defined __cplusplus
  }
#endif

#endif  /* _BALLOON_H */
