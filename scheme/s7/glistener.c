#include "glistener.h"

/* supplied by caller: help finder, evaluator, symbol table lookup
 * see s7.html#glistener and glistener.h for examples and documentation.
 */

#define strcopy(Dest, Src, Len) memcpy((void *)(Dest), (void *)(Src), Len)

struct glistener {
  GtkWidget *text, *scroller, *status;
  GtkTextBuffer *buffer;
  bool is_schemish;

  GtkTextTag *prompt_tag;
  char *prompt;
  int prompt_length;

  char **strings;
  int strings_size, strings_pos;
  bool first_time;
  char *status_message;

  GtkTextTag *flash_tag;
  int flashes;
  int flash_paren_pos;
  int flash_time;

  GtkTextTag *highlight_tag;
  int highlight_start, highlight_end;

  int insertion_position;
  GdkCursor *wait_cursor, *arrow_cursor;

  const char *(*helper)(glistener *g, const char *text);
  const char *(*checker)(glistener *g, const char *text);
  void (*evaluator)(glistener *g, const char *text);
  void (*completer)(glistener *g, bool (*symbol_func)(const char *symbol_name, void *data), void *data);
  void (*colorizer)(glistener *g, glistener_colorizer_t type, int start, int end);
  bool (*keyer)(glistener *g, GtkWidget *w, GdkEventKey *e);
};

#if ((!GTK_CHECK_VERSION(3, 0, 0))) && (!defined(GDK_KEY_Return))
  #define GDK_KEY_BackSpace GDK_BackSpace
  #define GDK_KEY_Down      GDK_Down
  #define GDK_KEY_Left      GDK_Left
  #define GDK_KEY_Return    GDK_Return
  #define GDK_KEY_Right     GDK_Right
  #define GDK_KEY_Tab       GDK_Tab
  #define GDK_KEY_Up        GDK_Up
  #define GDK_KEY_a         GDK_a
  #define GDK_KEY_b         GDK_b
  #define GDK_KEY_c         GDK_c
  #define GDK_KEY_d         GDK_d
  #define GDK_KEY_e         GDK_e
  #define GDK_KEY_f         GDK_f
  #define GDK_KEY_greater   GDK_greater
  #define GDK_KEY_k         GDK_k
  #define GDK_KEY_l         GDK_l
  #define GDK_KEY_less      GDK_less
  #define GDK_KEY_n         GDK_n
  #define GDK_KEY_p         GDK_p
  #define GDK_KEY_t         GDK_t
  #define GDK_KEY_u         GDK_u
  #define GDK_KEY_v         GDK_v
  #define GDK_KEY_w         GDK_w
  #define GDK_KEY_y         GDK_y
#endif 

#define EVENT_KEYVAL(Ev) (Ev)->keyval

#if (GTK_CHECK_VERSION(3, 0, 0) && defined(__GNUC__) && (!(defined(__cplusplus))))
  #define EVENT_STATE(Ev) ({ GdkModifierType Type;  gdk_event_get_state((GdkEvent *)Ev, &Type); Type; })
#else
  #define EVENT_STATE(Ev) (Ev)->state
#endif

#define ControlMask GDK_CONTROL_MASK
#define MetaMask GDK_MOD1_MASK


/* these are the functions we get from the caller:
 *   helper -- provide a brief string describing some entity (NULL = no help)
 *   checker -- check for obvious mistakes in an expression
 *   completer -- provide name completion (NULL = no completion)
 *   evaluator -- evaluate an expression (a C string) and normally print the result in the glistener window
 */

static const char *default_helper(glistener *g, const char *text)
{
  return(NULL);
}

void glistener_set_helper(glistener *g, const char *(*help)(glistener *g, const char *text))
{
  if (help)
    g->helper = help;
  else g->helper = default_helper;
}


static const char *default_checker(glistener *g, const char *text)
{
  return(NULL);
}

void glistener_set_checker(glistener *g, const char *(*check)(glistener *g, const char *text))
{
  if (check)
    g->checker = check;
  else g->checker = default_checker;
}


static void default_evaluator(glistener *g, const char *text)
{
  glistener_append_text(g, "\n?");
  glistener_append_prompt(g);
}

void glistener_set_evaluator(glistener *g, void (*eval)(glistener *g, const char *text))
{
  if (eval)
    g->evaluator = eval;
  else g->evaluator = default_evaluator;
}


static void default_completer(glistener *g, bool (*symbol_func)(const char *symbol_name, void *data), void *data)
{
}

void glistener_set_completer(glistener *g, void (*completer)(glistener *g, bool (*symbol_func)(const char *symbol_name, void *data), void *data))
{
  if (completer)
    g->completer = completer;
  else g->completer = default_completer;
}


static void default_colorizer(glistener *g, glistener_colorizer_t type, int start, int end)
{
}

void glistener_set_colorizer(glistener *g, void (*colorizer)(glistener *g, glistener_colorizer_t type, int start, int end))
{
  if (colorizer)
    g->colorizer = colorizer;
  else g->colorizer = default_colorizer;
}


static bool default_keyer(glistener *g, GtkWidget *w, GdkEventKey *e)
{
  return(false);
}

void glistener_set_keyer(glistener *g, bool (*key)(glistener *g, GtkWidget *w, GdkEventKey *e))
{
  if (key)
    g->keyer = key;
  else g->keyer = default_keyer;
  /* the keyer can itself block signal emission, so I think all bases are covered
   *   false -> built-in actions
   *   true -> skip built in 
   *   block/true -> block, then skip
   * the caller could simply add his signal to "key-press" in the widget, but then
   *   has less fine-tuned control of the built-in handler. 
   */
}


void glistener_is_schemish(glistener *g, bool is_schemish)
{
  g->is_schemish = is_schemish;
}


static PangoFontDescription *default_font = NULL; 
/* in case font is set before glistener is created -- this happens in Snd when the
 *   initialization script sets the listener font.  At that point in the startup process,
 *   the listener window does not exist.  Perhaps it would be safer to copy the value.
 */

void glistener_set_font(glistener *g, PangoFontDescription *font)
{
  if ((!g) || (!(g->text)))
    {
      default_font = font;
      return;
    }
  else default_font = NULL;
#if (!GTK_CHECK_VERSION(3, 0, 0))
  gtk_widget_modify_font(GTK_WIDGET(g->text), font);
#else
#if (!GTK_CHECK_VERSION(3, 16, 0))
  gtk_widget_override_font(GTK_WIDGET(g->text), font);
#endif
#endif
}


#if (!GTK_CHECK_VERSION(3, 0, 0))
static GdkColor *default_text_color = NULL;
static GdkColor *default_background_color = NULL;

void glistener_set_text_color(glistener *g, GdkColor *p)
{
  if ((g) && (g->text))
    {
      gtk_widget_modify_text(g->text, GTK_STATE_NORMAL, p);
      default_text_color = NULL;
    }
  else default_text_color = p;
}

void glistener_set_background_color(glistener *g, GdkColor *p)
{
  if ((g) && (g->text))
    {
      gtk_widget_modify_base(g->text, GTK_STATE_NORMAL, p);
      default_background_color = NULL;
    }
  else default_background_color = p;
}

#else
static GdkRGBA *default_text_color = NULL;
static GdkRGBA *default_background_color = NULL;

void glistener_set_text_color(glistener *g, GdkRGBA *p)
{
  if ((g) && (g->text))
    {
#if (!GTK_CHECK_VERSION(3, 16, 0))
      gtk_widget_override_color(g->text, GTK_STATE_FLAG_NORMAL, p);
#endif
      default_text_color = NULL;
    }
  else default_text_color = p;
}

void glistener_set_background_color(glistener *g, GdkRGBA *p)
{
  if ((g) && (g->text))
    {
#if (!GTK_CHECK_VERSION(3, 16, 0))
      gtk_widget_override_background_color(g->text, GTK_STATE_FLAG_NORMAL, p);
#endif
      default_background_color = NULL;
    }
  else default_background_color = p;
}
#endif



void glistener_post_status(glistener *g, const char *msg)
{
  if (g->status)
    {
      gtk_statusbar_pop(GTK_STATUSBAR(g->status), 1);
      gtk_statusbar_push(GTK_STATUSBAR(g->status), 1, msg);
    }
}

void glistener_clear_status(glistener *g)
{
  if (g->status)
    {
#if (GTK_CHECK_VERSION(3, 0, 0))
      gtk_statusbar_remove_all(GTK_STATUSBAR(g->status), 1);
#else
      gtk_statusbar_pop(GTK_STATUSBAR(g->status), 1);
#endif
      if (g->status_message)
	{
	  free(g->status_message);
	  g->status_message = NULL;
	}
    }
}

static void glistener_append_status(glistener *g, const char *msg)
{
  if ((g->status) && (msg))
    {
      int len;
      len = strlen(msg);
      if (g->status_message)
	{
	  char *new_msg;
	  len += (strlen(g->status_message) + 3);
	  new_msg = (char *)calloc(len, sizeof(char));
	  snprintf(new_msg, len, "%s %s", msg, g->status_message);
	  free(g->status_message);
	  g->status_message = new_msg;
	}
      else
	{
	  g->status_message = (char *)calloc(len + 1, sizeof(char));
	  strcopy(g->status_message, msg, len + 1);
	}
      gtk_statusbar_pop(GTK_STATUSBAR(g->status), 1);
      gtk_statusbar_push(GTK_STATUSBAR(g->status), 1, g->status_message);
    }
}





/* ---------------- cursor ---------------- */

void glistener_set_cursor_position(glistener *g, int position)
{
  GtkTextIter pos;
  /* -1 -> goto to end */
  if (position == -1)
    gtk_text_buffer_get_end_iter(g->buffer, &pos);
  else gtk_text_buffer_get_iter_at_offset(g->buffer, &pos, position);
  gtk_text_buffer_place_cursor(g->buffer, &pos);

  gtk_text_view_scroll_mark_onscreen(GTK_TEXT_VIEW(g->text), gtk_text_buffer_get_insert(g->buffer));
}


int glistener_cursor_position(glistener *g)
{
  GtkTextIter pos;
  gtk_text_buffer_get_iter_at_mark(g->buffer, &pos, gtk_text_buffer_get_insert(g->buffer));
  return(gtk_text_iter_get_offset(&pos));
}


static int glistener_cursor(glistener *g, GtkTextIter *cursor)
{
  gtk_text_buffer_get_iter_at_mark(g->buffer, cursor, gtk_text_buffer_get_insert(g->buffer));
  return(gtk_text_iter_get_offset(cursor));
}


void glistener_set_cursor_shape(glistener *g, GdkCursor *cursor_shape)
{
  gdk_window_set_cursor(gtk_text_view_get_window(GTK_TEXT_VIEW(g->text), GTK_TEXT_WINDOW_TEXT), cursor_shape);
}



/* ---------------- prompt ---------------- */

static void prompt_insert(glistener *g, GtkTextIter *pos, bool at_top)
{
  if (at_top)
    {
      if (g->prompt_tag)
	gtk_text_buffer_insert_with_tags(g->buffer, pos, (char *)(g->prompt + 1), -1, g->prompt_tag, NULL);
      else gtk_text_buffer_insert(g->buffer, pos, (char *)(g->prompt + 1), -1);
    }
  else
    {
      if (g->prompt_tag)
	gtk_text_buffer_insert_with_tags(g->buffer, pos, g->prompt, -1, g->prompt_tag, NULL);
      else gtk_text_buffer_insert(g->buffer, pos, g->prompt, -1);
      /* scroll fully to prompt and left so the new prompt is in view 
       */
      gtk_text_view_scroll_mark_onscreen(GTK_TEXT_VIEW(g->text), gtk_text_buffer_get_insert(g->buffer));
      gtk_adjustment_set_value(GTK_ADJUSTMENT(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(g->scroller))), 0.0);
    }
}


static GtkTextTag *default_prompt_tag = NULL;

void glistener_set_prompt_tag(glistener *g, GtkTextTag *m)
{
  if (!g)
    default_prompt_tag = m;
  else
    {
      g->prompt_tag = m;
      default_prompt_tag = NULL;
    }
  /* should this redisplay all the prompts? */
}


static char *default_prompt = NULL;

void glistener_set_prompt(glistener *g, const char *str)
{
  /* the internal version includes a preceding <cr>, and its length is in unicode terms
   *   also, if we have prompts displayed in the listener, they need to change to match
   *   the new one.
   *
   * glistener_set_prompt(g, "s7>")
   *
   * from Snd, to get a prompt of lambda: in Greek font: 
   *   (set! (listener-prompt) (string (integer->char #xce) (integer->char #xbb) #\:))
   *   (set! (listener-prompt) (byte-vector #xce #xbb (char->integer #\:)))
   * currently (see GDK_SUPER_MASK below), to type Greek characters, hold down the windoze-key
   */
  char *old_prompt;
  int old_prompt_length;

  if (!str) return;
  if (!g)
    {
      default_prompt = (char *)str;
      return;
    }
  default_prompt = NULL;
  old_prompt = g->prompt;
  old_prompt_length = g->prompt_length;

  g->prompt = (char *)calloc(strlen(str) + 2, sizeof(char));
  g->prompt[0] = '\n';
  strcat(g->prompt, str);
  g->prompt_length = g_utf8_strlen(g->prompt, -1);

  if (!g->text) return;

  if (old_prompt)
    {
      /* nothing will work if the prompt is changed in midcareer unless we remake all the preceding prompts */
      GtkTextIter scan, start, end;

      /* the first prompt does not have a <cr> so we handle it directly */
      gtk_text_buffer_get_start_iter(g->buffer, &start);
      gtk_text_buffer_get_iter_at_offset(g->buffer, &end, old_prompt_length - 1);
      gtk_text_buffer_delete(g->buffer, &start, &end);
      prompt_insert(g, &start, true);

      gtk_text_buffer_get_start_iter(g->buffer, &scan);
      gtk_text_buffer_create_mark(g->buffer, "prompt_pos", &scan, false); /* false -> "right gravity" */
      while (gtk_text_iter_forward_search(&scan, old_prompt, (GtkTextSearchFlags)0, &start, &end, NULL))
	{
	  gtk_text_buffer_move_mark_by_name(g->buffer, "prompt_pos", &end);
	  gtk_text_buffer_delete(g->buffer, &start, &end);
	  prompt_insert(g, &start, false);
	  gtk_text_buffer_get_iter_at_mark(g->buffer, &scan, gtk_text_buffer_get_mark(g->buffer, "prompt_pos"));
	}
      gtk_text_buffer_delete_mark_by_name(g->buffer, "prompt_pos");
      free(old_prompt);
    }
}


void glistener_append_prompt(glistener *g)
{
  if (g->text)
    {
      GtkTextIter end;
      gtk_text_buffer_get_end_iter(g->buffer, &end);
      prompt_insert(g, &end, false);
      gtk_text_buffer_get_end_iter(g->buffer, &end);
      gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(g->text), &end, 0.0, false, 0.0, 1.0);
    }
}


#if (!GTK_CHECK_VERSION(3, 0, 0))
/* backward search is buggy in gtk 2.20 (it's ok in gtk3 I think), and we depend on it!
 *   this code is ridiculous, but it's the least stupid thing I can find that seems to work.
 */
static gboolean prompt_backward_search(glistener *g, const GtkTextIter *iter, GtkTextIter *start, GtkTextIter *end)
{
  int cur_pos;
  GtkTextIter scan1, scan2, s1, s2;
  bool found_it = false;
  
  cur_pos = gtk_text_iter_get_offset(iter);
  gtk_text_buffer_get_start_iter(g->buffer, &scan1);
  gtk_text_buffer_get_start_iter(g->buffer, &scan2);

  while (true)
    {
      if (!gtk_text_iter_forward_search(&scan1, g->prompt, (GtkTextSearchFlags)0, &s1, &s2, NULL))
	return(found_it);
      if (gtk_text_iter_get_offset(&s2) > cur_pos)
	return(found_it);
      found_it = true;
      gtk_text_iter_forward_search(&scan2, g->prompt, (GtkTextSearchFlags)0, start, end, NULL);
      scan1 = s2;
      scan2 = s2;
    }
  return(false);
}

#else

static gboolean prompt_backward_search(glistener *g, const GtkTextIter *iter, GtkTextIter *start, GtkTextIter *end)
{
  return(gtk_text_iter_backward_search(iter, g->prompt, (GtkTextSearchFlags)0, start, end, NULL));
}
#endif


static int find_current_prompt(glistener *g)
{
  GtkTextIter it, start, end;
  int pos;
  
  pos = glistener_cursor_position(g);
  if (pos < g->prompt_length - 1)
    return(g->prompt_length - 1);

  gtk_text_buffer_get_iter_at_offset(g->buffer, &it, pos + g->prompt_length - 1);
  if (!prompt_backward_search(g, &it, &start, &end))
    return(g->prompt_length - 1);

  return(gtk_text_iter_get_offset(&start) + g->prompt_length);
}


int glistener_prompt_position(glistener *g)
{
  return(find_current_prompt(g));
}


static int find_previous_prompt(glistener *g, int pos)
{
  GtkTextIter it, start, end;
  int new_pos;
  
  if (pos < g->prompt_length - 1)
    return(g->prompt_length - 1);

  gtk_text_buffer_get_iter_at_offset(g->buffer, &it, pos);
  if (!prompt_backward_search(g, &it, &start, &end))
    return(g->prompt_length - 1);
  new_pos = gtk_text_iter_get_offset(&end);
  if (new_pos < pos)
    return(new_pos);
  it = start;
  gtk_text_iter_backward_char(&it);
  if (!prompt_backward_search(g, &it, &start, &end))
    return(g->prompt_length - 1);
  return(gtk_text_iter_get_offset(&end));
}


static int find_next_prompt(glistener *g)
{
  GtkTextIter it, start, end;
  glistener_cursor(g, &it);
  if (!gtk_text_iter_forward_search(&it, g->prompt, (GtkTextSearchFlags)0, &start, &end, NULL))
    gtk_text_buffer_get_end_iter(g->buffer, &end);
  return(gtk_text_iter_get_offset(&end) + 1);
}


static bool is_prompt_end(glistener *g, int end_pos)
{
  /* the prompt includes the preceding <cr> */
  GtkTextIter start, end;
  bool result = false;

  if (end_pos < g->prompt_length)
    return(true);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &end, end_pos);
  start = end;
  if (gtk_text_iter_backward_chars(&start, g->prompt_length))
    {
      char *txt;
      txt = gtk_text_iter_get_text(&start, &end);
      if (txt)
	{
	  result = (strcmp(txt, g->prompt) == 0);
	  g_free(txt);
	}
    }
  return(result);
}



/* ---------------- listener text ---------------- */

static void remember_listener_string(glistener *g, const char *str)
{
  int i, top, len;
  if (!str) return;

  if (g->strings_size == 0)
    {
      g->strings_size = 8;
      g->strings = (char **)calloc(g->strings_size, sizeof(char *));
    }
  
  g->strings_pos = 0;
  g->first_time = true;

  /* if str matches current history top entry, ignore it (as in tcsh) */
  if ((g->strings[0]) &&
      (strcmp(str, g->strings[0]) == 0))
    return;

  top = g->strings_size - 1;
  if (g->strings[top]) free(g->strings[top]);
  for (i = top; i > 0; i--) g->strings[i] = g->strings[i - 1];

  len = strlen(str) + 1;
  g->strings[0] = (char *)calloc(len, sizeof(char));
  strcopy(g->strings[0], str, len);
}


static void restore_listener_string(glistener *g, bool back)
{
  if (g->strings)
    {
      char *str;
      if (!(g->first_time))
	{
	  if (back)
	    g->strings_pos++;
	  else g->strings_pos--;
	}
      g->first_time = false;
      if (g->strings_pos < 0) g->strings_pos = 0;
      if (g->strings_pos > (g->strings_size - 1)) g->strings_pos = g->strings_size - 1;
      str = g->strings[g->strings_pos];
      if (str)
	glistener_append_text(g, str); 
    }
}

void glistener_clear(glistener *g)
{
  if (g->text)
    {
      GtkTextIter start, end;
      gtk_text_buffer_get_iter_at_offset(g->buffer, &start, g->prompt_length - 1);
      gtk_text_buffer_get_end_iter(g->buffer, &end); 
      gtk_text_buffer_delete(g->buffer, &start, &end);
    }
}


bool glistener_write(glistener *g, FILE *fp)
{
  char *str = NULL;
  GtkTextIter start, end;

  gtk_text_buffer_get_start_iter(g->buffer, &start);
  gtk_text_buffer_get_end_iter(g->buffer, &end);
  str = gtk_text_buffer_get_text(g->buffer, &start, &end, true);
  if (str)
    {
      size_t bytes;
      bytes = strlen(str);
      if (bytes > 0)
	{
	  if (fwrite((void *)str, sizeof(char), bytes, fp) != bytes)
	    {
	      g_free(str);
	      return(false);
	    }
	  g_free(str);
	}
    }
  return(true);
}


void glistener_append_text(glistener *g, const char *msg)
{
  if (g->text)
    {
      GtkTextIter end;
      gtk_text_buffer_get_end_iter(g->buffer, &end);
      gtk_text_buffer_insert(g->buffer, &end, (char *)msg, -1);
      gtk_text_view_scroll_mark_onscreen(GTK_TEXT_VIEW(g->text), gtk_text_buffer_get_insert(g->buffer));
    }
}


void glistener_insert_text(glistener *g, const char *text)
{
  if (g->text)
    {
      gtk_text_buffer_insert_at_cursor(g->buffer, text, -1);
      gtk_text_view_scroll_mark_onscreen(GTK_TEXT_VIEW(g->text), gtk_text_buffer_get_insert(g->buffer));
    }
}


char *glistener_text(glistener *g, int start, int end)
{
  GtkTextIter s, e;
  gtk_text_buffer_get_iter_at_offset(g->buffer, &s, start);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &e, end);
  return(gtk_text_buffer_get_text(g->buffer, &s, &e, true));
}


void glistener_scroll_to_end(glistener *g)
{
  if (g->text)
    {
      GtkTextIter end;
      gtk_text_buffer_get_end_iter(g->buffer, &end);
      gtk_text_buffer_place_cursor(g->buffer, &end);
      gtk_text_view_scroll_mark_onscreen(GTK_TEXT_VIEW(g->text), gtk_text_buffer_get_insert(g->buffer));
      gtk_adjustment_set_value(GTK_ADJUSTMENT(gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(g->scroller))), 0.0);
    }
}
/* there is apparently no way in gtk to make sure our prompt is still visible after
 *   a paned widget holding this listener changes its layout!
 */



/* ---------------- paren matching ---------------- */

static gboolean is_gt(gunichar c, gpointer data)
{
  return(c == '>');
}

static gboolean is_not_whitespace(gunichar c, gpointer data)
{
  return(!g_unichar_isspace(c));
}

static bool find_not_whitespace(glistener *g, int pos, GtkTextIter *limit)
{
  GtkTextIter scan;
  gtk_text_buffer_get_iter_at_offset(g->buffer, &scan, pos);  
  return(gtk_text_iter_forward_find_char(&scan, is_not_whitespace, NULL, limit));
}


static gboolean is_unslashed_double_quote(gunichar c, gpointer data)
{
  int *slashes = (int *)data;
  if (c == '\\')
    (*slashes)++;
  else
    {
      if ((c == '\"') &&
	  (*slashes & 1) == 0)
	return(true);
      *slashes = 0;
    }
  return(false);
}


static bool find_enclosing_string(glistener *g, int pos, int *d1, int *d2, GtkTextIter *start, GtkTextIter *end)
{
  GtkTextIter scan;
  int p1 = -1, p2 = -1, slashes = 0;
  gunichar c;
  bool found_left_quote = false;

  gtk_text_buffer_get_iter_at_offset(g->buffer, &scan, pos - 1);

  gtk_text_iter_backward_char(&scan);
  while (gtk_text_iter_compare(start, &scan) < 0)
    {
      c = gtk_text_iter_get_char(&scan);
      if (c == '\"')
	{
	  found_left_quote = true;
	  p1 = gtk_text_iter_get_offset(&scan);
	}
      else
	{
	  if (c == '\\')
	    {
	      if (found_left_quote)
		slashes++;
	    }
	  else
	    {
	      if ((found_left_quote) &&
		  ((slashes & 1) == 0))
		break;
	      found_left_quote = false;
	      p1 = -1;
	      slashes = 0;
	    }
	}
      gtk_text_iter_backward_char(&scan);
    }

  if (p1 != -1)
    gtk_text_buffer_get_iter_at_offset(g->buffer, &scan, p1);

  slashes = 0;
  if (gtk_text_iter_forward_find_char(&scan, is_unslashed_double_quote, &slashes, end))
    {
      if (found_left_quote)
	p2 = gtk_text_iter_get_offset(&scan);
      else 
	{
	  p1 = gtk_text_iter_get_offset(&scan);
	  if (gtk_text_iter_forward_find_char(&scan, is_unslashed_double_quote, &slashes, end))
	    p2 = gtk_text_iter_get_offset(&scan);
	}
    }

  if ((p1 == -1) || (p2 == -1))
    return(false);

  *d1 = p1;
  *d2 = p2 + 1;
  return(true);
}


static int find_string_end(glistener *g, int pos, GtkTextIter *limit)
{
  int d1, d2;
  GtkTextIter e;
  gtk_text_buffer_get_end_iter(g->buffer, &e);
  if (find_enclosing_string(g, pos, &d1, &d2, limit, &e))
    return(d2 - 1);
  return(-1);
}


static gboolean is_block_comment(gunichar c, gpointer data)
{
  int *last_c = (int *)data;
  if ((c == '#') &&
      (*last_c == '|'))
    return(true);
  *last_c = c;
  return(false);
}


static int find_open_block_comment(glistener *g, int pos, GtkTextIter *limit)
{
  GtkTextIter scan;
  int last_cs = 0;

  gtk_text_buffer_get_iter_at_offset(g->buffer, &scan, pos);  
  gtk_text_iter_backward_find_char(&scan, is_block_comment, &last_cs, limit);
  return(gtk_text_iter_get_offset(&scan));
}


static int find_close_block_comment(glistener *g, int pos, GtkTextIter *limit)
{
  GtkTextIter scan;
  int last_cs = 0;

  gtk_text_buffer_get_iter_at_offset(g->buffer, &scan, pos);  
  gtk_text_iter_forward_find_char(&scan, is_block_comment, &last_cs, limit);
  return(gtk_text_iter_get_offset(&scan));
}


static gboolean is_delimiter(gunichar c, gpointer data)
{
  return((g_unichar_isspace(c)) ||
	 (c == '(') ||
	 (c == ')') ||
	 (c == ';') ||
	 (c == '\"'));
  /* in s7, single-quote can appear in a name */
}


static void find_surrounding_word(glistener *g, int pos, 
				  gboolean (*checker)(gunichar c, gpointer data),
				  int *start_pos, int *end_pos, 
				  GtkTextIter *start_limit, GtkTextIter *end_limit)
{
  GtkTextIter start, end;
  int bpos, epos;

  *start_pos = pos; 
  bpos = gtk_text_iter_get_offset(start_limit);
  *end_pos = pos;
  epos = gtk_text_iter_get_offset(end_limit);

  gtk_text_buffer_get_iter_at_offset(g->buffer, &start, pos);
  /* gtk_text_buffer_get_iter_at_offset(g->buffer, &end, pos); */
  end = start;

  if (gtk_text_iter_compare(start_limit, &start) < 0)
    {
      if (gtk_text_iter_backward_find_char(&start, checker, NULL, start_limit))
	*start_pos = gtk_text_iter_get_offset(&start) + 1;
      else *start_pos = bpos;
    }

  if (gtk_text_iter_compare(&end, end_limit) < 0)
    {
      if (!is_delimiter(gtk_text_iter_get_char(&end), NULL))
	{
	  if (gtk_text_iter_forward_find_char(&end, checker, NULL, end_limit))
	    *end_pos = gtk_text_iter_get_offset(&end);
	  else *end_pos = epos;
	}
    }
}


static char *get_preceding_text(glistener *g, int pos, bool *in_string)
{
  GtkTextIter s1, e1, elimit;
  int start = 0, end = 0;

  gtk_text_buffer_get_iter_at_offset(g->buffer, &s1, find_current_prompt(g));
  gtk_text_buffer_get_iter_at_offset(g->buffer, &e1, pos);
  gtk_text_buffer_get_end_iter(g->buffer, &elimit);
  *in_string = false;

  if (gtk_text_iter_equal(&s1, &elimit)) /* s1 can't be beyond elimit=end iter */
    return(NULL);
  
  if ((gtk_text_iter_equal(&e1, &elimit)) ||
      (g_unichar_isspace(gtk_text_iter_get_char(&e1))))
    {
      find_surrounding_word(g, pos, is_delimiter, &start, &end, &s1, &e1);
      gtk_text_buffer_get_iter_at_offset(g->buffer, &elimit, start - 1);
      *in_string = (gtk_text_iter_get_char(&elimit) == '\"');
      gtk_text_buffer_get_iter_at_offset(g->buffer, &s1, start);
      gtk_text_buffer_get_iter_at_offset(g->buffer, &e1, end);
      return(gtk_text_buffer_get_text(g->buffer, &s1, &e1, true));
    }
  return(NULL);
}


static bool at_character_constant(glistener *g, int end_pos)
{
  GtkTextIter start, end;
  gtk_text_buffer_get_iter_at_offset(g->buffer, &end, end_pos);
  start = end;
  if (gtk_text_iter_backward_chars(&start, 2))
    {
      char *txt;
      bool result = false;
      txt = gtk_text_iter_get_text(&start, &end);
      if (txt)
	{
	  result = (strcmp(txt, "#\\") == 0);
	  g_free(txt);
	}
      return(result);
    }
  return(false);
}


static bool find_open_paren(glistener *g, int parens, int pos, int *highlight_pos, GtkTextIter *limit)
{
  GtkTextIter scan;
  int parens_at_line_end, ppos;
  bool end_scan = false;
  gunichar c = 0, last_c;

  parens_at_line_end = parens;
  gtk_text_buffer_get_iter_at_offset(g->buffer, &scan, pos);  
  while (gtk_text_iter_compare(limit, &scan) < 0)
    {
      last_c = c;
      c = gtk_text_iter_get_char(&scan);

      if (!at_character_constant(g, gtk_text_iter_get_offset(&scan)))
	{
	  if (c == (gunichar)'\"') 
	    {
	      int d1, d2, qpos;
	      GtkTextIter e;
	      qpos = gtk_text_iter_get_offset(&scan);
	      gtk_text_buffer_get_end_iter(g->buffer, &e);
	      if (find_enclosing_string(g, qpos, &d1, &d2, limit, &e))
		{
		  ppos = d1;
		}
	      else
		{
		  return(false);                /* no matching double-quote so we're probably in a string */
		}
	      gtk_text_buffer_get_iter_at_offset(g->buffer, &scan, ppos);
	      last_c = '\"';
	    }
	  else
	    {
	      if (c == '\n')
		{
		  if (end_scan)
		    return(true);
		  parens_at_line_end = parens;
		}
	      else
		{
		  if (c == (gunichar)';')
		    {
		      parens = parens_at_line_end;
		      end_scan = false;
		    }
		  else
		    {
		      if ((c == (gunichar)'|') &&
			  (last_c == (gunichar)'#')) /* we're looking backwards here, so in the end marker |# we see the # first */
			{
			  ppos = find_open_block_comment(g, gtk_text_iter_get_offset(&scan), limit);
			  gtk_text_buffer_get_iter_at_offset(g->buffer, &scan, ppos);
			  last_c = '#';
			}
		      else
			{
			  if (!end_scan)
			    {
			      if (c == ')')
				parens++; 
			      else
				{
				  if (c == '(')
				    {
				      parens--; 
				      if (parens == 0)
					{
					  (*highlight_pos) = gtk_text_iter_get_offset(&scan);
					  end_scan = true;
					}
				    }
				}
			    }
			}
		    }
		}
	    }
	}
      gtk_text_iter_backward_char(&scan);
    }

  return(parens == 0);
}


static bool find_close_paren(glistener *g, int parens, int pos, int *highlight_pos, GtkTextIter *limit)
{
  GtkTextIter scan;
  int ppos;
  gunichar c = 0, prev_c = 0;

  gtk_text_buffer_get_iter_at_offset(g->buffer, &scan, pos);  
  while (gtk_text_iter_compare(&scan, limit) < 0)
    {
      prev_c = c;
      c = gtk_text_iter_get_char(&scan);
      if (!at_character_constant(g, gtk_text_iter_get_offset(&scan)))
	{
	  if (c == (gunichar)'\"')
	    {
	      ppos = find_string_end(g, gtk_text_iter_get_offset(&scan), limit);
	      if (ppos != -1) 
		{
		  gtk_text_buffer_get_iter_at_offset(g->buffer, &scan, ppos);
		}
	      prev_c = '\"';
	    }
	  else
	    {
	      if (c == (gunichar)';')
		gtk_text_iter_forward_to_line_end(&scan);
	      else
		{
		  if ((c == (gunichar)'|') &&
		      (prev_c == (gunichar)'#'))
		    {
		      ppos = find_close_block_comment(g, gtk_text_iter_get_offset(&scan), limit);
		      gtk_text_buffer_get_iter_at_offset(g->buffer, &scan, ppos);
		    }
		  else
		    {
		      if (c == ')')
			{
			  parens--; 
			  if (parens == 0)
			    {
			      (*highlight_pos) = gtk_text_iter_get_offset(&scan);
			      return(true);
			    }
			}
		      else
			{
			  if (c == '(')
			    parens++;
			}
		    }
		}
	    }
	}
      gtk_text_iter_forward_char(&scan);
    }
  return(parens == 0);
}


static void add_inverse(glistener *g, int pos)
{
  GtkTextIter start, end;

  if (g->flash_paren_pos == -1) g->flash_paren_pos = pos;
  gtk_text_buffer_get_iter_at_offset(g->buffer, &start, pos);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &end, pos + 1);
  if (!g->flash_tag) g->flash_tag = gtk_text_buffer_create_tag(g->buffer, NULL, "background", "red", NULL);
  gtk_text_buffer_apply_tag(g->buffer, g->flash_tag, &start, &end);
}


static void remove_inverse(glistener *g, int pos)
{
  GtkTextIter start, end;

  gtk_text_buffer_get_iter_at_offset(g->buffer, &start, pos);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &end, pos + 1);
  if (!g->flash_tag) g->flash_tag = gtk_text_buffer_create_tag(g->buffer, NULL, "background", "red", NULL);
  gtk_text_buffer_remove_tag(g->buffer, g->flash_tag, &start, &end);
}


static gint flash_unbalanced_paren(gpointer data)
{
  glistener *g = (glistener *)data;

  g->flashes--;
  if (g->flashes & 1) 
    remove_inverse(g, g->flash_paren_pos); 
  else add_inverse(g, g->flash_paren_pos);
  if (g->flashes > 0)
    g_timeout_add_full(0, (guint32)g->flash_time, flash_unbalanced_paren, data, NULL);
  else 
    {
      remove_inverse(g, g->flash_paren_pos);
      g->flash_paren_pos = -1;
    }
  return(0);
}


static GtkTextTag *default_highlight_tag = NULL;

void glistener_set_highlight_tag(glistener *g, GtkTextTag *m)
{
  if (!g)
    default_highlight_tag = m;
  else
    {
      g->highlight_tag = m;
      default_highlight_tag = NULL;
    }
}


static void add_highlight(glistener *g, int bpos, int epos)
{
  GtkTextIter start, end;

  gtk_text_buffer_get_iter_at_offset(g->buffer, &start, bpos);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &end, epos);
  if (!g->highlight_tag) 
    g->highlight_tag = gtk_text_buffer_create_tag(g->buffer, NULL, "weight", PANGO_WEIGHT_BOLD, "foreground", "red", NULL);
  gtk_text_buffer_apply_tag(g->buffer, g->highlight_tag, &start, &end);
  g->highlight_start = bpos;
  g->highlight_end = epos;
}


static void remove_highlight(glistener *g)
{
  if ((g->highlight_tag) &&
      (g->highlight_start != -1))
    {
      GtkTextIter start, end;

      gtk_text_buffer_get_iter_at_offset(g->buffer, &start, g->highlight_start);
      gtk_text_buffer_get_iter_at_offset(g->buffer, &end, g->highlight_end);
      gtk_text_buffer_remove_tag(g->buffer, g->highlight_tag, &start, &end);
      g->highlight_start = -1;
      g->highlight_end = -1;
    }
}


static void check_for_offscreen_matching_paren(glistener *g, int pos)
{
  GtkTextIter p;
  GdkRectangle r;            /* actually cairo_rectangle_int_t */
  int y, height;
  
  gtk_text_view_get_visible_rect(GTK_TEXT_VIEW(g->text), &r);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &p, pos);
  gtk_text_view_get_line_yrange(GTK_TEXT_VIEW(g->text), (const GtkTextIter *)(&p), &y, &height);
  
  if (y < r.y)
    {
      GtkTextIter e1;
      char *text;
      
      /* p already points to (, so no need to mess with it */
      gtk_text_buffer_get_iter_at_offset(g->buffer, &e1, pos);
      if (!gtk_text_iter_ends_line(&e1))
	gtk_text_iter_forward_to_line_end(&e1);
      text = gtk_text_buffer_get_text(g->buffer, &p, &e1, false);
      if (text)
	{
	  glistener_post_status(g, text);
	  g_free(text);
	}
    }
}


static void check_parens(glistener *g)
{
  int pos;
  GtkTextIter scan, limit;
  gunichar c;

  remove_highlight(g);

  pos = glistener_cursor_position(g);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &scan, pos - 1);

  c = gtk_text_iter_get_char(&scan);
  if ((c == ')') &&
      (!at_character_constant(g, pos - 1)))
    {
      int opos = 0;
      gtk_text_buffer_get_iter_at_offset(g->buffer, &limit, find_current_prompt(g) - 1);
      if (find_open_paren(g, 1, pos - 2, &opos, &limit))
	{
	  add_highlight(g, opos, opos + 1);
	  check_for_offscreen_matching_paren(g, opos);
	  if (g->checker != default_checker)
	    {
	      char *text;
	      text = glistener_text(g, pos, opos);
	      if (text)
		{
		  const char *help;
		  help = g->checker(g, (const char *)text);
		  if (help)
		    glistener_post_status(g, help);
		  g_free(text);
		}
	    }
	}
    }
  else
    {
      gtk_text_iter_forward_char(&scan);
      c = gtk_text_iter_get_char(&scan);
      if ((c == '(') &&
	  (!at_character_constant(g, pos)))
	{
	  gtk_text_buffer_get_iter_at_offset(g->buffer, &limit, find_next_prompt(g));
	  if (find_close_paren(g, 1, pos + 1, &pos, &limit))
	    add_highlight(g, pos, pos + 1);
	}
    }
}



/* ---------------- text editing ---------------- */

static void word_upper(glistener *g, bool capitalize, bool upcase)
{
  int pos;
  GtkTextIter start, end;
  char *text = NULL, *up_text = NULL, *down_text = NULL;

  pos = glistener_cursor_position(g);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &start, pos);
  end = start;
  gtk_text_iter_forward_word_end(&end);

  if (upcase)
    {
      text = gtk_text_buffer_get_text(g->buffer, &start, &end, true);
      up_text = g_utf8_strup(text, -1);
    }
  else
    {
      if (!capitalize)
	{
	  text = gtk_text_buffer_get_text(g->buffer, &start, &end, true);
	  down_text = g_utf8_strdown(text, -1);
	}
      else
	{
	  GtkTextIter pstart;
	  char *text0;

	  gtk_text_buffer_get_iter_at_offset(g->buffer, &pstart, pos);
	  if (g_unichar_isspace(gtk_text_iter_get_char(&pstart)))
	    gtk_text_iter_forward_find_char(&pstart, is_not_whitespace, NULL, &end);
	  gtk_text_iter_forward_char(&pstart);

	  text0 = gtk_text_buffer_get_text(g->buffer, &start, &pstart, true);
	  text = gtk_text_buffer_get_text(g->buffer, &pstart, &end, true);

	  up_text = g_utf8_strup(text0, -1);
	  down_text = g_utf8_strdown(text, -1);
	  g_free(text0);
	}
    }

  gtk_text_buffer_delete(g->buffer, &start, &end);
  if (up_text)
    gtk_text_buffer_insert(g->buffer, &start, up_text, -1);
  if (down_text)
    gtk_text_buffer_insert(g->buffer, &start, down_text, -1);

  if (text) g_free(text);
  if (up_text) g_free(up_text);
  if (down_text) g_free(down_text);
}


static void text_transpose(glistener *g)
{
  int curpos;
  GtkTextIter start, end;

  curpos = glistener_cursor(g, &end);
  if (!is_prompt_end(g, curpos))
    {
      char *text, *new_text;
      start = end;
      gtk_text_iter_backward_char(&start);
      gtk_text_iter_forward_char(&end);
      text = gtk_text_buffer_get_text(g->buffer, &start, &end, false);
      new_text = g_utf8_strreverse(text, -1);
      gtk_text_buffer_delete(g->buffer, &start, &end);
      gtk_text_buffer_insert(g->buffer, &start, new_text, -1);
      g_free(text);
      g_free(new_text);
    }
}


static void clear_back_to_prompt(glistener *g)
{
  int beg, end;
  GtkTextIter start, last;

  end = glistener_cursor_position(g);
  beg = find_current_prompt(g);
  if (end <= beg) return;

  gtk_text_buffer_get_iter_at_offset(g->buffer, &start, beg);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &last, end); 
  gtk_text_buffer_delete(g->buffer, &start, &last);
}



/* ---------------- key bindings ---------------- */

void glistener_key_bindings(glistener *g, gpointer cls)
{
  /* emacs key bindings for the most part
   */
  GtkBindingSet *set;
  set = gtk_binding_set_by_class(cls);
  
  /* C-a start of line */
  gtk_binding_entry_remove(set, GDK_KEY_a, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_a, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* C-b back char */
  gtk_binding_entry_remove(set, GDK_KEY_b, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_b, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_VISUAL_POSITIONS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* M-b back word */
  gtk_binding_entry_remove(set, GDK_KEY_b, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_b, GDK_MOD1_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_WORDS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* C-d delete at cursor */
  gtk_binding_entry_remove(set, GDK_KEY_d, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_d, GDK_CONTROL_MASK,
			       "delete_from_cursor", 2,
			       G_TYPE_ENUM, GTK_DELETE_CHARS,
			       G_TYPE_INT, 1); /* -1 = delete to left of cursor */

  /* C-e end of line */
  gtk_binding_entry_remove(set, GDK_KEY_e, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_e, GDK_CONTROL_MASK, "move_cursor", 3, /* 3 = n_args */
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

 /* C-f forward char */
  gtk_binding_entry_remove(set, GDK_KEY_f, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_f, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_VISUAL_POSITIONS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

   /* M-f forward word */
  gtk_binding_entry_remove(set, GDK_KEY_f, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_f, GDK_MOD1_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_WORDS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

  /* C-n down line */
  gtk_binding_entry_remove(set, GDK_KEY_n, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_n, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINES,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

  /* C-p up line */
  gtk_binding_entry_remove(set, GDK_KEY_p, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_p, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINES,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* C-y yank <- clipboard */
  gtk_binding_entry_remove(set, GDK_KEY_y, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_y, GDK_CONTROL_MASK,
			       "paste_clipboard", 0);

  /* C-w delete region -> clipboard -- it's possible to clobber a prompt here */
  gtk_binding_entry_remove(set, GDK_KEY_w, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_w, GDK_CONTROL_MASK,
			       "cut_clipboard", 0);

  /* M-< start of file */
  gtk_binding_entry_remove(set, GDK_KEY_less, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_less, GDK_MOD1_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* M-> end of file */
  gtk_binding_entry_remove(set, GDK_KEY_greater, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_greater, GDK_MOD1_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

  /* down-arrow end of file */
  gtk_binding_entry_remove(set, GDK_KEY_Down, (GdkModifierType)0);
  gtk_binding_entry_add_signal(set, GDK_KEY_Down, (GdkModifierType)0, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

  /* up-arrow start of file */
  gtk_binding_entry_remove(set, GDK_KEY_Up, (GdkModifierType)0);
  gtk_binding_entry_add_signal(set, GDK_KEY_Up, (GdkModifierType)0, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_BUFFER_ENDS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* right-arrow end of line */
  gtk_binding_entry_remove(set, GDK_KEY_Right, (GdkModifierType)0);
  gtk_binding_entry_add_signal(set, GDK_KEY_Right, (GdkModifierType)0, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

  /* left-arrow start of line */
  gtk_binding_entry_remove(set, GDK_KEY_Left, (GdkModifierType)0);
  gtk_binding_entry_add_signal(set, GDK_KEY_Left, (GdkModifierType)0, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_DISPLAY_LINE_ENDS,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* C-v move down a window */
  gtk_binding_entry_remove(set, GDK_KEY_v, GDK_CONTROL_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_v, GDK_CONTROL_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_PAGES,
			       G_TYPE_INT, 1,
			       G_TYPE_BOOLEAN, false);

  /* M-v for up window */
  gtk_binding_entry_remove(set, GDK_KEY_v, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_v, GDK_MOD1_MASK, "move_cursor", 3,
			       G_TYPE_ENUM, GTK_MOVEMENT_PAGES,
			       G_TYPE_INT, -1,
			       G_TYPE_BOOLEAN, false);

  /* M-d delete word at cursor */
  gtk_binding_entry_remove(set, GDK_KEY_d, GDK_MOD1_MASK);
  gtk_binding_entry_add_signal(set, GDK_KEY_d, GDK_MOD1_MASK,
			       "delete_from_cursor", 2,
			       G_TYPE_ENUM, GTK_DELETE_WORD_ENDS,
			       G_TYPE_INT, 1);
}



static void glistener_return_callback(glistener *g);
static void glistener_completion(glistener *g, int end);


static gboolean glistener_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  glistener *g = (glistener *)data;

  if ((g->keyer == default_keyer) ||
      (!(g->keyer(g, w, event))))
    {
      guint key;
      GdkModifierType state;

      key = EVENT_KEYVAL(event);
      state = (GdkModifierType)EVENT_STATE(event);
      
      /* fprintf(stderr, "key: %d, state: %x\n", key, state); */
      
#if 1
      /* just a fun hack -- need to make a map to do this right 
       *   could be 1..3 bytes long
       *   perhaps C->c4, M->c5, []->3, CM->c8?
       */
      if (state & GDK_SUPER_MASK)
	{
	  char p[3];
	  p[0] = 0xce;
	  p[1] = key + (0xbb - GDK_KEY_l); /* works for lambda */
	  p[2] = 0;
	  glistener_insert_text(g, p);
	  g_signal_stop_emission((gpointer)w, g_signal_lookup("key_press_event", G_OBJECT_TYPE((gpointer)w)), 0);
	  return(false);
	}
#endif
      
      switch (key)
	{
	  /* further processing (by gtk) of the keystroke is blocked if we fall through */
	case GDK_KEY_a:
	  if (state & MetaMask)
	    glistener_set_cursor_position(g, find_previous_prompt(g, glistener_cursor_position(g)));
	  else return(false);
	  break;
	  
	case GDK_KEY_c:
	  if (state & MetaMask)
	    word_upper(g, true, false);
	  else return(false);
	  break;
	  
	case GDK_KEY_d:
	  if (state & ControlMask)
	    {
	      /* need to check for prompt just ahead */
	      if (!is_prompt_end(g, glistener_cursor_position(g) + g->prompt_length))
		return(false);
	      /* else we're sitting at (just in front of) the prompt so drop through and block the signal */
	    }
	  else
	    {
	      if (state & MetaMask)
		{
		  GtkTextIter p;
		  int i, pos, new_pos, cur;
		  bool hits_prompt = false;
		  
		  pos = glistener_cursor(g, &p);
		  gtk_text_iter_forward_word_end(&p);
		  new_pos = gtk_text_iter_get_offset(&p);
		  cur = pos + g->prompt_length;
		  
		  /* if there's a prompt somewhere between pos and new_pos, block this deletion */
		  for (i = 0; i < (new_pos - pos); i++)
		    {
		      hits_prompt = is_prompt_end(g, cur + i);
		      if (hits_prompt)
			break;
		    }
		  if (!hits_prompt)
		    return(false);
		}
	      else return(false);
	    }
	  break;
	  
	case GDK_KEY_e:
	  if (state & MetaMask)
	    glistener_set_cursor_position(g, find_next_prompt(g) - 1);
	  else return(false);
	  break;
	  
	case GDK_KEY_BackSpace:
	  /* need to check for prompt at cursor */
	  if (!is_prompt_end(g, glistener_cursor_position(g)))
	    return(false);
	  break;
	  
	case GDK_KEY_k:
	  if (state & ControlMask)
	    {
	      /* select to line end, copy to clipboard, delete */
	      GtkTextIter beg, end;
	      
	      gtk_text_buffer_get_iter_at_mark(g->buffer, &beg, gtk_text_buffer_get_mark(g->buffer, "insert"));
	      end = beg;
	      gtk_text_iter_forward_to_line_end(&end); /* was forward_to_end! */
	      if (!gtk_text_iter_equal(&beg, &end))
		{
		  gtk_text_buffer_select_range(g->buffer, &beg, &end);
		  gtk_text_buffer_cut_clipboard(g->buffer, gtk_widget_get_clipboard(w, GDK_SELECTION_CLIPBOARD), true);
		}
	    }
	  else return(false);
	  break;
	  
	case GDK_KEY_l:
	  if (state & MetaMask)
	    word_upper(g, false, false);
	  else return(false);
	  break;
	  
	case GDK_KEY_n:
	  if (state & MetaMask)
	    {
	      clear_back_to_prompt(g);
	      restore_listener_string(g, false);
	    }
	  else return(false);
	  break;
	  
	case GDK_KEY_p:
	  if (state & MetaMask)
	    {
	      clear_back_to_prompt(g);
	      restore_listener_string(g, true);
	    }
	  else return(false);
	  break;
	  
	case GDK_KEY_t:
	  if (state & ControlMask)
	    {
	      int pos;
	      pos = glistener_cursor_position(g);
	      if ((!is_prompt_end(g, pos)) &&
		  (!is_prompt_end(g, pos + g->prompt_length)))
		text_transpose(g);
	      else return(false);
	    }
	  else return(false);
	  break;
	  
	case GDK_KEY_u:
	  if (state & MetaMask)
	    word_upper(g, false, true);
	  else return(false);
	  break;
	  
	case GDK_KEY_w:
	  if (state & ControlMask)
	    {
	      GtkTextIter start, end;
	      bool has_selection;
	      has_selection = gtk_text_buffer_get_selection_bounds(g->buffer, &start, &end);
	      if (!has_selection)
		return(false);
	      if (gtk_text_iter_get_offset(&start) >= g->prompt_length)
		return(false);
	    }
	  else return(false);
	  break;
	  
	case GDK_KEY_Tab:
	  glistener_completion(g, glistener_cursor_position(g));
	  return(true);
	  
	case GDK_KEY_Return:
	  if (state & ControlMask)               /* C-return -> insert return no matter what */
	    glistener_insert_text(g, "\n");
	  else glistener_return_callback(g);
	  break;
	  
	default: 
	  return(false);
	}
    }
  
  g_signal_stop_emission((gpointer)w, g_signal_lookup("key_press_event", G_OBJECT_TYPE((gpointer)w)), 0);
  return(false);
}


static void post_help(glistener *g, int pos)
{
  GtkTextIter s1, e1;
  int start = 0, end = 0;
  char *text;

  if (g->helper == default_helper) return;

  gtk_text_buffer_get_iter_at_offset(g->buffer, &s1, find_current_prompt(g));
  gtk_text_buffer_get_iter_at_offset(g->buffer, &e1, find_next_prompt(g));
  find_surrounding_word(g, pos, is_delimiter, &start, &end, &s1, &e1);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &s1, start);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &e1, end);

  text = gtk_text_buffer_get_text(g->buffer, &s1, &e1, true);
  if (text)
    {
      const char *help;
      help = g->helper(g, text);
      if (help)
	glistener_post_status(g, help);
      g_free(text);
    }
}


static void check_for_open_paren_help(glistener *g)
{
  int pos;
  GtkTextIter limit;
  pos = glistener_cursor_position(g);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &limit, find_current_prompt(g) - 1);
  if (find_open_paren(g, 1, pos - 2, &pos, &limit))
    post_help(g, pos + 1);
}


static gboolean glistener_key_release(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  glistener *g = (glistener *)data;
  int cpos;
  GtkTextIter c;
  /* before doing anything, make sure we're not in the prompt! */

  cpos = glistener_cursor_position(g);
  if (cpos < g->prompt_length)
    glistener_set_cursor_position(g, g->prompt_length - 1);
  else
    {
      int bpos;
      bpos = find_current_prompt(g);
      if (cpos < bpos)
	glistener_set_cursor_position(g, bpos);
    }
 
  /* and mark matching paren, if any */
  gtk_text_buffer_get_iter_at_offset(g->buffer, &c, glistener_cursor_position(g) - 1);
  if (gtk_text_iter_get_char(&c) != ')')
    check_for_open_paren_help(g);
  else glistener_clear_status(g);
  check_parens(g);
  return(false);
}


static void text_insert(GtkTextBuffer *textbuffer, GtkTextIter *location, gchar *text, gint len, gpointer data)
{
  /* insert-text signal */
  glistener *g = (glistener *)data;
  g->insertion_position = gtk_text_iter_get_offset(location);
}


static void check_for_empty_listener(GtkTextView *w, gpointer data)
{
  /* cut-clipboard signal */
  glistener *g = (glistener *)data;
  if (gtk_text_buffer_get_char_count(g->buffer) == 0)
    {
      /* put a prompt back in! */
      GtkTextIter start;
      gtk_text_buffer_get_start_iter(g->buffer, &start);
      prompt_insert(g, &start, true);
    }
}


static gboolean glistener_button_release(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  glistener *g = (glistener *)data;

  if (EVENT_STATE(ev) & GDK_BUTTON2_MASK)
    glistener_set_cursor_position(g, g->insertion_position);
  else
    {
      int cpos;
      /* before doing anything, make sure we're not in the prompt! */

      cpos = glistener_cursor_position(g);
      if (cpos < g->prompt_length)
	glistener_set_cursor_position(g, g->prompt_length - 1);
      else
	{
	  int bpos;
	  bpos = find_current_prompt(g);
	  if (cpos < bpos)
	    glistener_set_cursor_position(g, bpos);
	}
    }
  check_parens(g);
  post_help(g, glistener_cursor_position(g));
  return(false);
}





/* ---------------- <cr> evaluation ---------------- */

static void eval_text(glistener *g, GtkTextIter *start, GtkTextIter *end)
{
  char *text;

  text = gtk_text_buffer_get_text(g->buffer, start, end, false);
  if (text)
    {
      GtkTextIter s, e, cursor_iter;
      gtk_text_buffer_get_iter_at_mark(g->buffer, &cursor_iter, gtk_text_buffer_get_insert(g->buffer));
      if (gtk_text_iter_forward_search(&cursor_iter, g->prompt, (GtkTextSearchFlags)0, &s, &e, NULL))
	glistener_append_text(g, text);
      glistener_set_cursor_shape(g, g->wait_cursor);
      remember_listener_string(g, text);
      g->evaluator(g, text);
      glistener_set_cursor_shape(g, g->arrow_cursor); 
      glistener_set_cursor_position(g, gtk_text_buffer_get_char_count(g->buffer));
      g_free(text);
    }
}


static int find_expression_limits(glistener *g, int *bpos, int *epos)
{
  GtkTextIter cursor, start, end;
  int pos;
  
  pos = glistener_cursor_position(g);
  if (pos < g->prompt_length - 1)
    *bpos = g->prompt_length - 1;
  else
    {
      gtk_text_buffer_get_iter_at_offset(g->buffer, &cursor, pos + g->prompt_length - 1);
      if (!prompt_backward_search(g, &cursor, &start, &end))
	*bpos = g->prompt_length - 1;
      else *bpos = gtk_text_iter_get_offset(&start) + g->prompt_length;
    }
  
  pos = glistener_cursor(g, &cursor);
  if (!gtk_text_iter_forward_search(&cursor, g->prompt, (GtkTextSearchFlags)0, &start, &end, NULL))
    {
      gtk_text_buffer_get_end_iter(g->buffer, &end);
      *epos = gtk_text_iter_get_offset(&end);
    }
  else *epos = gtk_text_iter_get_offset(&start);
  /* now the expression is between bpos and epos */

  gtk_text_buffer_get_iter_at_offset(g->buffer, &start, *bpos);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &end, *epos);

  if (g_unichar_isspace(gtk_text_iter_get_char(&start)))
    gtk_text_iter_forward_find_char(&start, is_not_whitespace, NULL, &end);
  if (gtk_text_iter_compare(&start, &end) >= 0)
    *bpos = *epos;
  else
    {
      GtkTextIter e1;
      e1 = end;
      gtk_text_iter_backward_char(&e1);
      if (g_unichar_isspace(gtk_text_iter_get_char(&e1)))
	gtk_text_iter_backward_find_char(&end, is_not_whitespace, NULL, &start);

      *bpos = gtk_text_iter_get_offset(&start);
      *epos = gtk_text_iter_get_offset(&end) + 1;
    }

  if (gtk_text_iter_compare(&start, &cursor) > 0)
    {
      GtkTextIter e1;
      e1 = cursor;
      gtk_text_iter_backward_char(&e1);
      if (g_unichar_isspace(gtk_text_iter_get_char(&e1)))
	gtk_text_iter_backward_find_char(&cursor, is_not_whitespace, NULL, &start);
      pos = gtk_text_iter_get_offset(&cursor);
    }
  return(pos);
}


static void glistener_return_callback(glistener *g)
{
  GtkTextIter scan_iter, end_iter, cursor_iter;
  int cursor_pos, start_pos, end_pos, expr_start = -1, any_start = -1, open_parens = 0, prev_start = -1;

  glistener_clear_status(g);
  
  if (!g->is_schemish)
    {
      GtkTextIter a, b;
      gtk_text_buffer_get_iter_at_offset(g->buffer, &a, find_current_prompt(g)); /* or perhaps line start if not on the prompt line? */
      gtk_text_buffer_get_iter_at_offset(g->buffer, &b, glistener_cursor_position(g));
      if (!gtk_text_iter_ends_line(&b)) 
	gtk_text_iter_forward_to_line_end(&b);
      eval_text(g, &a, &b);
      return;
    }

  remove_highlight(g);
  cursor_pos = find_expression_limits(g, &start_pos, &end_pos);
  if (start_pos >= end_pos) /* <cr> at end? */
    {
      glistener_append_text(g, "\n");
      return;
    }

  gtk_text_buffer_get_iter_at_offset(g->buffer, &scan_iter, start_pos);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &end_iter, end_pos);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &cursor_iter, cursor_pos);

  while (gtk_text_iter_compare(&scan_iter, &end_iter) < 0)
    {
      gunichar c;
      c = gtk_text_iter_get_char(&scan_iter);
      
      /* fprintf(stderr, "%c ", c); */
      if (!g_unichar_isspace(c))
	{
	  if (any_start == -1)
	    any_start = gtk_text_iter_get_offset(&scan_iter);
	  
	  switch (c)
	    {
	    case '"':
	      {
		int slashes = 0;
		GtkTextIter str_iter;
		str_iter = scan_iter;
		if (!gtk_text_iter_forward_find_char(&scan_iter, is_unslashed_double_quote, &slashes, &end_iter))
		  {
		    /* we're in an unclosed string constant */
		    glistener_insert_text(g, "\n");
		    return;
		  }
		gtk_text_iter_forward_char(&scan_iter); /* step over the close double-quote */
		
		if (expr_start == -1)
		  {
		    if (gtk_text_iter_compare(&cursor_iter, &scan_iter) <= 0)
		      {
			/* we're in a string with no surrounding context */
			eval_text(g, &str_iter, &scan_iter);
			return;
		      }
		    prev_start = gtk_text_iter_get_offset(&str_iter);
		  }
	      }
	      continue;
	      break;
	      
	    case ';':
	      {
		gtk_text_iter_forward_to_line_end(&scan_iter);
		if ((expr_start == -1) &&
		    (gtk_text_iter_compare(&cursor_iter, &scan_iter) <= 0))
		  {
		    /* we're in a comment and there's no enclosing context, but there might be a preceding expression */
		    if (prev_start != -1)
		      {
			GtkTextIter semi_iter;
			gtk_text_buffer_get_iter_at_offset(g->buffer, &semi_iter, prev_start);
			eval_text(g, &semi_iter, &scan_iter);
			return;
		      }
		    glistener_insert_text(g, "\n");
		    return;
		  }
	      }
	      break;
	      
	    case '#':
	      {
		/* the special cases here involve block comments and character constants 
		 *   there's also #<...> if user types inside the brackets and there's whitespace to confuse it
		 */
		GtkTextIter bc_iter, c_iter;
		gunichar nc;
		
		c_iter = scan_iter;
		bc_iter = scan_iter;
		gtk_text_iter_forward_char(&bc_iter);
		nc = gtk_text_iter_get_char(&bc_iter);
		
		if (nc == '|')
		  {
		    int last_cs = 0;
		    if (!gtk_text_iter_forward_find_char(&scan_iter, is_block_comment, &last_cs, &end_iter))
		      {
			/* we're in a unclosed block comment */
			glistener_insert_text(g, "\n");
			return;
		      }
		    if ((expr_start == -1) &&
			(gtk_text_iter_compare(&cursor_iter, &scan_iter) <= 0))
		      {
			/* we're in a block comment and there's no enclosing context, but there might be a preceding expression */
			if (prev_start != -1)
			  {
			    GtkTextIter semi_iter;
			    gtk_text_buffer_get_iter_at_offset(g->buffer, &semi_iter, prev_start);
			    eval_text(g, &semi_iter, &scan_iter);
			    return;
			  }
			glistener_insert_text(g, "\n");
			return;
		      }
		  }
		else
		  {
		    if (nc == '\\')
		      {
			gtk_text_iter_forward_chars(&scan_iter, 2);
			if (gtk_text_iter_get_char(&scan_iter) == 'x')
			  {
			    gtk_text_iter_forward_char(&scan_iter);
			    while (g_unichar_isdigit(gtk_text_iter_get_char(&scan_iter)))
			      gtk_text_iter_forward_char(&scan_iter);
			  }
			else gtk_text_iter_forward_char(&scan_iter);
			
			if (expr_start == -1)
			  {
			    if (gtk_text_iter_compare(&cursor_iter, &scan_iter) <= 0)
			      {
				/* we're in a character constant with no surrounding context */
				eval_text(g, &c_iter, &scan_iter);
				return;
			      }
			    prev_start = gtk_text_iter_get_offset(&c_iter);
			  }
			continue;
		      }
		    else
		      {
			/* a variable name can't start with '#', so I think we can assume #<... must have a closing > ? 
			 * other retricted chars: ,:`'
			 */
			if (nc == '<')
			  gtk_text_iter_forward_find_char(&scan_iter, is_gt, NULL, &end_iter);
		      }
		  }
	      }
	      break;
	      
	    case '(':
	      if (open_parens == 0)
		expr_start = gtk_text_iter_get_offset(&scan_iter);
	      open_parens++;
	      break;
	      
	    case ')':
	      open_parens--;
	      if (open_parens == 0)
		{
		  GtkTextIter c_iter;
		  /* see if the cursor is in the current expression */
		  
		  if (expr_start == -1)
		    {
		      /* unmatched close-paren */
		      add_inverse(g, gtk_text_iter_get_offset(&scan_iter));
		      g->flashes = 4;
		      g_timeout_add_full(0, (guint32)g->flash_time, flash_unbalanced_paren, (gpointer)g, NULL); 
		      glistener_post_status(g, "unmatched ')'");
		      return;
		    }
		  
		  gtk_text_iter_forward_char(&scan_iter);
		  c_iter = scan_iter;
		  while (g_unichar_isspace(gtk_text_iter_get_char(&c_iter)))
		    gtk_text_iter_forward_char(&c_iter);

		  if (gtk_text_iter_compare(&cursor_iter, &c_iter) <= 0)
		    {
		      /* we're in an expression with no surrounding context */
		      GtkTextIter c_iter;
		      if (any_start != -1)
			gtk_text_buffer_get_iter_at_offset(g->buffer, &c_iter, any_start);
		      else gtk_text_buffer_get_iter_at_offset(g->buffer, &c_iter, expr_start);
		      eval_text(g, &c_iter, &scan_iter);
		      return;
		    }
		  
		  if (any_start != -1) prev_start = any_start; else prev_start = expr_start;

		  expr_start = -1;
		  any_start = -1;
		  continue;
		}
	      break;
	    }
	}
      else
	{
	  if (expr_start == -1)
	    {
	      if (any_start != -1)
		{
		  if (gtk_text_iter_compare(&cursor_iter, &scan_iter) <= 0)
		    {
		      GtkTextIter c_iter;
		      /* an atom with no context */
		      gtk_text_buffer_get_iter_at_offset(g->buffer, &c_iter, any_start);
		      eval_text(g, &c_iter, &scan_iter);
		      return;
		    }
		  prev_start = any_start;
		  any_start = -1;
		}
	    }
	}
      
      gtk_text_iter_forward_char(&scan_iter);
    }
  /* we fell off the end */
  
  if (open_parens == 0)
    {
      GtkTextIter c_iter;
      /* an atom with no context */
      gtk_text_buffer_get_iter_at_offset(g->buffer, &c_iter, any_start);
      eval_text(g, &c_iter, &end_iter);
      return;
    }

  /* find unmatched open-paren, etc */
  if (open_parens > 1) /* if == 1, it's at expr_start */
    {
      GtkTextIter p_iter;
      gtk_text_buffer_get_iter_at_offset(g->buffer, &p_iter, start_pos);
      find_open_paren(g, open_parens - 1, end_pos, &expr_start, &p_iter);
    }

  add_inverse(g, expr_start);
  g->flashes = 4;
  g_timeout_add_full(0, (guint32)g->flash_time, flash_unbalanced_paren, (gpointer)g, NULL); 
  glistener_post_status(g, "unmatched '('");
  glistener_insert_text(g, "\n");
}




/* ---------------- <tab> completion and indentation ---------------- */

/* realpath eqv in glib? -- not available yet (many complaints ...)
 */

#ifndef _MSC_VER
#include <sys/stat.h>
#endif

static bool is_directory(const char *filename)
{
#ifndef _MSC_VER
  #ifdef S_ISDIR
    struct stat statbuf;
    return((stat(filename, &statbuf) >= 0) &&
	   (S_ISDIR(statbuf.st_mode)));
  #endif
#endif
  return(false);
}

static char *filename_completion(glistener *g, const char *partial_name)
{
  char *file_name = NULL, *directory_name = NULL, *temp, *new_name = NULL, *slash, *current_match = NULL, *result = NULL;
  int len = 0, flen, matches = 0;

  if (partial_name[0] == '~')
    {
      const char *home;
      home = g_getenv("HOME");
      if (home)
	{
	  new_name = (char *)calloc(strlen(partial_name) + strlen(home) + 1, sizeof(char));
	  strncpy(new_name, home, strlen(home));
	  strcat(new_name, (char *)(partial_name + 1));
	}
    }
  if (!new_name)
    {
      int new_len;
      new_len = strlen(partial_name) + 1;
      new_name = (char *)calloc(new_len, sizeof(char));
      strcopy(new_name, partial_name, new_len);
    }

  slash = g_utf8_strrchr(new_name, -1, (gunichar)'/');
  if (slash)
    {
      len = slash - new_name + 1;
      temp = (char *)malloc(len * sizeof(char));
      file_name = (char *)(new_name + len);
      strcopy(temp, new_name, len);
      temp[len - 1] = '\0';
      directory_name = realpath(temp, NULL);
      free(temp);
    }
  else
    {
      file_name = (char *)partial_name;
      directory_name = g_get_current_dir();
    }
  if (file_name)
    flen = strlen(file_name);
  else return(NULL);

  glistener_clear_status(g);

  if ((directory_name) && (file_name))
    {
      GDir *dir;
      dir = g_dir_open(directory_name, 0, NULL);
      while (true)
	{
	  const char *rname;
	  rname = g_dir_read_name(dir);
	  if (!rname) break;
	  if (strncmp(rname, file_name, flen) == 0)
	    {
	      if (strcmp(rname, file_name) != 0)
		glistener_append_status(g, rname);
	      if (current_match == NULL)
		{
		  len = strlen(rname);
		  current_match = (char *)calloc(len + 2, sizeof(char));
		  strcopy(current_match, rname, len + 2);
		}
	      else 
		{
		  int j;
		  matches++;
		  for (j = 0; j < len; j++)
		    if (current_match[j] != rname[j])
		      {
			current_match[j] = '\0';
			len = j;
			if (len <= flen)
			  {
			    /* can't extend current name because of ambiguous matches, so give up */
			    g_dir_close(dir);
			    if (directory_name) free(directory_name);
			    if (new_name) free(new_name);
			    return(NULL);
			  }
			break;
		      }
		}
	    }
	}
      if (dir) g_dir_close(dir);
    }

  if (len == flen)
    result = NULL;
  else
    {
      result = current_match;
      if ((slash) &&
	  (current_match))
	{
	  /* attach matched portion to user's indication of dir */
	  result = (char *)calloc(strlen(partial_name) + strlen(current_match) + 3, sizeof(char));
	  temp = g_utf8_strrchr(partial_name, -1, (gunichar)'/');
	  strncpy(result, partial_name, temp - partial_name + 1);
	  strcat(result, current_match);
	  free(current_match);
	}

      if ((result) && (matches == 0))
	{
	  char *str;
	  if ((directory_name) && (result[0] == '~'))
	    {
	      str = (char *)calloc(strlen(directory_name) + strlen(result) + 2, sizeof(char));
	      strcat(str, directory_name);
	      strcat(str, "/");
	      strcat(str, (char *)(result + 1));
	      if (is_directory(str))
		strcat(result, "/");
	      else strcat(result, "\"");
	      free(str);
	    }
	  else 
	    {
	      if (is_directory(result))
		strcat(result, "/");
	      else strcat(result, "\"");
	    }
	}
    }
  if (directory_name) free(directory_name);
  if (new_name) free(new_name);
  return(result);
}


typedef struct {
  const char *text;
  char *current_match;
  int len, tlen;
  glistener *g;
} match_info;

static bool compare_names(const char *symbol_name, void *data)
{
  match_info *m = (match_info *)data;
  if (strncmp(m->text, symbol_name, m->tlen) == 0)
    {
      if (strcmp(m->text, symbol_name) != 0)
	glistener_append_status(m->g, symbol_name);

      if (m->current_match == NULL)
	{
	  m->len = strlen(symbol_name);
	  m->current_match = (char *)calloc(m->len + 1, sizeof(char));
	  strcopy(m->current_match, symbol_name, m->len + 1);
	}
      else 
	{
	  int j;
	  for (j = 0; j < m->len; j++)
	    if (m->current_match[j] != symbol_name[j])
	      {
		m->current_match[j] = '\0';
		m->len = j;
		break;
	      }
	}
    }
  return((m->len > 0) && (m->len <= m->tlen));
}


static char *symbol_completion(glistener *g, const char *text)
{
  match_info *m;
  char *result = NULL;

  if (g->completer == default_completer) return(NULL);

  m = (match_info *)calloc(1, sizeof(match_info));
  m->text = text;
  m->tlen = strlen(text);
  m->len = 0;
  m->current_match = NULL;
  m->g = g;
  glistener_clear_status(g);
  g->completer(g, compare_names, (void *)m);
  if (m->len > m->tlen)
    result = m->current_match;
  else 
    {
      if (m->current_match) 
	free(m->current_match);
    }
  free(m);
  return(result);
}


static void glistener_completion(glistener *g, int pos)
{
  /* <whitespace><tab><any> -> indent (no-op if on prompt line and at end?) [tmp has old indentation code]
   * <text><tab><whitespace> -> try to complete
   * <text><tab><text> -> ???
   *
   * in check_parens, if paren found, check expr for 
   *   undefined vars, refedined globals, run lint for other errors
   *
   * when key release, also perhaps look for possible completions (with list if > 1) 
   */
  
  char *text;
  bool in_string = false;
  
  text = get_preceding_text(g, pos, &in_string);
  if ((text) && (*text))
    {
      char *new_name;
      if (!in_string)
	{
	  if ((g->is_schemish) &&
	      (text[0] == '\''))          /* quoted partial symbol name */
	    {
	      char *unq;
	      unq = symbol_completion(g, (char *)(text + 1));
	      if (unq)
		{
		  int len;
		  len = strlen(unq);
		  new_name = (char *)malloc((len + 2) * sizeof(char));
		  new_name[0] = '\'';
		  memcpy((void *)(new_name + 1), (void *)unq, len);
		  new_name[len + 1] = '\0';
		  free(unq);
		}
	      else new_name = NULL;
	    }
	  else new_name = symbol_completion(g, text);
	}
      else new_name = filename_completion(g, text);
      if (new_name)
	{
	  int old_len, new_len;
	  old_len = strlen(text);
	  new_len = strlen(new_name);
	  gtk_text_buffer_insert_at_cursor(g->buffer, (char *)(new_name + old_len), new_len - old_len);
	  free(new_name);
	}
      g_free(text);
    }
  else
    {
      /* here we're indenting.  This code assumes we're using a true fixed width font -- "nimbus mono 10" is not one.
       */
      int pos, bpos, epos, bline, cline, linepos, linecol;
      GtkTextIter cursor, curline, start_limit, end_limit;
      
      pos = find_expression_limits(g, &bpos, &epos);
      gtk_text_buffer_get_iter_at_offset(g->buffer, &start_limit, bpos - 1);
      gtk_text_buffer_get_iter_at_offset(g->buffer, &end_limit, epos);
      gtk_text_buffer_get_iter_at_offset(g->buffer, &cursor, pos);
      bline = gtk_text_iter_get_line(&start_limit);
      /* eline = gtk_text_iter_get_line(&end_limit); */
      cline = gtk_text_iter_get_line(&cursor);
      gtk_text_buffer_get_iter_at_line(g->buffer, &curline, cline);
      linepos = gtk_text_iter_get_offset(&curline);
      linecol = gtk_text_iter_get_line_offset(&cursor);
      
      if ((bline == cline) ||
	  (find_not_whitespace(g, linepos, &cursor)))
	glistener_insert_text(g, "    ");
      else
	{
	  GtkTextIter paren;
	  int oparen_pos;
	  if (!find_open_paren(g, 1, pos, &oparen_pos, &start_limit))
	    glistener_insert_text(g, "    ");
	  else
	    {
	      int oparen_col;
	      gtk_text_buffer_get_iter_at_offset(g->buffer, &paren, oparen_pos);
	      oparen_col = gtk_text_iter_get_line_offset(&paren);
	      /* we're at linecol, it's at oparen_col
	       */
	      if (oparen_col > linecol)
		{
		  int cols;
		  char *spaces;

		  cols = oparen_col - linecol + 2;
		  spaces = (char *)malloc((cols + 1) * sizeof(char));
		  memset((void *)spaces, 32, cols);
		  spaces[cols] = '\0';

		  /* now see what follows the unmatched ( */
		  gtk_text_iter_forward_char(&paren);
		  
		  if (gtk_text_iter_get_char(&paren) == '(')
		    {
		      glistener_insert_text(g, (const char *)(spaces + 1));
		      free(spaces);
		    }
		  else
		    {
		      GtkTextIter s1, e1;
		      int start = 0, end = 0;
		      char *text;
		      
		      glistener_insert_text(g, (const char *)spaces); /* default indentation */
		      free(spaces);

		      gtk_text_buffer_get_iter_at_offset(g->buffer, &start_limit, oparen_pos);
		      gtk_text_buffer_get_iter_at_offset(g->buffer, &end_limit, epos);
		      find_surrounding_word(g, oparen_pos + 1, is_delimiter, &start, &end, &start_limit, &end_limit);
		      gtk_text_buffer_get_iter_at_offset(g->buffer, &s1, start);
		      gtk_text_buffer_get_iter_at_offset(g->buffer, &e1, end);
		      text = gtk_text_buffer_get_text(g->buffer, &s1, &e1, true);
		      if (text)
			{
			  if (strcmp(text, "or") == 0)
			    glistener_insert_text(g, "  ");
			  else
			    {
			      if (strcmp(text, "and") == 0)
				glistener_insert_text(g, "   ");
			      else
				{
				  if (strcmp(text, "cond") == 0)
				    glistener_insert_text(g, "    ");
				  else
				    {
				      if (strcmp(text, "if") == 0)
					{
					  if (cline - bline == 1)
					    glistener_insert_text(g, "  ");
					}
				    }
				}
			    }
			  g_free(text);
			}
		    }
		}
	    }
	}
    }
}



/* ---------------- colorizing ----------------
 */

static void glistener_colorizer_callback(glistener *g)
{
  GtkTextIter scan_iter, end_iter;
  int cur_pos, start_pos, end_pos, expr_start = -1, any_start = -1, open_parens = 0, end_space_pos = 0;
  bool atom_awaits = false;

  if ((g->colorizer == default_colorizer) ||
      (!g->is_schemish))
    return;

  find_expression_limits(g, &start_pos, &end_pos);
  if (start_pos >= end_pos) return;
  cur_pos = start_pos;
  end_space_pos = start_pos - 1;

  gtk_text_buffer_get_iter_at_offset(g->buffer, &scan_iter, start_pos);
  gtk_text_buffer_get_iter_at_offset(g->buffer, &end_iter, end_pos);
  gtk_text_buffer_remove_all_tags(g->buffer, &scan_iter, &end_iter);
  
  while (gtk_text_iter_compare(&scan_iter, &end_iter) < 0)
    {
      gunichar c;

      c = gtk_text_iter_get_char(&scan_iter);
      cur_pos = gtk_text_iter_get_offset(&scan_iter);

      if (!g_unichar_isspace(c))
	{
	  if (any_start == -1)
	    any_start = cur_pos;

	  switch (c)
	    {
	    case '"':
	      {
		int slashes = 0;
		if (!gtk_text_iter_forward_find_char(&scan_iter, is_unslashed_double_quote, &slashes, &end_iter))
		  {
		    /* we're in an unclosed string constant */
		    g->colorizer(g, GLISTENER_STRING, any_start, end_pos);
		    return;
		  }

		gtk_text_iter_forward_char(&scan_iter); /* step over the close double-quote */
		g->colorizer(g, GLISTENER_STRING, cur_pos, gtk_text_iter_get_offset(&scan_iter)); 
	      }
	      continue;
	      break;
	      
	    case ';':
	      {
		gtk_text_iter_forward_to_line_end(&scan_iter);
		g->colorizer(g, GLISTENER_COMMENT, cur_pos, gtk_text_iter_get_offset(&scan_iter)); 
	      }
	      break;
	      
	    case '#':
	      {
		/* the special cases here involve block comments and character constants 
		 *   there's also #<...> if user types inside the brackets and there's whitespace to confuse it
		 */
		GtkTextIter bc_iter;
		gunichar nc;
		
		bc_iter = scan_iter;
		gtk_text_iter_forward_char(&bc_iter);
		nc = gtk_text_iter_get_char(&bc_iter);
		
		if (nc == '|')
		  {
		    int last_cs = 0;
		    bool found_end;
		    found_end = gtk_text_iter_forward_find_char(&scan_iter, is_block_comment, &last_cs, &end_iter);
		    g->colorizer(g, GLISTENER_BLOCK_COMMENT, cur_pos, gtk_text_iter_get_offset(&scan_iter) + 1);
		    if ((!found_end) || (expr_start == -1)) /* we're in a unclosed block comment or there's no enclosing context */
		      return;
		  }
		else
		  {
		    if (nc == '\\')
		      {
			gtk_text_iter_forward_chars(&scan_iter, 2);
			if (gtk_text_iter_get_char(&scan_iter) == 'x')
			  {
			    gtk_text_iter_forward_char(&scan_iter);
			    while (g_unichar_isdigit(gtk_text_iter_get_char(&scan_iter)))
			      gtk_text_iter_forward_char(&scan_iter);
			  }
			else gtk_text_iter_forward_char(&scan_iter);
			g->colorizer(g, GLISTENER_CHARACTER, cur_pos, gtk_text_iter_get_offset(&scan_iter));
			
			if (expr_start == -1) /* we're in a character constant with no surrounding context */
			  return;
			continue;
		      }
		    else
		      {
			/* a variable name can't start with '#', so I think we can assume #<... must have a closing > ? 
			 * other retricted chars: ,:`'
			 */
			if (nc == '<')
			  {
			    gtk_text_iter_forward_find_char(&scan_iter, is_gt, NULL, &end_iter);
			    g->colorizer(g, GLISTENER_BRACKET, cur_pos, gtk_text_iter_get_offset(&scan_iter) + 1);
			  }
			else atom_awaits = true; /* #t for example */
		      }
		  }
	      }
	      break;
	      
	    case '(':
	      if (open_parens == 0)
		expr_start = cur_pos;
	      open_parens++;
	      end_space_pos = cur_pos;
	      break;
	      
	    case ')':
	      if (atom_awaits)
		{
		  g->colorizer(g, GLISTENER_ATOM, end_space_pos + 1, cur_pos);
		  atom_awaits = false;
		}
	      end_space_pos = cur_pos;
	  
	      open_parens--;
	      if (open_parens == 0)
		{
		  /* if this is an unmatched ')' we have already reported that */
		  if (expr_start == -1) /* unmatched close-paren */
		    return;

		  gtk_text_iter_forward_char(&scan_iter);
		  g->colorizer(g, GLISTENER_LIST, any_start, cur_pos);
		  
		  expr_start = -1;
		  any_start = -1;
		  continue;
		}
	      break;

	    default:
	      atom_awaits = true;
	      break;
	    }
	}
      else
	{
	  if (atom_awaits)
	    {
	      g->colorizer(g, GLISTENER_ATOM, end_space_pos + 1, cur_pos);
	      atom_awaits = false;
	    }
	  end_space_pos = cur_pos;
	  if (expr_start == -1)
	    any_start = -1;
	}
      
      gtk_text_iter_forward_char(&scan_iter);
    }
  if (atom_awaits)
    g->colorizer(g, GLISTENER_ATOM, end_space_pos + 1, end_pos);
}


static void colorize_listener(GtkTextBuffer *buffer, void *data)
{
  glistener_colorizer_callback((glistener *)data);
}




/* ---------------- testing ----------------
 *
 * these functions are intended for regression test suites.  They make it possible to mimic
 *    user <cr> and <tab> and see the resultant output.
 */

char *glistener_evaluate(glistener *g)
{
  int start, end;
  glistener_return_callback(g);
  start = find_previous_prompt(g, glistener_cursor_position(g));
  end = find_current_prompt(g) - g->prompt_length;
  return(glistener_text(g, start, end));
}


char *glistener_complete(glistener *g)
{
  int start, end;
  glistener_completion(g, glistener_cursor_position(g));
  start = find_previous_prompt(g, glistener_cursor_position(g));
  end = find_current_prompt(g) - g->prompt_length;
  return(glistener_text(g, start, end));
}



/* ---------------- new listener ---------------- */


#define SIGNAL_CONNECT(Widget, Signal, Function, Data) g_signal_connect(G_OBJECT(Widget), Signal, G_CALLBACK(Function), (gpointer)Data)
#define SIGNAL_CONNECT_AFTER(Widget, Signal, Function, Data) g_signal_connect_after(G_OBJECT(Widget), Signal, G_CALLBACK(Function), (gpointer)Data)

glistener *glistener_new(GtkWidget *parent, void (*initializations)(glistener *g, GtkWidget *new_listener))
{
  glistener *g;
  GtkWidget *vb;

  /* make a new glistener, set defaults */
  g = (glistener *)calloc(1, sizeof(glistener));
  g->is_schemish = true;
  g->helper = default_helper;
  g->checker = default_checker;
  g->completer = default_completer;
  g->evaluator = default_evaluator;
  g->colorizer = default_colorizer;
  g->keyer = default_keyer;
  g->prompt_tag = NULL;
  g->prompt = NULL;
  g->prompt_length = 0;
  g->strings = NULL;
  g->strings_size = 0;
  g->strings_pos = 0;
  g->first_time = true;
  g->flash_tag = NULL;
  g->flashes = 0;
  g->flash_paren_pos = -1;
  g->flash_time = 150;
  g->highlight_tag = NULL;
  g->highlight_start = -1;
  g->highlight_end = -1;
  g->insertion_position = 0;
  g->wait_cursor = NULL; 
  g->arrow_cursor = NULL;
  g->status_message = NULL;

  /* make the listener widgets */
  g->scroller = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(g->scroller), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  g->text = gtk_text_view_new();
  g->buffer = gtk_text_buffer_new(NULL);

  gtk_text_view_set_buffer(GTK_TEXT_VIEW(g->text), g->buffer);
  gtk_text_view_set_editable(GTK_TEXT_VIEW(g->text), true);
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(g->text), GTK_WRAP_NONE);
  gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(g->text), true);
  gtk_text_view_set_left_margin(GTK_TEXT_VIEW(g->text), 4);
  gtk_container_add(GTK_CONTAINER(g->scroller), g->text);
  gtk_widget_set_events(g->text, GDK_ALL_EVENTS_MASK);

  if (default_font)
    glistener_set_font(g, default_font);
  else glistener_set_font(g, pango_font_description_from_string("Monospace 11"));
  glistener_key_bindings(g, GTK_TEXT_VIEW_GET_CLASS(g->text));

  if (default_text_color)
    glistener_set_text_color(g, default_text_color);
  if (default_background_color)
    glistener_set_background_color(g, default_background_color);

  if (initializations)
    initializations(g, g->text);

  SIGNAL_CONNECT(g->text, "key_press_event",      glistener_key_press,      (gpointer)g);
  SIGNAL_CONNECT(g->text, "key_release_event",    glistener_key_release,    (gpointer)g);
  SIGNAL_CONNECT(g->text, "button_release_event", glistener_button_release, (gpointer)g);

  SIGNAL_CONNECT_AFTER(g->buffer, "insert-text",   text_insert,              (gpointer)g);
  SIGNAL_CONNECT_AFTER(g->buffer, "changed",       colorize_listener,        (gpointer)g);
  SIGNAL_CONNECT_AFTER(g->text,   "cut-clipboard", check_for_empty_listener, (gpointer)g);

  if (!g->prompt)
    {
      if (default_prompt)
	glistener_set_prompt(g, default_prompt);
      else
	{
	  g->prompt = (char *)calloc(3, sizeof(char));
	  g->prompt[0] = '\n';
	  g->prompt[1] = '>';
	  g->prompt_length = g_utf8_strlen(g->prompt, -1);
	}
    }
  if (default_prompt_tag)
    glistener_set_prompt_tag(g, default_prompt_tag);
  if (default_highlight_tag)
    glistener_set_highlight_tag(g, default_highlight_tag);

  /* put in the first prompt without the preceding <cr> */
  {
    GtkTextIter start;
    gtk_text_buffer_get_start_iter(g->buffer, &start);
    prompt_insert(g, &start, true);
  }

  if (!g->wait_cursor) g->wait_cursor = GDK_CURSOR_NEW(GDK_WATCH);
  if (!g->arrow_cursor) g->arrow_cursor = GDK_CURSOR_NEW(GDK_LEFT_PTR);

#if (!GTK_CHECK_VERSION(3, 0, 0))
  vb = gtk_table_new(2, 1, false);
  if (parent)
    gtk_container_add(GTK_CONTAINER(parent), vb);
  gtk_table_attach(GTK_TABLE(vb), g->scroller, 0, 1, 0, 1, /* left right top bottom */
		   (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		   (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		   0, 0);

  g->status = gtk_statusbar_new();
  gtk_table_attach(GTK_TABLE(vb), g->status, 0, 1, 1, 2,
		   (GtkAttachOptions)(GTK_EXPAND | GTK_FILL), 
		   (GtkAttachOptions)(GTK_FILL), 
		   0, 0);
#else
  vb = gtk_grid_new();
  if (parent)
    gtk_container_add(GTK_CONTAINER(parent), vb);
  gtk_widget_set_halign(g->scroller, GTK_ALIGN_FILL);
  gtk_widget_set_valign(g->scroller, GTK_ALIGN_FILL);
  gtk_widget_set_hexpand(g->scroller, TRUE);
  gtk_widget_set_vexpand(g->scroller, TRUE);
  gtk_grid_attach(GTK_GRID(vb), g->scroller, 0, 0, 1, 1); /* left top w h */
 
  g->status = gtk_statusbar_new();
  gtk_widget_set_halign(g->status, GTK_ALIGN_FILL); 
  gtk_grid_attach(GTK_GRID(vb), g->status, 0, 1, 1, 1);
#endif
  gtk_widget_show(g->text);
  gtk_widget_show(g->scroller);
  gtk_widget_show(g->status);
  gtk_widget_show(vb);

  return(g); 
}

/* changes:
 * 19-Mar-15: changed strcopy macro.
 * 7-June:    added keyer function.
 * 4-June:    added colorizer function.
 * 28-May:    added checker function.
 */

/* C-s/r could prompt in the status area if it were a text entry, not a label widget -- is that possible?
 *    there's also gtk_overlay (see gtk-demo/overlay.c) in 3.9.8 -- but it says "static position".
 */
