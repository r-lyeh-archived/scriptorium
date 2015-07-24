#include "jog.h"

string JogReader::random_seed;

const char* token_name_lookup[] =
{
  "???", "EOF", "EOL", "Identifier", "char", 
  "double", "float", "long", "int", "#2^31", "#2^63", "String", "(Unknown)",
  "!", "%", "&", "(", ")", "*", "+", ",", "-", ".", "/",
  ":", ";", "<", "=", ">", "?",
  "[", "\\", "]", "^",
  "{", "|", "}", "~",

  "!=", "%=", "&&", "&=", "*=", "+=", "++", "--", "-=", "/=",
  "<<", "<<=", "<=", "==", ">=", ">>", ">>=", ">>>", ">>>=",
  "^=",
  "|=", "||",

  "abstract", "assert",
  "break",
  "case", "catch", "class", "const", "continue",
  "default", "do",
  "else", "enum", "extends",
  "false", "final", "finally", "for",
  "goto",
  "if", "implements", "import", "instanceof", "interface",
  "native", "new", "null",
  "package", "private", "protected", "public",
  "return",
  "static", "strictfp", "super", "switch", "synchronized",
  "throw", "throws", "transient", "true", "try",
  "volatile",
  "while",
  "???"
};

KeywordMap keywords;

int jog_char_to_value( int ch )
{
  if (ch >= '0' && ch <= '9') return (ch - '0');
  if (ch >= 'a' && ch <= 'z') return (ch - 'a') + 10;
  if (ch >= 'A' && ch <= 'A') return (ch - 'A') + 10;
  return -1;
}

int jog_is_digit( int ch, int base )
{
  int value = jog_char_to_value(ch);
  return (value >= 0 && value < base);
}

//=============================================================================
//  JogReader
//=============================================================================
JogReader::JogReader( const char* filename )
  : line(1), column(1)
{
  FILE* infile = fopen(filename,"rb");
  if ( !infile )
  {
    StringBuilder buffer;
    buffer.print( "File \"" );
    buffer.print( filename );
    buffer.print( "\" not found." );

    Ref<JogError> error = new JogError( buffer.to_string(), NULL, 0, 0 );
    throw error;
  }

  fseek( infile, 0, SEEK_END );
  int size = ftell(infile);
  fseek( infile, 0, SEEK_SET );

  char* data = new char[size];
  fread( data, 1, size, infile );
  fclose(infile);

  init( filename, data, size );
  delete data;
}

JogReader::JogReader( const char* filename, const char* data, int size )
{
  init( filename, data, size );
}

JogReader::JogReader( Ref<JogReader> existing )
{
  original_size = existing->original_size;
  data = new short int[original_size];
  memcpy( data, existing->data, original_size * sizeof(short int) );
  filename = existing->filename;
  line = column = 1;
  pos = 0;
  remaining = original_size;
}

void JogReader::init( const char* filename, const char* data, int size )
{
  // Copy and clean the data - convert tabs to 2 spaces each, getting
  // rid of cursor return (13) but leaving line feed (10), converting
  // \uABCD sequences into single 16-bit values, and converting
  // $SEED into the random seed string.
  this->filename = new ASCIIString(filename);
  int   extra_chars = 1;

  const char* src = data - 1;
  int count = size + 1;
  while (--count)
  {
    switch (*(++src))
    {
      case '\t': ++extra_chars; break;
      case 13:   --extra_chars; break;
      case '\\':
        if (count>=5 && *(src+1)=='u') extra_chars -= 5;
        break;
      case '$':
        if (count>=4 && src[1]=='S' && src[2]=='E' && src[3]=='E' && src[4]=='D')
        {
          extra_chars += (random_seed.length() - 5);
        }
    }
  }

  original_size = size + extra_chars;
  remaining = original_size;
  this->data = new short int[remaining];

  src = data - 1;
  short int* dest = this->data - 1;
  count = size + 1;
  while (--count)
  {
    char ch = *(++src);
    switch (ch)
    {
      case '\t':
        *(++dest) = ' ';
        *(++dest) = ' ';
        break;

      case 13: 
        break;

      case '\\':
        if (count >= 5 && *(src+1)=='u')
        {
          ++src;
          int h1 = jog_char_to_value(*(++src));
          int h2 = jog_char_to_value(*(++src));
          int h3 = jog_char_to_value(*(++src));
          int h4 = jog_char_to_value(*(++src));
          *(++dest) = (short int)((h1<<12)|(h2<<8)|(h3<<4)|h4);
        }
        else
        {
          *(++dest) = ch;
        }
        break;

      case '$':
        if (count>=4 && src[1]=='S' && src[2]=='E' && src[3]=='E' && src[4]=='D')
        {
          for (unsigned int i=0; i<random_seed.length(); ++i)
          {
            *(++dest) = random_seed[i];
          }
          count -= 4;
          src += 4;
        }
        else
        {
          *(++dest) = ch;
        }
        break;

      default:
        *(++dest) = ch;
    }
  }

  *(++dest) = '\n';

  line = 1;
  column = 1;
  pos = 0;
}

JogReader::~JogReader()
{
  delete data;
}

int JogReader::peek()
{
  if (remaining) return data[pos];
  return -1;
}

int JogReader::peek( int num_ahead )
{
  if (remaining >= num_ahead) return data[pos+(num_ahead-1)];
  return -1;
}

int JogReader::read()
{
  if ( --remaining >= 0 )
  {
    int ch = data[pos++];
    if (ch == 10)
    {
      ++line;
      column = 1;
    }
    else
    {
      ++column;
    }
    return ch;
  }
  remaining = 0;
  return -1;
}

bool JogReader::consume( int ch )
{
  if (peek() != ch) return false;
  read();
  return true;
}


//=============================================================================
//  JogScanner
//=============================================================================
JogScanner::JogScanner( Ref<JogReader> reader ) : reader(reader)
{
  set_up_keywords();
  prep_next();
}

JogScanner::JogScanner( RefList<JogToken>& tokens )
{
  set_up_keywords();

  Ref<JogToken> first = tokens[0];
  Ref<JogToken> eof = new JogToken(first->reader,first->line,first->column);
  eof->type = TOKEN_EOF;
  pending_stack.add( eof );
  while (tokens.count) pending_stack.add(tokens.remove_last());
}

void JogScanner::set_up_keywords()
{
  if (keywords.size() > 0) return;   // already set up

  keywords["abstract"]     = TOKEN_ABSTRACT;
  keywords["assert"]       = TOKEN_ASSERT;
  keywords["break"]        = TOKEN_BREAK;
  keywords["case"]         = TOKEN_CASE;
  keywords["catch"]        = TOKEN_CATCH;
  keywords["class"]        = TOKEN_CLASS;
  keywords["const"]        = TOKEN_CONST;
  keywords["continue"]     = TOKEN_CONTINUE;
  keywords["default"]      = TOKEN_DEFAULT;
  keywords["do"]           = TOKEN_DO;
  keywords["else"]         = TOKEN_ELSE;
  keywords["enum"]         = TOKEN_ENUM;
  keywords["extends"]      = TOKEN_EXTENDS;
  keywords["false"]        = TOKEN_FALSE;
  keywords["final"]        = TOKEN_FINAL;
  keywords["finally"]      = TOKEN_FINALLY;
  keywords["for"]          = TOKEN_FOR;
  keywords["if"]           = TOKEN_IF;
  keywords["implements"]   = TOKEN_IMPLEMENTS;
  keywords["import"]       = TOKEN_IMPORT;
  keywords["instanceof"]   = TOKEN_INSTANCEOF;
  keywords["interface"]    = TOKEN_INTERFACE;
  keywords["native"]       = TOKEN_NATIVE;
  keywords["new"]          = TOKEN_NEW;
  keywords["null"]         = TOKEN_NULL;
  keywords["package"]      = TOKEN_PACKAGE;
  keywords["private"]      = TOKEN_PRIVATE;
  keywords["protected"]    = TOKEN_PROTECTED;
  keywords["public"]       = TOKEN_PUBLIC;
  keywords["return"]       = TOKEN_RETURN;
  keywords["static"]       = TOKEN_STATIC;
  keywords["strictfp"]     = TOKEN_STRICTFP;
  keywords["super"]        = TOKEN_SUPER;
  keywords["switch"]       = TOKEN_SWITCH;
  keywords["synchronized"] = TOKEN_SYNCHRONIZED;
  keywords["throw"]        = TOKEN_THROW;
  keywords["throws"]       = TOKEN_THROWS;
  keywords["transient"]    = TOKEN_TRANSIENT;
  keywords["true"]         = TOKEN_TRUE;
  keywords["try"]          = TOKEN_TRY;
  keywords["volatile"]     = TOKEN_VOLATILE;
  keywords["while"]        = TOKEN_WHILE;
}

void JogScanner::consume_ws()
{
  int ch=reader->peek();
  while (ch == ' ')
  {
    reader->read();
    ch = reader->peek();
  }
}

bool JogScanner::has_another()
{
  return (peek()->type != TOKEN_EOF);
}

Ref<JogToken> JogScanner::peek()
{
  if (pending_stack.count) return pending_stack.last();
  return next;
}

Ref<JogToken> JogScanner::peek( int num_ahead )
{
  if (num_ahead <= 1) return peek();

  set_mark();
  while (--num_ahead) read();
  Ref<JogToken> t = peek();
  rewind_to_mark();
  return t;
}

Ref<JogToken> JogScanner::read()
{
  Ref<JogToken> t;
  if (pending_stack.count) 
  {
    t = pending_stack.remove_last();
  }
  else 
  {
    t = next;
    prep_next();
  }

  if (marks.count) history_stack.add(t);
  return t;
}

void JogScanner::set_mark()
{
  marks.add( history_stack.count );
}

void JogScanner::clear_mark()
{
  int index = marks.remove_last();
  if (index == 0) history_stack.clear();
}

void JogScanner::rewind_to_mark()
{
  int index = marks.remove_last();
  while (history_stack.count > index)
  {
    pending_stack.add( history_stack.remove_last() );
  }
}

bool JogScanner::next_is( int token_type )
{
  return (peek()->type == token_type);
}

bool JogScanner::next_is( const char* identifier )
{
  if (peek()->type != TOKEN_ID) return false;
  return peek()->content->equals(identifier);
}

bool JogScanner::consume( int token_type )
{
  if (peek()->type != token_type) return false;
  read();
  return true;
}

bool JogScanner::consume( const char* identifier )
{
  if (peek()->type != TOKEN_ID) return false;
  if ( !peek()->content->equals(identifier) ) return false;
  read();
  return true;
}

void JogScanner::must_consume( int token_type, const char* error_mesg )
{
  if (consume(token_type)) return;
  throw peek()->error(error_mesg);
}

void JogScanner::must_consume_semicolon( Ref<JogToken> t )
{
  if ( !consume(TOKEN_SEMICOLON) )
  {
    if (t->line == peek()->line)
    {
      throw peek()->error( "Syntax error - semicolon (;) expected." );
    }
    else
    {
      throw t->error( "This command is missing the semicolon (;) at the end." );
    }
  }
}

Ref<JogString> JogScanner::must_read_id( const char* error_mesg )
{
  Ref<JogToken>  t = peek();

  if (t->type != TOKEN_ID)
  {
    throw t->error(error_mesg);
  }
  
  Ref<JogString> result = read()->content;
  return result;
}

void JogScanner::prep_next()
{
  for (;;)
  {
prep_next_token:
    consume_ws();

    next = new JogToken( reader, reader->line, reader->column );

    int ch = reader->peek();

    if (ch == -1)
    {
      next->type = TOKEN_EOF;
      return;
    }

    if (ch == 10)
    {
      reader->read();
      continue;
      //next->type = TOKEN_EOL;
      //return;
    }

    if ((ch>='a' && ch<='z') || (ch>='A' && ch<='Z') || (ch=='_') || ch=='$')
    {
      unicode_buffer.clear();
      unicode_buffer.print( (char) reader->read() );
      ch = reader->peek();
      while ((ch>='a' && ch<='z') || (ch>='A' && ch<='Z') || (ch=='_') 
          || (ch>='0' && ch<='9') || (ch == '$'))
      {
        unicode_buffer.print( (char) reader->read() );
        ch = reader->peek();
      }

      Ref<JogString> string = new JogString(unicode_buffer.to_string());
      Ref<ASCIIString> ascii_string = string->to_ascii();

      KeywordMap::iterator entry = keywords.find(ascii_string->data);
      if (entry == keywords.end())
      {
        // regular ID
        next->type = TOKEN_ID;
        next->content = string;
      }
      else
      {
        // found a keyword
        next->type = entry->second;
      }
      return;
    }

    if (ch>='0' && ch<='9')
    {
      scan_number();
      return;
    }

    if (ch == '.')
    {
      int ch2 = reader->peek(2);
      if (ch2>='0' && ch2<='9')
      {
        scan_number();
        return;
      }
    }

    switch (reader->read())
    {
      case '!': 
        if (reader->consume('=')) next->type = TOKEN_NE; 
        else                      next->type = TOKEN_BANG; 
        return;

      case '%':
        if (reader->consume('=')) next->type = TOKEN_MOD_ASSIGN;
        else                      next->type = TOKEN_PERCENT;
        return;

      case '&':
        if (reader->consume('&'))      next->type = TOKEN_LOGICAL_AND;
        else if (reader->consume('=')) next->type = TOKEN_AND_ASSIGN;
        else                           next->type = TOKEN_AMPERSAND;
        return;

      case '(': next->type = TOKEN_LPAREN;    return;
      case ')': next->type = TOKEN_RPAREN;    return;

      case '*':
        if (reader->consume('=')) next->type = TOKEN_MUL_ASSIGN;
        else                      next->type = TOKEN_STAR;
        return;

      case '+':
        if (reader->consume('='))      next->type = TOKEN_ADD_ASSIGN;
        else if (reader->consume('+')) next->type = TOKEN_INCREMENT;
        else                           next->type = TOKEN_PLUS;
        return;

      case ',': next->type = TOKEN_COMMA;     return;

      case '-':
        if (reader->consume('='))      next->type = TOKEN_SUB_ASSIGN;
        else if (reader->consume('-')) next->type = TOKEN_DECREMENT;
        else                           next->type = TOKEN_MINUS;
        return;

      case '.': 
        next->type = TOKEN_PERIOD;
        return;

      case '/':
        if (reader->consume('/'))
        {
          // Discard single-line comment and return terminating EOL.
          while (reader->peek() != 10) reader->read();
          next->column = reader->column;
          reader->read();
          //next->type = TOKEN_EOL;
          //return;
          continue;
        }
        else if (reader->consume('*'))
        {
          // Discard multi-line comment.
          while ((ch = reader->read()) != -1)
          {
            if (ch == '*')
            {
              if (reader->consume('/'))
              {
                goto prep_next_token;
              }
            }
          }
          throw next->error( "End of file looking for end of comment (\"*" "/\")." );
        }
        else if (reader->consume('='))
        {
          next->type = TOKEN_DIV_ASSIGN;
        }
        else
        {
          next->type = TOKEN_SLASH;
        }
        return;

      case ':': next->type = TOKEN_COLON;        return;
      case ';': next->type = TOKEN_SEMICOLON;    return;

      case '<':
        if (reader->consume('<'))
          if (reader->consume('=')) next->type = TOKEN_SHL_ASSIGN;
          else                      next->type = TOKEN_SHL;
        else if (reader->consume('=')) next->type = TOKEN_LE;
        else                           next->type = TOKEN_LT;
        return;

      case '=':
        if (reader->consume('=')) next->type = TOKEN_EQ;
        else                      next->type = TOKEN_ASSIGN;
        return;

      case '>':
        if (reader->consume('>'))
        {
          if (reader->consume('>'))
          {
            if (reader->consume('=')) next->type = TOKEN_SHR_ASSIGN;
            else                      next->type = TOKEN_SHR;
          }
          else
          {
            if (reader->consume('=')) next->type = TOKEN_SHRX_ASSIGN;
            else                      next->type = TOKEN_SHRX;
          }
        }
        else if (reader->consume('=')) next->type = TOKEN_GE;
        else                           next->type = TOKEN_GT;
        return;

      case '?': next->type = TOKEN_QUESTIONMARK; return;

      case '[': next->type = TOKEN_LBRACKET;   return;
      case '\\':next->type = TOKEN_BACKSLASH;  return;
      case ']': next->type = TOKEN_RBRACKET;   return;

      case '^': 
        if (reader->consume('=')) next->type = TOKEN_XOR_ASSIGN;
        else next->type = TOKEN_CARET;
        return;

      case '{': next->type = TOKEN_LCURLY;    return;

      case '|': 
        if (reader->consume('=')) next->type = TOKEN_OR_ASSIGN;
        else if (reader->consume('|')) next->type = TOKEN_LOGICAL_OR;
        else next->type = TOKEN_PIPE;
        return;

      case '}': next->type = TOKEN_RCURLY;    return;
      case '~': next->type = TOKEN_TILDE;     return;

      case '\'':
        next->type = TOKEN_LITERAL_CHAR;
        unicode_buffer.clear();
        unicode_buffer.print( scan_char() );
        next->content = new JogString(unicode_buffer.to_string());
        must_consume_char('\'');
        return;

      case '"':
        scan_string();
        must_consume_char('"');
        return;

      default:
        next->type = TOKEN_UNKNOWN;
        throw next->error("Unrecognized symbol.");
    }
  }
}

void JogScanner::error( const char* mesg )
{
  throw peek()->error(mesg);
}

bool JogScanner::consume_char( int ch )
{
  if (reader->peek() != ch) return false;
  reader->read();
  return true;
}

void JogScanner::must_consume_char( int ch )
{
  if (consume_char(ch)) return;

  StringBuilder buffer;
  buffer.print( '\"' );
  buffer.print( (char) ch );
  buffer.print( "\" expected." );
  error( buffer.to_string() );
}

void JogScanner::scan_number()
{
  int ch = reader->read();
  int ch2 = reader->peek();

  if (ch == '0' && ch2 != '.' && ch2 != 'e' && ch2 != 'E' && ch2 != 'f' && ch2 != 'F')
  {
    if (reader->consume('x'))
    {
      // hex literal
      long long int n = 0;
      int bits = 0;
      ch = reader->peek();
      while ((ch>='a' && ch<='f') || (ch>='A' && ch<='F') || (ch>='0' && ch<='9'))
      {
        reader->read();
        if (ch >= '0' && ch <= '9')      ch -= '0';
        else if (ch >= 'a' && ch <= 'f') ch -= ('a'-10);
        else                             ch -= ('A'-10);

        n = (n << 4) | ch;
        bits += 4;

        ch = reader->peek();
      }

      if (bits == 0)
      {
        throw next->error( "Expected a hexadecimal number after \"0x\"." );
      }

      if (reader->consume('L') || reader->consume('l'))
      {
        if (bits > 64)
        {
          throw next->error( "Literal 'long' is too large." );
        }
        next->type = TOKEN_LITERAL_LONG;
        next->content = to_hex_string( n, 64 );
      }
      else
      {
        if (bits <= 32)
        {
          next->type = TOKEN_LITERAL_INT;
          next->content = to_hex_string( n, 32 );
        }
        else
        {
          throw next->error( "Literal 'int' is too large." );
        }
      }
    }
    else
    {
      // octal literal - convert to hex for storage
      buffer.clear();
      long long int n = 0;
      int bits = 0;
      ch = reader->peek();
      while (ch>='0' && ch<='7')
      {
        reader->read();
        n = (n << 3) | (ch - '0');
        buffer.print((char)ch);
        bits += 3;
        ch = reader->peek();
      }

      if (reader->consume('L') || reader->consume('l'))
      {
        if (bits > 64)
        {
          if (buffer.count > 22 || buffer[0] > '1')
          {
            throw next->error( "Literal 'long' is too large." );
          }
        }
        next->type = TOKEN_LITERAL_LONG;
        next->content = to_hex_string( n, 64 );
      }
      else
      {
        if (bits > 32)
        {
          if (buffer.count > 11 || buffer[0] > '3')
          {
            throw next->error( "Literal 'int' is too large." );
          }
        }

        next->type = TOKEN_LITERAL_INT;
        next->content = to_hex_string( n, 32 );
      }
    }
  }
  else
  {
    // Base 10 integer or real - scan all digits into buffer and process
    // it after.
    buffer.clear();
    bool is_real = false;
    bool saw_exponent = false;

    buffer.print( (char) ch );
    ch = reader->peek();
    for (;;)
    {
      if (ch >= '0' && ch <= '9')
      {
        buffer.print( (char) ch );
      }
      else if (ch == '.')
      {
        if (is_real) break;
        is_real = true;
        buffer.print('.');
      }
      else if (ch == 'e' || ch == 'E')
      {
        if (saw_exponent) throw next->error( "Syntax error in literal real number." );
        buffer.print('E');
        is_real = true;
        saw_exponent = true;
        reader->read();
        if (reader->consume('-')) buffer.print('-');
        ch = reader->peek();
        continue;
      }
      else
      {
        break;
      }
      reader->read();
      ch = reader->peek();
    }

    if (is_real)
    {
      scan_real();
    }
    else
    {
      // base 10 integer
      long long int n = 0;
      for (int i=0; i<buffer.count; ++i)
      {
        ch = buffer[i];
        if (n < 0)
        {
          throw next->error( "Literal 'long' is too large." );
        }
        n = n * 10 + (ch - '0');
      }

      if (reader->consume('L') || reader->consume('l'))
      {
        static long long int lowest_64 = 0;
        if ( !lowest_64 )
        {
          lowest_64 = 0x80000000L;
          lowest_64 <<= 32;
        }
        if (n == lowest_64)
        {
          next->type = TOKEN_LITERAL_LONG_V;
        }
        else if (n < 0)
        {
          throw next->error( "Literal 'long' is too large." );
        }
        else
        {
          next->type = TOKEN_LITERAL_LONG;
          next->content = to_hex_string( n, 64 );
        }
      }
      else if (reader->consume('F') || reader->consume('f'))
      {
        // change to float
        float f = (float) n;
        next->type = TOKEN_LITERAL_FLOAT;
        next->content = to_hex_string( (long long int)(*(JogInt32*)&f), 32 );
      }
      else
      {
        if (n == (1L<<31L))
        {
          next->type = TOKEN_LITERAL_INT_V;
        }
        else if (n & ~0x7fffffff)
        {
          throw next->error( "Literal 'int' is too large." );
        }
        else
        {
          next->type = TOKEN_LITERAL_INT;
          next->content = to_hex_string( n, 32 );
        }
      }
    }
  }
}

void JogScanner::scan_real()
{
  double n = 0;
  int power = 0;

  // whole part
  int i;
  for (i=0; i<buffer.count; ++i)
  {
    int ch = buffer[i];
    if (ch >= '0' && ch <= '9')
    {
      n = n * 10 + (ch - '0');
    }
    else
    {
      break;
    }
  }

  if (i < buffer.count && buffer[i] == '.')
  {
    for (++i; i<buffer.count; ++i)
    {
      int ch = buffer[i];
      if (ch >= '0' && ch <= '9')
      {
        n = n * 10 + (ch - '0');
        --power;
      }
      else
      {
        break;
      }
    }
  }

  if (i < buffer.count && buffer[i] == 'E')
  {
    int negative = false;
    int exponent = 0;
    ++i;
    if (i < buffer.count && buffer[i] == '-')
    {
      negative = true;
      ++i;
    }

    for (; i<buffer.count; ++i)
    {
      int ch = buffer[i];
      if (ch >= '0' && ch <= '9')
      {
        exponent = exponent * 10 + (ch - '0');
      }
      else
      {
        break;
      }
    }

    if (negative) power -= exponent;
    else          power += exponent;
  }

  while (power < 0)
  {
    n = n / 10.0;
    ++power;
  }

  while (power > 0)
  {
    n = n * 10.0;
    --power;
  }

  if (reader->consume('f') || reader->consume('F'))
  {
    float f = (float) n;
    next->type = TOKEN_LITERAL_FLOAT;
    next->content = to_hex_string( (long long int)(*(JogInt32*)&f), 32 );
  }
  else
  {
    next->type = TOKEN_LITERAL_DOUBLE;
    next->content = to_hex_string( *((long long int*)&n), 64 );
  }
}

Ref<JogString> JogScanner::to_hex_string( long long int n, int bits )
{
  unicode_buffer.clear();
  while (bits)
  {
    int digit = (n >> (bits-4)) & 15;
    if (digit <= 9) digit += '0';
    else            digit += ('A'-10);
    unicode_buffer.print( (char) digit );
    bits -= 4;
  }
  return new JogString( unicode_buffer.to_string() );
}

void JogScanner::scan_string()
{
  unicode_buffer.clear();
  while (reader->peek() != '"')
  {
    int ch = scan_char();
    if (ch == -1)
    {
      throw next->error( "Closing quotes (\") expected." );
    }
    unicode_buffer.print( (char) ch );
  }
  next->content = new JogString(unicode_buffer.to_string());
  next->type = TOKEN_STRING;
}

int JogScanner::scan_char()
{
  int ch = reader->read();

  if (ch == '\\')
  {
    // escape sequence
    ch = reader->read();
    switch (ch)
    {
      case 'b': return 8;
      case 't': return 9;
      case 'n': return 10;
      case 'f': return 12;
      case 'r': return 13;
      case '"': return '"';
      case '\'': return '\'';
      case '\\': return '\\';
      default:
        if (jog_is_digit(ch,8))
        {
          int d1 = jog_char_to_value(ch);
          if (jog_is_digit(reader->peek(),8))
          {
            int d2 = jog_char_to_value(reader->read());
            if (d1 < 4 && jog_is_digit(reader->peek(),8))
            {
              int d3 = jog_char_to_value(reader->read());
              return (d1<<6) | (d2<<3) | d3;
            }
            else
            {
              return (d1<<3) | d2;
            }
          }
          else
          {
            return d1;
          }
        }
        error( "The special backslash character '\\' is followed by an illegal "
            "character sequence." );
    }
  }
  else
  {
    if (ch < 32 || ch == 127) return -1;
    return ch;
  }
  return -1;
}


