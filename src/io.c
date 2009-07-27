/*
 * The Sly Scheme I/O
 * Copyright (c) 2009 Alex Queiroz <asandroq@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sly.h"
#include "io.h"
#include "object.h"

#define SLY_UCS_CHAR_TAB            0x0009
#define SLY_UCS_LINE_FEED           0x000A
#define SLY_UCS_LINE_TAB            0x000B
#define SLY_UCS_FORM_FEED           0x000C
#define SLY_UCS_CAR_RETURN          0x000D
#define SLY_UCS_SPACE               0x0020
#define SLY_UCS_NEXT_LINE           0x0085
#define SLY_UCS_LINE_SEP            0x2028
#define SLY_UCS_PARA_SEP            0x0029
#define SLY_UCS_LEFT_QUOTE          0x201C
#define SLY_UCS_RIGHT_QUOTE         0x201D
#define SLY_UCS_ELLIPSIS            0x2026
#define SLY_UCS_FRACTION_SLASH      0x2044

/* using the Unicode private range */
#define SLY_UCS_EOF                 0xF000
#define SLY_UCS_NO_CHAR             0xF001

/*
 * character sets
 */
static const sly_char_t delim_set[]     = {'(', ')', '[', ']', '"', ';', '#', '\0'};
static const sly_char_t pec_begin_set[] = {'+', '-', '.', '\0'};
static const sly_char_t sym_begin_set[] = {'!', '$', '%', '&', '*', '/',
                                           ':', '<', '=', '>', '?', '^',
                                           '_', '~', '\0'};

/*
 * some string constants
 */

static const sly_char_t false_str[]     = {'#', 'f', '\0'};
static const sly_char_t true_str[]      = {'#', 't', '\0'};
static const sly_char_t space_str[]     = {'#', '\\', 's', 'p', 'a', 'c', 'e', '\0'};
static const sly_char_t newline_str[]   = {'#', '\\', 'n', 'e', 'w', 'l', 'i', 'n', 'e', '\0'};
static const sly_char_t ellipsis_str[]  = {'.', '.', '.', '\0'};

static int is_line_ending(sly_char_t c)
{
  return c == SLY_UCS_LINE_FEED || c == SLY_UCS_CAR_RETURN ||
         c == SLY_UCS_NEXT_LINE || c == SLY_UCS_LINE_SEP;
}

static int is_space(sly_char_t c)
{
  return is_line_ending(c) || c == SLY_UCS_SPACE ||
         (c >= SLY_UCS_CHAR_TAB && c <= SLY_UCS_CAR_RETURN) ||
         (c > 0x1FFF && c < 0x200B);
}

static int is_char_in_set(sly_char_t c, const sly_char_t *set)
{
  const sly_char_t *p;

  p = set;
  while(*p != 0) {
    if(c == *p++) {
      return 1;
    }
  }

  return 0;
}

static int str_len(const sly_char_t *str)
{
  int i;

  for(i = 0; str[i] != 0; i++);

  return i;
}

/*
 * char encodings
 */

static int char2utf8(sly_char_t c, uint8_t *buf, uint8_t *sz)
{
  int ret = 1;

  if(c < 0x80) {
    *sz = 1;
    buf[0] = (uint8_t)c;
  } else if(c < 0x800) {
    *sz = 2;
    buf[0] = 0xC0 | (uint8_t)((c >>  6) & 0x1F);
    buf[1] = 0x80 | (uint8_t)(c & 0x3F);
  } else if(c > 0xD7FF && c < 0xE000) {
    /* this range is for surrogates */
    ret = 0;
  } else if(c < 0x10000) {
    *sz = 3;
    buf[0] = 0xE0 | (uint8_t)((c >> 12) & 0x0F);
    buf[1] = 0x80 | (uint8_t)((c >>  6) & 0x3F);
    buf[2] = 0x80 | (uint8_t)(c & 0x3F);
  } else if(c < 0x110000) {
    *sz = 4;
    buf[0] = 0xF0 | (uint8_t)((c >> 18) & 0x07);
    buf[1] = 0x80 | (uint8_t)((c >> 12) & 0x3F);
    buf[2] = 0x80 | (uint8_t)((c >>  6) & 0x3F);
    buf[3] = 0x80 | (uint8_t)(c & 0x3F);
  } else {
    ret = 0;
  }

  return ret;
}

static int char2utf16(sly_char_t c, uint8_t *buf_, uint8_t *sz)
{
  int ret = 1;
  uint16_t *buf = (uint16_t*)buf_;

  if(c > 0xD7FF && c < 0xE000) {
    /* this range is for surrogates */
    ret = 0;
  } else if(c < 0xFFFF) {
    *sz = 2;
    buf[0] = (uint16_t)c;
  } else if(c < 0x110000) {
    *sz = 4;
    c -= 0x10000;
    buf[0] = 0xD800 | (uint16_t)((c >> 10) & 0x3FF);
    buf[1] = 0xDC00 | (uint16_t)(c & 0x3FF);
  } else {
    ret = 0;
  }

  return ret;
}

static int char2latin1(sly_char_t c, uint8_t *buf, uint8_t *sz)
{
  int ret = 1;

  *sz = 1;
  if(c < 0x100) {
    buf[0] = (uint8_t)c;
  } else if(c > 0x1FFF && c < 0x200B) {
    /* several compatibility spaces */
    buf[0] = (uint8_t)' ';
  } else if(c == SLY_UCS_LEFT_QUOTE ||
            c == SLY_UCS_RIGHT_QUOTE) {
    buf[0] = (uint8_t)'"';
  } else if(c == SLY_UCS_ELLIPSIS) {
    *sz = 3;
    buf[0] = buf[1] = buf[2] = '.';
  } else if(c == SLY_UCS_FRACTION_SLASH) {
    buf[0] = (uint8_t)'/';
  } else if(c > 0xD7FF && c < 0xE000) {
    /* this range is for surrogates */
    ret = 0;
  } else {
    buf[0] = 191; /* inverted question mark */
  }

  return ret;
}

static uint8_t* from_string(sly_string_t *str, uint8_t char_enc)
{
  uint32_t i, size;
  uint8_t *ret, sz, buf[4];
  int (*func)(sly_char_t,uint8_t*,uint8_t*);

  switch(char_enc) {
  case SLY_CHAR_ENC_UTF8:
    func = char2utf8;
    break;
  case SLY_CHAR_ENC_UTF16:
    func = char2utf16;
    break;
  case SLY_CHAR_ENC_LATIN1:
    func = char2latin1;
    break;
  default:
    return NULL;
  }

  /* calculating size of final string */
  for(size = 0, i = 0; i < str->size; i++, size += sz) {
    if(!func(str->chars[i], buf, &sz)) {
      return NULL;
    }
  }

  ret = (uint8_t*)malloc((size+1) * sizeof(uint8_t));
  if(!ret) {
    return NULL;
  }

  ret[size] = '\0';
  for(size = 0, i = 0; i < str->size; i++, size += sz) {
    func(str->chars[i], buf, &sz);
    memcpy(&ret[size], buf, sz * sizeof(uint8_t));
  }

  return ret;
}

/*
 * A string buffer
 */

struct sly_sbuffer_t {
  size_t pos;
  size_t size;
  sly_char_t *str;
};

sly_sbuffer_t* sly_io_create_sbuffer(void)
{
  sly_sbuffer_t *ret;

  ret = (sly_sbuffer_t*) malloc(sizeof(sly_sbuffer_t));
  if(ret == NULL) {
    return NULL;
  }

  /* most identifiers are small */
  ret->pos = 0;
  ret->size = 32;
  ret->str = (sly_char_t*) malloc(ret->size * sizeof(sly_char_t));
  if(ret->str == NULL) {
    free(ret);
    return NULL;
  }
  ret->str[0] = '\0';

  return ret;
}

void sly_io_destroy_sbuffer(sly_sbuffer_t* buffer)
{
  if(buffer != NULL) {
    if(buffer->str != NULL) {
      free(buffer->str);
    }
    free(buffer);
  }
}

void sly_sbuffer_assign(sly_sbuffer_t* buffer, const char* str)
{
  size_t i, len;

  len = strlen(str) + 1;
  if(len > buffer->size) {
    sly_char_t *t;

    t = (sly_char_t*) realloc(buffer->str, len * sizeof(sly_char_t));
    if(t == NULL) {
      return;
    }
    buffer->str = t;
    buffer->size = len;
  }

  for(i = 0; i < len; i++) {
    buffer->str[i] = str[i];
  }
  buffer->pos = len - 1;
  buffer->str[len] = '\0';
}

void sly_sbuffer_add(sly_sbuffer_t* buffer, sly_char_t c)
{
  if(buffer == NULL) {
    return;
  }

  if(buffer->pos == buffer->size - 1) {
    sly_char_t *t;
    size_t size;

    size = buffer->size * 3 / 2;
    t = (sly_char_t*) realloc(buffer->str, size * sizeof(sly_char_t));
    if(t == NULL) {
      return;
    }
    buffer->str = t;
    buffer->size = size;
  }
  
  buffer->str[buffer->pos++] = c;
  buffer->str[buffer->pos] = '\0';
}

int sly_sbuffer_equalp(sly_sbuffer_t* buffer, const sly_char_t* str)
{
  return buffer->pos == (uint32_t)str_len(str) &&
         memcmp(buffer->str, str, buffer->pos * sizeof(sly_char_t)) == 0;
}

/*
 * file ports
 */

struct fp_priv {
  FILE *f;
  sly_char_t chr;
  uint8_t closable;
};

static int fp_finish(sly_port_t *self)
{
  int ret;
  struct fp_priv *priv;

  priv = (struct fp_priv*)SLY_PORT(self)->private;

  if(priv->closable) {
    ret = fclose(priv->f) == 0;
  } else {
    ret = 1;
  }
  free(priv);

  return ret;
}

static int fp_read_char_utf8(sly_iport_t *self, sly_char_t *c)
{
  FILE* f;
  uint8_t b1, b2[3];

  f = ((struct fp_priv*)SLY_PORT(self)->private)->f;
  if(fread(&b1, sizeof(uint8_t), 1, f) < 1) {
    if(feof(f)) {
      *c = SLY_UCS_EOF;
      return 1;
    } else {
      return 0;
    }
  }

  if(b1 >= 0xC0) {
    if(b1 < 0xE0) {
      if(fread(b2, sizeof(uint8_t), 1, f) < 1) {
        if(feof(f)) {
          *c = SLY_UCS_EOF;
          return 1;
        } else {
          return 0;
        }
      }
      if((b2[0] & 0xC0) == 0x80) {
        *c = ((b1 & 0x1F) << 6) | (b2[0] & 0x3F);
      } else {
        return 0;
      }
    } else if(b1 < 0xF0) {
      if(fread(b2, sizeof(uint8_t), 2, f) < 2) {
        if(feof(f)) {
          *c = SLY_UCS_EOF;
          return 1;
        } else {
          return 0;
        }
      }
      if(((b2[0] & 0xC0) == 0x80) && ((b2[1] & 0xC0) == 0x80)) {
        *c = ((b1 & 0x0F) << 12) |
             ((b2[0] & 0x3F) << 6) |
              (b2[1] & 0x3F);
      } else {
        return 0;
      }
    } else if(b1 <= 0xF4) {
      if(fread(b2, sizeof(uint8_t), 3, f) < 3) {
        if(feof(f)) {
          *c = SLY_UCS_EOF;
          return 1;
        } else {
          return 0;
        }
      }
      if(((b1 == 0xF4 && ((b2[0] & 0xF0) == 0x80)) ||
          ((b2[0] & 0xC0) == 0x80)) &&
          ((b2[1] & 0xC0) == 0x80) &&
          ((b2[2] & 0xC0) == 0x80)) {
        *c = ((b1 & 0x7) << 18) |
             ((b2[0] & 0x3F) << 12) |
             ((b2[1] & 0x3F) << 6) |
              (b2[2] & 0x3F);
      } else {
        return 0;
      }
    } else {
      return 0;
    }
  } else if(b1 & 0x80) {
    return 0;
  } else {
    *c = (sly_char_t)b1;
  }

  return 1;
}

static int fp_read_char_utf16(sly_iport_t *self, sly_char_t *c)
{
  FILE* f;
  uint8_t *b1, *b2;
  sly_ucs2_t c1, c2;

  b1 = (uint8_t*)&c1;
  b2 = (uint8_t*)&c2;

  f = ((struct fp_priv*)SLY_PORT(self)->private)->f;
  if(fread(b1, sizeof(uint8_t), 2, f) < 2) {
    if(feof(f)) {
      *c = SLY_UCS_EOF;
      return 1;
    } else {
      return 0;
    }
  }

  if((c1 & 0xFC00) != 0xD800) {
    /* non-surrogate */
    *c = (sly_char_t)c1;
  } else {
    if(fread(b2, sizeof(uint8_t), 2, f) < 2) {
      if(feof(f)) {
        *c = SLY_UCS_EOF;
        return 1;
      } else {
        return 0;
      }
    }

    if((c2 & 0xFC00) != 0xDC00) {
      /* broken surrogate pair */
      return 0;
    }

    *c = (((c1 & 0x3FF) << 10) | (c2 & 0x3FF)) + 0x10000;
  }

  return 1;
}

static int fp_read_char_latin1(sly_iport_t *self, sly_char_t *c)
{
  FILE* f;
  uint8_t b;

  f = ((struct fp_priv*)SLY_PORT(self)->private)->f;
  if(fread(&b, sizeof(uint8_t), 1, f) < 1) {
    if(feof(f)) {
      *c = SLY_UCS_EOF;
      return 1;
    } else {
      return 0;
    }
  }

  *c = (sly_char_t)b;

  return 1;
}

static int fp_read_once(sly_iport_t *self, sly_char_t *c)
{
  switch(SLY_PORT(self)->char_enc) {
  case SLY_CHAR_ENC_UTF8:
    return fp_read_char_utf8(self, c);

  case SLY_CHAR_ENC_UTF16:
    return fp_read_char_utf16(self, c);

  case SLY_CHAR_ENC_LATIN1:
    return fp_read_char_latin1(self, c);

  default:
    return 0;
  }
}

static int fp_read_char(sly_iport_t *self, sly_char_t *c)
{
  struct fp_priv *p = (struct fp_priv*)SLY_PORT(self)->private;

  if(p->chr == SLY_UCS_NO_CHAR) {
    if(!fp_read_once(self, &p->chr)) {
      return 0;
    }
  }

  *c = p->chr;
  if(!fp_read_once(self, &p->chr)) {
    return 0;
  }

  return 1;
}

static int fp_peek_char(sly_iport_t *self, sly_char_t *c)
{
  struct fp_priv *p = (struct fp_priv*)SLY_PORT(self)->private;

  if(p->chr == SLY_UCS_NO_CHAR) {
    if(!fp_read_once(self, &p->chr)) {
      return 0;
    }
  }

  *c = p->chr;

  return 1;
}

static int fp_flush(sly_oport_t *p)
{
  return fflush(((struct fp_priv*)SLY_PORT(p)->private)->f) == 0;
}

static int fp_write_char(sly_oport_t *p, sly_char_t c)
{
  int ret;
  FILE *f;
  uint8_t sz, buf[4];

  switch(SLY_PORT(p)->char_enc) {
  case SLY_CHAR_ENC_UTF8:
    ret = char2utf8(c, buf, &sz);
    break;
  case SLY_CHAR_ENC_UTF16:
    ret = char2utf16(c, buf, &sz);
    break;
  case SLY_CHAR_ENC_LATIN1:
    ret = char2latin1(c, buf, &sz);
    break;
  default:
    ret = 0;
    break;
  }

  if(ret) {
    f = ((struct fp_priv*)SLY_PORT(p)->private)->f;
    if(fwrite(buf, sizeof(uint8_t), sz, f) < sz) {
      return 0;
    }
  }

  return ret;
}

/*
 * generic port interface
 */

static sly_char_t peek_char(sly_state_t* S, sly_iport_t *p)
{
  sly_char_t res;

  if(p->peek_char(p, &res)) {
    return res;
  } else {
    sly_push_string(S, "cannot peek char");
    return (sly_char_t)sly_error(S, 1);
  }
}

static sly_char_t read_char(sly_state_t* S, sly_iport_t *p)
{
  sly_char_t res;

  if(p->read_char(p, &res)) {
    return res;
  } else {
    sly_push_string(S, "cannot read char");
    return (sly_char_t)sly_error(S, 1);
  }
}

static void flush(sly_state_t* S, sly_oport_t *p)
{
  if(!p->flush(p)) {
    sly_push_string(S, "cannot flush port");
    sly_error(S, 1);
  }
}

static void write_char(sly_state_t* S, sly_oport_t *p, sly_char_t c)
{
  if(!p->write_char(p, c)) {
    sly_push_string(S, "cannot write char");
    sly_error(S, 1);
  }
}

/*
 * reader
 */

static void read_string(sly_state_t* S, sly_iport_t* in, sly_sbuffer_t* buf)
{
  sly_char_t c;

  /* remove left quote */
  read_char(S, in);
  
  c = read_char(S, in);
  sly_sbuffer_assign(buf, "");

  while(c != '"') {

    if(c == SLY_UCS_EOF) {
      sly_push_string(S, "unterminated string");
      sly_error(S, 1);
    }

    if(c == '\\') {
      c = read_char(S, in);

      if(c == SLY_UCS_EOF) {
        sly_push_string(S, "unterminated string");
        sly_error(S, 1);
      }

      switch(c) {
      case 'a':
        c = '\a';
        break;

      case 'b':
        c = '\b';
        break;

      case 't':
        c = '\t';
        break;

      case 'n':
        c = '\n';
        break;

      case 'v':
        c = '\v';
        break;

      case 'f':
        c = '\f';
        break;

      case 'r':
        c = '\r';
        break;
      }
    }

    sly_sbuffer_add(buf, c);
    c = read_char(S, in);
  };
}

static void read_till_delimiter(sly_state_t* S, sly_iport_t* in, sly_sbuffer_t* buf)
{
  sly_char_t c;

  sly_sbuffer_assign(buf, "");

  c = read_char(S, in);
  sly_sbuffer_add(buf, c);

  c = peek_char(S, in);
  while(!is_space(c) && c != SLY_UCS_EOF && !is_char_in_set(c, delim_set)) {
    sly_sbuffer_add(buf, c);
    read_char(S, in);
    c = peek_char(S, in);
  };
}

static int digit_value(sly_char_t c, int base, uint8_t *value)
{
  int ret;
  uint8_t i;
  static const sly_char_t digits[] = {'0', '1', '2', '3', '4', '5', '6', '7',
                                      '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};

  ret = 0;
  for(i = 0; i < base; i++) {
    if(c == digits[i]) {
      ret = 1;
      if(value) {
        *value = i;
      }
      break;
    }
  }

  return ret;
}

static int parse_number(sly_sbuffer_t *buf, sly_object_t *res)
{
  uint8_t val;
  sly_char_t *str;
  uint32_t exp, ret;
  int i, j, base, is_exact;

  i = 0;
  base = -1;
  is_exact = -1;
  str = buf->str;

  /* are there prefixes? */
  while(str[i] == '#') {
    ++i;
    if(str[i] == 'i') {
      if(is_exact != -1) {
	return 0;
      }
      is_exact = 0;
    } else if(str[i] == 'e') {
      if(is_exact != -1) {
	return 0;
      }
      is_exact = 1;
    } else if(str[i] == 'b') {
      if(base != -1) {
	return 0;
      }
      base = 2;
    } else if(str[i] == 'd') {
      if(base != -1) {
	return 0;
      }
      base = 10;
    } else if(str[i] == 'o') {
      if(base != -1) {
	return 0;
      }
      base = 8;
    } else if(str[i] == 'x') {
      if(base != -1) {
	return 0;
      }
      base = 16;
    } else {
      return 0;
    }
    ++i;
  }

  base = base == -1 ? 10 : base;
  is_exact = is_exact == -1 ? 1 : is_exact;

  /* for now, just fixnums */
  ret = 0;
  exp = 1;
  for(j = buf->pos - 1; j >= i; j--) {
    if(!digit_value(buf->str[j], base, &val)) {
      return 0;
    }

    ret += val * exp;
    exp *= base;
  }
  res->type = SLY_TYPE_FIXNUM;
  res->value.fixnum = ret;

  return 1;
}

static void sly_io_read_i(sly_state_t* S, sly_sbuffer_t *buf, sly_iport_t* in, sly_object_t* res)
{
  sly_char_t c;
  sly_gcobject_t *obj;

  res->type = SLY_TYPE_UNDEF;

  c = peek_char(S, in);
  while(is_space(c) || c == ';') {
    /* eating up whitespace and comments */
    while(is_space(c)) {
      read_char(S, in);
      c = peek_char(S, in);
    }

    if(c == ';') {
      while(!is_line_ending(c)) {
	c = read_char(S, in);
      }
      c = peek_char(S, in);
    }
  }

  if(c == SLY_UCS_EOF) {
    read_char(S, in);
    res->type = SLY_TYPE_EOF;
    return;
  }

  /* strings */
  if(c == '"') {
    read_string(S, in, buf);
    res->type = SLY_TYPE_STRING;
    res->value.gc = sly_create_string(S, buf->str, buf->pos);
    return;
  }

  if(c == '#') {
    read_till_delimiter(S, in, buf);
    if(sly_sbuffer_equalp(buf, false_str)) {
      /* boolean false */
      res->type = SLY_TYPE_BOOL;
      res->value.bool = 0;
    } else if(sly_sbuffer_equalp(buf, true_str)) {
      /* boolean true */
      res->type = SLY_TYPE_BOOL;
      res->value.bool = 1;
    } else if(buf->str[1] == '\\') {
      /* character */
      res->type = SLY_TYPE_CHAR;
      if(sly_sbuffer_equalp(buf, space_str)) {
	res->value.chr = SLY_UCS_SPACE;
      } else if(sly_sbuffer_equalp(buf, newline_str)) {
	res->value.chr = SLY_UCS_LINE_FEED;
      } else if(buf->pos == 3) {
	res->value.chr = buf->str[2];
      } else {
        sly_push_string(S, "unknown read syntax");
        sly_error(S, 1);
      }
    } else if(buf->str[1] == 'i' ||
	      buf->str[1] == 'e' ||
	      buf->str[1] == 'b' ||
	      buf->str[1] == 'd' ||
	      buf->str[1] == 'o' ||
	      buf->str[1] == 'x') {
      /* number */
      if(!parse_number(buf, res)) {
        sly_push_string(S, "cannot parse number");
        sly_error(S, 1);
      }
    } else {
      sly_push_string(S, "unknown read syntax");
      sly_error(S, 1);
    }
    return;
  }

  /* symbols */
  if(is_char_in_set(c, pec_begin_set)) {
    read_till_delimiter(S, in, buf);
    if(buf->str[0] == '+' || buf->str[0] == '-') {
      if(buf->pos == 1) {
	obj = sly_create_string(S, buf->str, buf->pos);
	*res = sly_create_symbol(S, SLY_STRING(obj));
      } else {
	/* number */
        if(!parse_number(buf, res)) {
          sly_push_string(S, "cannot parse number");
          sly_error(S, 1);
        }
      }
    } else if(sly_sbuffer_equalp(buf, ellipsis_str)) {
      obj = sly_create_string(S, buf->str, buf->pos);
      *res = sly_create_symbol(S, SLY_STRING(obj));
    } else {
      sly_push_string(S, "unknown read syntax");
      sly_error(S, 1);
    }
    return;
  }

  if(is_char_in_set(c, sym_begin_set) || isalpha(c)) {
    read_till_delimiter(S, in, buf);
    obj = sly_create_string(S, buf->str, buf->pos);
    *res = sly_create_symbol(S, SLY_STRING(obj));
    return;
  }

  if(digit_value(c, 10, NULL)) {
    /* base 10 number */
    read_till_delimiter(S, in, buf);
    if(!parse_number(buf, res)) {
      sly_push_string(S, "cannot parse number");
      sly_error(S, 1);
    }
    return;
  }
}

sly_object_t sly_io_read(sly_state_t *S, sly_iport_t *p)
{
  sly_object_t ret;
  sly_sbuffer_t *buffer;

  buffer = sly_io_create_sbuffer();
  sly_io_read_i(S, buffer, p, &ret);
  sly_io_destroy_sbuffer(buffer);

  return ret;
}

/*
 * writer
 */

static void write_string(sly_state_t *S, sly_string_t* s, sly_oport_t *port, int quote)
{
  uint32_t i;

  if(quote) {
    write_char(S, port, '"');
  }

  for(i = 0; i < s->size; i++) {
    write_char(S, port, s->chars[i]);
  }

  if(quote) {
    write_char(S, port, '"');
  }
}

static void sly_io_write_i(sly_state_t *S, sly_object_t* obj,
                           sly_oport_t *port, int quote)
{
  uint32_t i;
  char buf[64];

  switch(obj->type) {

  case SLY_TYPE_UNDEF:
    sly_io_write_c_string(S, "<#undef>", port);
    break;

  case SLY_TYPE_NIL:
    sly_io_write_c_string(S, "()", port);
    break;

  case SLY_TYPE_BOOL:
    write_char(S, port, (sly_char_t)'#');
    if(obj->value.bool) {
      write_char(S, port, (sly_char_t)'t');
    } else {
      write_char(S, port, (sly_char_t)'f');
    }
    break;

  case SLY_TYPE_FIXNUM:
    snprintf(buf, 64, "%d", obj->value.fixnum);
    sly_io_write_c_string(S, buf, port);
    break;

  case SLY_TYPE_CHAR:
    if(quote) {
      write_char(S, port, (sly_char_t)'#');
      write_char(S, port, (sly_char_t)'\\');
    }

    write_char(S, port, obj->value.chr);
    break;

  case SLY_TYPE_SYMBOL:
    write_string(S, obj->value.symbol->str, port, 0);
    break;

  case SLY_TYPE_CLOSURE:
    sly_io_write_c_string(S, "<#closure>", port);
    break;

  case SLY_TYPE_PAIR: {
    sly_object_t cdr;
    sly_gcobject_t *p = obj->value.gc;

    write_char(S, port, (sly_char_t)'(');
    for(;;) {
    
      sly_io_write(S, &(SLY_PAIR(p)->car), port);

      cdr = SLY_PAIR(p)->cdr;
      if(cdr.type == SLY_TYPE_NIL) {
        break;
      } else if(cdr.type != SLY_TYPE_PAIR) {
        sly_io_write_c_string(S, " . ", port);
        sly_io_write(S, &cdr, port);
        break;
      } else {
        write_char(S, port, (sly_char_t)' ');
        p = cdr.value.gc;
      }
    }
    write_char(S, port, (sly_char_t)')');
  }
    break;

  case SLY_TYPE_CONTI:
    snprintf(buf, 64, "<#continuation %u>", SLY_CONTI(obj->value.gc)->size);
    sly_io_write_c_string(S, buf, port);
    break;

  case SLY_TYPE_BOX:
    sly_io_write_c_string(S, "#&", port);
    sly_io_write(S, &(SLY_BOX(obj->value.gc)->value), port);
    break;

  case SLY_TYPE_STRING:
    write_string(S, SLY_STRING(obj->value.gc), port, quote);
    break;

  case SLY_TYPE_VECTOR:
    sly_io_write_c_string(S, "#(", port);
    for(i = 0; i < SLY_VECTOR(obj->value.gc)->size; i++) {
      if(i != 0) {
        write_char(S, port, (sly_char_t)' ');
      }
      sly_io_write(S, SLY_VECTOR(obj->value.gc)->data + i, port);
    }
    sly_io_write_c_string(S, ")", port);
    break;

  case SLY_TYPE_INPUT_PORT:
    sly_io_write_c_string(S, "<#input port>", port);
    break;

  case SLY_TYPE_OUTPUT_PORT:
    sly_io_write_c_string(S, "<#output port>", port);
    break;

  default:
    sly_io_write_c_string(S, "Unknown type!", port);
  }
}

uint8_t *sly_io_to_latin1(sly_state_t *S, sly_string_t *str)
{
  uint8_t *ret;

  ret = from_string(str, SLY_CHAR_ENC_LATIN1);
  if(!ret) {
    sly_push_string(S, "error converting string to latin-1");
    sly_error(S, 1);
  }

  return ret;
}

uint8_t *sly_io_to_utf8(sly_state_t *S, sly_string_t *str)
{
  uint8_t *ret;

  ret = from_string(str, SLY_CHAR_ENC_UTF8);
  if(!ret) {
    sly_push_string(S, "error converting string to UTF-8");
    sly_error(S, 1);
  }

  return ret;
}

sly_ucs2_t *sly_io_to_utf16(sly_state_t *S, sly_string_t *str)
{
  sly_ucs2_t *ret;

  ret = (sly_ucs2_t*)from_string(str, SLY_CHAR_ENC_UTF16);
  if(!ret) {
    sly_push_string(S, "error converting string to UTF-16");
    sly_error(S, 1);
  }

  return ret;
}

sly_gcobject_t *sly_io_create_ifport(sly_state_t *S, FILE* file)
{
  sly_gcobject_t *port;
  struct fp_priv *priv;

  port = sly_create_iport(S);
  priv = (struct fp_priv*)malloc(sizeof(struct fp_priv));

  priv->f = file;
  priv->closable = 1;
  priv->chr = SLY_UCS_NO_CHAR;
  SLY_PORT(port)->private = priv;
  SLY_PORT(port)->finish = fp_finish;
  SLY_PORT(port)->type = SLY_TYPE_PORT_FILE;

  SLY_IPORT(port)->peek_char = fp_peek_char;
  SLY_IPORT(port)->read_char = fp_read_char;

  return port;
}

sly_gcobject_t *sly_io_create_ofport(sly_state_t *S, FILE* file)
{
  sly_gcobject_t *port;
  struct fp_priv *priv;

  port = sly_create_oport(S);
  priv = (struct fp_priv*)malloc(sizeof(struct fp_priv));

  priv->f = file;
  priv->closable = 1;
  priv->chr = SLY_UCS_NO_CHAR;
  SLY_PORT(port)->private = priv;
  SLY_PORT(port)->finish = fp_finish;
  SLY_PORT(port)->type = SLY_TYPE_PORT_FILE;

  SLY_OPORT(port)->flush = fp_flush;
  SLY_OPORT(port)->write_char = fp_write_char;

  return port;
}

sly_gcobject_t *sly_io_create_stdin(sly_state_t *S)
{
  sly_gcobject_t *port;

  port = sly_io_create_ifport(S, stdin);
  ((struct fp_priv*)SLY_PORT(port)->private)->closable = 0;

  return port;
}

sly_gcobject_t *sly_io_create_stdout(sly_state_t *S)
{
  sly_gcobject_t *port;

  port = sly_io_create_ofport(S, stdout);
  ((struct fp_priv*)SLY_PORT(port)->private)->closable = 0;

  return port;
}

sly_gcobject_t *sly_io_create_stderr(sly_state_t *S)
{
  sly_gcobject_t *port;

  port = sly_io_create_ofport(S, stderr);
  ((struct fp_priv*)SLY_PORT(port)->private)->closable = 0;

  return port;
}

sly_gcobject_t *sly_io_open_ifile(sly_state_t *S, sly_string_t *str, uint8_t char_enc)
{
  char *fname;
  FILE *file;

  fname = from_string(str, char_enc);
  if(!fname) {
    sly_push_string(S, "cannot convert file name");
    sly_error(S, 1);
  }

  file = fopen(fname, "rb");
  if(!file) {
    sly_push_string(S, "cannot open input file: ");
    sly_push_string(S, fname);
    free(fname);
    sly_error(S, 2);
  }
  free(fname);

  return sly_io_create_ifport(S, file);
}

sly_gcobject_t *sly_io_open_ofile(sly_state_t *S, sly_string_t *str, uint8_t char_enc)
{
  char *fname;
  FILE *file;

  fname = from_string(str, char_enc);
  if(!fname) {
    sly_push_string(S, "cannot convert file name");
    sly_error(S, 1);
  }

  file = fopen(fname, "wb");
  if(!file) {
    sly_push_string(S, "cannot open output file: ");
    sly_push_string(S, fname);
    free(fname);
    sly_error(S, 2);
  }
  free(fname);

  return sly_io_create_ofport(S, file);
}

void sly_io_close_iport(sly_state_t *S, sly_iport_t *port)
{
  S = S;

  if(SLY_PORT(port)->type == SLY_TYPE_PORT_FILE) {
    fclose(((struct fp_priv*)SLY_PORT(port)->private)->f);
  }
}

void sly_io_close_oport(sly_state_t *S, sly_oport_t *port)
{
  S = S;

  if(SLY_PORT(port)->type == SLY_TYPE_PORT_FILE) {
    fclose(((struct fp_priv*)SLY_PORT(port)->private)->f);
  }
}

void sly_io_write_c_string(sly_state_t *S, const char* s, sly_oport_t *port)
{
  const char *p;

  for(p = s; *p; p++) {
    write_char(S, port, (sly_char_t)*p);
  }
}

void sly_io_write_symbol(sly_state_t *S, sly_symbol_t *sym, sly_oport_t *port)
{
  write_string(S, sym->str, port, 0);
}

/* TODO: create 'eol' parameter */
void sly_io_newline(sly_state_t *S, sly_oport_t *port)
{
#ifdef _WIN32
  write_char(S, port, (sly_char_t)'\r');
#endif
  write_char(S, port, (sly_char_t)'\n');

  flush(S, port);
}

void sly_io_write(sly_state_t *S, sly_object_t *obj, sly_oport_t *port)
{
  sly_io_write_i(S, obj, port, 1);
  flush(S, port);
}

void sly_io_display(sly_state_t *S, sly_object_t *obj, sly_oport_t *port)
{
  sly_io_write_i(S, obj, port, 0);
  flush(S, port);
}

