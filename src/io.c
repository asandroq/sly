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

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif

#include "sly.h"
#include "io.h"
#include "object.h"

#define SLY_IO_PAUSE                10000

#define SLY_UCS_LEFT_QUOTE          0x201C
#define SLY_UCS_RIGHT_QUOTE         0x201D
#define SLY_UCS_ELLIPSIS            0x2026
#define SLY_UCS_FRACTION_SLASH      0x2044

/*
 * utilities
 */

char* sly_strdup(const char* str)
{
  char *ret;
  size_t len;

  len = strlen(str) + 1;
  ret = (char*) malloc(len * sizeof(char));
  if(ret == NULL) {
    return NULL;
  }
  strncpy(ret, str, len);

  return ret;
}

/*
 * char encodings
 */

typedef int (*sly_from_char_t)(sly_char_t,uint8_t*,uint8_t*);

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

static int char2utf16(sly_char_t c, uint16_t *buf, uint8_t *sz)
{
  int ret = 1;

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
  sly_from_char_t func;

  switch(char_enc) {
  case SLY_CHAR_ENC_UTF8:
    func = char2utf8;
    break;
  case SLY_CHAR_ENC_UTF16:
    func = (sly_from_char_t)char2utf16;
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
  char *str;
};

sly_sbuffer_t* sly_sbuffer_new(void)
{
  sly_sbuffer_t *ret;

  ret = (sly_sbuffer_t*) malloc(sizeof(sly_sbuffer_t));
  if(ret == NULL) {
    return NULL;
  }

  /* most identifiers are small */
  ret->pos = 0;
  ret->size = 32;
  ret->str = (char*) malloc(ret->size * sizeof(char));
  if(ret->str == NULL) {
    free(ret);
    return NULL;
  }
  ret->str[0] = '\0';

  return ret;
}

void sly_sbuffer_destroy(sly_sbuffer_t* buffer)
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
  size_t len;

  len = strlen(str) + 1;
  if(len > buffer->size) {
    char *t;

    t = (char*) realloc(buffer->str, len * sizeof(char));
    if(t == NULL) {
      return;
    }
    buffer->str = t;
    buffer->size = len;
  }

  buffer->pos = len - 1;
  strncpy(buffer->str, str, len);
}

void sly_sbuffer_add(sly_sbuffer_t* buffer, char c)
{
  if(buffer == NULL) {
    return;
  }

  if(buffer->pos == buffer->size - 1) {
    char *t;
    size_t size;

    size = buffer->size * 3 / 2;
    t = (char*) realloc(buffer->str, size * sizeof(char));
    if(t == NULL) {
      return;
    }
    buffer->str = t;
    buffer->size = size;
  }
  
  buffer->str[buffer->pos++] = c;
  buffer->str[buffer->pos] = '\0';
}

const char* sly_sbuffer_string(sly_sbuffer_t* buffer)
{
  if(buffer != NULL) {
    return buffer->str;
  } else {
    return NULL;
  }
}

int sly_sbuffer_equalp(sly_sbuffer_t* buffer, const char* str)
{
  return strcmp(buffer->str, str) == 0;
}

/*
 * file ports
 */

#ifdef _WIN32

static int fp_fill(sly_iport_t *p_)
{
  BOOL ret;
  DWORD nread;
  sly_ifport_t *p = SLY_IFPORT(p_);

  ret = ReadFile(p->in, p->buffer, SLY_PORT_BUF_SIZE, &nread, NULL);

  if(ret) {
    p->beg = 0;
    p->end = nread - 1;
    return 1;
  } else {
    if(GetLastError() == ERROR_HANDLE_EOF) {
      p->beg = 0;
      p->end = nread - 1;
      return 2;
    } else {
      return 0;
    }
  }
}

static int fp_flush(sly_oport_t *p_)
{
  BOOL ret;
  DWORD nwritten;
  sly_ofport_t *p = SLY_OFPORT(p_);

  ret = WriteFile(p->out,
                  &p->buffer[p->beg],
                  p->end - p->beg + 1,
                  &nwritten,
                  NULL);
  if(ret) {
    p->beg += nwritten - 1;
    if(p->beg == p->end - 1) {
      /* reset buffer if empty */
      p->beg = p->end = 0;
    }
  }

  return ret ? 1 : 0;
}

#else

static int fp_fill(sly_iport_t *p_)
{
  ssize_t ret;
  sly_ifport_t *p = SLY_IFPORT(p_);

 fill_again:
  ret = read(p->in,
             p->buffer,
             SLY_PORT_BUF_SIZE);
  if(ret < 0 && (errno == EAGAIN || errno == EINTR)) {
    usleep(SLY_IO_PAUSE);
    goto fill_again;
  }

  if(ret < 0) {
    /* error */
    return -1;
  } else if(ret == 0) {
    /* end of file */
    p->beg = p->end = 0;
    return 2;
  } else {
    p->beg = 0;
    p->end = ret - 1;
    return 1;
  }
}

static int fp_flush(sly_oport_t *p_)
{
  ssize_t ret;
  sly_ofport_t *p = SLY_OFPORT(p_);

 flush_again:
  ret = write(p->out,
              &p->buffer[p->beg],
              p->end - p->beg);
  if(ret < 0 && (errno == EAGAIN || errno == EINTR)) {
    usleep(SLY_IO_PAUSE);
    goto flush_again;
  }

  if(ret > 0) {
    p->beg += ret - 1;
    if(p->beg == p->end - 1) {
      /* reset buffer if empty */
      p->beg = p->end = 0;
    }
  }

  return ret < 0 ? 0 : 1;
}

#endif

static int fp_write_byte(sly_oport_t *p_, uint8_t b)
{
  sly_ofport_t *p = SLY_OFPORT(p_);

  while(p->end == SLY_PORT_BUF_SIZE) {
    if(!fp_flush(p_)) {
      return 0;
    }
  }

  p->buffer[p->end++] = b;

  return 1;
}

static int fp_write_char(sly_oport_t *p_, sly_char_t c)
{
  int ret;
  uint8_t sz, buf[4];
  sly_ofport_t *p = SLY_OFPORT(p_);

  switch(SLY_PORT(p)->char_enc) {
  case SLY_CHAR_ENC_UTF8:
    ret = char2utf8(c, buf, &sz);
    break;
  case SLY_CHAR_ENC_UTF16:
    ret = char2utf16(c, (uint16_t*)buf, &sz);
    break;
  case SLY_CHAR_ENC_LATIN1:
    ret = char2latin1(c, buf, &sz);
    break;
  default:
    ret = 0;
    break;
  }

  if(ret) {
    while(SLY_PORT_BUF_SIZE - p->end < sz) {
      if(!fp_flush(p_)) {
        return 0;
      }
    }

    memcpy(&p->buffer[p->end], buf, sz * sizeof(uint8_t));
    p->end += sz;
  } else {
    return 0;
  }

  return ret;
}

/*
 * generic port interface
 */

static uint8_t port_peek_byte(sly_state_t* S, sly_iport_t *p)
{
  uint8_t res;

  if(p->peek_byte(p, &res)) {
    return res;
  } else {
    sly_push_string(S, "cannot peek byte");
    return (uint8_t)sly_error(S, 1);
  }
}

static sly_char_t port_peek_char(sly_state_t* S, sly_iport_t *p)
{
  sly_char_t res;

  if(p->peek_char(p, &res)) {
    return res;
  } else {
    sly_push_string(S, "cannot peek char");
    return (sly_char_t)sly_error(S, 1);
  }
}

static uint8_t port_read_byte(sly_state_t* S, sly_iport_t *p)
{
  uint8_t res;

  if(p->read_byte(p, &res)) {
    return res;
  } else {
    sly_push_string(S, "cannot read byte");
    return (uint8_t)sly_error(S, 1);
  }
}

static sly_char_t port_read_char(sly_state_t* S, sly_iport_t *p)
{
  sly_char_t res;

  if(p->read_char(p, &res)) {
    return res;
  } else {
    sly_push_string(S, "cannot read char");
    return (sly_char_t)sly_error(S, 1);
  }
}

static void port_flush(sly_state_t* S, sly_oport_t *p)
{
  if(!p->flush(p)) {
    sly_push_string(S, "cannot flush port");
    sly_error(S, 1);
  }
}

static void port_write_byte(sly_state_t* S, sly_oport_t *p, uint8_t b)
{
  if(!p->write_byte(p, b)) {
    sly_push_string(S, "cannot write byte");
    sly_error(S, 1);
  }
}

static void port_write_char(sly_state_t* S, sly_oport_t *p, sly_char_t c)
{
  if(!p->write_char(p, c)) {
    sly_push_string(S, "cannot write char");
    sly_error(S, 1);
  }
}

/*
 * reader
 */

static void read_string(FILE* in, sly_sbuffer_t* buf)
{
  int c;
  
  c = getc(in);
  sly_sbuffer_assign(buf, "");
  while(c != '"') {
    if(c == EOF) {
      /* ERROR */
    }
    sly_sbuffer_add(buf, c);
    c = getc(in);
  };
}

static void read_till_delimiter(FILE* in, sly_sbuffer_t* buf)
{
  int c;

  c = getc(in);
  sly_sbuffer_assign(buf, "");
  while(c != ' ' && c != '\n' && c != '(' &&
	c != ')' && c != '"'  && c != ';' && c != EOF) {
    sly_sbuffer_add(buf, c);
    c = getc(in);
  };
  ungetc(c, in);
}

static void parse_number(const char *str, sly_object_t *res)
{
  int i, base, is_exact;

  i = 0;
  base = -1;
  is_exact = -1;

  fprintf(stderr, "%c\n", str[i]);
  /* are there prefixes? */
  while(str[i] == '#') {
    ++i;
    if(str[i] == 'i') {
      if(is_exact != -1) {
	/* ERROR */
	res->type = SLY_TYPE_NIL;
	return;
      }
      is_exact = 0;
    } else if(str[i] == 'e') {
      if(is_exact != -1) {
	/* ERROR */
	res->type = SLY_TYPE_NIL;
	return;
      }
      is_exact = 1;
    } else if(str[i] == 'b') {
      if(base != -1) {
	/* ERROR */
	res->type = SLY_TYPE_NIL;
	return;
      }
      base = 2;
    } else if(str[i] == 'd') {
      if(base != -1) {
	/* ERROR */
	res->type = SLY_TYPE_NIL;
	return;
      }
      base = 10;
    } else if(str[i] == 'o') {
      if(base != -1) {
	/* ERROR */
	res->type = SLY_TYPE_NIL;
	return;
      }
      base = 8;
    } else if(str[i] == 'x') {
      if(base != -1) {
	/* ERROR */
	res->type = SLY_TYPE_NIL;
	return;
      }
      base = 16;
    } else {
      /* ERROR */
      res->type = SLY_TYPE_NIL;
      return;
    }
    ++i;
  }

  fprintf(stderr, "%c\n", str[i]);
  base = base == -1 ? 10 : base;
  is_exact = is_exact == -1 ? 1 : is_exact;

  /* special complex cases */
  
}

static void sly_io_read_i(sly_state_t* S, sly_sbuffer_t *buf, FILE* in, sly_object_t* res)
{
  int c;
  sly_gcobject_t *obj;

  res->type = SLY_TYPE_NIL;

  c = getc(in);
  while(isspace(c) || c == ';') {
    /* eating up whitespace and comments */
    while(isspace(c)) {
      c = getc(in);
    }

    if(c == ';') {
      while(c != '\n') {
	c = getc(in);
      }
    }
  }

  if(c == EOF) {
    res->type = SLY_TYPE_EOF;
    return;
  }

  /* strings */
  if(c == '"') {
    read_string(in, buf);
    res->type = SLY_TYPE_STRING;
    res->value.gc = sly_create_string(S, sly_sbuffer_string(buf), 0);
    return;
  }

  if(c == '#') {
    ungetc(c, in);
    read_till_delimiter(in, buf);
    if(sly_sbuffer_equalp(buf, "#f")) {
      /* boolean false */
      res->type = SLY_TYPE_BOOL;
      res->value.bool = 0;
    } else if(sly_sbuffer_equalp(buf, "#t")) {
      /* boolean true */
      res->type = SLY_TYPE_BOOL;
      res->value.bool = 1;
    } else if(buf->str[1] == '\\') {
      /* character */
      res->type = SLY_TYPE_CHAR;
      if(sly_sbuffer_equalp(buf, "#\\space")) {
	res->value.chr = ' ';
      } else if(sly_sbuffer_equalp(buf, "#\\newline")) {
	res->value.chr = '\n';
      } else if(buf->size == 3) {
	res->value.chr = buf->str[2];
      } else {
	/* error */
	res->type = SLY_TYPE_NIL;
      }
    } else if(buf->str[1] == 'i' ||
	      buf->str[1] == 'e' ||
	      buf->str[1] == 'b' ||
	      buf->str[1] == 'd' ||
	      buf->str[1] == 'o' ||
	      buf->str[1] == 'x') {
      /* number */
      parse_number(buf->str, res);
    } else {
      /* ERROR */
      res->type = SLY_TYPE_NIL;
    }
    return;
  }

  /* symbols */
  if(strchr("+-.", c)) {
    ungetc(c, in);
    read_till_delimiter(in, buf);
    if(buf->str[0] == '+' || buf->str[0] == '-') {
      if(buf->size == 1) {
	obj = sly_create_string(S, buf->str, 0);
	*res = sly_create_symbol(S, SLY_STRING(obj));
      } else {
	/* number */
	parse_number(buf->str, res);
      }
    } else if(strcmp(buf->str, "...") == 0) {
      obj = sly_create_string(S, buf->str, 0);
      *res = sly_create_symbol(S, SLY_STRING(obj));
    } else {
      /* ERROR */
      res->type = SLY_TYPE_NIL;
    }
    return;
  }

  if(strchr("!$%&*/:<=>?^_~", c) || isalpha(c)) {
    ungetc(c, in);
    read_till_delimiter(in, buf);
    obj = sly_create_string(S, buf->str, 0);
    *res = sly_create_symbol(S, SLY_STRING(obj));
    return;
  }

  if(isdigit(c)) {
    /* base 10 number */
    ungetc(c, in);
    parse_number(buf->str, res);
    return;
  }
}

sly_object_t sly_io_read(sly_state_t *S)
{
  FILE *in;
  sly_object_t ret;
  sly_sbuffer_t *buffer;

  in = stdin;
  buffer = sly_sbuffer_new();
  sly_io_read_i(S, buffer, in, &ret);
  sly_sbuffer_destroy(buffer);

  return ret;
}

/*
 * writer
 */

static void write_string(sly_state_t *S, sly_string_t* s, sly_oport_t *port, int quote)
{
  uint32_t i;

  if(quote) {
    port_write_char(S, port, SLY_UCS_LEFT_QUOTE);
  }

  for(i = 0; i < s->size; i++) {
    port_write_char(S, port, s->chars[i]);
  }

  if(quote) {
    port_write_char(S, port, SLY_UCS_RIGHT_QUOTE);
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
    port_write_char(S, port, (sly_char_t)'#');
    if(obj->value.bool) {
      port_write_char(S, port, (sly_char_t)'t');
    } else {
      port_write_char(S, port, (sly_char_t)'f');
    }
    break;

  case SLY_TYPE_FIXNUM:
    snprintf(buf, 64, "%d", obj->value.fixnum);
    sly_io_write_c_string(S, buf, port);
    break;

  case SLY_TYPE_CHAR:
    if(quote) {
      port_write_char(S, port, (sly_char_t)'#');
      port_write_char(S, port, (sly_char_t)'\\');
    }

    port_write_char(S, port, obj->value.chr);
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

    port_write_char(S, port, (sly_char_t)'(');
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
        port_write_char(S, port, (sly_char_t)' ');
        p = cdr.value.gc;
      }
    }
    port_write_char(S, port, (sly_char_t)')');
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
        port_write_char(S, port, (sly_char_t)' ');
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

sly_gcobject_t *sly_io_create_ifport(sly_state_t *S, sly_file_t file)
{
  sly_gcobject_t *port;

  port = sly_create_ifport(S);

  /*SLY_IPORT(port)->read_byte = fp_read_byte;
    SLY_IPORT(port)->read_char = fp_read_char;*/

  SLY_IFPORT(port)->in = file;

  return port;
}

sly_gcobject_t *sly_io_create_ofport(sly_state_t *S, sly_file_t file)
{
  sly_gcobject_t *port;

  port = sly_create_ofport(S);

  SLY_OPORT(port)->flush = fp_flush;
  SLY_OPORT(port)->write_byte = fp_write_byte;
  SLY_OPORT(port)->write_char = fp_write_char;

  SLY_OFPORT(port)->out = file;

  return port;
}

#ifdef _WIN32

sly_gcobject_t *sly_io_create_stdin(sly_state_t *S)
{
  sly_file_t file;

  file = GetStdHandle(STD_INPUT_HANDLE);

  return sly_io_create_ifport(S, file);
}

sly_gcobject_t *sly_io_create_stdout(sly_state_t *S)
{
  sly_file_t file;

  file = GetStdHandle(STD_OUTPUT_HANDLE);

  return sly_io_create_ofport(S, file);
}

sly_gcobject_t *sly_io_create_stderr(sly_state_t *S)
{
  sly_file_t file;

  file = GetStdHandle(STD_ERROR_HANDLE);

  return sly_io_create_ofport(S, file);
}

sly_gcobject_t *sly_io_open_ifile(sly_state_t *S, sly_string_t *str, uint8_t char_enc)
{
  sly_file_t file;
  sly_ucs2_t *fname;

  fname = from_string(str, SLY_CHAR_ENC_UTF16);

  file = CreateFileW(fname, GENERIC_READ, FILE_SHARE_READ,
                     NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if(file == INVALID_HANDLE_VALUE) {
    sly_push_string(S, "cannot open input file: ");
    sly_push_string(S, fname);
    free(fname);
    sly_error(S, 2);
  }

  return sly_io_create_ifport(S, file);
}

sly_gcobject_t *sly_io_open_ofile(sly_state_t *S, sly_string_t *str, uint8_t char_enc)
{
  sly_file_t file;
  sly_ucs2_t *fname;

  fname = from_string(str, SLY_CHAR_ENC_UTF16);

  file = CreateFileW(fname, GENERIC_WRITE, 0,
                     NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
  if(file == INVALID_HANDLE_VALUE) {
    sly_push_string(S, "cannot open output file: ");
    sly_push_string(S, fname);
    free(fname);
    sly_error(S, 2);
  }

  return sly_io_create_ofport(S, file);
}

void sly_io_close_iport(sly_state_t *S, sly_iport_t *port)
{
  if(SLY_PORT(port)->type == SLY_TYPE_PORT_FILE) {
    int ret;
    BOOL ret;

    ret = CloseHandle(SLY_IFPORT(port)->in);
    if(!ret) {
      sly_push_string(S, "cannot close input port");
      sly_error(S, 1);
    }
  }
}

void sly_io_close_oport(sly_state_t *S, sly_oport_t *port)
{
  if(SLY_PORT(port)->type == SLY_TYPE_PORT_FILE) {
    BOOL ret;

    ret = CloseHandle(SLY_OFPORT(port)->out);
    if(!ret) {
      sly_push_string(S, "cannot close output port");
      sly_error(S, 1);
    }
  }
}

#else

sly_gcobject_t *sly_io_create_stdin(sly_state_t *S)
{
  sly_file_t file;

  file = STDIN_FILENO;

  return sly_io_create_ifport(S, file);
}

sly_gcobject_t *sly_io_create_stdout(sly_state_t *S)
{
  sly_file_t file;

  file = STDOUT_FILENO;

  return sly_io_create_ofport(S, file);
}

sly_gcobject_t *sly_io_create_stderr(sly_state_t *S)
{
  sly_file_t file;

  file = STDERR_FILENO;

  return sly_io_create_ofport(S, file);
}

sly_gcobject_t *sly_io_open_ifile(sly_state_t *S, sly_string_t *str, uint8_t char_enc)
{
  char *fname;
  sly_file_t file;

  fname = from_string(str, char_enc);
  if(!fname) {
    sly_push_string(S, "cannot convert file name");
    sly_error(S, 1);
  }

  file = open(fname, O_RDONLY | O_LARGEFILE | O_NONBLOCK);
  if(file < 0) {
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
  sly_file_t file;

  fname = from_string(str, char_enc);
  if(!fname) {
    sly_push_string(S, "cannot convert file name");
    sly_error(S, 1);
  }

  file = open(fname, O_WRONLY | O_LARGEFILE | O_NONBLOCK | O_CREAT);
  if(file < 0) {
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
  if(SLY_PORT(port)->type == SLY_TYPE_PORT_FILE) {
    int ret;

  close_in_again:
    ret = close(SLY_IFPORT(port)->in);
    if(ret < 0) {
      if(errno == EINTR) {
        usleep(SLY_IO_PAUSE);
        goto close_in_again;
      } else {
        sly_push_string(S, "cannot close input port");
        sly_error(S, 1);
      }
    }
  }
}

void sly_io_close_oport(sly_state_t *S, sly_oport_t *port)
{
  if(SLY_PORT(port)->type == SLY_TYPE_PORT_FILE) {
    int ret;

  close_out_again:
    ret = close(SLY_OFPORT(port)->out);
    if(ret < 0) {
      if(errno == EINTR) {
        usleep(SLY_IO_PAUSE);
        goto close_out_again;
      } else {
        sly_push_string(S, "cannot close output port");
        sly_error(S, 1);
      }
    }
  }
}

#endif

void sly_io_write_c_string(sly_state_t *S, const char* s, sly_oport_t *port)
{
  const char *p;

  for(p = s; *p; p++) {
    port_write_char(S, port, (sly_char_t)*p);
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
  port_write_char(S, port, (sly_char_t)'\r');
#endif
  port_write_char(S, port, (sly_char_t)'\n');

  port_flush(S, port);
}

void sly_io_write(sly_state_t *S, sly_object_t *obj, sly_oport_t *port)
{
  sly_io_write_i(S, obj, port, 1);
  port_flush(S, port);
}

void sly_io_display(sly_state_t *S, sly_object_t *obj, sly_oport_t *port)
{
  sly_io_write_i(S, obj, port, 0);
  port_flush(S, port);
}

