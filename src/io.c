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

static void write_string(sly_string_t* s, int quote)
{
  uint32_t i, c;

  if(quote) {
    printf("\"");
  }

  for(i = 0; i < s->size; i++) {
    c = s->chars[i];
    printf("%c", c > 31 && c < 128 ? c : '#');
  }

  if(quote) {
    printf("\"");
  }
}

void sly_io_write_symbol(sly_symbol_t *sym)
{
  write_string(sym->str, 0);
}

void sly_io_write(sly_object_t* obj)
{
  uint32_t i;

  switch(obj->type) {

  case SLY_TYPE_UNDEF:
    printf("<#undef>");
    break;

  case SLY_TYPE_NIL:
    printf("()");
    break;

  case SLY_TYPE_BOOL:
    if(obj->value.bool) {
      printf("#t");
    } else {
      printf("#f");
    }
    break;

  case SLY_TYPE_FIXNUM:
    printf("%d", obj->value.fixnum);
    break;

  case SLY_TYPE_CHAR:
    printf("#\\%c", obj->value.chr);
    break;

  case SLY_TYPE_SYMBOL:
    write_string(obj->value.symbol->str, 0);
    break;

  case SLY_TYPE_CLOSURE:
    printf("<#closure>");
    break;

  case SLY_TYPE_PAIR:
    printf("(");
    sly_io_write(&(((sly_pair_t*)obj->value.gc)->car));
    printf(" . ");
    sly_io_write(&(((sly_pair_t*)obj->value.gc)->cdr));
    printf(")");
    break;

  case SLY_TYPE_CONTI:
    printf("<#continuation %u>", SLY_CONTI(obj->value.gc)->size);
    break;

  case SLY_TYPE_BOX:
    printf("#&");
    sly_io_write(&(SLY_BOX(obj->value.gc)->value));
    break;

  case SLY_TYPE_STRING:
    write_string(SLY_STRING(obj->value.gc), 1);
    break;

  case SLY_TYPE_VECTOR:
    printf("#(");
    for(i = 0; i < SLY_VECTOR(obj->value.gc)->size; i++) {
      printf(" ");
      sly_io_write(SLY_VECTOR(obj->value.gc)->data + i);
    }
    printf(")");
    break;

  default:
    printf("Unknown type!");
  }
}

