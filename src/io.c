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

#include "sly.h"

#include <stdio.h>

#include "io.h"

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
    printf("<#closure %u>", ((sly_closure_t*)obj->value.gc)->entry_point);
    break;

  case SLY_TYPE_PAIR:
    printf("(");
    sly_io_write(&(((sly_pair_t*)obj->value.gc)->car));
    printf(" . ");
    sly_io_write(&(((sly_pair_t*)obj->value.gc)->cdr));
    printf(")");
    break;

  case SLY_TYPE_CONTI:
    printf("<#continuation %u>", ((sly_conti_t*)obj->value.gc)->size);
    break;

  case SLY_TYPE_BOX:
    printf("#&");
    sly_io_write(&(((sly_box_t*)obj->value.gc)->value));
    break;

  case SLY_TYPE_STRING:
    write_string((sly_string_t*)obj->value.gc, 1);
    break;

  case SLY_TYPE_VECTOR:
    printf("#(");
    for(i = 0; i < ((sly_vector_t*)obj->value.gc)->size; i++) {
      printf(" ");
      sly_io_write(((sly_vector_t*)obj->value.gc)->data + i);
    }
    printf(")");
    break;

  default:
    printf("Unknown type!");
  }
}

