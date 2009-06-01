/*
 * The Sly Scheme System
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

#include <stdlib.h>

#include "sly.h"
#include "object.h"
#include "state.h"

static int string_equal_p(sly_string_t *s1, sly_string_t *s2)
{
  if(s1->size != s2->size) {
    return 0;
  }

  return memcmp(s1->chars, s2->chars, s1->size * sizeof(sly_char_t)) == 0;
}

static sly_string_t* string_copy(sly_state_t* S, sly_string_t* s)
{
  sly_string_t *ret;

  ret = sly_gc_alloc_string(&S->store, s->size);
  memcpy(ret->chars, s->chars, s->size * sizeof(sly_char_t));

  return ret;
}

static sly_string_t* string_copy_extern(sly_string_t* s)
{
  uint32_t size;
  sly_string_t *ret;

  size = SLY_SIZE_OF_STRING(s->size);

  ret = (sly_string_t*)malloc(size);
  /* TODO: test and throw error */
  memcpy(ret, s, size);

  return ret;
}

sly_object_t sly_create_string(sly_state_t* S, const char* str)
{
  size_t len, i;
  sly_object_t obj;

  len = strlen(str);

  obj.type = SLY_TYPE_STRING;
  obj.value.gc = (sly_gcobject_t*)sly_gc_alloc_string(&S->store, len);

  for(i = 0; i < len; i++) {
    ((sly_string_t*)obj.value.gc)->chars[i] = (sly_char_t)str[i];
  }

  return obj;
}

sly_object_t sly_create_symbol(sly_state_t* S, sly_string_t *str)
{
  sly_object_t obj;
  sly_symbol_t *tmp;

  /* is the symbol already there? */
  for(tmp = S->symbol_table; tmp != NULL; tmp = tmp->next) {
    if(string_equal_p(tmp->str, str)) {
      break;
    }
  }

  if(tmp == NULL) {
    /* adding new symbol */
    tmp = (sly_symbol_t*)malloc(sizeof(sly_symbol_t));
    tmp->str = string_copy_extern(str);
    tmp->next = S->symbol_table;
    S->symbol_table = tmp;
  }

  obj.type = SLY_TYPE_SYMBOL;
  obj.value.symbol = tmp;

  return obj;
}

