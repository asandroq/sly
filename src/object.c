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
#include <string.h>

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

sly_gcobject_t *sly_create_box(sly_state_t *S)
{
  sly_box_t *ret;

  ret = (sly_box_t*)sly_gc_alloc(&S->store, SLY_SIZE_OF_BOX);
  if(ret) {
    SLY_GCOBJECT(ret)->type = SLY_TYPE_BOX;
  }

  return SLY_GCOBJECT(ret);
}

sly_gcobject_t *sly_create_sclosure(sly_state_t *S, uint32_t entry, uint32_t nr_vars)
{
  sly_closure_t *ret;

  ret = (sly_closure_t*)sly_gc_alloc(&S->store, SLY_SIZE_OF_CLOSURE(nr_vars));
  if(ret) {
    SLY_GCOBJECT(ret)->type = SLY_TYPE_CLOSURE;
    ret->is_c = 0;
    ret->nr_free = nr_vars;
    ret->entry_point.scm = entry;
  }

  return SLY_GCOBJECT(ret);
}

sly_gcobject_t *sly_create_cclosure(sly_state_t *S, sly_cfunction_t func, uint32_t nr_vars)
{
  sly_closure_t *ret;

  ret = (sly_closure_t*)sly_gc_alloc(&S->store, SLY_SIZE_OF_CLOSURE(nr_vars));
  if(ret) {
    SLY_GCOBJECT(ret)->type = SLY_TYPE_CLOSURE;
    ret->is_c = 1;
    ret->nr_free = nr_vars;
    ret->entry_point.c = func;
  }

  return SLY_GCOBJECT(ret);
}

sly_gcobject_t *sly_create_pair(sly_state_t *S)
{
  sly_pair_t *ret;

  ret = (sly_pair_t*)sly_gc_alloc(&S->store, SLY_SIZE_OF_PAIR);
  if(ret) {
    SLY_GCOBJECT(ret)->type = SLY_TYPE_PAIR;
  }

  return SLY_GCOBJECT(ret);
}

sly_gcobject_t *sly_create_conti(sly_state_t *S, uint32_t stack_size)
{
  sly_conti_t *ret;

  ret = (sly_conti_t*)sly_gc_alloc(&S->store, SLY_SIZE_OF_CONTI(stack_size));
  if(ret) {
    SLY_GCOBJECT(ret)->type = SLY_TYPE_CONTI;
    ret->size = stack_size;
  }

  return SLY_GCOBJECT(ret);
}

sly_gcobject_t *sly_create_string(sly_state_t *S, const sly_char_t* str, uint32_t size)
{
  sly_string_t *ret;

  ret = (sly_string_t*)sly_gc_alloc(&S->store, SLY_SIZE_OF_STRING(size));

  if(ret) {
    uint32_t i;

    SLY_GCOBJECT(ret)->type = SLY_TYPE_STRING;
    ret->size = size;

    if(str) {
      for(i = 0; i < size; i++) {
	ret->chars[i] = str[i];
      }
    } else {
      for(i = 0; i < size; i++) {
	ret->chars[i] = (sly_char_t)' ';
      }
    }
  }

  return SLY_GCOBJECT(ret);
}

sly_gcobject_t *sly_create_string_from_ascii(sly_state_t *S, const char* str)
{
  int len;
  sly_string_t *ret;

  len = strlen(str);
  ret = (sly_string_t*)sly_gc_alloc(&S->store, SLY_SIZE_OF_STRING(len));

  if(ret) {
    int i;

    SLY_GCOBJECT(ret)->type = SLY_TYPE_STRING;
    ret->size = len;

    for(i = 0; i < len; i++) {
      ret->chars[i] = (sly_char_t)str[i];
    }
  }

  return SLY_GCOBJECT(ret);
}

sly_gcobject_t *sly_create_vector(sly_state_t *S, uint32_t size)
{
  sly_vector_t* ret;

  ret = (sly_vector_t*)sly_gc_alloc(&S->store, SLY_SIZE_OF_VECTOR(size));

  if(ret) {
    uint32_t i;

    SLY_GCOBJECT(ret)->type = SLY_TYPE_VECTOR;
    ret->size = size;

    for(i = 0; i < size; i++) {
      ret->data[i].type = SLY_TYPE_UNDEF;
    }
  }

  return SLY_GCOBJECT(ret);
}

sly_gcobject_t *sly_create_dyn_bind(sly_state_t *S)
{
  sly_dyn_bind_t *ret;

  ret = (sly_dyn_bind_t*)sly_gc_alloc(&S->store, SLY_SIZE_OF_DYN_BIND);
  if(ret) {
    SLY_GCOBJECT(ret)->type = SLY_TYPE_DYN_BIND;
  }

  return SLY_GCOBJECT(ret);
}

sly_gcobject_t *sly_create_iport(sly_state_t *S)
{
  sly_iport_t* ret;

  ret = (sly_iport_t*)sly_gc_alloc(&S->store, SLY_SIZE_OF_IPORT);

  if(ret) {
    SLY_GCOBJECT(ret)->type = SLY_TYPE_INPUT_PORT;
    SLY_PORT(ret)->char_enc = SLY_CHAR_ENC_LATIN1;
    SLY_PORT(ret)->finish = NULL;
    SLY_PORT(ret)->private = NULL;

    /* register port to be finalised when collected */
    sly_gc_add_port(&S->store, SLY_PORT(ret));
  }

  return SLY_GCOBJECT(ret);
}

sly_gcobject_t *sly_create_oport(sly_state_t *S)
{
  sly_oport_t* ret;

  ret = (sly_oport_t*)sly_gc_alloc(&S->store, SLY_SIZE_OF_OPORT);

  if(ret) {
    SLY_GCOBJECT(ret)->type = SLY_TYPE_OUTPUT_PORT;
    SLY_PORT(ret)->char_enc = SLY_CHAR_ENC_LATIN1;
    SLY_PORT(ret)->finish = NULL;
    SLY_PORT(ret)->private = NULL;

    /* register port to be finalised when collected */
    sly_gc_add_port(&S->store, SLY_PORT(ret));
  }

  return SLY_GCOBJECT(ret);
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

sly_object_t sly_create_symbol_from_ascii(sly_state_t* S, const char* name)
{
  sly_gcobject_t *str;

  str = sly_create_string_from_ascii(S, name);
  return sly_create_symbol(S, SLY_STRING(str));
}
