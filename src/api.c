/*
 * The Sly Scheme API
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

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#ifdef SLY_DEBUG_API
#include <assert.h>
#endif

#include "sly.h"

#include "io.h"
#include "vm.h"
#include "state.h"

static int numberp(sly_state_t* S, uint32_t idx)
{
  return S->stack[idx].type == SLY_TYPE_FIXNUM;
}

static uint32_t calc_index(sly_state_t* S, int idx)
{
  /*
   * TODO: better checking of index interval
   * permitted
   */

  if(idx < 0) {
    /* counting from the top */
    idx = S->sp + idx;
  } else {
    /* count from the frame pointer */
    idx += S->fp + 1;
  }

#ifdef SLY_DEBUG_API
  assert(idx > (int)S->fp && idx < (int)S->sp);
#endif

  return idx;
}

int sly_error(sly_state_t* S, uint32_t num)
{
  int i;

#ifdef SLY_DEBUG_API
  assert(num < S->sp - S->fp);
#endif

  sly_push_current_error_port(S);

  /* error objects are on top of stack */
  sly_push_string(S, "Error: ");
  sly_display(S, -1, -2);
  S->sp--;
  for(i = -num-1; i < -1; ++i) {
    sly_display(S, i, -1);
  }
  sly_newline(S, -1);
  S->sp--;

  S->sp -= num;
  sly_vm_dump(S);

  sly_close(S);
  abort();

  /* execution never gets here */
  return 0;
}

int sly_get_top(sly_state_t* S)
{
  /*
   * the current stack height only goes as down
   * as the last frame pushed
   */
  return S->sp - S->fp - 1;
}

void sly_pop(sly_state_t* S, uint32_t num)
{
#ifdef SLY_DEBUG_API
  assert(num < S->sp - S->fp);
#endif

  S->sp -= num;
}

static int check_type(sly_state_t* S, int idx, int type)
{
  idx = calc_index(S, idx);

  return S->stack[idx].type == type;
}

int sly_boxp(sly_state_t* S, int idx)
{
  return check_type(S, idx, SLY_TYPE_BOX);
}

int sly_charp(sly_state_t* S, int idx)
{
  return check_type(S, idx, SLY_TYPE_CHAR);
}

int sly_integerp(sly_state_t* S, int idx)
{
  return check_type(S, idx, SLY_TYPE_FIXNUM);
}

int sly_numberp(sly_state_t* S, int idx)
{
  return check_type(S, idx, SLY_TYPE_FIXNUM);
}

int sly_pairp(sly_state_t* S, int idx)
{
  return check_type(S, idx, SLY_TYPE_PAIR);
}

int sly_listp(sly_state_t* S, int idx)
{
  sly_gcobject_t *p;

  idx = calc_index(S, idx);

  if(S->stack[idx].type == SLY_TYPE_NIL) {
    return 1;
  }

  if(S->stack[idx].type != SLY_TYPE_PAIR) {
    return 0;
  }

  p = S->stack[idx].value.gc;
  for(;;) {
    sly_object_t cdr;

    cdr = SLY_PAIR(p)->cdr;
    if(cdr.type == SLY_TYPE_NIL) {
      return 1;
    } else if(cdr.type != SLY_TYPE_PAIR) {
      return 0;
    } else {
      p = cdr.value.gc;
    }
  }
}

int sly_stringp(sly_state_t* S, int idx)
{
  return check_type(S, idx, SLY_TYPE_STRING);
}

int sly_vectorp(sly_state_t* S, int idx)
{
  return check_type(S, idx, SLY_TYPE_VECTOR);
}

int sly_procedurep(sly_state_t* S, int idx)
{
  return check_type(S, idx, SLY_TYPE_CLOSURE);
}

int sly_eof_objectp(sly_state_t* S, int idx)
{
  return check_type(S, idx, SLY_TYPE_EOF);
}

int sly_input_portp(sly_state_t* S, int idx)
{
  return check_type(S, idx, SLY_TYPE_INPUT_PORT);
}

int sly_output_portp(sly_state_t* S, int idx)
{
  return check_type(S, idx, SLY_TYPE_OUTPUT_PORT);
}

void sly_push_value(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

  S->stack[S->sp++] = S->stack[idx];
}

void sly_push_boolean(sly_state_t* S, int bool)
{
  S->stack[S->sp].type = SLY_TYPE_BOOL;
  S->stack[S->sp++].value.bool = bool ? 1 : 0;
}

void sly_push_char(sly_state_t* S, sly_char_t c)
{
  S->stack[S->sp].type = SLY_TYPE_CHAR;
  S->stack[S->sp++].value.chr = c;
}

void sly_push_integer(sly_state_t* S, sly_fixnum_t num)
{
  S->stack[S->sp].type = SLY_TYPE_FIXNUM;
  S->stack[S->sp++].value.fixnum = num;
}

void sly_push_cclosure(sly_state_t* S, sly_cfunction_t func, uint32_t nr_vars)
{
  uint32_t i;
  sly_gcobject_t *cl;

  cl = sly_create_cclosure(S, func, nr_vars);

  for(i = 0; i < nr_vars; i++) {
    SLY_CLOSURE(cl)->free_vars[i] = S->stack[S->sp-i-1];
  }
  S->sp -= nr_vars;

  S->stack[S->sp].type = SLY_TYPE_CLOSURE;
  S->stack[S->sp++].value.gc = cl;
}

void sly_push_string(sly_state_t* S, const char* str)
{
  uint32_t i, len;
  sly_gcobject_t *obj;

  len = strlen(str);

  obj = sly_create_string(S, NULL, len);
  for(i = 0; i < len; i++) {
    SLY_STRING(obj)->chars[i] = (sly_char_t)str[i];
  }

  S->stack[S->sp].type = SLY_TYPE_STRING;
  S->stack[S->sp++].value.gc = obj;
}

void sly_push_vector(sly_state_t* S, uint32_t size)
{
  sly_object_t obj;

  obj.type = SLY_TYPE_VECTOR;
  obj.value.gc = sly_create_vector(S, size);

  S->stack[S->sp++] = obj;
}

void sly_get_global(sly_state_t* S, const char *name)
{
  int glob;
  sly_object_t obj;

  obj = sly_create_symbol_from_ascii(S, name);

  glob = sly_st_get_global_index(S, obj.value.symbol);
  if(glob < 0) {
    STK(S->sp++).type = SLY_TYPE_NIL;
  } else {
    STK(S->sp++) = S->global_env.vars[glob].value;
  }
}

sly_fixnum_t sly_to_integer(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(numberp(S, idx));
#endif

  return S->stack[idx].value.fixnum;
}

sly_char_t* sly_to_string(sly_state_t* S, int idx)
{
  uint32_t size;
  sly_char_t *ret;
  sly_string_t *str;

  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_STRING);
#endif

  str = (sly_string_t*)S->stack[idx].value.gc;
  size = str->size;
  ret = (sly_char_t*)malloc((size+1) * sizeof(sly_char_t));
  if(!ret) {
    sly_push_string(S, "could not allocate memory");
    sly_error(S, 1);
  }

  ret[size] = '\0';
  memcpy(ret, str->chars, size * sizeof(sly_char_t));

  return ret;
}

uint8_t* sly_to_string_latin1(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_STRING);
#endif
  
  return sly_io_to_latin1(S, SLY_STRING(S->stack[idx].value.gc));
}

uint8_t* sly_to_string_utf8(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_STRING);
#endif

  return sly_io_to_utf8(S, SLY_STRING(S->stack[idx].value.gc));
}

sly_cp2_t* sly_to_string_utf16(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_STRING);
#endif

  return sly_io_to_utf16(S, SLY_STRING(S->stack[idx].value.gc));
}

int sly_less_than(sly_state_t* S, int idx1, int idx2)
{
  idx1 = calc_index(S, idx1);
  idx2 = calc_index(S, idx2);

  return S->stack[idx1].value.fixnum < S->stack[idx2].value.fixnum;
}

int sly_greater_than(sly_state_t* S, int idx1, int idx2)
{
  idx1 = calc_index(S, idx1);
  idx2 = calc_index(S, idx2);

  return S->stack[idx1].value.fixnum > S->stack[idx2].value.fixnum;
}

void sly_symbol_to_string(sly_state_t* S, int idx)
{
  uint32_t size;
  sly_symbol_t *sym;
  sly_gcobject_t *str;

  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_SYMBOL);
#endif

  sym = S->stack[idx].value.symbol;
  size = sym->str->size;
  str = sly_create_string(S, NULL, size);
  memcpy(SLY_STRING(str)->chars, sym->str->chars, size * sizeof(sly_char_t));

  S->stack[S->sp].type = SLY_TYPE_STRING;
  S->stack[S->sp++].value.gc = str;
}

void sly_string_to_symbol(sly_state_t* S, int idx)
{
  sly_object_t sym;

  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_STRING);
#endif

  sym = sly_create_symbol(S, SLY_STRING((S->stack[idx]).value.gc));
  S->stack[S->sp++] = sym;
}

void sly_invert(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(numberp(S, idx));
#endif

  sly_push_integer(S, 1);
}

void sly_unary_minus(sly_state_t* S, int idx)
{
  sly_fixnum_t res;

  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(numberp(S, idx));
#endif

  res = - S->stack[idx].value.fixnum;
  sly_push_integer(S, res);
}

void sly_add(sly_state_t* S, uint32_t nr_nums)
{
#ifdef SLY_DEBUG_API
  assert(nr_nums < S->sp - S->fp);
#endif

  if(nr_nums == 0) {
    sly_push_integer(S, 0);
  } else if(nr_nums == 1) {

#ifdef SLY_DEBUG_API
    assert(numberp(S, S->sp-1));
#endif

  } else {
    sly_fixnum_t res;
    uint32_t i, first, last;

    first = calc_index(S, -nr_nums);
    last = calc_index(S, -1);

    for(res = 0, i = first; i <= last; i++) {

#ifdef SLY_DEBUG_API
      assert(numberp(S, i));
#endif

      res += S->stack[i].value.fixnum;
    }

    S->sp -= nr_nums;
    S->stack[S->sp].type = SLY_TYPE_FIXNUM;
    S->stack[S->sp++].value.fixnum = res;
  }
}

void sly_subtract(sly_state_t* S, uint32_t nr_nums)
{
#ifdef SLY_DEBUG_API
  assert(nr_nums < S->sp - S->fp);
#endif

  if(nr_nums == 1) {

#ifdef SLY_DEBUG_API
    assert(numberp(S, S->sp-1));
#endif

    S->stack[S->sp-1].value.fixnum = -S->stack[S->sp-1].value.fixnum;
  } else {
    sly_fixnum_t res;
    uint32_t i, first, last;

    first = calc_index(S, -nr_nums);
    last = calc_index(S, -1);

#ifdef SLY_DEBUG_API
    assert(numberp(S, first));
#endif

    res = S->stack[first].value.fixnum;

    for(i = first + 1; i <= last; i++) {

#ifdef SLY_DEBUG_API
      assert(numberp(S, i));
#endif

      res -= S->stack[i].value.fixnum;
    }

    S->sp -= nr_nums;
    S->stack[S->sp].type = SLY_TYPE_FIXNUM;
    S->stack[S->sp++].value.fixnum = res;
  }
}

void sly_divide(sly_state_t* S, uint32_t nr_nums)
{
#ifdef SLY_DEBUG_API
  assert(nr_nums < S->sp - S->fp);
#endif

  if(nr_nums == 1) {

#ifdef SLY_DEBUG_API
    assert(numberp(S, S->sp-1));
#endif

    S->stack[S->sp-1].value.fixnum = 1;
  } else {
    sly_fixnum_t res;
    uint32_t i, first, last;

    first = calc_index(S, -nr_nums);
    last = calc_index(S, -1);

#ifdef SLY_DEBUG_API
    assert(numberp(S, first));
#endif

    res = S->stack[first].value.fixnum;

    for(i = first + 1; i <= last; i++) {

#ifdef SLY_DEBUG_API
      assert(numberp(S, i));
#endif

      res /= S->stack[i].value.fixnum;
    }

    S->sp -= nr_nums;
    S->stack[S->sp].type = SLY_TYPE_FIXNUM;
    S->stack[S->sp++].value.fixnum = res;
  }
}

void sly_round(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
    assert(numberp(S, idx));
#endif

    S->stack[S->sp].type = SLY_TYPE_FIXNUM;
    S->stack[S->sp++].value.fixnum = S->stack[idx].value.fixnum;
}

void sly_remainder(sly_state_t* S, int idx1, int idx2)
{
  idx1 = calc_index(S, idx1);
  idx2 = calc_index(S, idx2);

#ifdef SLY_DEBUG_API
  assert(numberp(S, idx1));
  assert(numberp(S, idx2));
#endif

  S->stack[S->sp].type = SLY_TYPE_FIXNUM;
  S->stack[S->sp++].value.fixnum =
    S->stack[idx1].value.fixnum % S->stack[idx2].value.fixnum;
}

void sly_number_to_string(sly_state_t* S, int idx)
{
  char tmp[64];
  uint32_t i, len;
  sly_gcobject_t *str;

  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_FIXNUM);
#endif

  snprintf(tmp, 64, "%ld", S->stack[idx].value.fixnum);
  len = strlen(tmp);

  str = sly_create_string(S, NULL, len);
  for(i = 0; i < len; i++) {
    SLY_STRING(str)->chars[i] = (sly_char_t)tmp[i];
  }

  S->stack[S->sp].type = SLY_TYPE_STRING;
  S->stack[S->sp++].value.gc = str;
}

void sly_box(sly_state_t* S)
{
  sly_gcobject_t *box;

  box = sly_create_box(S);
  SLY_BOX(box)->value = STK(S->sp-1);

  /* replace top of stack with box */
  STK(S->sp-1).type = SLY_TYPE_BOX;
  STKGC(S->sp-1) = box;
}

void sly_unbox(sly_state_t* S)
{
  sly_object_t obj;

#ifdef SLY_DEBUG_API
  assert(STK(S->sp-1).type == SLY_TYPE_BOX);
#endif

  obj = SLY_BOX(STKGC(S->sp-1))->value;

  /* replace top of stack with box contents */
  STK(S->sp-1) = obj;
}

void sly_set_box(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(STK(idx).type == SLY_TYPE_BOX);
#endif

  SLY_BOX(STKGC(idx))->value = STK(S->sp-1);
  --S->sp;
}

void sly_cons(sly_state_t* S, int idx1, int idx2)
{
  sly_gcobject_t *pair;

  idx1 = calc_index(S, idx1);
  idx1 = calc_index(S, idx1);

  pair = sly_create_pair(S);
  SLY_PAIR(pair)->car = S->stack[idx1];
  SLY_PAIR(pair)->cdr = S->stack[idx2];

  S->stack[S->sp].type = SLY_TYPE_PAIR;
  S->stack[S->sp++].value.gc = pair;
}

void sly_car(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_PAIR);
#endif

  S->stack[S->sp++] = SLY_PAIR(S->stack[idx].value.gc)->car;
}

void sly_cdr(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_PAIR);
#endif

  S->stack[S->sp++] = SLY_PAIR(S->stack[idx].value.gc)->cdr;
}

uint32_t sly_string_length(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_STRING);
#endif

  return SLY_STRING(S->stack[idx].value.gc)->size;
}

sly_char_t sly_string_ref(sly_state_t* S, uint32_t pos, int idx)
{
  sly_gcobject_t *str;

  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_STRING);
#endif

  str = S->stack[idx].value.gc;

#ifdef SLY_DEBUG_API
  assert(pos < SLY_STRING(str)->size);
#endif

  return SLY_STRING(str)->chars[pos];
}

void sly_concat(sly_state_t* S, uint32_t nr_strs)
{
#ifdef SLY_DEBUG_API
  assert(nr_strs < S->sp - S->fp);
#endif

  if(nr_strs == 0) {
    sly_push_string(S, "");
  } else {
    sly_gcobject_t *str, *tmp;
    uint32_t i, j, k, first, last, total_size = 0;

    first = calc_index(S, -nr_strs);
    last = calc_index(S, -1);

    for(i = first; i <= last; i++) {

#ifdef SLY_DEBUG_API
      assert(S->stack[i].type == SLY_TYPE_STRING);
#endif

      total_size += SLY_STRING(S->stack[i].value.gc)->size;
    }

    str = sly_create_string(S, NULL, total_size);
    for(k = 0, i = first; i <= last; i++) {
      tmp = S->stack[i].value.gc;
      for(j = 0; j < SLY_STRING(tmp)->size; j++) {
        SLY_STRING(str)->chars[k++] = SLY_STRING(tmp)->chars[j];
      }
    }

    S->sp -= nr_strs;
    S->stack[S->sp].type = SLY_TYPE_STRING;
    S->stack[S->sp++].value.gc = str;
  }
}

uint32_t sly_vector_length(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_VECTOR);
#endif

  return SLY_VECTOR(S->stack[idx].value.gc)->size;
}

void sly_vector_ref(sly_state_t* S, uint32_t pos, int idx)
{
  sly_gcobject_t *vec;

  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_VECTOR);
#endif

  vec = S->stack[idx].value.gc;

#ifdef SLY_DEBUG_API
  assert(pos < SLY_VECTOR(vec)->size);
#endif

  S->stack[S->sp++] = SLY_VECTOR(vec)->data[pos];
}

void sly_vector_set(sly_state_t* S, uint32_t pos, int idx)
{
  sly_gcobject_t *vec;

  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_VECTOR);
#endif

  vec = S->stack[idx].value.gc;

#ifdef SLY_DEBUG_API
  assert(pos < SLY_VECTOR(vec)->size);
#endif

  SLY_VECTOR(vec)->data[pos] = S->stack[--S->sp];
}

void sly_apply(sly_state_t* S, int idx, uint32_t nr_args)
{
  uint32_t proc;
  sly_object_t p;

  proc = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(nr_args > 0);
  assert(nr_args < S->sp - S->fp);
  assert(S->stack[proc].type == SLY_TYPE_CLOSURE);
  assert(sly_listp(S, -1));
#endif

  /* add arguments from list */
  --nr_args;
  for(p = S->stack[S->sp-1], S->sp--; p.type == SLY_TYPE_PAIR; nr_args++) {
    S->stack[S->sp++] = SLY_PAIR(p.value.gc)->car;
    p = SLY_PAIR(p.value.gc)->cdr;
  }

  sly_vm_call(S, S->stack[proc], nr_args);
  S->stack[S->sp++] = S->accum;
}

void sly_eval(sly_state_t *S, int idx)
{
  int proc;
  sly_gcobject_t *pair;

  idx = calc_index(S, idx);

  sly_get_global(S, "compile-toplevel");
  proc = S->sp - 1;

  /* pushing argument */
  pair = sly_create_pair(S);
  SLY_PAIR(pair)->car = S->stack[idx];
  SLY_PAIR(pair)->cdr.type = SLY_TYPE_NIL;

  S->stack[S->sp].type = SLY_TYPE_PAIR;
  S->stack[S->sp++].value.gc = pair;

  /* the compiler is in Scheme land */
  sly_vm_call(S, STK(proc), 1);
  sly_vm_load(S, S->accum);

  /* put result over procedure */
  S->stack[S->sp-1] = S->accum;
}

void sly_call(sly_state_t *S, uint32_t n_args)
{
  int proc;

  proc = calc_index(S, -n_args-1);

#ifdef SLY_DEBUG_API
  assert(S->stack[proc].type = SLY_TYPE_CLOSURE);
#endif

  sly_vm_call(S, S->stack[proc], n_args);
  S->stack[S->sp++] = S->accum;
}

void sly_load_file(sly_state_t* S, int idx)
{
  int idx2;
  sly_module_t mod;

  idx = calc_index(S, idx);
  idx2 = calc_index(S, -1);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type = SLY_TYPE_STRING);
#endif
 
  sly_get_global(S, "compile-from-port");
  S->stack[S->sp++] = S->stack[idx];
  sly_open_input_file(S);
  sly_call(S, 1);

  sly_vm_vector_to_module(S->stack[S->sp-1], &mod);

  sly_vm_link_run_module(S, &mod);
  S->sp = ++idx2 + 1;
  S->stack[idx2] = S->accum;
}

void sly_push_current_input_port(sly_state_t *S)
{
  sly_get_global(S, "current-input-port");
  sly_vm_call(S, STK(S->sp-1), 0);
  STK(S->sp-1) = S->accum;
}

void sly_push_current_output_port(sly_state_t *S)
{
  sly_get_global(S, "current-output-port");
  sly_vm_call(S, STK(S->sp-1), 0);
  STK(S->sp-1) = S->accum;
}

void sly_push_current_error_port(sly_state_t *S)
{
  sly_get_global(S, "current-error-port");
  sly_vm_call(S, STK(S->sp-1), 0);
  STK(S->sp-1) = S->accum;
}

void sly_newline(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_OUTPUT_PORT);
#endif

  sly_io_newline(S, &S->stack[idx]);
  S->accum.type = SLY_TYPE_VOID;
}

void sly_read(sly_state_t *S, int idx)
{
  int proc;

  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_INPUT_PORT);
#endif

  sly_get_global(S, "read");
  proc = S->sp - 1;
  
  /* pushing port */
  S->stack[S->sp++] = S->stack[idx];

  /* the reader is in Scheme land */
  sly_vm_call(S, STK(proc), 1);

  /* put result over procedure */
  S->stack[S->sp-1] = S->accum;
}

void sly_write(sly_state_t* S, int idx1, int idx2)
{
  idx1 = calc_index(S, idx1);
  idx2 = calc_index(S, idx2);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx2].type == SLY_TYPE_OUTPUT_PORT);
#endif

  sly_io_write(S, &S->stack[idx1], &S->stack[idx2]);
}

void sly_display(sly_state_t* S, int idx1, int idx2)
{
  idx1 = calc_index(S, idx1);
  idx2 = calc_index(S, idx2);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx2].type == SLY_TYPE_OUTPUT_PORT);
#endif

  sly_io_display(S, &S->stack[idx1], &S->stack[idx2]);
  S->accum.type = SLY_TYPE_VOID;
}

void sly_open_input_file(sly_state_t* S)
{
  int idx;
  sly_gcobject_t *port;

  idx = calc_index(S, -1);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_STRING);
#endif

  port = sly_io_open_ifile(S, &S->stack[idx], SLY_CHAR_ENC_LATIN1);

  S->stack[idx].type = SLY_TYPE_INPUT_PORT;
  S->stack[idx].value.gc = port;
}

void sly_open_output_file(sly_state_t* S)
{
  int idx;
  sly_gcobject_t *port;

  idx = calc_index(S, -1);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_STRING);
#endif

  port = sly_io_open_ofile(S, &S->stack[idx], SLY_CHAR_ENC_LATIN1);

  S->stack[idx].type = SLY_TYPE_OUTPUT_PORT;
  S->stack[idx].value.gc = port;
}

void sly_close_input_port(sly_state_t* S)
{
  int idx = calc_index(S, -1);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_INPUT_PORT);
#endif

  sly_io_close_iport(S, &S->stack[idx]);
  S->sp--;
}

void sly_close_output_port(sly_state_t* S)
{
  int idx = calc_index(S, -1);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_OUTPUT_PORT);
#endif

  sly_io_close_oport(S, &S->stack[idx]);
  S->sp--;
}

void sly_set_global(sly_state_t* S, const char* name)
{
  int idx;
  sly_object_t sym;

  sym = sly_create_symbol_from_ascii(S, name);

  /* is the global already there? */
  idx = sly_st_get_global_index(S, sym.value.symbol);
  if(idx > 0) {
    S->global_env.vars[idx].value = S->stack[--S->sp];
  } else {
    idx = S->global_env.size;
    sly_st_enlarge_globals(S, 1);
    S->global_env.vars[idx].symbol = sym.value.symbol;
    S->global_env.vars[idx].value = S->stack[--S->sp];
  }
}

void sly_register(sly_state_t* S, sly_reg_t* regs)
{
  int i;

  for(i = 0; regs[i].name != NULL; i++) {
    sly_push_cclosure(S, regs[i].func, 0);
    sly_set_global(S, regs[i].name);
  }
}

void sly_repl(sly_state_t *S)
{
  int idx1, idx2;
  sly_object_t *out;

  idx1 = calc_index(S, -2);
  idx2 = calc_index(S, -1);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx1].type == SLY_TYPE_INPUT_PORT);
  assert(S->stack[idx2].type == SLY_TYPE_OUTPUT_PORT);
#endif

  out = &S->stack[idx2];

  while(1) {
    sly_io_write_c_string(S, "#;> ", out);
    sly_read(S, -2);

    if(sly_eof_objectp(S, -1)) {
      S->sp--;
      break;
    }

    sly_eval(S, -1);
    if(S->stack[S->sp-1].type != SLY_TYPE_VOID) {
      sly_write(S, -1, -3);
      sly_newline(S, -3);
    }
    S->sp -= 2;
  }

  /* removing ports */
  S->sp -= 2;
}
