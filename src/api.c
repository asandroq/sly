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
  printf("Error: ");
  for(i = -num-1; i < -1; ++i) {
    sly_display(S, i, -1);
  }
  printf("\n");

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

sly_ucs2_t* sly_to_string_utf16(sly_state_t* S, int idx)
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

void sly_number_to_string(sly_state_t* S, int idx)
{
  char tmp[64];
  uint32_t i, len;
  sly_gcobject_t *str;

  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_FIXNUM);
#endif

  snprintf(tmp, 64, "%d", S->stack[idx].value.fixnum);
  len = strlen(tmp);

  str = sly_create_string(S, NULL, len);
  for(i = 0; i < len; i++) {
    SLY_STRING(str)->chars[i] = (sly_char_t)tmp[i];
  }

  S->stack[S->sp].type = SLY_TYPE_STRING;
  S->stack[S->sp++].value.gc = str;
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
  sly_object_t p;
  uint32_t first, proc;

  proc = calc_index(S, idx);
  first = calc_index(S, -nr_args);

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
  idx = calc_index(S, idx);

  /* pushing argument */
  S->stack[S->sp++] = S->stack[idx];

  sly_vm_call(S, S->global_env.vars[S->sly_eval].value, 1);
  S->stack[S->sp++] = S->accum;
}

void sly_push_current_input_port(sly_state_t *S)
{
  /* for now, I'll grab the port at a fixed address */
  S->stack[S->sp++] = S->stack[0];
}

void sly_push_current_output_port(sly_state_t *S)
{
  /* for now, I'll grab the port at a fixed address */
  S->stack[S->sp++] = S->stack[1];
}

void sly_push_current_error_port(sly_state_t *S)
{
  /* for now, I'll grab the port at a fixed address */
  S->stack[S->sp++] = S->stack[2];
}

void sly_newline(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_OUTPUT_PORT);
#endif

  sly_io_newline(S, &S->stack[idx]);
}

void sly_read(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_INPUT_PORT);
#endif

  sly_io_read(S, &S->stack[idx], &S->stack[S->sp++]);
}

void sly_read_token(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_INPUT_PORT);
#endif

  sly_io_read_token(S, &S->stack[idx], &S->stack[S->sp++]);
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
}

void sly_open_input_file(sly_state_t* S, int idx)
{
  sly_gcobject_t *port;

  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_STRING);
#endif

  port = sly_io_open_ifile(S, &S->stack[idx], SLY_CHAR_ENC_LATIN1);

  S->stack[S->sp].type = SLY_TYPE_INPUT_PORT;
  S->stack[S->sp++].value.gc = port;
}

void sly_open_output_file(sly_state_t* S, int idx)
{
  sly_gcobject_t *port;

  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_STRING);
#endif

  port = sly_io_open_ofile(S, &S->stack[idx], SLY_CHAR_ENC_LATIN1);

  S->stack[S->sp].type = SLY_TYPE_OUTPUT_PORT;
  S->stack[S->sp++].value.gc = port;
}

void sly_close_input_port(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_INPUT_PORT);
#endif

  sly_io_close_iport(S, &S->stack[idx]);
}

void sly_close_output_port(sly_state_t* S, int idx)
{
  idx = calc_index(S, idx);

#ifdef SLY_DEBUG_API
  assert(S->stack[idx].type == SLY_TYPE_OUTPUT_PORT);
#endif

  sly_io_close_oport(S, &S->stack[idx]);
}

void sly_set_global(sly_state_t* S, const char* name)
{
  int idx;
  uint32_t i, len;
  sly_object_t sym;
  sly_gcobject_t *str;

  len = strlen(name);
  str = sly_create_string(S, NULL, len);
  for(i = 0; i < len; i++) {
    SLY_STRING(str)->chars[i] = (sly_char_t)name[i];
  }

  sym = sly_create_symbol(S, SLY_STRING(str));

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

