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

#include <stddef.h>

#include "sly.h"
#include "state.h"

static int calc_index(sly_state_t* S, int idx)
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
    idx = S->fp + idx + 1;
  }

  return idx;
}

int sly_get_top(sly_state_t* S)
{
  /*
   * the current stack height only goes as down
   * as the last frame pushed
   */
  return S->sp - S->fp - 1;
}

void sly_push_boolean(sly_state_t* S, int bool)
{
  S->stack[S->sp].type = SLY_TYPE_BOOL;
  S->stack[S->sp++].value.bool = bool ? 1 : 0;
}

void sly_push_cclosure(sly_state_t* S, sly_cfunction_t func, int nr_vars)
{
  int i;
  sly_gcobject_t *cl;

  cl = sly_create_cclosure(S, func, nr_vars);

  for(i = 0; i < nr_vars; i++) {
    SLY_CLOSURE(cl)->free_vars[i] = S->stack[S->sp-i-1];
  }
  S->sp -= nr_vars;

  S->stack[S->sp].type = SLY_TYPE_CLOSURE;
  S->stack[S->sp++].value.gc = cl;
}

int sly_greater_than(sly_state_t* S, int idx1, int idx2)
{
  idx1 = calc_index(S, idx1);
  idx2 = calc_index(S, idx2);

  return S->stack[idx1].value.fixnum > S->stack[idx2].value.fixnum;
}

void sly_set_global(sly_state_t* S, const char* name)
{
  int idx;
  sly_object_t sym;
  sly_gcobject_t *str;

  str = sly_create_string(S, name, 0);
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

