/*
 * The Sly Scheme internal library
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

#include <assert.h>

#include "sly.h"
#include "io.h"
#include "state.h"

/*
 * pushes a new dynamic binding on the stack
 * using the tag and value given
 */
static int dynamic_bind(sly_state_t* S)
{
  return 0;
}

/*
 * search for a dynamic binding with the given tag
 * on the stack, and pushes the associanted value or
 * a default one if the binding was not found
 * the tag and a default value must have been pushed
 * on the stack
 */
static int dynamic_lookup(sly_state_t* S)
{
  int idx;

  for(idx = S->sp - 3; idx >= 0; idx--) {
    if(STK(idx).type == SLY_TYPE_DYN_BIND &&
       SLY_OBJ_EQ(SLY_DYN_BIND(STKGC(idx))->tag, STK(S->sp-2))) {
      STK(S->sp++) = SLY_DYN_BIND(STKGC(idx))->value;
      break;
    }
  }

  return 1;
}

/*
 * search for a dynamic binding with the given tag
 * on the stack, sets the associated value
 * of the binding with the given argument, and
 * returns true if successful, false otherwise
 * the tag and the value must have been pushed
 * on the stack
 */
static int dynamic_store(sly_state_t* S)
{
  int idx;

  for(idx = S->sp - 3; idx >= 0; idx--) {
    if(STK(idx).type == SLY_TYPE_DYN_BIND &&
       SLY_OBJ_EQ(SLY_DYN_BIND(STKGC(idx))->tag, STK(S->sp-2))) {
      SLY_DYN_BIND(STKGC(idx))->value = STK(S->sp-1);
      break;
    }
  }

  STK(S->sp).type = SLY_TYPE_BOOL;
  if(idx < 0) {
    STK(S->sp++).value.bool = 0;
  } else {
    STK(S->sp++).value.bool = 1;
  }

  return 1;
}

static int read_token(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  assert(STK(S->sp-1).type == SLY_TYPE_INPUT_PORT);

  STK(S->sp++).type = SLY_TYPE_NIL;
  sly_io_read_token(S, &STK(S->sp-2), &STK(S->sp-1));

  return 1;
}

static sly_reg_t lib_regs[] = {
  {"##dynamic-bind", dynamic_bind},
  {"##dynamic-lookup", dynamic_lookup},
  {"##dynamic-store", dynamic_store},
  {"##read-token", read_token},
  {NULL, NULL}
};

int sly_open_lib(sly_state_t* S)
{
  sly_register(S, lib_regs);

  return 0;
}
