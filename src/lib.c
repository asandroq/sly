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

/*
 * creates an input port that is the
 * system's standard input
 */
static int open_stdin_port(sly_state_t* S)
{
  STKGC(S->sp) = sly_io_create_stdin(S);
  STK(S->sp++).type = SLY_TYPE_INPUT_PORT;

  return 1;
}

/*
 * creates an input port that is the
 * system's standard output
 */
static int open_stdout_port(sly_state_t* S)
{
  STKGC(S->sp) = sly_io_create_stdout(S);
  STK(S->sp++).type = SLY_TYPE_OUTPUT_PORT;

  return 1;
}

static int open_stderr_port(sly_state_t* S)
{
  STKGC(S->sp) = sly_io_create_stderr(S);
  STK(S->sp++).type = SLY_TYPE_OUTPUT_PORT;

  return 1;
}

/*
 * parses input into tokens to be used
 * by the 'read' procedure'
 */
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

/* creates syntactic closures */
static int make_syn_clo(sly_state_t* S)
{
  sly_object_t synclo;
  int nargs = sly_get_top(S);

  if(nargs != 3) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  synclo.type = SLY_TYPE_SYNCLO;
  synclo.value.gc = sly_create_syn_closure(S);

  SLY_SYNCLO(synclo.value.gc)->env  = STK(S->sp-3);
  SLY_SYNCLO(synclo.value.gc)->free = STK(S->sp-2);
  SLY_SYNCLO(synclo.value.gc)->exp  = STK(S->sp-1);

  STK(S->sp++) = synclo;
  return 1;
}


static int syn_clo_p(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  STK(S->sp).type = SLY_TYPE_BOOL;
  if(STK(S->sp-1).type == SLY_TYPE_SYNCLO) {
    STK(S->sp++).value.bool = 1;
  } else {
    STK(S->sp++).value.bool = 0;
  }

  return 1;
}

static int syn_clo_env(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(STK(S->sp-1).type != SLY_TYPE_SYNCLO) {
    sly_push_string(S, "argument must be a syntactic closure");
    sly_error(S, 1);
  }

  STK(S->sp++) = SLY_SYNCLO(STK(S->sp-1).value.gc)->env;
  return 1;
}

static int syn_clo_free(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(STK(S->sp-1).type != SLY_TYPE_SYNCLO) {
    sly_push_string(S, "argument must be a syntactic closure");
    sly_error(S, 1);
  }

  STK(S->sp++) = SLY_SYNCLO(STK(S->sp-1).value.gc)->free;
  return 1;
}

static int syn_clo_exp(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(STK(S->sp-1).type != SLY_TYPE_SYNCLO) {
    sly_push_string(S, "argument must be a syntactic closure");
    sly_error(S, 1);
  }

  STK(S->sp++) = SLY_SYNCLO(STK(S->sp-1).value.gc)->exp;
  return 1;
}

static sly_reg_t lib_regs[] = {
  {"##dynamic-lookup", dynamic_lookup},
  {"##dynamic-store", dynamic_store},
  {"##open-standard-input", open_stdin_port},
  {"##open-standard-output", open_stdout_port},
  {"##open-standard-error", open_stderr_port},
  {"##read-token", read_token},
  {"make-syntactic-closure", make_syn_clo},
  {"syntactic-closure?", syn_clo_p},
  {"syntactic-closure-env", syn_clo_env},
  {"syntactic-closure-free", syn_clo_free},
  {"syntactic-closure-exp", syn_clo_exp},
  {NULL, NULL}
};

int sly_open_lib(sly_state_t* S)
{
  sly_register(S, lib_regs);

  return 0;
}
