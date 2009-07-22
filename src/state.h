/*
 * The Sly Scheme system
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

#ifndef __SLY_STATE_H__
#define __SLY_STATE_H__

#include "sly.h"
#include "gc.h"

typedef struct sly_env_t sly_env_t;
typedef struct sly_env_var_t sly_env_var_t;

/* an entry in an environment */
struct sly_env_var_t {

  /* entry in the symbol table */
  sly_symbol_t *symbol;

  /* the actual value */
  sly_object_t value;
};

/* a global environment */
struct sly_env_t {
  uint32_t size;
  sly_env_var_t *vars;
};

struct sly_state_t {

  /* the size of the bytecode vector used */
  uint32_t code_size;

  /* stack allocated size */
  uint32_t stack_size;

  /* number of constants */
  uint32_t nr_consts;

  /* where is the top of the stack */
  uint32_t sp;

  /* the frame pointer */
  uint32_t fp;

  /* the program counter */
  uint32_t pc;

  /* global index of the 'sly-eval' procedure */
  uint32_t sly_eval;

  /* accumulator register */
  sly_object_t accum;

  /* the current procedure */
  sly_object_t proc;

  /* global environment */
  sly_env_t global_env;

  /* the bytecode to be interpreted */
  uint32_t *code;

  /* the machine stack */
  sly_object_t *stack;

  /* constants */
  sly_object_t *consts;

  /* symbol table */
  sly_symbol_t *symbol_table;

  /* VM memory */
  sly_store_t store;
};

void sly_st_enlarge_globals(sly_state_t* S, uint32_t more);
int sly_st_get_global_index(sly_state_t* S, sly_symbol_t *global);


#endif
