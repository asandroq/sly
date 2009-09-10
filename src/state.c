/*
 * The Sly Scheme system
 * Copyright © 2009 Alex Queiroz <asandroq@gmail.com>
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
#include "io.h"
#include "vm.h"
#include "state.h"

int sly_open_lib(sly_state_t* S);

/* garbage collector callback */
struct gc_data {
  sly_state_t *S;
  uint32_t state, count;
};

static struct gc_data gc_data;

static sly_object_t* gc_callback(void *ud)
{
  struct gc_data *gc_data;

  gc_data = (struct gc_data*) ud;

  for(;;) {
    if(gc_data->state == 0) {
      /* stack */
      if(gc_data->count == gc_data->S->sp) {
	gc_data->state++;
	gc_data->count = 0;
	continue;
      } else {
	return &gc_data->S->stack[gc_data->count++];
      }
    } else if(gc_data->state == 1) {
      /* globals */
      if(gc_data->count == gc_data->S->global_env.size) {
	gc_data->state++;
	gc_data->count = 0;
	continue;
      } else {
	return &gc_data->S->global_env.vars[gc_data->count++].value;
      }
    } else if(gc_data->state == 2) {
      /* constants */
      if(gc_data->count == gc_data->S->nr_consts) {
	gc_data->state++;
	gc_data->count = 0;
	continue;
      } else {
	return &gc_data->S->consts[gc_data->count++];
      }
    } else if(gc_data->state == 3) {
      /* registers */
      if(gc_data->count == 0) {
	gc_data->count++;
	return &gc_data->S->accum;
      } else if(gc_data->count == 1) {
	gc_data->count++;
	return &gc_data->S->proc;
      } else {
	gc_data->state++;
	gc_data->count = 0;
	continue;
      }
    } else {
      gc_data->state = 0;
      gc_data->count = 0;
      return NULL;
    }
  }
}

static sly_state_t* sly_create_state(void)
{
  sly_state_t *S = NULL;

  S = (sly_state_t*)malloc(sizeof(sly_state_t));
  if(!S) {
    return NULL;
  }

  /*
   * store
   */

  gc_data.S = S;
  gc_data.state = 0;
  gc_data.count = 0;
  if(sly_gc_init(&S->store, gc_callback, &gc_data) == 0) {
    return NULL;
  }

  S->pc = 0;
  S->fp = 0;

  /*
   * code
   */

  S->code_size = 1;
  S->code = (uint32_t*)malloc(sizeof(uint32_t));
  if(S->code == NULL) {
    free(S);
    return NULL;
  }
  S->code[SLY_HALT_ADDRESS] = (uint32_t) SLY_OP_HALT;

  /*
   * stack
   */

  S->sp = 0;
  S->stack = (sly_object_t*)malloc(sizeof(sly_object_t) * 1024);
  if(S->stack) {
    S->stack_size = 1024;
  } else {
    free(S->code);
    free(S);
    return NULL;
  }

  /*
   * pushing on the stack the fundamental ports
   * this may seem a bootstrapping problem
   * but the store is already set, only care
   * with the stack pointer is needed
   */
  S->sp = 0;
  S->stack[0].type = SLY_TYPE_INPUT_PORT;
  S->stack[0].value.gc = sly_io_create_stdin(S);
  S->sp = 1;
  S->stack[1].type = SLY_TYPE_OUTPUT_PORT;
  S->stack[1].value.gc = sly_io_create_stdout(S);
  S->sp = 2;
  S->stack[2].type = SLY_TYPE_OUTPUT_PORT;
  S->stack[2].value.gc = sly_io_create_stderr(S);
  S->sp = 3;

  /*
   * symbol table
   */

  S->symbol_table = NULL;

  /*
   * constants
   */

  S->consts = NULL;
  S->nr_consts = 0;

  /*
   * globals
   */
  S->global_env.size = 0;
  S->global_env.vars = NULL;

  /*
   * registers
   */
  S->proc.type = SLY_TYPE_UNDEF;
  S->accum.type = SLY_TYPE_UNDEF;

  /*
   * virtual machine
   */
  sly_vm_init(S);

  return S;
}

sly_state_t* sly_open(void)
{
  sly_state_t *S = sly_create_state();

  /* register C lib */
  sly_open_lib(S);

  return S;
}

void sly_close(sly_state_t* S)
{
  if(S) {
    sly_gc_finish(&S->store);

    if(S->code) {
      free(S->code);
    }

    if(S->stack) {
      free(S->stack);
    }

    if(S->symbol_table) {
      sly_symbol_t *sym;
      for(sym = S->symbol_table; sym != NULL;) {
	sly_symbol_t *tmp = sym->next;
	free(sym->str);
	free(sym);
	sym = tmp;
      }
    }

    free(S);
  }
}

void sly_st_enlarge_globals(sly_state_t* S, uint32_t more)
{
  sly_env_var_t *vars;

  more += S->global_env.size;
  vars = (sly_env_var_t*)realloc(S->global_env.vars,
				 more * sizeof(sly_env_var_t));

  /* TODO: test return and throw error */
  S->global_env.vars = vars;
  S->global_env.size = more;
}

int sly_st_get_global_index(sly_state_t* S, sly_symbol_t *global)
{
  size_t i;

  for(i = 0; i < S->global_env.size; i++) {
    if(global == S->global_env.vars[i].symbol) {
      return i;
    }
  }

  return -1;
}

