/*
 * The Sly Scheme compiler
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

#ifndef __SLY_SCHEME_H__
#define __SLY_SCHEME_H__

#if __GNUC__ > 2
#include <stdint.h>
#else
#include <inttypes.h>
#endif

/* type of small integers */
typedef uint32_t sly_fixnum_t;

/* the state of the Sly virtual machine */
typedef struct sly_state_t sly_state_t;

/* type of C functions callable by the VM */
typedef int (*sly_cfunction_t)(sly_state_t* S);

/* array of C functions to be registered in the state */
typedef struct sly_reg_t {
  const char *name;
  sly_cfunction_t func;
} sly_reg_t;

sly_state_t* sly_open(void);
void sly_close(sly_state_t* S);

/*
 * shows the top nr elements of the stack and
 * then aborts
 */
void sly_error(sly_state_t* S, int nr);

/*
 * register several functions as globals
 * at once
 */
void sly_register(sly_state_t* S, sly_reg_t* regs);

int sly_load_file(sly_state_t* S, const char *fname);

/*
 * object manipulation API
 */

/* the height of the current interface stack */
int sly_get_top(sly_state_t* S);

/* push values onto the stack */
void sly_push_value(sly_state_t* S, int idx);
void sly_push_boolean(sly_state_t* S, int bool);
void sly_push_integer(sly_state_t* S, sly_fixnum_t num);
void sly_push_cclosure(sly_state_t* S, sly_cfunction_t func, int nr_vars);
void sly_push_string(sly_state_t* S, const char* str);

/* compare values on the stack */
int sly_greater_than(sly_state_t* S, int idx1, int idx2);

/* arithmetic */
void sly_unary_minus(sly_state_t* S, int idx);
void sly_add(sly_state_t* S, int idx1, int idx2);
void sly_sub(sly_state_t* S, int idx1, int idx2);

/* number I/O */
void sly_number_to_string(sly_state_t* S, int idx);

/* I/O */
void sly_write(sly_state_t* S, int idx);
void sly_display(sly_state_t* S, int idx);

/* sets the current object on top of the stack as a global */
void sly_set_global(sly_state_t* S, const char* name);

#endif
