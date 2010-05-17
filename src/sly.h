/*
 * The Sly Scheme compiler
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

#ifndef __SLY_SCHEME_H__
#define __SLY_SCHEME_H__

#include <stdint.h>

/* character encodings */
#define SLY_CHAR_ENC_UTF8         1
#define SLY_CHAR_ENC_UTF16        2
#define SLY_CHAR_ENC_LATIN1       3

/* code point types */
typedef uint8_t sly_cp1_t;
typedef uint16_t sly_cp2_t;
typedef uint32_t sly_cp4_t;
typedef sly_cp4_t sly_char_t;

/* type of small integers */
typedef intptr_t sly_fixnum_t;

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
int sly_error(sly_state_t* S, uint32_t nr);

/*
 * register several functions as globals
 * at once
 */
void sly_register(sly_state_t* S, sly_reg_t* regs);

/* the input and output ports must be on stack */
void sly_repl(sly_state_t *S);

int sly_load_buffer(sly_state_t* S, const uint8_t *buffer);

/*
 * object manipulation API
 */

int sly_get_top(sly_state_t* S);
void sly_pop(sly_state_t* S, uint32_t num);

/* type predicates */
int sly_boxp(sly_state_t* S, int idx);
int sly_charp(sly_state_t* S, int idx);
int sly_integerp(sly_state_t* S, int idx);
int sly_numberp(sly_state_t* S, int idx);
int sly_pairp(sly_state_t* S, int idx);
int sly_listp(sly_state_t* S, int idx);
int sly_stringp(sly_state_t* S, int idx);
int sly_vectorp(sly_state_t* S, int idx);
int sly_procedurep(sly_state_t* S, int idx);
int sly_eof_objectp(sly_state_t* S, int idx);
int sly_input_portp(sly_state_t* S, int idx);
int sly_output_portp(sly_state_t* S, int idx);

/* push values onto the stack */
void sly_get_global(sly_state_t* S, const char *name);
void sly_push_value(sly_state_t* S, int idx);
void sly_push_boolean(sly_state_t* S, int bool);
void sly_push_char(sly_state_t* S, sly_char_t chr);
void sly_push_integer(sly_state_t* S, sly_fixnum_t num);
void sly_push_cclosure(sly_state_t* S, sly_cfunction_t func, uint32_t nr_vars);
void sly_push_string(sly_state_t* S, const char* str);
void sly_push_vector(sly_state_t* S, uint32_t size);

void sly_push_current_input_port(sly_state_t *S);
void sly_push_current_output_port(sly_state_t *S);
void sly_push_current_error_port(sly_state_t *S);

/* get values from the stack */
sly_fixnum_t sly_to_integer(sly_state_t* S, int idx);

sly_char_t* sly_to_string(sly_state_t* S, int idx);
sly_cp1_t* sly_to_string_latin1(sly_state_t* S, int idx);
sly_cp1_t* sly_to_string_utf8(sly_state_t* S, int idx);
sly_cp2_t* sly_to_string_utf16(sly_state_t* S, int idx);

/* compare values on the stack */
int sly_less_than(sly_state_t* S, int idx1, int idx2);
int sly_greater_than(sly_state_t* S, int idx1, int idx2);

/* convert values on stack */
void sly_symbol_to_string(sly_state_t* S, int idx);
void sly_string_to_symbol(sly_state_t* S, int idx);

/* arithmetic */
void sly_invert(sly_state_t* S, int idx);
void sly_unary_minus(sly_state_t* S, int idx);
void sly_add(sly_state_t* S, uint32_t nr_numbers);
void sly_subtract(sly_state_t* S, uint32_t nr_numbers);
void sly_divide(sly_state_t* S, uint32_t nr_numbers);
void sly_round(sly_state_t* S, int idx);
void sly_remainder(sly_state_t* S, int idx1, int idx2);

/* number I/O */
void sly_number_to_string(sly_state_t* S, int idx);

/* boxes */
void sly_box(sly_state_t* S);
void sly_unbox(sly_state_t* S);
void sly_set_box(sly_state_t* S, int idx);

/* lists */
void sly_cons(sly_state_t* S, int idx1, int idx2);
void sly_car(sly_state_t* S, int idx);
void sly_cdr(sly_state_t* S, int idx);

/* strings */
uint32_t sly_string_length(sly_state_t* S, int idx);
sly_char_t sly_string_ref(sly_state_t* S, uint32_t pos, int idx);
void sly_concat(sly_state_t* S, uint32_t nr_strings);

/* vectors */
uint32_t sly_vector_length(sly_state_t* S, int idx);
void sly_vector_ref(sly_state_t* S, uint32_t pos, int idx);
void sly_vector_set(sly_state_t* S, uint32_t pos, int idx);

/* control */
void sly_apply(sly_state_t* S, int idx, uint32_t nr_args);

/* evaluation */
void sly_eval(sly_state_t* S, int idx);
void sly_call(sly_state_t* S, uint32_t n_args);
void sly_load_file(sly_state_t* S, int idx);

/* I/O */
void sly_open_input_file(sly_state_t* S);
void sly_open_output_file(sly_state_t* S);
void sly_close_input_port(sly_state_t* S);
void sly_close_output_port(sly_state_t* S);
void sly_newline(sly_state_t* S, int idx);
void sly_write(sly_state_t* S, int idx1, int idx2);
void sly_display(sly_state_t* S, int idx1, int idx2);

/* sets the current object on top of the stack as a global */
void sly_set_global(sly_state_t* S, const char* name);

#endif
