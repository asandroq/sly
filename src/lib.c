/*
 * The Sly Scheme runtime
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

#include "sly.h"
#include "object.h"

/*
 * R5RS 6.2.5
 */

static int compare(sly_state_t* S, int c)
{
  int i, ret, nargs = sly_get_top(S);

  if(nargs == 0) {
    sly_push_string(S, "cannot compare zero numbers");
    sly_error(S, 1);
  } else if(nargs == 1) {
    if(!sly_numberp(S, 0)) {
      sly_push_string(S, "cannot compare single non-number");
      sly_error(S, 1);
    } else {
      sly_push_boolean(S, 1);
    }
  } else {
    for(i = 0; i < nargs - 1; i++) {
      switch(c) {
      case 0:
        ret = sly_less_than(S, i, i+1);
        break;
      case 1:
        ret = sly_greater_than(S, i, i+1);
        break;
      }
      if(!ret) {
	sly_push_boolean(S, 0);
	return 1;
      }
    }
    sly_push_boolean(S, 1);
  }

  return 1;
}

static int less_than(sly_state_t* S)
{
  return compare(S, 0);
}

static int greater_than(sly_state_t* S)
{
  return compare(S, 1);
}

static int plus(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs == 0) {
    sly_push_integer(S, 0);
  } else if(nargs == 1) {
    if(!sly_numberp(S, 0)) {
      sly_push_string(S, "cannot add single non-number");
      sly_error(S, 1);
    }
  } else {
    sly_add(S, nargs);
  }

  return 1;
}

static int minus(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs == 0) {
    sly_push_string(S, "not enough numbers to subtract");
    sly_error(S, 1);
  } else if(nargs == 1) {
    sly_unary_minus(S, 0);
  } else  {
    sly_subtract(S, nargs);
  }

  return 1;
}

static int divide(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs == 0) {
    sly_push_string(S, "not enough numbers to divide");
    sly_error(S, 1);
  } else if(nargs == 1) {
    sly_invert(S, 0);
  } else  {
    sly_divide(S, nargs);
  }

  return 1;
}

static int round(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  } else {
    sly_round(S, 0);
  }

  return 1;
}

static int quotient(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 2) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  } else {
    sly_divide(S, 2);
  }

  return 1;
}

static int remainder(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 2) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  } else {
    sly_remainder(S, 0, 1);
  }

  return 1;
}

/*
 * R5RS 6.2.6
 */

static int number_to_string(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  } else {
    sly_number_to_string(S, 0);
  }

  return 1;
}

/*
 * R5RS 6.3.2
 */

static int cons(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 2) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  sly_cons(S, 0, 1);

  return 1;
}

static int car(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(!sly_pairp(S, 0)) {
    sly_push_string(S, "cannot extract the car of non-pair");
    sly_error(S, 1);
  }

  sly_car(S, 0);

  return 1;
}

static int cdr(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(!sly_pairp(S, 0)) {
    sly_push_string(S, "cannot extract the cdr of non-pair");
    sly_error(S, 1);
  }

  sly_cdr(S, 0);

  return 1;
}

static int listp(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(sly_listp(S, 0)) {
    sly_push_boolean(S, 1);
  } else {
    sly_push_boolean(S, 0);
  }

  return 1;
}

/*
 * R5RS 6.3.3
 */

static int symbol_to_string(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  sly_symbol_to_string(S, 0);

  return 1;
}

/*
 * R5RS 6.3.5
 */

static int stringp(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(sly_stringp(S, 0)) {
    sly_push_boolean(S, 1);
  } else {
    sly_push_boolean(S, 0);
  }

  return 1;
}

static int string_length(sly_state_t* S)
{
  uint32_t len;
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  len = sly_string_length(S, 0);
  sly_push_integer(S, len);

  return 1;
}

static int string_ref(sly_state_t* S)
{
  sly_char_t c;
  int i, nargs = sly_get_top(S);

  if(nargs != 2) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(!sly_stringp(S, 0)) {
    sly_push_string(S, "cannot index non-string");
    sly_error(S, 1);
  }

  if(!sly_integerp(S, 1)) {
    sly_push_string(S, "cannot index string using a non-integer");
    sly_error(S, 1);
  }

  i = sly_to_integer(S, 1);
  c = sly_string_ref(S, i, 0);
  sly_push_char(S, c);

  return 1;
}

static int string_append(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs == 0) {
    sly_push_string(S, "cannot append zero strings");
    sly_error(S, 1);
  } else {
    sly_concat(S, nargs);
  }

  return 1;
}

/*
 * R5RS 6.3.6
 */

static int vectorp(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(sly_vectorp(S, 0)) {
    sly_push_boolean(S, 1);
  } else {
    sly_push_boolean(S, 0);
  }

  return 1;
}

static int make_vector(sly_state_t* S)
{
  int i, size, nargs = sly_get_top(S);

  if(nargs != 1 && nargs != 2) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(!sly_integerp(S, 0)) {
    sly_push_string(S, "non-integer size given to vector: ");
    sly_push_value(S, 0);
    sly_error(S, 2);
  }

  size = sly_to_integer(S, 0);
  sly_push_vector(S, size);

  if(nargs == 2) {
    for(i = 0; i < size; i++) {
      sly_push_value(S, 1);
      sly_vector_set(S, i, -2);
    }
  }

  return 1;
}

static int vector_length(sly_state_t* S)
{
  uint32_t len;
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  len = sly_vector_length(S, 0);
  sly_push_integer(S, len);

  return 1;
}

static int vector_ref(sly_state_t* S)
{
  int i, nargs = sly_get_top(S);

  if(nargs != 2) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(!sly_vectorp(S, 0)) {
    sly_push_string(S, "cannot index non-vector");
    sly_error(S, 1);
  }

  if(!sly_integerp(S, 1)) {
    sly_push_string(S, "cannot index vector using a non-integer");
    sly_error(S, 1);
  }

  i = sly_to_integer(S, 1);
  sly_vector_ref(S, i, 0);

  return 1;
}

static int vector_set(sly_state_t* S)
{
  int i, nargs = sly_get_top(S);

  if(nargs != 3) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(!sly_vectorp(S, 0)) {
    sly_push_string(S, "cannot set non-vector");
    sly_error(S, 1);
  }

  if(!sly_integerp(S, 1)) {
    sly_push_string(S, "cannot index vector using a non-integer");
    sly_error(S, 1);
  }

  i = sly_to_integer(S, 1);
  sly_vector_set(S, i, 0);
  sly_pop(S, 1);

  return 1;
}

/*
 * R5RS 6.4
 */

static int apply(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs < 2) {
    sly_push_string(S, "too few arguments to apply");
    sly_error(S, 1);
  }

  if(!sly_procedurep(S, 0)) {
    sly_push_string(S, "cannot apply non-procedure");
    sly_error(S, 1);
  }

  if(!sly_listp(S, -1)) {
    sly_push_string(S, "last argument to apply must be a pair");
    sly_error(S, 1);
  }

  sly_apply(S, 0, nargs-1);

  return 1;
}

/*
 * R5RS 6.5
 */

static int eval(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  sly_eval(S, 0);

  return 1;
}

/*
 * R5RS 6.6.1
 */

static int input_portp(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(sly_input_portp(S, 0)) {
    sly_push_boolean(S, 1);
  } else {
    sly_push_boolean(S, 0);
  }

  return 1;
}

static int eof_objectp(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(sly_eof_objectp(S, 0)) {
    sly_push_boolean(S, 1);
  } else {
    sly_push_boolean(S, 0);
  }

  return 1;
}

static int output_portp(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(sly_output_portp(S, 0)) {
    sly_push_boolean(S, 1);
  } else {
    sly_push_boolean(S, 0);
  }

  return 1;
}

static int current_input_port(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 0) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  sly_push_current_input_port(S);

  return 1;
}

static int current_output_port(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 0) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  sly_push_current_output_port(S);

  return 1;
}

static int current_error_port(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 0) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  sly_push_current_error_port(S);

  return 1;
}

static int open_input_file(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(!sly_stringp(S, 0)) {
    sly_push_string(S, "file name must be a string: ");
    sly_push_value(S, 0);
    sly_error(S, 2);
  }

  sly_open_input_file(S, 0);

  return 1;
}

static int open_output_file(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(!sly_stringp(S, 0)) {
    sly_push_string(S, "file name must be a string: ");
    sly_push_value(S, 0);
    sly_error(S, 2);
  }

  sly_open_output_file(S, 0);

  return 1;
}

static int close_input_port(sly_state_t *S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(!sly_input_portp(S, 0)) {
    sly_push_string(S, "cannot close non-input port");
    sly_push_value(S, 0);
    sly_error(S, 2);
  }

  sly_close_input_port(S, 0);

  return 0;
}

static int close_output_port(sly_state_t *S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(!sly_output_portp(S, 0)) {
    sly_push_string(S, "cannot close non-output port");
    sly_push_value(S, 0);
    sly_error(S, 2);
  }

  sly_close_output_port(S, 0);

  return 0;
}

/*
 * R5RS 6.6.3
 */

static int read_token(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 0 && nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(nargs == 0) {
    sly_push_current_input_port(S);
  }

  sly_read_token(S, 0);

  return 1;
}

static int write(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1 && nargs != 2) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(nargs == 1) {
    sly_push_current_output_port(S);
  }

  sly_write(S, 0, 1);

  return 0;
}

static int display(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1 && nargs != 2) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(nargs == 1) {
    sly_push_current_output_port(S);
  }

  sly_display(S, 0, 1);

  return 0;
}

static int newline(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 0 && nargs != 1) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(nargs == 0) {
    sly_push_current_output_port(S);
  }

  sly_newline(S, 0);

  return 0;
}

static int write_char(sly_state_t* S)
{
  int nargs = sly_get_top(S);

  if(nargs != 1 && nargs != 2) {
    sly_push_string(S, "wrong number of arguments");
    sly_error(S, 1);
  }

  if(!sly_charp(S, 0)) {
    sly_push_string(S, "cannot write non-char: ");
    sly_push_value(S, 0);
    sly_error(S, 2);
  }

  if(nargs == 1) {
    sly_push_current_output_port(S);
  }

  sly_display(S, 0, 1);

  return 0;
}

static int error(sly_state_t* S)
{
  return sly_error(S, sly_get_top(S));
}

static sly_reg_t lib_regs[] = {
  {"<", less_than},
  {">", greater_than},
  {"+", plus},
  {"-", minus},
  {"/", divide},
  {"round", round},
  {"quotient", quotient},
  {"remainder", remainder},
  {"number->string", number_to_string},
  {"cons", cons},
  {"car", car},
  {"cdr", cdr},
  {"list?", listp},
  {"symbol->string", symbol_to_string},
  {"string?", stringp},
  {"string-length", string_length},
  {"string-ref", string_ref},
  {"string-append", string_append},
  {"vector?", vectorp},
  {"make-vector", make_vector},
  {"vector-length", vector_length},
  {"vector-ref", vector_ref},
  {"vector-set!", vector_set},
  {"apply", apply},
  {"eval", eval},
  {"eof-object?", eof_objectp},
  {"input-port?", input_portp},
  {"output-port?", output_portp},
  {"current-input-port", current_input_port},
  {"current-output-port", current_output_port},
  {"current-error-port", current_error_port},
  {"open-input-file", open_input_file},
  {"open-output-file", open_output_file},
  {"close-input-port", close_input_port},
  {"close-output-port", close_output_port},
  {"##read-token", read_token},
  {"write", write},
  {"display", display},
  {"newline", newline},
  {"write-char", write_char},
  {"error", error},
  {NULL, NULL}
};

int sly_open_lib(sly_state_t* S)
{
  sly_register(S, lib_regs);

  return 0;
}

