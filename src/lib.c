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

#include <stddef.h>

#include "sly.h"
#include "object.h"

static int greater_than(sly_state_t* S)
{
  int i, nargs = sly_get_top(S);

  if(nargs < 2) {
    sly_push_boolean(S, 1);
  } else {
    for(i = 0; i < nargs - 1; i++) {
      if(!sly_greater_than(S, i, i+i)) {
	sly_push_boolean(S, 0);
	return 1;
      }
    }
    sly_push_boolean(S, 1);
  }

  return 1;
}

static sly_reg_t lib_regs[] = {
  {">", greater_than},
  {NULL, NULL}
};

int sly_open_lib(sly_state_t* S)
{
  sly_register(S, lib_regs);

  return 0;
}

