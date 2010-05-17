/*
 * The Sly Scheme driver
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
#include <stdlib.h>

#include "sly.h"

int main(int argc, char *argv[])
{
  int repl;
  sly_state_t* S;

  repl = 0;
  if(argc == 1) {
    repl = 1;
  } else if(argc != 2) {
    fprintf(stderr, "%s: Need file to run.\n", argv[0]);
    exit(13);
  }

  S = sly_open();

  if(repl) {
    sly_push_current_input_port(S);
    sly_push_current_output_port(S);
    sly_repl(S);
  } else {
    sly_push_string(S, argv[1]);
    sly_load_file(S, -1);
    sly_pop(S, 2);
  }

  sly_close(S);

  return 0;
}

