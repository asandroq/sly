/*
 * The Sly Scheme garbage collector
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

#ifndef __SLY_GC_H__
#define __SLY_GC_H_

#include "sly.h"
#include "object.h"

typedef struct sly_Store sly_Store;

/*
 * this callback is called by the garbage collector
 * to get all the mutator roots. It must return NULL
 * when there are no more roots to scan
 */
typedef sly_Object* (*sly_Roots_Callback)(void*);

struct sly_Store {
  
  /* the total size of a semispace */
  uint32_t capacity;

  /* the used size of the semispace */
  uint32_t size;

  /* address of memory received from OS */
  void *os_address;

  /* the space from where objects are copied */
  void *from_space;

  /* the space to which objects are copied */
  void *to_space;

  /* roots callback */
  sly_Roots_Callback roots_cb;

  /* opaque data to pass to callback */
  void *roots_cb_data;
};

int sly_gc_init(sly_Store *S, sly_Roots_Callback cb, void* ud);
void sly_gc_finish(sly_Store *S);

sly_Box          *sly_gc_alloc_box(sly_Store *S);
sly_Closure      *sly_gc_alloc_closure(sly_Store *S, uint32_t nr_vars);
sly_Pair         *sly_gc_alloc_pair(sly_Store *S);
sly_Continuation *sly_gc_alloc_continuation(sly_Store *S, uint32_t stack_size);
sly_String       *sly_gc_alloc_string(sly_Store *S, uint32_t size);
sly_Vector       *sly_gc_alloc_vector(sly_Store *S, uint32_t size);

#endif
