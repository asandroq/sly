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

#include "sly.h"

#include <stdlib.h>
#include <assert.h>

#include "gc.h"
#include "object.h"

/*
 * the store, i.e., memory
 */

#define SLY_INITIAL_SPACE_SIZE     ((uint32_t)(1 << 7))
#define SLY_IMMEDIATE_P(o)         ((o)->type < SLY_TYPE_CLOSURE)
#define SLY_FORWARD_TAG            199

/* forward reference to object in to-space */
typedef struct sly_Forward_Ref sly_Forward_Ref;

struct sly_Forward_Ref {
  SLY_GC_BASE;
  sly_GCObject *ref;
};

int sly_gc_init(sly_Store *S, sly_Roots_Callback cb, void* ud)
{
  /* alloc heap */
  S->from_space = malloc(SLY_INITIAL_SPACE_SIZE * 2);
  if(S->from_space == NULL) {
    return 0;
  }

  S->size = 0;
  S->os_address = S->from_space;
  S->capacity = SLY_INITIAL_SPACE_SIZE;
  S->to_space = S->from_space + (SLY_INITIAL_SPACE_SIZE);

  S->roots_cb = cb;
  S->roots_cb_data = ud;

  return 1;
}

void sly_gc_finish(sly_Store *S)
{
  free(S->os_address);
  S->size = S->capacity = 0;
  S->from_space = S->to_space = NULL;
  S->roots_cb = S->roots_cb_data = NULL;
}

static uint32_t sizeof_gcobj(sly_GCObject* obj)
{
  uint32_t size;

  switch(obj->type) {
  case SLY_TYPE_CLOSURE:
    size = SLY_SIZE_OF_CLOSURE(((sly_Closure*)obj)->nr_free);
    break;

  case SLY_TYPE_PAIR:
    size = SLY_SIZE_OF_PAIR;
    break;

  case SLY_TYPE_CONTINUATION:
    size = SLY_SIZE_OF_CONTINUATION(((sly_Continuation*)obj)->size);
    break;

  case SLY_TYPE_BOX:
    size = SLY_SIZE_OF_BOX;
    break;

  case SLY_TYPE_STRING:
    size = SLY_SIZE_OF_STRING(((sly_String*)obj)->size);
    break;

  case SLY_TYPE_VECTOR:
    size = SLY_SIZE_OF_VECTOR(((sly_Vector*)obj)->size);
    break;

  default:
    size = 0;
  }

  return size;
}

static void copy_object(sly_Store* S, sly_Object* obj)
{
  void *to;
  uint32_t size;

  if(SLY_IMMEDIATE_P(obj)) {
    /* if not heap-allocated, bail */
    return;
  }

  if(obj->value.gc->type == SLY_FORWARD_TAG) {
    /* already copied, just update pointer */
    obj->value.gc = ((sly_Forward_Ref*)obj->value.gc)->ref;
    return;
  }

  /* copying */
  to = S->to_space + S->size;
  size = sizeof_gcobj(obj->value.gc);
  memcpy(to, obj->value.gc, size);
  S->size += size;

  /* leave a forwarding pointer and update */
  obj->value.gc->type = SLY_FORWARD_TAG;
  ((sly_Forward_Ref*)obj->value.gc)->ref = to;
  obj->value.gc = to;
}

static void collect_garbage(sly_Store* S)
{
  /*
   * classic, simple 2-space copy collector
   * using Cheney's algorithm
   */
  void *scan;
  sly_Object *obj;
  uint32_t old_size;

  if(!S->roots_cb) {
    return;
  }

  old_size = S->size;
  S->size = 0;

  /* copying roots */
  while((obj = S->roots_cb(S->roots_cb_data))) {
    copy_object(S, obj);
  }

  /* now scan to-space */
  scan = S->to_space;
  while(scan < S->to_space + S->size) {
    uint32_t i, size;
    sly_GCObject *gcobj;

    gcobj = (sly_GCObject*)scan;
    size = sizeof_gcobj(gcobj);

    switch(gcobj->type) {
    case SLY_TYPE_CLOSURE:
      for(i = 0; i < ((sly_Closure*)gcobj)->nr_free; i++) {
	copy_object(S, &(((sly_Closure*)gcobj)->free_vars[i]));
      }
      break;

    case SLY_TYPE_PAIR:
      copy_object(S, &((sly_Pair*)gcobj)->car);
      copy_object(S, &((sly_Pair*)gcobj)->cdr);
      break;

    case SLY_TYPE_CONTINUATION:
      for(i = 0; i < ((sly_Continuation*)gcobj)->size; i++) {
	copy_object(S, &(((sly_Continuation*)gcobj)->stack[i]));
      }
      break;

    case SLY_TYPE_BOX:
      copy_object(S, &((sly_Box*)gcobj)->value);
      break;

    case SLY_TYPE_VECTOR:
      for(i = 0; i < ((sly_Vector*)gcobj)->size; i++) {
	copy_object(S, &(((sly_Vector*)gcobj)->data[i]));
      }
      break;
    }

    scan += size;
  }

  /* swap spaces */
  scan = S->from_space;
  S->from_space = S->to_space;
  S->to_space = scan;
}

static int expand_store(sly_Store* S)
{
  void *tmp;
  uint32_t old_size, size;

  old_size = S->capacity;

  /* new size is 30% larger, multiple of 4 */
  size = old_size * 4 / 3;
  size -= size % 4;

  tmp = malloc(size * 2);
  if(tmp == NULL) {
    return 0;
  } else {
    S->capacity = size;

    /* copy objects to new memory */
    S->to_space = tmp;
    collect_garbage(S);

    /* fix pointers */
    S->to_space = tmp + size;
    free(S->os_address);
    S->os_address = tmp;
    return 1;
  }
}

static void* alloc_from_store(sly_Store *S, uint32_t size)
{
  void *ret;

  /* allocating only 4-byte aligned blocks */
  assert(size % 4 == 0);

  if(S->capacity - S->size < size) {
    /* not enough space, try to find some */
    collect_garbage(S);

    while(S->capacity - S->size < size) {
      /* expand store until it fits */
      if(expand_store(S) == 0) {
	return NULL;
      }
    }
  }

  /* allocs from the from space */
  ret = S->from_space + S->size;
  S->size += size;

  return ret;
}

sly_Box *sly_gc_alloc_box(sly_Store *S)
{
  sly_Box *ret;

  ret = (sly_Box*)alloc_from_store(S, SLY_SIZE_OF_BOX);
  if(ret) {
    ret->type = SLY_TYPE_BOX;
  }

  return ret;
}

sly_Closure *sly_gc_alloc_closure(sly_Store *S, uint32_t nr_vars)
{
  sly_Closure *ret;

  ret = (sly_Closure*) alloc_from_store(S, SLY_SIZE_OF_CLOSURE(nr_vars));
  if(ret) {
    ret->type = SLY_TYPE_CLOSURE;
    ret->nr_free = nr_vars;
  }

  return ret;
}

sly_Pair *sly_gc_alloc_pair(sly_Store *S)
{
  sly_Pair *ret;

  ret = (sly_Pair*) alloc_from_store(S, SLY_SIZE_OF_PAIR);
  if(ret) {
    ret->type = SLY_TYPE_PAIR;
  }

  return ret;
}

sly_Continuation *sly_gc_alloc_continuation(sly_Store *S, uint32_t stack_size)
{
  sly_Continuation *ret;

  ret = (sly_Continuation*)alloc_from_store(S, SLY_SIZE_OF_CONTINUATION(stack_size));
  if(ret) {
    ret->type = SLY_TYPE_CONTINUATION;
    ret->size = stack_size;
  }

  return ret;
}

sly_String *sly_gc_alloc_string(sly_Store *S, uint32_t size)
{
  sly_String *ret;

  ret = (sly_String*)alloc_from_store(S, SLY_SIZE_OF_STRING(size));

  if(ret) {
    ret->type = SLY_TYPE_STRING;
    ret->size = size;
  }

  return ret;
}

sly_Vector *sly_gc_alloc_vector(sly_Store *S, uint32_t size)
{
  sly_Vector* ret;

  ret = (sly_Vector*)alloc_from_store(S, SLY_SIZE_OF_VECTOR(size));

  if(ret) {
    uint32_t i;

    ret->type = SLY_TYPE_VECTOR;
    ret->size = size;

    for(i = 0; i < size; i++) {
      ret->data[i].type = SLY_TYPE_UNDEF;
    }
  }

  return ret;
}

