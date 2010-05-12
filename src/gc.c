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
#include <string.h>
#include <assert.h>

#include "gc.h"
#include "object.h"

#ifdef SLY_DTRACE
#include "sly_provider.h"
#endif

/*
 * the store, i.e., memory
 */

#if defined(__LP64__) || defined(__LLP64__)
#define WSIZE     8
#else
#define WSIZE     4
#endif

#define SLY_INITIAL_SPACE_SIZE     ((uint32_t)(1 << 7))
#define SLY_IMMEDIATE_P(o)         ((o)->type < SLY_TYPE_CLOSURE)
#define SLY_FORWARD_TAG            199

/* forward reference to object in to-space */
typedef struct sly_forward_t sly_forward_t;

struct sly_forward_t {
  sly_gcobject_t base;
  sly_gcobject_t *ref;
};

static uint32_t sizeof_gcobj(sly_gcobject_t* obj)
{
  uint32_t size;

  switch(obj->type) {
  case SLY_TYPE_CLOSURE:
    size = SLY_SIZE_OF_CLOSURE(SLY_CLOSURE(obj)->nr_free);
    break;

  case SLY_TYPE_PAIR:
    size = SLY_SIZE_OF_PAIR;
    break;

  case SLY_TYPE_CONTI:
    size = SLY_SIZE_OF_CONTI(SLY_CONTI(obj)->size);
    break;

  case SLY_TYPE_BOX:
    size = SLY_SIZE_OF_BOX;
    break;

  case SLY_TYPE_STRING:
    size = SLY_SIZE_OF_STRING(SLY_STRING(obj)->size);
    break;

  case SLY_TYPE_VECTOR:
    size = SLY_SIZE_OF_VECTOR(SLY_VECTOR(obj)->size);
    break;

  case SLY_TYPE_DYN_BIND:
    size = SLY_SIZE_OF_DYN_BIND;
    break;

  case SLY_TYPE_INPUT_PORT:
    size = SLY_SIZE_OF_IPORT;
    break;

  case SLY_TYPE_OUTPUT_PORT:
    size = SLY_SIZE_OF_OPORT;
    break;

  default:
    abort();
  }

  return size;
}

static void copy_object(sly_store_t* S, sly_object_t* obj)
{
  void *to;
  uint32_t size;

  if(SLY_IMMEDIATE_P(obj)) {
    /* if not heap-allocated, bail */
    return;
  }

  if(obj->value.gc->type == SLY_FORWARD_TAG) {
    /* already copied, just update pointer */
    obj->value.gc = ((sly_forward_t*)obj->value.gc)->ref;
    return;
  }

  /* actual copy */
  to = S->to_space + S->size;
  size = sizeof_gcobj(obj->value.gc);
  memcpy(to, obj->value.gc, size);
  S->size += size;

  /* leave a forwarding pointer and update */
  obj->value.gc->type = SLY_FORWARD_TAG;
  ((sly_forward_t*)obj->value.gc)->ref = to;
  obj->value.gc = to;
}

static void collect_fobjs(sly_store_t *S)
{
  sly_fobj_t *fobj, *prev;

  for(prev = NULL, fobj = S->fobjs; fobj;) {
    if(fobj->obj->type == SLY_FORWARD_TAG) {
      fobj->obj = ((sly_forward_t*)fobj->obj)->ref;
      prev = fobj;
      fobj = fobj->next;
    } else {
      sly_fobj_t *tmp = fobj->next;

      /* if object was not copied then it's dead */
      if(prev) {
        prev->next = tmp;
      } else {
        S->fobjs = tmp;
      }
      if(fobj->obj->type == SLY_TYPE_INPUT_PORT ||
         fobj->obj->type == SLY_TYPE_OUTPUT_PORT) {
        sly_port_t *port;

        port = SLY_PORT(fobj->obj);
        port->finish(port);
      }
      free(fobj);
      fobj = tmp;
    }
  }
}

static void collect_garbage(sly_store_t* S)
{
  /*
   * classic, simple 2-space copy collector
   * using Cheney's algorithm
   */
  void *scan;
  sly_object_t *obj;

#ifdef SLY_DTRACE
  int old_size = S->size;
  SLY_GC_START();
#endif

  if(!S->roots_cb) {
    return;
  }

  S->size = 0;

  /* copying roots */
  while((obj = S->roots_cb(S->roots_cb_data))) {
    copy_object(S, obj);
  }

  /* now scan to-space */
  scan = S->to_space;
  while(scan < S->to_space + S->size) {
    uint32_t i, size;
    sly_gcobject_t *gcobj;

    gcobj = SLY_GCOBJECT(scan);
    size = sizeof_gcobj(gcobj);

    switch(gcobj->type) {
    case SLY_TYPE_CLOSURE:
      for(i = 0; i < SLY_CLOSURE(gcobj)->nr_free; i++) {
	copy_object(S, &(SLY_CLOSURE(gcobj)->free_vars[i]));
      }
      break;

    case SLY_TYPE_PAIR:
      copy_object(S, &(SLY_PAIR(gcobj)->car));
      copy_object(S, &(SLY_PAIR(gcobj)->cdr));
      break;

    case SLY_TYPE_CONTI:
      for(i = 0; i < SLY_CONTI(gcobj)->size; i++) {
	copy_object(S, &(SLY_CONTI(gcobj)->stack[i]));
      }
      break;

    case SLY_TYPE_BOX:
      copy_object(S, &(SLY_BOX(gcobj)->value));
      break;

    case SLY_TYPE_VECTOR:
      for(i = 0; i < SLY_VECTOR(gcobj)->size; i++) {
	copy_object(S, &(SLY_VECTOR(gcobj)->data[i]));
      }
      break;

    case SLY_TYPE_DYN_BIND:
      copy_object(S, &(SLY_DYN_BIND(gcobj)->tag));
      copy_object(S, &(SLY_DYN_BIND(gcobj)->value));
      break;
    }

    scan += size;
  }

  /* swap spaces */
  scan = S->from_space;
  S->from_space = S->to_space;
  S->to_space = scan;

  /* process objects that need finalisation */
  collect_fobjs(S);

#ifdef SLY_DTRACE
  SLY_GC_END(old_size, S->size);
#endif
}

static int expand_store(sly_store_t* S)
{
  void *tmp;
  uint32_t old_size, size;

  old_size = S->capacity;

  /* new size is 30% larger, word aligned */
  size = old_size * 4 / 3;
  size -= size % WSIZE;

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

int sly_gc_init(sly_store_t *S, sly_roots_cb_t cb, void* ud)
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

  S->fobjs = NULL;

  S->roots_cb = cb;
  S->roots_cb_data = ud;

  return 1;
}

void sly_gc_finish(sly_store_t *S)
{
  sly_fobj_t *fobj;

  for(fobj = S->fobjs; fobj;) {
    sly_fobj_t *t;

    t = fobj->next;
    if(fobj->obj->type == SLY_TYPE_INPUT_PORT ||
       fobj->obj->type == SLY_TYPE_OUTPUT_PORT) {
      sly_port_t *port;

      port = SLY_PORT(fobj->obj);
      port->finish(port);
    }
    free(fobj);
    fobj = t;
  }

  free(S->os_address);
  S->size = S->capacity = 0;
  S->from_space = S->to_space = NULL;
  S->roots_cb = S->roots_cb_data = NULL;
}

void* sly_gc_alloc(sly_store_t *S, uint32_t size)
{
  void *ret;

  /* allocating only word-multiple sized blocks */
  assert(size % WSIZE == 0);

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

  /* returned address must be word aligned */
  assert((uintptr_t)ret % WSIZE == 0);

  return ret;
}

void sly_gc_add_port(sly_store_t *S, sly_port_t *port)
{
  sly_fobj_t *link;

  if(!port) {
    return;
  }

  link = (sly_fobj_t*)malloc(sizeof(sly_fobj_t));
  if(!link) {
    return;
  }

  link->next = S->fobjs;
  link->obj = SLY_GCOBJECT(port);

  S->fobjs = link;
}
