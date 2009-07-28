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

/* #include <stdio.h> */
#include <stdlib.h>
#include <string.h>
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

  case SLY_TYPE_INPUT_PORT:
    size = SLY_SIZE_OF_IPORT;
    break;

  case SLY_TYPE_OUTPUT_PORT:
    size = SLY_SIZE_OF_OPORT;
    break;

  default:
    size = 0;
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
      fobj = fobj->next;
      prev = fobj;
    } else {
      /* if object was not copied then it's dead */
      if(prev) {
        prev->next = fobj->next;
      } else {
        S->fobjs = fobj->next;
      }
      if(fobj->obj->type == SLY_TYPE_INPUT_PORT ||
         fobj->obj->type == SLY_TYPE_OUTPUT_PORT) {
        sly_port_t *port;

        port = SLY_PORT(fobj->obj);
        port->finish(port);
      }
      free(fobj);
      fobj = prev->next;
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
  sly_root_t *root;
  sly_object_t *obj;
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

  /* extra roots */
  for(root = S->roots; root; root = root->next) {
    copy_object(S, root->obj);
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
    }

    scan += size;
  }

  /* swap spaces */
  scan = S->from_space;
  S->from_space = S->to_space;
  S->to_space = scan;

  /* process objects that need finalisation */
  collect_fobjs(S);

  /*fprintf(stderr, "GC before: %d after: %d\n\n", old_size, S->size);*/
}

static int expand_store(sly_store_t* S)
{
  void *tmp;
  uint32_t old_size, size;

  old_size = S->capacity;

  /* new size is 30% larger, multiple of 4 */
  size = old_size * 4 / 3;
  size -= size % 4;

  /*fprintf(stderr, "Expanding store from %d to %d\n\n", old_size, size);*/

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

  S->roots = NULL;
  S->fobjs = NULL;

  S->roots_cb = cb;
  S->roots_cb_data = ud;

  return 1;
}

void sly_gc_finish(sly_store_t *S)
{
  sly_root_t *root;
  sly_fobj_t *fobj;

  free(S->os_address);
  S->size = S->capacity = 0;
  S->from_space = S->to_space = NULL;
  S->roots_cb = S->roots_cb_data = NULL;

  for(root = S->roots; root;) {
    sly_root_t *t;

    t = root->next;
    free(root);
    root = t;
  }

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
}

void* sly_gc_alloc(sly_store_t *S, uint32_t size)
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

void sly_gc_protect(sly_store_t *S, sly_object_t *obj)
{
  sly_root_t *link;

  link = (sly_root_t*)malloc(sizeof(sly_root_t));
  if(!link) {
    return;
  }

  link->obj = obj;
  link->next = S->roots;

  S->roots = link;
}

void sly_gc_release(sly_store_t *S, sly_object_t *obj)
{
  sly_root_t *prev, *root;

  for(prev = NULL, root = S->roots; root; root = root->next) {
    if(root->obj == obj) {
      if(prev) {
        prev->next = root->next;
      } else {
        S->roots = root->next;
      }
      free(root);
      break;
    }
    prev = root;
  }
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

