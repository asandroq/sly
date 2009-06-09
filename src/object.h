/*
 * The Sly Scheme system
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

#ifndef __SLY_OBJECT_H__
#define __SLY_OBJECT_H__

#include "sly.h"

/*
 * data types tags
 */
#define SLY_TYPE_UNDEF            1
#define SLY_TYPE_NIL              2
#define SLY_TYPE_EOF              3
#define SLY_TYPE_BOOL             4
#define SLY_TYPE_FIXNUM           5
#define SLY_TYPE_CHAR             6
#define SLY_TYPE_SYMBOL           7
#define SLY_TYPE_CLOSURE          8
#define SLY_TYPE_PAIR             9
#define SLY_TYPE_CONTI           10
#define SLY_TYPE_BOX             11
#define SLY_TYPE_STRING          12
#define SLY_TYPE_VECTOR          13

#define SLY_SIZE_OF_BOX \
   (sizeof(sly_box_t))
#define SLY_SIZE_OF_PAIR \
   (sizeof(sly_pair_t))
#define SLY_SIZE_OF_CLOSURE(n) \
   (sizeof(sly_closure_t) + (n) * sizeof(sly_object_t))
#define SLY_SIZE_OF_CONTI(n) \
   (sizeof(sly_conti_t) + (n) * sizeof(sly_object_t))
#define SLY_SIZE_OF_STRING(n) \
   (sizeof(sly_string_t) + (n) * sizeof(sly_char_t))
#define SLY_SIZE_OF_VECTOR(n) \
   (sizeof(sly_vector_t) + (n) * sizeof(sly_object_t))

/* forward type declarations */
typedef struct sly_object_t sly_object_t;
typedef struct sly_gcobject_t sly_gcobject_t;

typedef uint32_t sly_char_t;
typedef struct sly_box_t sly_box_t;
typedef struct sly_closure_t sly_closure_t;
typedef struct sly_pair_t sly_pair_t;
typedef struct sly_conti_t sly_conti_t;
typedef struct sly_string_t sly_string_t;
typedef struct sly_vector_t sly_vector_t;

typedef struct sly_symbol_t sly_symbol_t;

/* casts */
#define SLY_GCOBJECT(obj) ((sly_gcobject_t*)(obj))
#define SLY_BOX(obj)      ((sly_box_t*)(obj))
#define SLY_CLOSURE(obj)  ((sly_closure_t*)(obj))
#define SLY_PAIR(obj)     ((sly_pair_t*)(obj))
#define SLY_CONTI(obj)    ((sly_conti_t*)(obj))
#define SLY_STRING(obj)   ((sly_string_t*)(obj))
#define SLY_VECTOR(obj)   ((sly_vector_t*)(obj))

/* value types */
struct sly_object_t {

  /* the runtime type tag */
  uint8_t type;

  /* the value of this object */
  union {
    /* immediates */
    uint8_t bool;
    sly_char_t chr;
    sly_fixnum_t fixnum;
    sly_symbol_t *symbol;

    /* collectable objects */
    sly_gcobject_t *gc;
  } value;
};

struct sly_gcobject_t {
  uint32_t type;
};

struct sly_box_t {
  sly_gcobject_t base;
  sly_object_t value;
};

struct sly_closure_t {
  sly_gcobject_t base;
  uint8_t is_c;
  union {
    uint32_t scm;
    sly_cfunction_t c;
  } entry_point;
  uint32_t nr_free;
  sly_object_t free_vars[0];
};

struct sly_pair_t {
  sly_gcobject_t base;
  sly_object_t car;
  sly_object_t cdr;  
};

struct sly_conti_t {
  sly_gcobject_t base;
  uint32_t size;
  sly_object_t stack[0];
};

struct sly_string_t {
  sly_gcobject_t base;
  uint32_t size;
  sly_char_t chars[0];
};

struct sly_vector_t {
  sly_gcobject_t base;
  uint32_t size;
  sly_object_t data[0];
};

/*
 * entry in the symbol table
 * symbols are not collected
 * some hash functions:
 * http://burtleburtle.net/bob/c/lookup3.c
 */
struct sly_symbol_t {
  /* symbol textual representation */
  sly_string_t *str;

  /* next symbol in chain */
  sly_symbol_t *next;
};

/*
 * object creation
 */


sly_gcobject_t *sly_create_box(sly_state_t *S);
sly_gcobject_t *sly_create_sclosure(sly_state_t *S, uint32_t entry, uint32_t nr_vars);
sly_gcobject_t *sly_create_cclosure(sly_state_t *S, sly_cfunction_t func, uint32_t nr_vars);
sly_gcobject_t *sly_create_pair(sly_state_t *S);
sly_gcobject_t *sly_create_conti(sly_state_t *S, uint32_t stack_size);
sly_gcobject_t *sly_create_string(sly_state_t *S, const char* str, uint32_t size);
sly_gcobject_t *sly_create_vector(sly_state_t *S, uint32_t size);

sly_object_t sly_create_symbol(sly_state_t* S, sly_string_t* str);

#endif
