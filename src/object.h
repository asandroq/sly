/*
 * The Sly Scheme system
 * Copyright (c) 2009, 2010 Alex Queiroz <asandroq@gmail.com>
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

#include <stdio.h>

#include "sly.h"

/*
 * data types tags
 */
#define SLY_TYPE_VOID             1
#define SLY_TYPE_UNDEF            2
#define SLY_TYPE_NIL              3
#define SLY_TYPE_EOF              4
#define SLY_TYPE_BOOL             5
#define SLY_TYPE_FIXNUM           6
#define SLY_TYPE_CHAR             7
#define SLY_TYPE_SYMBOL           8
#define SLY_TYPE_CLOSURE          9
#define SLY_TYPE_PAIR            10
#define SLY_TYPE_CONTI           11
#define SLY_TYPE_BOX             12
#define SLY_TYPE_STRING          13
#define SLY_TYPE_VECTOR          14
#define SLY_TYPE_DYN_BIND        15
#define SLY_TYPE_SYNCLO          16
#define SLY_TYPE_INPUT_PORT      17
#define SLY_TYPE_OUTPUT_PORT     18

/* sizes for GC */
#define SLY_SIZE_OF_BOX                         \
  (sizeof(sly_box_t))
#define SLY_SIZE_OF_PAIR                        \
  (sizeof(sly_pair_t))
#define SLY_SIZE_OF_CLOSURE(n)                          \
  (sizeof(sly_closure_t) + (n) * sizeof(sly_object_t))
#define SLY_SIZE_OF_CONTI(n)                            \
  (sizeof(sly_conti_t) + (n) * sizeof(sly_object_t))
#if defined(__LP64__) || defined(__LLP64__)
#define SLY_SIZE_OF_STRING(n)                           \
  (sizeof(sly_string_t) + (n % 2 ? n+1 : n) * sizeof(sly_char_t))
#else
#define SLY_SIZE_OF_STRING(n)                           \
  (sizeof(sly_string_t) + (n) * sizeof(sly_char_t))
#endif
#define SLY_SIZE_OF_VECTOR(n)                           \
  (sizeof(sly_vector_t) + (n) * sizeof(sly_object_t))
#define SLY_SIZE_OF_DYN_BIND                    \
  (sizeof(sly_dyn_bind_t))
#define SLY_SIZE_OF_SYNCLO                      \
  (sizeof(sly_syn_closure_t))
#define SLY_SIZE_OF_IPORT                       \
  (sizeof(sly_iport_t))
#define SLY_SIZE_OF_OPORT                       \
  (sizeof(sly_oport_t))

/* port types */
#define SLY_TYPE_PORT_FILE          1

/* port buffer size */
#define SLY_PORT_BUF_SIZE         512

/* file descriptors */
#ifdef _WIN32
typedef HANDLE sly_file_t;
#else
typedef int sly_file_t;
#endif

/* forward type declarations */
typedef struct sly_object_t sly_object_t;
typedef struct sly_gcobject_t sly_gcobject_t;

typedef struct sly_box_t sly_box_t;
typedef struct sly_closure_t sly_closure_t;
typedef struct sly_pair_t sly_pair_t;
typedef struct sly_conti_t sly_conti_t;
typedef struct sly_string_t sly_string_t;
typedef struct sly_vector_t sly_vector_t;
typedef struct sly_dyn_bind_t sly_dyn_bind_t;
typedef struct sly_syn_closure_t sly_syn_closure_t;

typedef struct sly_symbol_t sly_symbol_t;

typedef struct sly_port_t      sly_port_t;
typedef struct sly_iport_t     sly_iport_t;
typedef struct sly_oport_t     sly_oport_t;

/* casts */
#define SLY_GCOBJECT(obj)      ((sly_gcobject_t*)(obj))
#define SLY_BOX(obj)           ((sly_box_t*)(obj))
#define SLY_CLOSURE(obj)       ((sly_closure_t*)(obj))
#define SLY_PAIR(obj)          ((sly_pair_t*)(obj))
#define SLY_CONTI(obj)         ((sly_conti_t*)(obj))
#define SLY_STRING(obj)        ((sly_string_t*)(obj))
#define SLY_VECTOR(obj)        ((sly_vector_t*)(obj))
#define SLY_DYN_BIND(obj)      ((sly_dyn_bind_t*)(obj))
#define SLY_SYNCLO(obj)        ((sly_syn_closure_t*)(obj))
#define SLY_PORT(obj)          ((sly_port_t*)(obj))
#define SLY_IPORT(obj)         ((sly_iport_t*)(obj))
#define SLY_OPORT(obj)         ((sly_oport_t*)(obj))

#define SLY_OBJ_EQ(o1, o2)                                              \
  (((o1).type == (o2).type) && ((o1).value.symbol == (o2).value.symbol))

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
  size_t size;
  sly_object_t stack[0];
};

struct sly_string_t {
  sly_gcobject_t base;
  size_t size;
  sly_char_t chars[0];
};

struct sly_vector_t {
  sly_gcobject_t base;
  size_t size;
  sly_object_t data[0];
};

struct sly_dyn_bind_t {
  sly_gcobject_t base;
  sly_object_t tag;
  sly_object_t value;
};

struct sly_syn_closure_t {
  sly_gcobject_t base;
  sly_object_t env;
  sly_object_t free;
  sly_object_t exp;
};

/* I/O port "classes" */
struct sly_port_t {
  sly_gcobject_t base;

  /* port type */
  uint8_t type;

  /* character encoding of this port */
  uint8_t char_enc;

  /* finalisation function */
  int (*finish)(sly_port_t *self);

  /* private port data */
  void *private;
};

struct sly_iport_t {
  sly_port_t base;

  int (*peek_char)(sly_iport_t *self, sly_char_t *c);
  int (*read_char)(sly_iport_t *self, sly_char_t *c);
};

struct sly_oport_t {
  sly_port_t base;

  int (*flush)(sly_oport_t* self);
  int (*write_char)(sly_oport_t* self, sly_char_t c);
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
sly_gcobject_t *sly_create_string(sly_state_t *S, const sly_char_t* str, uint32_t size);
sly_gcobject_t *sly_create_string_from_ascii(sly_state_t *S, const char* str);
sly_gcobject_t *sly_create_vector(sly_state_t *S, uint32_t size);
sly_gcobject_t *sly_create_dyn_bind(sly_state_t *S);
sly_gcobject_t *sly_create_syn_closure(sly_state_t *S);
sly_gcobject_t *sly_create_iport(sly_state_t *S);
sly_gcobject_t *sly_create_oport(sly_state_t *S);

sly_object_t sly_create_symbol(sly_state_t* S, sly_string_t* str);
sly_object_t sly_create_symbol_from_ascii(sly_state_t* S, const char* name);

#endif
