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
#define SLY_TYPE_UNDEF                 1
#define SLY_TYPE_NIL                   2
#define SLY_TYPE_BOOL                  3
#define SLY_TYPE_FIXNUM                4
#define SLY_TYPE_CHAR                  5
#define SLY_TYPE_SYMBOL                6
#define SLY_TYPE_CLOSURE               7
#define SLY_TYPE_PAIR                  8
#define SLY_TYPE_CONTINUATION          9
#define SLY_TYPE_BOX                  10
#define SLY_TYPE_STRING               11
#define SLY_TYPE_VECTOR               12

#define SLY_SIZE_OF_BOX \
   (sizeof(sly_Box))
#define SLY_SIZE_OF_PAIR \
   (sizeof(sly_Pair))
#define SLY_SIZE_OF_CLOSURE(n) \
   (sizeof(sly_Closure) + (n) * sizeof(sly_Object))
#define SLY_SIZE_OF_CONTINUATION(n) \
   (sizeof(sly_Continuation) + (n) * sizeof(sly_Object))
#define SLY_SIZE_OF_STRING(n) \
   (sizeof(sly_String) + (n) * sizeof(sly_Char))
#define SLY_SIZE_OF_VECTOR(n) \
   (sizeof(sly_Vector) + (n) * sizeof(sly_Object))

/* forward type declarations */
typedef struct sly_Object sly_Object;
typedef struct sly_GCObject sly_GCObject;

typedef uint32_t sly_Char;
typedef struct sly_Box sly_Box;
typedef struct sly_Closure sly_Closure;
typedef struct sly_Pair sly_Pair;
typedef struct sly_Continuation sly_Continuation;
typedef struct sly_String sly_String;
typedef struct sly_Vector sly_Vector;

typedef struct sly_Symbol sly_Symbol;

/* value types */
struct sly_Object {

  /* the runtime type tag */
  uint8_t type;

  /* the value of this object */
  union {
    /* immediates */
    uint8_t bool;
    sly_Char chr;
    uint32_t fixnum;
    sly_Symbol *symbol;

    /* collectable objects */
    sly_GCObject *gc;
  } value;
};

#define SLY_GC_BASE uint32_t type

struct sly_GCObject {
  SLY_GC_BASE;
};

struct sly_Box {
  SLY_GC_BASE;
  sly_Object value;
};

struct sly_Closure {
  SLY_GC_BASE;
  uint32_t entry_point;
  uint32_t nr_free;
  sly_Object free_vars[0];
};

struct sly_Pair {
  SLY_GC_BASE;
  sly_Object car;
  sly_Object cdr;  
};

struct sly_Continuation {
  SLY_GC_BASE;
  uint32_t size;
  sly_Object stack[0];
};

struct sly_String {
  SLY_GC_BASE;
  uint32_t size;
  sly_Char chars[0];
};

struct sly_Vector {
  SLY_GC_BASE;
  uint32_t size;
  sly_Object data[0];
};

/*
 * entry in the symbol table
 * symbols are not collected
 * some hash functions:
 * http://burtleburtle.net/bob/c/lookup3.c
 */
struct sly_Symbol {
  /* symbol textual representation */
  sly_String *str;

  /* next symbol in chain */
  sly_Symbol *next;
};

#endif
