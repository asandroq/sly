/*
 * The Sly Scheme compiler
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
#include <string.h>
#include <assert.h>

#if __GNUC__ > 2
#include <stdint.h>
#else
#include <inttypes.h>
#endif

/*
 * Sly bytecode is an array of 32-bit values for
 * efficiency (data alignment), but wasteful because
 * most instructions do not have operands. It is
 * therefore hostile for embedded systems but optimised
 * for 32-bit machines with large RAMs. It is gonna
 * be like this for now for simplicity, but the
 * situation may improve in the future.
 *
 * Further reading:
 * http://compilers.iecc.com/comparch/article/04-12-156
 *
 * There are two types of instructions:
 * Type A: First byte is operand, the rest is for
 *         alignment only.
 * Type B: First byte is operand, remaining 3 bytes
 *         are used for single operand.
 */

/* instructions without operands */
#define SLY_OP_LOAD_NIL                1
#define SLY_OP_LOAD_FALSE              2
#define SLY_OP_LOAD_TRUE               3
#define SLY_OP_LOAD_UNDEF              4
#define SLY_OP_LOAD_ZERO               5
#define SLY_OP_LOAD_ONE                6
#define SLY_OP_PUSH                    7
#define SLY_OP_LOAD_0                  8
#define SLY_OP_LOAD_1                  9
#define SLY_OP_LOAD_2                 10
#define SLY_OP_LOAD_3                 11
#define SLY_OP_CALL                   12
#define SLY_OP_RETURN                 13
#define SLY_OP_SAVE_CONT              14
#define SLY_OP_REST_CONT              15
#define SLY_OP_BOX                    16
#define SLY_OP_OPEN_BOX               17
#define SLY_OP_TAIL_CALL              18
#define SLY_OP_HALT                   19
#define SLY_OP_ABORT                  20

#define SLY_OP_NULL_P                 40
#define SLY_OP_BOOL_P                 41
#define SLY_OP_CHAR_P                 42
#define SLY_OP_FIXNUM_P               43
#define SLY_OP_PAIR_P                 44

/* primitives optimised as instructions */
#define SLY_OP_INC                    60
#define SLY_OP_DEC                    61
#define SLY_OP_FIXNUM_TO_CHAR         62
#define SLY_OP_CHAR_TO_FIXNUM         63
#define SLY_OP_ZERO_P                 64
#define SLY_OP_NOT                    65
#define SLY_OP_PLUS                   66
#define SLY_OP_MINUS                  67
#define SLY_OP_MULT                   68
#define SLY_OP_CONS                   69
#define SLY_OP_CAR                    70
#define SLY_OP_CDR                    71
#define SLY_OP_NUM_EQ                 72
#define SLY_OP_EQ                     73
#define SLY_OP_EQV                    74
#define SLY_OP_MAKE_STRING            75
#define SLY_OP_STRING_SET             76
#define SLY_OP_STRING_TO_SYMBOL       77
#define SLY_OP_MAKE_VECTOR            78
#define SLY_OP_VECTOR_SET             79
#define SLY_OP_WRITE                  80
#define SLY_OP_DEBUG                  81

/* instructions with one operand */
#define SLY_OP_LOAD_FIXNUM           120
#define SLY_OP_LOAD_CHAR             121
#define SLY_OP_LOAD                  122
#define SLY_OP_MAKE_CLOSURE          123
#define SLY_OP_JMP_IF                124
#define SLY_OP_JMP                   125
#define SLY_OP_LOAD_FREE             126
#define SLY_OP_ASSIGN                127
#define SLY_OP_ASSIGN_FREE           128
#define SLY_OP_FRAME                 129
#define SLY_OP_LOAD_LOCAL            130
#define SLY_OP_INSERT_BOX            131
#define SLY_OP_ASSIGN_LOCAL          132
#define SLY_OP_POP                   133
#define SLY_OP_GLOBAL_REF            134
#define SLY_OP_CHECKED_GLOBAL_REF    135
#define SLY_OP_GLOBAL_SET            136
#define SLY_OP_CHECKED_GLOBAL_SET    137
#define SLY_OP_CONST                 138
#define SLY_OP_CONST_INIT            139
#define SLY_OP_ARITY_EQ              140
#define SLY_OP_ARITY_GE              141
#define SLY_OP_LISTIFY               142

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

#define IS_TYPE_B(instr) ((instr) > 119)

#define EXTRACT_OP(instr)   ((uint8_t)((instr) & 0x000000ff))
#define EXTRACT_ARG(instr)  ((uint32_t)((instr) >> 8))

/* debugging information */
struct  opcode_ {
  uint8_t op;
  const char *name;
};

typedef struct opcode_ opcode_t;

static opcode_t global_opcodes[] = {
  {SLY_OP_LOAD_NIL,          "LOAD-NIL"},
  {SLY_OP_LOAD_FALSE,        "LOAD-FALSE"},
  {SLY_OP_LOAD_TRUE,         "LOAD-TRUE"},
  {SLY_OP_LOAD_ZERO,         "LOAD-ZERO"},
  {SLY_OP_LOAD_ONE,          "LOAD-ONE"},
  {SLY_OP_LOAD_FIXNUM,       "LOAD-FIXNUM"},
  {SLY_OP_LOAD_CHAR,         "LOAD-CHAR"},
  {SLY_OP_PUSH,              "PUSH"},
  {SLY_OP_LOAD_0,            "LOAD0"},
  {SLY_OP_LOAD_1,            "LOAD1"},
  {SLY_OP_LOAD_2,            "LOAD2"},
  {SLY_OP_LOAD_3,            "LOAD3"},
  {SLY_OP_LOAD,              "LOAD"},
  {SLY_OP_MAKE_CLOSURE,      "MAKE-CLOSURE"},
  {SLY_OP_CALL,              "CALL"},
  {SLY_OP_RETURN,            "RETURN"},
  {SLY_OP_JMP_IF,            "JMP-IF"},
  {SLY_OP_JMP,               "JMP"},
  {SLY_OP_LOAD_FREE,         "LOAD-FREE"},
  {SLY_OP_SAVE_CONT,         "SAVE-CONT"},
  {SLY_OP_REST_CONT,         "REST-CONT"},
  {SLY_OP_ASSIGN,            "ASSIGN"},
  {SLY_OP_ASSIGN_FREE,       "ASSIGN-FREE"},
  {SLY_OP_BOX,               "BOX"},
  {SLY_OP_OPEN_BOX,          "OPEN-BOX"},
  {SLY_OP_FRAME,             "FRAME"},
  {SLY_OP_TAIL_CALL,         "TAIL-CALL"},
  {SLY_OP_HALT,              "HALT"},
  {SLY_OP_LOAD_LOCAL,        "LOAD-LOCAL"},
  {SLY_OP_INSERT_BOX,        "INSERT-BOX"},
  {SLY_OP_ASSIGN_LOCAL,      "ASSIGN-LOCAL"},
  {SLY_OP_POP,               "POP"},
  {SLY_OP_GLOBAL_REF,             "GLOBAL-REF"},
  {SLY_OP_CHECKED_GLOBAL_REF,     "CHECKED-GLOBAL-REF"},
  {SLY_OP_GLOBAL_SET,             "GLOBAL-SET"},
  {SLY_OP_CHECKED_GLOBAL_SET,     "CHECKED-GLOBAL-SET"},
  {SLY_OP_LOAD_UNDEF,             "LOAD-UNDEF"},
  {SLY_OP_CONST,                  "CONST"},
  {SLY_OP_CONST_INIT,             "CONST-INIT"},
  {SLY_OP_ARITY_EQ,               "ARITY="},
  {SLY_OP_ARITY_GE,               "ARITY>="},
  {SLY_OP_LISTIFY,                "LISTIFY"},
  {SLY_OP_ABORT,                  "ABORT"},
  {SLY_OP_NULL_P,                 "NULL?"},
  {SLY_OP_BOOL_P,                 "BOOL?"},
  {SLY_OP_CHAR_P,                 "CHAR?"},
  {SLY_OP_FIXNUM_P,               "FIXNUM?"},
  {SLY_OP_PAIR_P,                 "PAIR?"},
  {SLY_OP_INC,                    "INC"},
  {SLY_OP_DEC,                    "DEC"},
  {SLY_OP_FIXNUM_TO_CHAR,         "FIXNUM->CHAR"},
  {SLY_OP_CHAR_TO_FIXNUM,         "CHAR->FIXNUM"},
  {SLY_OP_ZERO_P,                 "ZERO?"},
  {SLY_OP_NOT,                    "NOT"},
  {SLY_OP_PLUS,                   "PLUS"},
  {SLY_OP_MINUS,                  "MINUS"},
  {SLY_OP_MULT,                   "MULT"},
  {SLY_OP_CONS,                   "CONS"},
  {SLY_OP_CAR,                    "CAR"},
  {SLY_OP_CDR,                    "CDR"},
  {SLY_OP_NUM_EQ,                 "NUM-EQ"},
  {SLY_OP_EQ,                     "EQ?"},
  {SLY_OP_EQV,                    "EQV?"},
  {SLY_OP_MAKE_STRING,            "MAKE-STRING"},
  {SLY_OP_STRING_SET,             "STRING-SET"},
  {SLY_OP_STRING_TO_SYMBOL,       "STRING->SYMBOL"},
  {SLY_OP_MAKE_VECTOR,            "MAKE-VECTOR"},
  {SLY_OP_VECTOR_SET,             "VECTOR-SET"},
  {SLY_OP_WRITE,                  "WRITE"},
  {SLY_OP_DEBUG,                  "DEBUG"},
  {0, NULL}
};

/*
 * Data types
 */

/* forward type declarations */
typedef struct sly_Object_ sly_Object;
typedef struct sly_GCObject_ sly_GCObject;

typedef uint32_t sly_Char;
typedef struct sly_Box_ sly_Box;
typedef struct sly_Closure_ sly_Closure;
typedef struct sly_Pair_ sly_Pair;
typedef struct sly_Continuation_ sly_Continuation;
typedef struct sly_String_ sly_String;
typedef struct sly_Vector_ sly_Vector;

typedef struct sly_Symbol_ sly_Symbol;

/* value types */
struct sly_Object_ {

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

struct sly_GCObject_ {
  SLY_GC_BASE;
};

struct sly_Box_ {
  SLY_GC_BASE;
  sly_Object value;
};

struct sly_Closure_ {
  SLY_GC_BASE;
  uint32_t entry_point;
  uint32_t nr_free;
  sly_Object free_vars[0];
};

struct sly_Pair_ {
  SLY_GC_BASE;
  sly_Object car;
  sly_Object cdr;  
};

struct sly_Continuation_ {
  SLY_GC_BASE;
  uint32_t size;
  sly_Object stack[0];
};

struct sly_String_ {
  SLY_GC_BASE;
  uint32_t size;
  sly_Char chars[0];
};

struct sly_Vector_ {
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
struct sly_Symbol_ {
  /* symbol textual representation */
  sly_String *str;

  /* next symbol in chain */
  sly_Symbol *next;
};

/*
 * the store, i.e., memory
 */

#define SLY_INITIAL_SPACE_SIZE     ((uint32_t)(1 << 7))
#define SLY_IMMEDIATE_P(o)         ((o)->type < SLY_TYPE_CLOSURE)
#define SLY_FORWARD_TAG            199

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

/*
 * this callback is called by the garbage collector
 * to get all the mutator roots. It must return NULL
 * when there are no more roots to scan
 */
typedef sly_Object* (*sly_Roots_Callback)(void*);

/* forward reference to object in to-space */
typedef struct sly_Forward_Ref_ sly_Forward_Ref;

struct sly_Forward_Ref_ {
  SLY_GC_BASE;
  sly_GCObject *ref;
};

typedef struct sly_Store_ sly_Store;

struct sly_Store_ {
  
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

static int init_store(sly_Store *S, sly_Roots_Callback cb, void* ud)
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

static void finish_store(sly_Store *S)
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

static sly_Box *alloc_box(sly_Store *S)
{
  sly_Box *ret;

  ret = (sly_Box*)alloc_from_store(S, SLY_SIZE_OF_BOX);
  if(ret) {
    ret->type = SLY_TYPE_BOX;
  }

  return ret;
}

static sly_Closure *alloc_closure(sly_Store *S, uint32_t nr_vars)
{
  sly_Closure *ret;

  ret = (sly_Closure*) alloc_from_store(S, SLY_SIZE_OF_CLOSURE(nr_vars));
  if(ret) {
    ret->type = SLY_TYPE_CLOSURE;
    ret->nr_free = nr_vars;
  }

  return ret;
}

static sly_Pair *alloc_pair(sly_Store *S)
{
  sly_Pair *ret;

  ret = (sly_Pair*) alloc_from_store(S, SLY_SIZE_OF_PAIR);
  if(ret) {
    ret->type = SLY_TYPE_PAIR;
  }

  return ret;
}

static sly_Continuation *alloc_continuation(sly_Store *S, uint32_t stack_size)
{
  sly_Continuation *ret;

  ret = (sly_Continuation*)alloc_from_store(S, SLY_SIZE_OF_CONTINUATION(stack_size));
  if(ret) {
    ret->type = SLY_TYPE_CONTINUATION;
    ret->size = stack_size;
  }

  return ret;
}

static sly_String *alloc_string(sly_Store *S, uint32_t size)
{
  sly_String *ret;

  ret = (sly_String*)alloc_from_store(S, SLY_SIZE_OF_STRING(size));

  if(ret) {
    ret->type = SLY_TYPE_STRING;
    ret->size = size;
  }

  return ret;
}

static sly_Vector* alloc_vector(sly_Store *S, uint32_t size)
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

/*
 * the virtual machine
 */

typedef struct sly_Env_ sly_Env;
typedef struct sly_Env_Var_ sly_Env_Var;

/* an entry in an environment */
struct sly_Env_Var_ {

  /* entry in the symbol table */
  sly_Symbol *symbol;

  /* the actual value */
  sly_Object value;
};

/* a global environment */
struct sly_Env_ {
  uint32_t size;
  sly_Env_Var *vars;
};

/* the state of the Sly interpreter */
typedef struct sly_State_ sly_State;

struct sly_State_ {

  /* the size of the bytecode vector used */
  uint32_t code_size;

  /* stack allocated size */
  uint32_t stack_size;

  /* number of constants */
  uint32_t nr_consts;

  /* where is the top of the stack */
  uint32_t sp;

  /* the frame pointer */
  uint32_t fp;

  /* the program counter */
  uint32_t pc;

  /* accumulator register */
  sly_Object accum;

  /* the current procedure */
  sly_Object proc;

  /* global environment */
  sly_Env global_env;

  /* the bytecode to be interpreted */
  uint32_t *code;

  /* the machine stack */
  sly_Object *stack;

  /* constants */
  sly_Object *consts;

  /* symbol table */
  sly_Symbol *symbol_table;

  /* VM memory */
  sly_Store store;
};

/* garbage collector callback */

struct gc_data {
  sly_State *D;
  uint32_t state, count;
};

static struct gc_data gc_data;

static sly_Object* gc_callback(void *ud)
{
  struct gc_data *gc_data;

  gc_data = (struct gc_data*) ud;

  for(;;) {
    if(gc_data->state == 0) {
      /* stack */
      if(gc_data->count == gc_data->D->sp) {
	gc_data->state++;
	gc_data->count = 0;
	continue;
      } else {
	return &gc_data->D->stack[gc_data->count++];
      }
    } else if(gc_data->state == 1) {
      /* globals */
      if(gc_data->count == gc_data->D->global_env.size) {
	gc_data->state++;
	gc_data->count = 0;
	continue;
      } else {
	return &gc_data->D->global_env.vars[gc_data->count++].value;
      }
    } else if(gc_data->state == 2) {
      /* constants */
      if(gc_data->count == gc_data->D->nr_consts) {
	gc_data->state++;
	gc_data->count = 0;
	continue;
      } else {
	return &gc_data->D->consts[gc_data->count++];
      }
    } else if(gc_data->state == 3) {
      /* registers */
      if(gc_data->count == 0) {
	gc_data->count++;
	return &gc_data->D->accum;
      } else if(gc_data->count == 1) {
	gc_data->count++;
	return &gc_data->D->proc;
      } else {
	gc_data->state++;
	gc_data->count = 0;
	continue;
      }
    } else {
      gc_data->state = 0;
      gc_data->count = 0;
      return NULL;
    }
  }
}

sly_State* sly_init(void)
{
  sly_State *D = NULL;

  D = (sly_State*)malloc(sizeof(sly_State));
  if(!D) {
    return NULL;
  }

  /* store */
  gc_data.D = D;
  gc_data.state = 0;
  gc_data.count = 0;
  if(init_store(&D->store, gc_callback, &gc_data) == 0) {
    return NULL;
  }

  D->pc = 0;
  D->fp = 0;

  /* code */
  D->code_size = 1;
  D->code = (uint32_t*)malloc(sizeof(uint32_t));
  if(D->code == NULL) {
    free(D);
    return NULL;
  }
  /* instruction to halt execution always at address 0 */
  D->code[0] = (uint32_t) SLY_OP_HALT;

  /* stack */
  D->sp = 0;
  D->stack = (sly_Object*)malloc(sizeof(sly_Object) * 1024);
  if(D->stack) {
    D->stack_size = 1024;
  } else {
    free(D->code);
    free(D);
    return NULL;
  }

  D->symbol_table = NULL;

  D->consts = NULL;
  D->nr_consts = 0;

  /* globals */
  D->global_env.size = 0;
  D->global_env.vars = NULL;

  /* registers */
  D->proc.type = SLY_TYPE_UNDEF;
  D->accum.type = SLY_TYPE_UNDEF;

  return D;
}

void sly_close(sly_State* D)
{
  if(D) {
    finish_store(&D->store);

    if(D->code) {
      free(D->code);
    }

    if(D->stack) {
      free(D->stack);
    }

    if(D->symbol_table) {
      sly_Symbol *sym;
      for(sym = D->symbol_table; sym != NULL;) {
	sly_Symbol *tmp = sym->next;
	free(sym->str);
	free(sym);
	sym = tmp;
      }
    }

    free(D);
  }
}

static int string_equal_p(sly_String *s1, sly_String *s2)
{
  if(s1->size != s2->size) {
    return 0;
  }

  return memcmp(s1->chars, s2->chars, s1->size * sizeof(sly_Char)) == 0;
}

static sly_String* string_copy(sly_State* D, sly_String* s)
{
  sly_String *ret;

  ret = alloc_string(&D->store, s->size);
  memcpy(ret->chars, s->chars, s->size * sizeof(sly_Char));

  return ret;
}

static sly_String* string_copy_extern(sly_String* s)
{
  uint32_t size;
  sly_String *ret;

  size = SLY_SIZE_OF_STRING(s->size);

  ret = (sly_String*)malloc(size);
  /* TODO: test and throw error */
  memcpy(ret, s, size);

  return ret;
}

static sly_Object sly_make_symbol(sly_State* D, sly_String *str)
{
  sly_Object obj;
  sly_Symbol *tmp;

  /* is the symbol already there? */
  for(tmp = D->symbol_table; tmp != NULL; tmp = tmp->next) {
    if(string_equal_p(tmp->str, str)) {
      break;
    }
  }

  if(tmp == NULL) {
    /* adding new symbol */
    tmp = (sly_Symbol*)malloc(sizeof(sly_Symbol));
    tmp->str = string_copy_extern(str);
    tmp->next = D->symbol_table;
    D->symbol_table = tmp;
  }

  obj.type = SLY_TYPE_SYMBOL;
  obj.value.symbol = tmp;

  return obj;
}

/*
 * writer
 */

static void write_string(sly_String* s, int quote)
{
  uint32_t i, c;

  if(quote) {
    printf("\"");
  }

  for(i = 0; i < s->size; i++) {
    c = s->chars[i];
    printf("%c", c > 31 && c < 128 ? c : '#');
  }

  if(quote) {
    printf("\"");
  }
}

static void write_obj(sly_Object* obj)
{
  uint32_t i;

  switch(obj->type) {

  case SLY_TYPE_UNDEF:
    printf("<#undef>");
    break;

  case SLY_TYPE_NIL:
    printf("()");
    break;

  case SLY_TYPE_BOOL:
    if(obj->value.bool) {
      printf("#t");
    } else {
      printf("#f");
    }
    break;

  case SLY_TYPE_FIXNUM:
    printf("%d", obj->value.fixnum);
    break;

  case SLY_TYPE_CHAR:
    printf("#\\%c", obj->value.chr);
    break;

  case SLY_TYPE_SYMBOL:
    write_string(obj->value.symbol->str, 0);
    break;

  case SLY_TYPE_CLOSURE:
    printf("<#closure %u>", ((sly_Closure*)obj->value.gc)->entry_point);
    break;

  case SLY_TYPE_PAIR:
    printf("(");
    write_obj(&(((sly_Pair*)obj->value.gc)->car));
    printf(" . ");
    write_obj(&(((sly_Pair*)obj->value.gc)->cdr));
    printf(")");
    break;

  case SLY_TYPE_CONTINUATION:
    printf("<#continuation %u>", ((sly_Continuation*)obj->value.gc)->size);
    break;

  case SLY_TYPE_BOX:
    printf("#&");
    write_obj(&(((sly_Box*)obj->value.gc)->value));
    break;

  case SLY_TYPE_STRING:
    write_string((sly_String*)obj->value.gc, 1);
    break;

  case SLY_TYPE_VECTOR:
    printf("#(");
    for(i = 0; i < ((sly_Vector*)obj->value.gc)->size; i++) {
      printf(" ");
      write_obj(((sly_Vector*)obj->value.gc)->data + i);
    }
    printf(")");
    break;

  default:
    printf("Unknown type!");
  }
}

/*
 * debugging
 */

static void dump_instr(uint32_t instr)
{
  uint8_t op;
  opcode_t* dbg;

  op = EXTRACT_OP(instr);
  for(dbg = global_opcodes; dbg->name != NULL; dbg++) {
    if(dbg->op == op) {
      printf("%u\t%s", (uint32_t)op, dbg->name);
      if(IS_TYPE_B(op)) {
	printf(" %u", EXTRACT_ARG(instr));
      }
      break;
    }
  }
}

static void disassemble(sly_State* D)
{
  uint32_t i;

  printf("Code listing:\n");
  for(i = 0; i < D->code_size; i++) {
    printf("\t%u\t", i);
    dump_instr(D->code[i]);
    printf("\n");
  }
}

void sly_dump(sly_State* D)
{
  uint32_t i;

  printf("Instruction: ");
  dump_instr(D->code[D->pc]);
  printf("\n");

  printf("Registers:\n");
  printf("\taccum: "); write_obj(&D->accum); printf("\n");
  printf("\tclosure: "); write_obj(&D->proc); printf("\n");
  printf("\tPC: %d\n", D->pc);
  printf("\tFP: %d\n", D->fp);

  printf("Stack:");
  for(i = 0; i < D->sp; i++) {
    printf(" ");
    write_obj(D->stack + i);
  }
#if 0
  printf("\n\n");

  printf("Globals:");
  for(i = 0; i < D->global_env.size; i++) {
    sly_Env_Var var = D->global_env.vars[i];
    printf(" [");
    if(var.symbol) {
      write_string(var.symbol->str, 0);
    }
    printf(" . ");
    write_obj(&var.value);
    printf("]");
  }
  printf("\n\n");

  printf("Constants:");
  for(i = 0; i < D->nr_consts; i++) {
    printf(" ");
    write_obj(D->consts + i);
  }
#endif
  printf("\n\n");
}

static void sly_abort(sly_State *D)
{
  sly_dump(D);

  sly_close(D);
  abort();
}

static void check_alloc(sly_State *D, void* ptr)
{
  if(ptr == NULL) {
    fprintf(stderr, "sly: Out of memory!\n");
    sly_abort(D);
  }
}

#define SLY_SET_BOOL(cond)		\
  do {					\
    if(cond) {				\
      D->accum.type = SLY_TYPE_BOOL;	\
      D->accum.value.bool = 1;		\
    } else {				\
      D->accum.type = SLY_TYPE_BOOL;	\
      D->accum.value.bool = 0;		\
    }					\
  } while(0)

/*
 * When a call to a procedure is to be
 * performed, the stack must have this
 * configuration:
 *
 *      +=====================+
 *      | number of arguments |
 *      +---------------------+
 *      |        arg 1        |
 *      |          .          |
 *      |          .          |
 *      |          .          |
 *      |        arg N        |
 *      +---------------------+
 *      | saved frame pointer |
 *      +---------------------+
 *      |   saved procedure   !
 *      +---------------------+
 *      |    return address   |
 *      +=====================+
 *      |    previous frame   |
 */
int sly_vm_run(sly_State* D)
{
  int go_on = 1, debug = 0;

  /*disassemble(D);*/

  while(go_on) {
    sly_Object tmp;
    register uint32_t instr;
    uint32_t i, j, dw1, dw2;

    if(debug) {
      sly_dump(D);
      getchar();
    }
    assert(D->pc < D->code_size);

    instr = D->code[D->pc++];

    switch(EXTRACT_OP(instr)) {

    case SLY_OP_LOAD_NIL:
      D->accum.type = SLY_TYPE_NIL;
      break;

    case SLY_OP_LOAD_FALSE:
      D->accum.type = SLY_TYPE_BOOL;
      D->accum.value.bool = 0;
      break;

    case SLY_OP_LOAD_TRUE:
      D->accum.type = SLY_TYPE_BOOL;
      D->accum.value.bool = 1;
      break;

    case SLY_OP_LOAD_ZERO:
      D->accum.type = SLY_TYPE_FIXNUM;
      D->accum.value.fixnum = 0;
      break;

    case SLY_OP_LOAD_ONE:
      D->accum.type = SLY_TYPE_FIXNUM;
      D->accum.value.fixnum = 1;
      break;

    case SLY_OP_LOAD_FIXNUM:
      D->accum.type = SLY_TYPE_FIXNUM;
      D->accum.value.fixnum = EXTRACT_ARG(instr);
      break;

    case SLY_OP_LOAD_CHAR:
      D->accum.type = SLY_TYPE_CHAR;
      D->accum.value.chr = (sly_Char) EXTRACT_ARG(instr);
      break;

    case SLY_OP_INC:
      D->accum.value.fixnum++;
      break;

    case SLY_OP_DEC:
      D->accum.value.fixnum--;
      break;

    case SLY_OP_FIXNUM_TO_CHAR:
      D->accum.type = SLY_TYPE_CHAR;
      D->accum.value.chr = (sly_Char) D->accum.value.fixnum;
      break;

    case SLY_OP_CHAR_TO_FIXNUM:
      D->accum.type = SLY_TYPE_FIXNUM;
      D->accum.value.fixnum = (uint32_t) D->accum.value.chr;
      break;

    case SLY_OP_NULL_P:
      SLY_SET_BOOL(D->accum.type == SLY_TYPE_NIL);
      break;

    case SLY_OP_ZERO_P:
      SLY_SET_BOOL(D->accum.value.fixnum == 0);
      break;

    case SLY_OP_NOT:
      SLY_SET_BOOL(D->accum.type == SLY_TYPE_BOOL &&
		    D->accum.value.bool == 0);
      break;

    case SLY_OP_BOOL_P:
      SLY_SET_BOOL(D->accum.type == SLY_TYPE_BOOL);
      break;

    case SLY_OP_CHAR_P:
      SLY_SET_BOOL(D->accum.type == SLY_TYPE_CHAR);
      break;

    case SLY_OP_FIXNUM_P:
      SLY_SET_BOOL(D->accum.type == SLY_TYPE_FIXNUM);
      break;

    case SLY_OP_PAIR_P:
      SLY_SET_BOOL(D->accum.type == SLY_TYPE_PAIR);
      break;

    case SLY_OP_PUSH:
      D->stack[D->sp++] = D->accum;
      break;

    case SLY_OP_PLUS:
      D->accum.value.fixnum =
	(D->stack[--D->sp]).value.fixnum + D->accum.value.fixnum;
      break;

    case SLY_OP_MINUS:
      D->accum.value.fixnum =
	(D->stack[--D->sp]).value.fixnum - D->accum.value.fixnum;
      break;

    case SLY_OP_MULT:
      D->accum.value.fixnum =
	(D->stack[--D->sp]).value.fixnum * D->accum.value.fixnum;
      break;

    case SLY_OP_LOAD_0:
      D->accum = D->stack[D->fp-1];
      break;

    case SLY_OP_LOAD_1:
      D->accum = D->stack[D->fp-2];
      break;

    case SLY_OP_LOAD_2:
      D->accum = D->stack[D->fp-3];
      break;

    case SLY_OP_LOAD_3:
      D->accum = D->stack[D->fp-4];
      break;

    case SLY_OP_LOAD:
      D->accum = D->stack[D->fp-EXTRACT_ARG(instr)-1];
      break;

    case SLY_OP_MAKE_CLOSURE:
      /* number of free variables */
      dw1 = EXTRACT_ARG(instr);

      tmp.type = SLY_TYPE_CLOSURE;
      tmp.value.gc = (sly_GCObject*) alloc_closure(&D->store, dw1);
      check_alloc(D, tmp.value.gc);
      D->accum = tmp;

      /*
       * There is always a jump after this instruction, to jump over the
       * closure code. So the closure entry point is PC + 1
       */
      ((sly_Closure*)D->accum.value.gc)->entry_point = D->pc + 1;

      /* gathering free variables */
      for(i = 0; i < dw1; i++) {
	((sly_Closure*)D->accum.value.gc)->free_vars[i] = D->stack[D->sp-i-1];
      }
      D->sp -= dw1;
      break;

    case SLY_OP_TAIL_CALL:
      /*
       * the arguments to the callee must be shifted down
       * removing the arguments of the caller
       */
      /* number of arguments newly pushed */
      dw1 = (D->stack[D->sp-1]).value.fixnum;

      /* number of arguments of old procedure */
      dw2 = (D->stack[D->fp]).value.fixnum;

      /* where to copy the new values */
      i = D->fp - dw2;

      /* where the values will be copied from */
      j = D->sp - dw1 - 1;

      D->sp = i + dw1 + 1;
      memcpy(D->stack+i, D->stack+j, (dw1+1) * sizeof(sly_Object));

      /* fall through */

    case SLY_OP_CALL:
      if(D->accum.type != SLY_TYPE_CLOSURE) {
	sly_abort(D);
      }

      /* frame pointer */
      D->fp = D->sp - 1;

      /* setting current procedure */
      D->proc = D->accum;

      /* jumping to closure body */
      D->pc = ((sly_Closure*)D->proc.value.gc)->entry_point;
      break;

    case SLY_OP_RETURN:
      /* removing number of arguments */
      dw1 = (D->stack[--D->sp]).value.fixnum;

      /* removing arguments */
      D->sp -= dw1;

      /* restoring previous frame pointer */
      D->fp = (D->stack[--D->sp]).value.fixnum;

      /* restoring previous procedure */
      D->proc = D->stack[--D->sp];

      /* jumping to return address */
      D->pc = (D->stack[--D->sp]).value.fixnum;
      break;

    case SLY_OP_JMP_IF:
      if(!(D->accum.type == SLY_TYPE_BOOL && D->accum.value.bool == 0)) {
	D->pc += EXTRACT_ARG(instr);
      }
      break;

    case SLY_OP_JMP:
      D->pc += EXTRACT_ARG(instr);
      break;

    case SLY_OP_LOAD_FREE:
      D->accum = ((sly_Closure*)D->proc.value.gc)->free_vars[EXTRACT_ARG(instr)];
      break;

    case SLY_OP_SAVE_CONT:
      dw1 = D->sp * sizeof(sly_Object);

      tmp.type = SLY_TYPE_CONTINUATION;
      tmp.value.gc = (sly_GCObject*) alloc_continuation(&D->store, D->sp);
      check_alloc(D, tmp.value.gc);
      D->accum = tmp;

      /* copying stack */
      memcpy(((sly_Continuation*)D->accum.value.gc)->stack, D->stack, dw1);

      /* removing number of arguments from the stack */
      D->sp--;
      break;

    case SLY_OP_REST_CONT:
      /* return value is on the stack */
      tmp = D->stack[--D->sp];

      /* restoring stack */
      D->sp = ((sly_Continuation*)D->accum.value.gc)->size;
      memcpy(D->stack, ((sly_Continuation*)D->accum.value.gc)->stack, D->sp * sizeof(sly_Object));

      D->accum = tmp;
      break;

    case SLY_OP_ASSIGN:
      assert((D->stack[D->fp-EXTRACT_ARG(instr)-1]).type == SLY_TYPE_BOX);
      ((sly_Box*)(D->stack[D->fp-EXTRACT_ARG(instr)-1]).value.gc)->value = D->accum;
      break;

    case SLY_OP_ASSIGN_FREE:
      assert((((sly_Closure*)D->proc.value.gc)->free_vars[EXTRACT_ARG(instr)]).type == SLY_TYPE_BOX);
      ((sly_Box*)(((sly_Closure*)D->proc.value.gc)->free_vars[EXTRACT_ARG(instr)]).value.gc)->value = D->accum;
      break;

    case SLY_OP_BOX:
      tmp.type = SLY_TYPE_BOX;
      tmp.value.gc = (sly_GCObject*) alloc_box(&D->store);
      check_alloc(D, tmp.value.gc);

      ((sly_Box*)tmp.value.gc)->value = D->accum;
      D->accum = tmp;
      break;

    case SLY_OP_OPEN_BOX:
      assert(D->accum.type == SLY_TYPE_BOX);
      D->accum = ((sly_Box*)D->accum.value.gc)->value;
      break;

    case SLY_OP_FRAME:
      /* pushing return address */
      (D->stack[D->sp  ]).type = SLY_TYPE_FIXNUM;
      (D->stack[D->sp++]).value.fixnum = EXTRACT_ARG(instr);

      /* pushing current procedure */
      D->stack[D->sp++] = D->proc;

      /* pushing frame pointer */
      (D->stack[D->sp  ]).type = SLY_TYPE_FIXNUM;
      (D->stack[D->sp++]).value.fixnum = D->fp;
      break;

    case SLY_OP_HALT:
      write_obj(&D->accum);
      printf("\n");
      go_on = 0;
      break;

    case SLY_OP_LOAD_LOCAL:
      D->accum = D->stack[D->fp+EXTRACT_ARG(instr)+1];
      break;

    case SLY_OP_INSERT_BOX:
      i = D->fp-EXTRACT_ARG(instr)-1;

      tmp.type = SLY_TYPE_BOX;
      tmp.value.gc = (sly_GCObject*) alloc_box(&D->store);
      check_alloc(D, tmp.value.gc);

      ((sly_Box*)tmp.value.gc)->value = D->stack[i];
      D->stack[i] = tmp;
      break;

    case SLY_OP_ASSIGN_LOCAL:
      assert((D->stack[D->fp+EXTRACT_ARG(instr)+1]).type == SLY_TYPE_BOX);
      ((sly_Box*)(D->stack[D->fp+EXTRACT_ARG(instr)+1]).value.gc)->value = D->accum;
      break;

    case SLY_OP_POP:
      D->sp -= EXTRACT_ARG(instr);
      break;

    case SLY_OP_CHECKED_GLOBAL_REF:
      if(D->global_env.vars[EXTRACT_ARG(instr)].value.type == SLY_TYPE_UNDEF) {
	printf("Undefined global referenced: ");
	write_string(D->global_env.vars[EXTRACT_ARG(instr)].symbol->str, 0);
	printf("\n");
	sly_abort(D);
      }
      /* fall through */

    case SLY_OP_GLOBAL_REF:
      D->accum = D->global_env.vars[EXTRACT_ARG(instr)].value;
      break;

    case SLY_OP_CHECKED_GLOBAL_SET:
      if(D->global_env.vars[EXTRACT_ARG(instr)].value.type == SLY_TYPE_UNDEF) {
	printf("Undefined global assigned: ");
	write_string(D->global_env.vars[EXTRACT_ARG(instr)].symbol->str, 0);
	printf("\n");
	sly_abort(D);
      }
      /* fall through */

    case SLY_OP_GLOBAL_SET:
      D->global_env.vars[EXTRACT_ARG(instr)].value = D->accum;
      break;

    case SLY_OP_LOAD_UNDEF:
      D->accum.type = SLY_TYPE_UNDEF;
      break;

    case SLY_OP_CONST:
      D->accum = D->consts[EXTRACT_ARG(instr)];
      break;

    case SLY_OP_CONST_INIT:
      D->consts[EXTRACT_ARG(instr)] = D->accum;
      break;

    case SLY_OP_ARITY_EQ:
      if(D->stack[D->fp].value.fixnum != EXTRACT_ARG(instr)) {
	printf("Arity mismatch!\n");
	sly_abort(D);
      }
      break;

    case SLY_OP_ARITY_GE:
      if(D->stack[D->fp].value.fixnum < EXTRACT_ARG(instr)) {
	printf("Variable arity mismatch!\n");
	sly_abort(D);
      }
      break;

    case SLY_OP_LISTIFY:
      /* number of fixed arguments */
      dw1 = EXTRACT_ARG(instr);

      /* number of variables arguments */
      dw2 = D->stack[D->fp].value.fixnum - dw1;

      /* consing */
      D->accum.type = SLY_TYPE_NIL;
      for(i = D->fp - 1; i > D->fp - (dw2 + 1); i--) {
	tmp.type = SLY_TYPE_PAIR;
	tmp.value.gc = (sly_GCObject*)alloc_pair(&D->store);
	check_alloc(D, tmp.value.gc);

	((sly_Pair*)tmp.value.gc)->car = D->stack[i];
	((sly_Pair*)tmp.value.gc)->cdr = D->accum;

	D->accum = tmp;
      }

      /* adjusting stack */
      D->stack[D->fp - dw2] = D->accum;
      D->fp -= dw2 - 1;
      D->stack[D->fp].type = SLY_TYPE_FIXNUM;
      D->stack[D->fp].value.fixnum = dw1 + 1;
      D->sp = D->fp + 1;
      break;

    case SLY_OP_ABORT:
      sly_abort(D);
      break;

    case SLY_OP_CONS:
      tmp.type = SLY_TYPE_PAIR;
      tmp.value.gc = (sly_GCObject*) alloc_pair(&D->store);
      check_alloc(D, tmp.value.gc);

      ((sly_Pair*)tmp.value.gc)->car = D->stack[--D->sp];
      ((sly_Pair*)tmp.value.gc)->cdr = D->accum;
      D->accum = tmp;
      break;

    case SLY_OP_CAR:
      D->accum = ((sly_Pair*)D->accum.value.gc)->car;
      break;

    case SLY_OP_CDR:
      D->accum = ((sly_Pair*)D->accum.value.gc)->cdr;
      break;

    case SLY_OP_NUM_EQ:
      /* TODO: throw error if args are not numbers */
      SLY_SET_BOOL(D->accum.value.fixnum == D->stack[--D->sp].value.fixnum);
      break;

    case SLY_OP_EQ:
      SLY_SET_BOOL(D->accum.type == D->stack[D->sp-1].type &&
		    D->accum.value.symbol == D->stack[D->sp-1].value.symbol);
      --D->sp;
      break;

    case SLY_OP_EQV:
      SLY_SET_BOOL(D->accum.type == D->stack[D->sp-1].type &&
		    D->accum.value.symbol == D->stack[D->sp-1].value.symbol);
      --D->sp;
      break;

    case SLY_OP_MAKE_STRING:
      /* string size */
      dw1 = D->accum.value.fixnum;

      tmp.type = SLY_TYPE_STRING;
      tmp.value.gc = (sly_GCObject*)alloc_string(&D->store, dw1);
      check_alloc(D, tmp.value.gc);

      D->accum = tmp;
      break;

    case SLY_OP_STRING_SET:
      dw1 = D->accum.value.chr;
      dw2 = D->stack[--D->sp].value.fixnum;
      D->accum = D->stack[--D->sp];
      ((sly_String*)D->accum.value.gc)->chars[dw2] = dw1;
      break;

    case SLY_OP_STRING_TO_SYMBOL:
      tmp = sly_make_symbol(D, (sly_String*)D->accum.value.gc);
      D->accum = tmp;
      break;

    case SLY_OP_MAKE_VECTOR:
      /* vector size */
      dw1 = D->accum.value.fixnum;

      tmp.type = SLY_TYPE_VECTOR;
      tmp.value.gc = (sly_GCObject*)alloc_vector(&D->store, dw1);
      check_alloc(D, tmp.value.gc);

      D->accum = tmp;
      break;

    case SLY_OP_VECTOR_SET:
      tmp = D->accum;
      dw1 = D->stack[--D->sp].value.fixnum;
      D->accum = D->stack[--D->sp];
      ((sly_Vector*)D->accum.value.gc)->data[dw1] = tmp;    
      break;

    case SLY_OP_WRITE:
      write_obj(&D->accum);
      break;

    case SLY_OP_DEBUG:
      if(D->accum.value.bool == 0) {
	debug = 0;
      } else {
	debug = 1;
      }
    }
  }

  return 1;
}

/*
 * loading
 */

typedef struct sly_Module_ sly_Module;

struct sly_Module_ {

  /* uninterned globals */
  uint32_t nr_globals;
  sly_String **globals;

  /* constants */
  uint32_t nr_consts;

  /* code */
  uint32_t *code, code_size;
};

static void sly_destroy_module(sly_Module *M)
{
  uint32_t i;

  free(M->code);

  for(i = 0; i < M->nr_globals; i++) {
    free(M->globals[i]);
  }
  free(M->globals);
}

static uint32_t sly_link_module(sly_State* D, sly_Module *mod)
{
  sly_Env env;
  sly_Env_Var *vars;
  sly_Object obj, *tmp;
  uint32_t *code;
  uint32_t i, j, dw, consts_base, code_base, growth;

  env.size = mod->nr_globals;
  env.vars = (sly_Env_Var*)malloc(env.size * sizeof(sly_Env_Var));
  /* TODO: test return and throw error */

  /*
   * adding global to environment
   * a little trick is used here, I use the value
   * of the global var to store the index where the
   * global will be mapped to when linking the code
   *
   */
  for(growth = 0, i = 0; i < env.size; i++) {
    obj = sly_make_symbol(D, mod->globals[i]);
    env.vars[i].symbol = obj.value.symbol;
    env.vars[i].value.type = SLY_TYPE_FIXNUM;

    for(j = 0; j < D->global_env.size; j++) {
      if(env.vars[i].symbol == D->global_env.vars[j].symbol) {
	env.vars[i].value.value.fixnum = j;
	break;
      }
    }

    if(j == D->global_env.size) {
      env.vars[i].value.value.fixnum = D->global_env.size + growth++;
    }
  }

  /* enlarging global environment */
  if(growth > 0) {
    dw = D->global_env.size + growth;
    vars = (sly_Env_Var*)realloc(D->global_env.vars,
				  dw * sizeof(sly_Env_Var));
    /* TODO: test return and throw error */
    D->global_env.vars = vars;

    for(i = 0; i < env.size; i++) {
      j = env.vars[i].value.value.fixnum;
      if(!(j < D->global_env.size)) {
	D->global_env.vars[j].symbol = env.vars[i].symbol;
	D->global_env.vars[j].value.type = SLY_TYPE_UNDEF;
      }
    }
    D->global_env.size = dw;
  }

  /* enlarging constants */
  consts_base = D->nr_consts;
  dw = D->nr_consts + mod->nr_consts;
  tmp = (sly_Object*)realloc(D->consts, dw * sizeof(sly_Object));
  /* TODO: test return and throw error */
  D->consts = tmp;
  for(i = D->nr_consts; i < dw; i++) {
    D->consts[i].type = SLY_TYPE_UNDEF;
  }
  D->nr_consts = dw;

  /* enlarging code */
  code_base = D->code_size;
  dw = D->code_size + mod->code_size;
  code = (uint32_t*)realloc(D->code, dw * sizeof(uint32_t));
  /* TODO: test return and throw error */
  D->code_size = dw;
  D->code = code;

  for(i = 0; i < mod->code_size; i++) {
    uint32_t instr, op;

    instr = mod->code[i];
    op = EXTRACT_OP(instr);

    switch(op) {

    case SLY_OP_FRAME:
      dw = EXTRACT_ARG(instr);
      dw += code_base;
      instr = ((uint32_t)op) | dw << 8;
      break;

    case SLY_OP_CONST:
    case SLY_OP_CONST_INIT:
      dw = EXTRACT_ARG(instr);
      dw += consts_base;
      instr = ((uint32_t)op) | dw << 8;
      break;

    case SLY_OP_GLOBAL_REF:
    case SLY_OP_CHECKED_GLOBAL_REF:
    case SLY_OP_GLOBAL_SET:
    case SLY_OP_CHECKED_GLOBAL_SET:
      dw = EXTRACT_ARG(instr);
      dw = env.vars[dw].value.value.fixnum;
      instr = ((uint32_t)op) | dw << 8;
      break;
    }

    D->code[code_base+i] = instr;
  }

  free(env.vars);

  return code_base;
}

static int get_next(FILE* f, uint32_t *next)
{
  int ret;

  ret = fscanf(f, " %u", next);
  if(ret == EOF || ret == 0) {
    return 0;
  } else {
    return 1;
  }
}

static int get_fixnum(FILE* f, uint32_t *num)
{
  int ret;
  uint32_t b1, b2, b3, b4;

  ret = get_next(f, &b1);
  if(!ret) {
    return 0;
  }
  ret = get_next(f, &b2);
  if(!ret) {
    return 0;
  }
  ret = get_next(f, &b3);
  if(!ret) {
    return 0;
  }
  ret = get_next(f, &b4);
  if(!ret) {
    return 0;
  }

  *num = b1 | (b2 << 8) | (b3 << 16) | (b4 << 24);

  return 1;
}

static int get_string(FILE *f, sly_String **str)
{
  int ret;
  uint32_t i, dw1, dw2;

  /* string size */
  ret = get_fixnum(f, &dw1);
  if(!ret) {
    return 0;
  }

  *str = (sly_String*)malloc(SLY_SIZE_OF_STRING(dw1));
  /* TODO: test return and throw error */
  (*str)->size  = dw1;
  (*str)->type = SLY_TYPE_STRING;

  for(i = 0; i < dw1; i++) {
    ret = get_fixnum(f, &dw2);
    if(!ret) {
      free(*str);
      *str = NULL;
      return 0;
    }
    (*str)->chars[i] = dw2;
  }

  return 1;
}

static int load_code_from_file(sly_Module *mod, const char* fname)
{
  int ret;
  FILE *f;
  uint32_t i, dw1, dw2;

  /* opening input file */
  f = fopen(fname, "r");
  if(!f) {
    return 0;
  }

  /* bytecode beginning */
  ret = fscanf(f, " #(");
  if(ret == EOF) {
    fclose(f);
    return 0;
  }

  /* reading number of globals */
  ret = get_fixnum(f, &dw1);
  if(!ret) {
    fclose(f);
    return 0;
  }

  mod->nr_globals = dw1;
  dw2 = dw1 * sizeof(sly_String*);
  mod->globals = (sly_String**)malloc(dw2);
  /* TODO: test return and throw error */
  memset(mod->globals, 0x00, dw2);

  /* reading globals */
  for(i = 0; i < mod->nr_globals; i++) {

    ret = get_string(f, &mod->globals[i]);
    if(!ret) {
      fclose(f);
      return 0;
    }
  }

  /* reading number of constants */
  ret = get_fixnum(f, &dw1);
  if(!ret) {
    fclose(f);
    return -1;
  }

  mod->nr_consts = dw1;

  /* reading code size */
  ret = get_fixnum(f, &dw1);
  if(!ret) {
    fclose(f);
    return -1;
  }

  mod->code_size = dw1;
  mod->code = (uint32_t*)malloc(dw1 * sizeof(uint32_t));
  /* TODO: test return and throw error */

  /* reading actual code */
  for(i = 0; i < mod->code_size; i++) {
    uint32_t instr;

    ret = get_next(f, &instr);
    if(!ret) {
      /* unexpected end */
      sly_destroy_module(mod);
      fclose(f);
      return 0;
    }

    /* retrieve operands if any */
    if(IS_TYPE_B(instr)) {
      ret = get_fixnum(f, &dw1);
      if(!ret) {
	sly_destroy_module(mod);
	fclose(f);
	return 0;
      }

      instr |= (dw1 << 8);
    }

    mod->code[i] = instr;
  }

  fclose(f);
  return 1;
}

int sly_load_file(sly_State* D, const char *fname)
{
  sly_Module mod;

  /* tries to load code into module */
  if(!load_code_from_file(&mod, fname)) {
    return 0;
  }

  D->pc = sly_link_module(D, &mod);
  sly_destroy_module(&mod);

  /* initial frame on stack with address of halt instruction */
  /* return address */
  (D->stack[D->sp  ]).type = SLY_TYPE_FIXNUM;
  (D->stack[D->sp++]).value.fixnum = 0;

  /* saved procedure */
  D->stack[D->sp++] = D->proc;

  /* saved frame pointer */
  (D->stack[D->sp  ]).type = SLY_TYPE_FIXNUM;
  (D->stack[D->sp++]).value.fixnum = D->fp;

  /* number of arguments */
  (D->stack[D->sp  ]).type = SLY_TYPE_FIXNUM;
  (D->stack[D->sp++]).value.fixnum = 0;

  D->fp = D->sp - 1;

  return sly_vm_run(D);
}


/*
 * entry point
 */

int main(int argc, char *argv[])
{
  sly_State* D;

  if(argc != 2) {
    fprintf(stderr, "%s: Need file to run.\n", argv[0]);
    exit(13);
  }

  D = sly_init();

  /* tries to load initial environment */
  sly_load_file(D, "init.fasl");

  /* tries to load compiler */
  sly_load_file(D, "compiler.fasl");

  if(!sly_load_file(D, argv[1])) {
    printf("Error!\n");
  }

  sly_close(D);

  return 0;
}

