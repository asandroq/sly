/*
 * The Duna Scheme compiler
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
 * Duna bytecode is an array of 32-bit values for
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

/* basic VM instructions */
#define DUNA_OP_LOAD_NIL                1
#define DUNA_OP_LOAD_FALSE              2
#define DUNA_OP_LOAD_TRUE               3
#define DUNA_OP_LOAD_ZERO               4
#define DUNA_OP_LOAD_ONE                5
#define DUNA_OP_LOAD_FIXNUM             6
#define DUNA_OP_LOAD_CHAR               7
#define DUNA_OP_PUSH                    8
#define DUNA_OP_LOAD_0                  9
#define DUNA_OP_LOAD_1                 10
#define DUNA_OP_LOAD_2                 11
#define DUNA_OP_LOAD_3                 12
#define DUNA_OP_LOAD                   13
#define DUNA_OP_MAKE_CLOSURE           14
#define DUNA_OP_CALL                   15
#define DUNA_OP_RETURN                 16
#define DUNA_OP_JMP_IF                 17
#define DUNA_OP_JMP                    18
#define DUNA_OP_LOAD_FREE              19
#define DUNA_OP_SAVE_CONT              20
#define DUNA_OP_REST_CONT              21
#define DUNA_OP_ASSIGN                 22
#define DUNA_OP_ASSIGN_FREE            23
#define DUNA_OP_BOX                    24
#define DUNA_OP_OPEN_BOX               25
#define DUNA_OP_FRAME                  26
#define DUNA_OP_TAIL_CALL              27
#define DUNA_OP_HALT                   28
#define DUNA_OP_LOAD_LOCAL             29
#define DUNA_OP_INSERT_BOX             30
#define DUNA_OP_ASSIGN_LOCAL           31
#define DUNA_OP_POP                    32
#define DUNA_OP_GLOBAL_REF             33
#define DUNA_OP_CHECKED_GLOBAL_REF     34
#define DUNA_OP_GLOBAL_SET             35
#define DUNA_OP_CHECKED_GLOBAL_SET     36
#define DUNA_OP_LOAD_UNDEF             37
#define DUNA_OP_CONST                  38
#define DUNA_OP_CONST_INIT             39
#define DUNA_OP_STRING                 40
#define DUNA_OP_STRING_TO_SYMBOL       41

/* type predicates */
#define DUNA_OP_NULL_P                 81
#define DUNA_OP_BOOL_P                 82
#define DUNA_OP_CHAR_P                 83
#define DUNA_OP_FIXNUM_P               84

/* primitives optimised as instructions */
#define DUNA_OP_INC                   101
#define DUNA_OP_DEC                   102
#define DUNA_OP_FIXNUM_TO_CHAR        103
#define DUNA_OP_CHAR_TO_FIXNUM        104
#define DUNA_OP_ZERO_P                105
#define DUNA_OP_NOT                   106
#define DUNA_OP_PLUS                  107
#define DUNA_OP_MINUS                 108
#define DUNA_OP_MULT                  109
#define DUNA_OP_CONS                  110
#define DUNA_OP_CAR                   111
#define DUNA_OP_CDR                   112
#define DUNA_OP_NUM_EQ                113
#define DUNA_OP_EQ                    114
#define DUNA_OP_EQV                   115

/*
 * data types tags
 */
#define DUNA_TYPE_UNDEF                 1
#define DUNA_TYPE_NIL                   2
#define DUNA_TYPE_BOOL                  3
#define DUNA_TYPE_FIXNUM                4
#define DUNA_TYPE_CHAR                  5
#define DUNA_TYPE_SYMBOL                6
#define DUNA_TYPE_CLOSURE               7
#define DUNA_TYPE_PAIR                  8
#define DUNA_TYPE_CONTINUATION          9
#define DUNA_TYPE_BOX                  10
#define DUNA_TYPE_STRING               11

#define IS_TYPE_B(instr) \
   ((instr) == DUNA_OP_LOAD_FIXNUM ||            \
    (instr) == DUNA_OP_LOAD_CHAR ||              \
    (instr) == DUNA_OP_MAKE_CLOSURE ||           \
    (instr) == DUNA_OP_JMP_IF ||                 \
    (instr) == DUNA_OP_JMP ||                    \
    (instr) == DUNA_OP_LOAD_FREE ||              \
    (instr) == DUNA_OP_ASSIGN_FREE ||            \
    (instr) == DUNA_OP_FRAME ||                  \
    (instr) == DUNA_OP_INSERT_BOX ||             \
    (instr) == DUNA_OP_ASSIGN_LOCAL ||           \
    (instr) == DUNA_OP_LOAD ||                   \
    (instr) == DUNA_OP_ASSIGN ||                 \
    (instr) == DUNA_OP_LOAD_LOCAL ||             \
    (instr) == DUNA_OP_POP ||                    \
    (instr) == DUNA_OP_GLOBAL_REF ||             \
    (instr) == DUNA_OP_CHECKED_GLOBAL_REF ||     \
    (instr) == DUNA_OP_GLOBAL_SET ||             \
    (instr) == DUNA_OP_CHECKED_GLOBAL_SET ||     \
    (instr) == DUNA_OP_CONST ||                  \
    (instr) == DUNA_OP_CONST_INIT)

#define EXTRACT_OP(instr)   ((uint8_t)((instr) & 0x000000ff))
#define EXTRACT_ARG(instr)  ((uint32_t)((instr) >> 8))

/* debugging information */
struct  opcode_ {
  uint8_t op;
  const char *name;
};

typedef struct opcode_ opcode_t;

static opcode_t global_opcodes[] = {
  {DUNA_OP_LOAD_NIL,          "LOAD-NIL"},
  {DUNA_OP_LOAD_FALSE,        "LOAD-FALSE"},
  {DUNA_OP_LOAD_TRUE,         "LOAD-TRUE"},
  {DUNA_OP_LOAD_ZERO,         "LOAD-ZERO"},
  {DUNA_OP_LOAD_ONE,          "LOAD-ONE"},
  {DUNA_OP_LOAD_FIXNUM,       "LOAD-FIXNUM"},
  {DUNA_OP_LOAD_CHAR,         "LOAD-CHAR"},
  {DUNA_OP_PUSH,              "PUSH"},
  {DUNA_OP_LOAD_0,            "LOAD0"},
  {DUNA_OP_LOAD_1,            "LOAD1"},
  {DUNA_OP_LOAD_2,            "LOAD2"},
  {DUNA_OP_LOAD_3,            "LOAD3"},
  {DUNA_OP_LOAD,              "LOAD"},
  {DUNA_OP_MAKE_CLOSURE,      "MAKE-CLOSURE"},
  {DUNA_OP_CALL,              "CALL"},
  {DUNA_OP_RETURN,            "RETURN"},
  {DUNA_OP_JMP_IF,            "JMP-IF"},
  {DUNA_OP_JMP,               "JMP"},
  {DUNA_OP_LOAD_FREE,         "LOAD-FREE"},
  {DUNA_OP_SAVE_CONT,         "SAVE-CONT"},
  {DUNA_OP_REST_CONT,         "REST-CONT"},
  {DUNA_OP_ASSIGN,            "ASSIGN"},
  {DUNA_OP_ASSIGN_FREE,       "ASSIGN-FREE"},
  {DUNA_OP_BOX,               "BOX"},
  {DUNA_OP_OPEN_BOX,          "OPEN-BOX"},
  {DUNA_OP_FRAME,             "FRAME"},
  {DUNA_OP_TAIL_CALL,         "TAIL-CALL"},
  {DUNA_OP_HALT,              "HALT"},
  {DUNA_OP_LOAD_LOCAL,        "LOAD-LOCAL"},
  {DUNA_OP_INSERT_BOX,        "INSERT-BOX"},
  {DUNA_OP_ASSIGN_LOCAL,      "ASSIGN-LOCAL"},
  {DUNA_OP_POP,               "POP"},
  {DUNA_OP_GLOBAL_REF,             "GLOBAL-REF"},
  {DUNA_OP_CHECKED_GLOBAL_REF,     "CHECKED-GLOBAL-REF"},
  {DUNA_OP_GLOBAL_SET,             "GLOBAL-SET"},
  {DUNA_OP_CHECKED_GLOBAL_SET,     "CHECKED-GLOBAL-SET"},
  {DUNA_OP_LOAD_UNDEF,             "LOAD-UNDEF"},
  {DUNA_OP_CONST,                  "CONST"},
  {DUNA_OP_CONST_INIT,             "CONST-INIT"},
  {DUNA_OP_STRING,                 "STRING"},
  {DUNA_OP_STRING_TO_SYMBOL,       "STRING->SYMBOL"},
  {DUNA_OP_NULL_P,            "NULL?"},
  {DUNA_OP_BOOL_P,            "BOOL?"},
  {DUNA_OP_CHAR_P,            "CHAR?"},
  {DUNA_OP_FIXNUM_P,          "FIXNUM?"},
  {DUNA_OP_INC,               "INC"},
  {DUNA_OP_DEC,               "DEC"},
  {DUNA_OP_FIXNUM_TO_CHAR,    "FIXNUM->CHAR"},
  {DUNA_OP_CHAR_TO_FIXNUM,    "CHAR->FIXNUM"},
  {DUNA_OP_ZERO_P,            "ZERO?"},
  {DUNA_OP_NOT,               "NOT"},
  {DUNA_OP_PLUS,              "PLUS"},
  {DUNA_OP_MINUS,             "MINUS"},
  {DUNA_OP_MULT,              "MULT"},
  {DUNA_OP_CONS,              "CONS"},
  {DUNA_OP_CAR,               "CAR"},
  {DUNA_OP_CDR,               "CDR"},
  {DUNA_OP_NUM_EQ,            "NUM-EQ"},
  {DUNA_OP_EQ,                "EQ?"},
  {DUNA_OP_EQV,               "EQV?"},
  {0, NULL}
};

/*
 * Data types
 */

/* forward type declarations */
typedef struct duna_Object_ duna_Object;
typedef struct duna_GCObject_ duna_GCObject;

typedef uint32_t duna_Char;
typedef struct duna_Box_ duna_Box;
typedef struct duna_Closure_ duna_Closure;
typedef struct duna_Pair_ duna_Pair;
typedef struct duna_Continuation_ duna_Continuation;
typedef struct duna_String_ duna_String;
typedef struct duna_Symbol_ duna_Symbol;

/* value types */
struct duna_Object_ {

  /* the runtime type tag */
  uint8_t type;

  /* the value of this object */
  union {
    /* immediates */
    uint8_t bool;
    duna_Char chr;
    uint32_t fixnum;
    duna_Symbol *symbol;

    /* collectable objects */
    duna_GCObject *gc;
  } value;
};

#define DUNA_GC_BASE uint32_t type

struct duna_GCObject_ {
  DUNA_GC_BASE;
};

struct duna_Box_ {
  DUNA_GC_BASE;
  duna_Object value;
};

struct duna_Closure_ {
  DUNA_GC_BASE;
  uint32_t entry_point;
  uint32_t nr_free;
  duna_Object free_vars[0];
};

struct duna_Pair_ {
  DUNA_GC_BASE;
  duna_Object car;
  duna_Object cdr;  
};

struct duna_Continuation_ {
  DUNA_GC_BASE;
  uint32_t size;
  duna_Object stack[0];
};

struct duna_String_ {
  DUNA_GC_BASE;
  uint32_t size;
  duna_Char chars[0];
};

/*
 * entry in the symbol table
 * symbols are not collected
 * some hash functions:
 * http://burtleburtle.net/bob/c/lookup3.c
 */
struct duna_Symbol_ {
  /* symbol textual representation */
  duna_String *str;

  /* next symbol in chain */
  duna_Symbol *next;
};

/*
 * the store, i.e., memory
 */

#define DUNA_INITIAL_SPACE_SIZE     ((uint32_t)(1 << 7))
#define DUNA_IMMEDIATE_P(o)         ((o)->type < DUNA_TYPE_CLOSURE)
#define DUNA_FORWARD_TAG            199

#define DUNA_SIZE_OF_BOX \
   (sizeof(duna_Box))
#define DUNA_SIZE_OF_PAIR \
   (sizeof(duna_Pair))
#define DUNA_SIZE_OF_CLOSURE(n) \
   (sizeof(duna_Closure) + (n) * sizeof(duna_Object))
#define DUNA_SIZE_OF_CONTINUATION(n) \
   (sizeof(duna_Continuation) + (n) * sizeof(duna_Object))
#define DUNA_SIZE_OF_STRING(n) \
   (sizeof(duna_String) + (n) * sizeof(duna_Char))

/*
 * this callback is called by the garbage collector
 * to get all the mutator roots. It must return NULL
 * when there are no more roots to scan
 */
typedef duna_Object* (*duna_Roots_Callback)(void*);

/* forward reference to object in to-space */
typedef struct duna_Forward_Ref_ duna_Forward_Ref;

struct duna_Forward_Ref_ {
  DUNA_GC_BASE;
  duna_GCObject *ref;
};

typedef struct duna_Store_ duna_Store;

struct duna_Store_ {
  
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
  duna_Roots_Callback roots_cb;

  /* opaque data to pass to callback */
  void *roots_cb_data;
};

static int init_store(duna_Store *S, duna_Roots_Callback cb, void* ud)
{
  /* alloc heap */
  S->from_space = malloc(DUNA_INITIAL_SPACE_SIZE * 2);
  if(S->from_space == NULL) {
    return 0;
  }

  S->size = 0;
  S->os_address = S->from_space;
  S->capacity = DUNA_INITIAL_SPACE_SIZE;
  S->to_space = S->from_space + (DUNA_INITIAL_SPACE_SIZE);

  S->roots_cb = cb;
  S->roots_cb_data = ud;

  return 1;
}

static void finish_store(duna_Store *S)
{
  free(S->os_address);
  S->size = S->capacity = 0;
  S->from_space = S->to_space = NULL;
  S->roots_cb = S->roots_cb_data = NULL;
}

static uint32_t sizeof_gcobj(duna_GCObject* obj)
{
  uint32_t size;

  switch(obj->type) {
  case DUNA_TYPE_CLOSURE:
    size = DUNA_SIZE_OF_CLOSURE(((duna_Closure*)obj)->nr_free);
    break;

  case DUNA_TYPE_PAIR:
    size = DUNA_SIZE_OF_PAIR;
    break;

  case DUNA_TYPE_CONTINUATION:
    size = DUNA_SIZE_OF_CONTINUATION(((duna_Continuation*)obj)->size);
    break;

  case DUNA_TYPE_BOX:
    size = DUNA_SIZE_OF_BOX;
    break;

  case DUNA_TYPE_STRING:
    size = DUNA_SIZE_OF_STRING(((duna_String*)obj)->size);
    break;

  default:
    size = 0;
  }

  return size;
}

static void copy_object(duna_Store* S, duna_Object* obj)
{
  void *to;
  uint32_t size;

  if(DUNA_IMMEDIATE_P(obj)) {
    /* if not heap-allocated, bail */
    return;
  }

  if(obj->value.gc->type == DUNA_FORWARD_TAG) {
    /* already copied, just update pointer */
    obj->value.gc = ((duna_Forward_Ref*)obj->value.gc)->ref;
    return;
  }

  /* copying */
  to = S->to_space + S->size;
  size = sizeof_gcobj(obj->value.gc);
  memcpy(to, obj->value.gc, size);
  S->size += size;

  /* leave a forwarding pointer and update */
  obj->value.gc->type = DUNA_FORWARD_TAG;
  ((duna_Forward_Ref*)obj->value.gc)->ref = to;
  obj->value.gc = to;
}

static void collect_garbage(duna_Store* S)
{
  /*
   * classic, simple 2-space copy collector
   * using Cheney's algorithm
   */
  void *scan;
  duna_Object *obj;
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
    duna_GCObject *gcobj;

    gcobj = (duna_GCObject*)scan;
    size = sizeof_gcobj(gcobj);

    switch(gcobj->type) {
    case DUNA_TYPE_CLOSURE:
      for(i = 0; i < ((duna_Closure*)gcobj)->nr_free; i++) {
	copy_object(S, &(((duna_Closure*)gcobj)->free_vars[i]));
      }
      break;

    case DUNA_TYPE_PAIR:
      copy_object(S, &((duna_Pair*)gcobj)->car);
      copy_object(S, &((duna_Pair*)gcobj)->car);
      break;

    case DUNA_TYPE_CONTINUATION:
      for(i = 0; i < ((duna_Continuation*)gcobj)->size; i++) {
	copy_object(S, &(((duna_Continuation*)gcobj)->stack[i]));
      }
      break;

    case DUNA_TYPE_BOX:
      copy_object(S, &((duna_Box*)gcobj)->value);
      break;
    }

    scan += size;
  }

  /* swap spaces */
  scan = S->from_space;
  S->from_space = S->to_space;
  S->to_space = scan;
}

static int expand_store(duna_Store* S)
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

static void* alloc_from_store(duna_Store *S, uint32_t size)
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

static duna_Box *alloc_box(duna_Store *S)
{
  duna_Box *ret;

  ret = (duna_Box*)alloc_from_store(S, DUNA_SIZE_OF_BOX);
  if(ret) {
    ret->type = DUNA_TYPE_BOX;
  }

  return ret;
}

static duna_Closure *alloc_closure(duna_Store *S, uint32_t nr_vars)
{
  duna_Closure *ret;

  ret = (duna_Closure*) alloc_from_store(S, DUNA_SIZE_OF_CLOSURE(nr_vars));
  if(ret) {
    ret->type = DUNA_TYPE_CLOSURE;
    ret->nr_free = nr_vars;
  }

  return ret;
}

static duna_Pair *alloc_pair(duna_Store *S)
{
  duna_Pair *ret;

  ret = (duna_Pair*) alloc_from_store(S, DUNA_SIZE_OF_PAIR);
  if(ret) {
    ret->type = DUNA_TYPE_PAIR;
  }

  return ret;
}

static duna_Continuation *alloc_continuation(duna_Store *S, uint32_t stack_size)
{
  duna_Continuation *ret;

  ret = (duna_Continuation*)alloc_from_store(S, DUNA_SIZE_OF_CONTINUATION(stack_size));
  if(ret) {
    ret->type = DUNA_TYPE_CONTINUATION;
    ret->size = stack_size;
  }

  return ret;
}

static duna_String *alloc_string(duna_Store *S, uint32_t size)
{
  duna_String *ret;

  ret = (duna_String*)alloc_from_store(S, DUNA_SIZE_OF_STRING(size));

  if(ret) {
    ret->type = DUNA_TYPE_STRING;
    ret->size = size;
  }

  return ret;
}

/*
 * the virtual machine
 */

typedef struct duna_Env_ duna_Env;
typedef struct duna_Env_Var_ duna_Env_Var;

/* an entry in an environment */
struct duna_Env_Var_ {

  /* entry in the symbol table */
  duna_Symbol *symbol;

  /* the actual value */
  duna_Object value;
};

/* a global environment */
struct duna_Env_ {
  uint32_t size;
  duna_Env_Var *vars;
};

/* the state of the Duna interpreter */
typedef struct duna_State_ duna_State;

struct duna_State_ {

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
  duna_Object accum;

  /* the current procedure */
  duna_Object proc;

  /* global environment */
  duna_Env global_env;

  /* the bytecode to be interpreted */
  uint32_t *code;

  /* the machine stack */
  duna_Object *stack;

  /* constants */
  duna_Object *consts;

  /* symbol table */
  duna_Symbol *symbol_table;

  /* VM memory */
  duna_Store store;
};

/* garbage collector callback */

struct gc_data {
  duna_State *D;
  uint32_t state, count;
};

static struct gc_data gc_data;

static duna_Object* gc_callback(void *ud)
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

duna_State* duna_init(void)
{
  duna_State *D = NULL;

  D = (duna_State*)malloc(sizeof(duna_State));
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
  D->code[0] = (uint32_t) DUNA_OP_HALT;

  /* stack */
  D->sp = 0;
  D->stack = (duna_Object*)malloc(sizeof(duna_Object) * 1024);
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
  D->proc.type = DUNA_TYPE_UNDEF;
  D->accum.type = DUNA_TYPE_UNDEF;

  return D;
}

void duna_close(duna_State* D)
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
      duna_Symbol *sym;
      for(sym = D->symbol_table; sym != NULL;) {
	duna_Symbol *tmp = sym->next;
	free(sym->str);
	free(sym);
	sym = tmp;
      }
    }

    free(D);
  }
}

static int string_equal_p(duna_String *s1, duna_String *s2)
{
  if(s1->size != s2->size) {
    return 0;
  }

  return memcmp(s1->chars, s2->chars, s1->size * sizeof(duna_Char)) == 0;
}

static duna_String* string_copy(duna_State* D, duna_String* s)
{
  duna_String *ret;

  ret = alloc_string(&D->store, s->size);
  memcpy(ret->chars, s->chars, s->size * sizeof(duna_Char));

  return ret;
}

static duna_String* string_copy_extern(duna_String* s)
{
  uint32_t size;
  duna_String *ret;

  size = DUNA_SIZE_OF_STRING(s->size);

  ret = (duna_String*)malloc(size);
  /* TODO: test and throw error */
  memcpy(ret, s, size);

  return ret;
}

static duna_Object duna_make_symbol(duna_State* D, duna_String *str)
{
  duna_Object obj;
  duna_Symbol *tmp;

  /* is the symbol already there? */
  for(tmp = D->symbol_table; tmp != NULL; tmp = tmp->next) {
    if(string_equal_p(tmp->str, str)) {
      break;
    }
  }

  if(tmp == NULL) {
    /* adding new symbol */
    tmp = (duna_Symbol*)malloc(sizeof(duna_Symbol));
    tmp->str = string_copy_extern(str);
    tmp->next = D->symbol_table;
    D->symbol_table = tmp;
  }

  obj.type = DUNA_TYPE_SYMBOL;
  obj.value.symbol = tmp;

  return obj;
}

/*
 * writer
 */

static void write_string(duna_String* s, int quote)
{
  uint32_t i;

  if(quote) {
    printf("\"");
  }

  for(i = 0; i < s->size; i++) {
    printf("%c", s->chars[i]);
  }

  if(quote) {
    printf("\"");
  }
}

static void write_obj(duna_Object* obj)
{
  switch(obj->type) {

  case DUNA_TYPE_UNDEF:
    printf("<#undefined>");
    break;

  case DUNA_TYPE_NIL:
    printf("()");
    break;

  case DUNA_TYPE_BOOL:
    if(obj->value.bool) {
      printf("#t");
    } else {
      printf("#f");
    }
    break;

  case DUNA_TYPE_FIXNUM:
    printf("%d", obj->value.fixnum);
    break;

  case DUNA_TYPE_CHAR:
    printf("#\\%c", obj->value.chr);
    break;

  case DUNA_TYPE_SYMBOL:
    write_string(obj->value.symbol->str, 0);
    break;

  case DUNA_TYPE_CLOSURE:
    printf("<#closure %u>", ((duna_Closure*)obj->value.gc)->entry_point);
    break;

  case DUNA_TYPE_PAIR:
    printf("(");
    write_obj(&(((duna_Pair*)obj->value.gc)->car));
    printf(" . ");
    write_obj(&(((duna_Pair*)obj->value.gc)->cdr));
    printf(")");
    break;

  case DUNA_TYPE_CONTINUATION:
    printf("<#continuation %u>", ((duna_Continuation*)obj->value.gc)->size);
    break;

  case DUNA_TYPE_BOX:
    printf("#&");
    write_obj(&(((duna_Box*)obj->value.gc)->value));
    break;

  case DUNA_TYPE_STRING:
    write_string((duna_String*)obj->value.gc, 1);
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

static void disassemble(duna_State* D)
{
  uint32_t i;

  printf("Code listing:\n");
  for(i = 0; i < D->code_size; i++) {
    printf("\t%u\t", i);
    dump_instr(D->code[i]);
    printf("\n");
  }
}

void duna_dump(duna_State* D)
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
  printf("\n\n");
}

static void check_alloc(duna_State *D, void* ptr)
{
  if(ptr == NULL) {
    fprintf(stderr, "duna: Out of memory!\n");
    duna_close(D);
    exit(13);
  }
}

#define DUNA_SET_BOOL(cond)		\
  do {					\
    if(cond) {				\
      D->accum.type = DUNA_TYPE_BOOL;	\
      D->accum.value.bool = 1;		\
    } else {				\
      D->accum.type = DUNA_TYPE_BOOL;	\
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
int duna_vm_run(duna_State* D)
{
  int go_on = 1;

  disassemble(D);

  while(go_on) {
    register uint32_t instr;
    uint32_t i, j, dw1, dw2;
    /* still unsure about this, should be a register? */
    duna_Object tmp;

    /* debugging */
    duna_dump(D);
    assert(D->pc < D->code_size);

    instr = D->code[D->pc++];

    switch(EXTRACT_OP(instr)) {

    case DUNA_OP_LOAD_NIL:
      D->accum.type = DUNA_TYPE_NIL;
      break;

    case DUNA_OP_LOAD_FALSE:
      D->accum.type = DUNA_TYPE_BOOL;
      D->accum.value.bool = 0;
      break;

    case DUNA_OP_LOAD_TRUE:
      D->accum.type = DUNA_TYPE_BOOL;
      D->accum.value.bool = 1;
      break;

    case DUNA_OP_LOAD_ZERO:
      D->accum.type = DUNA_TYPE_FIXNUM;
      D->accum.value.fixnum = 0;
      break;

    case DUNA_OP_LOAD_ONE:
      D->accum.type = DUNA_TYPE_FIXNUM;
      D->accum.value.fixnum = 1;
      break;

    case DUNA_OP_LOAD_FIXNUM:
      D->accum.type = DUNA_TYPE_FIXNUM;
      D->accum.value.fixnum = EXTRACT_ARG(instr);
      break;

    case DUNA_OP_LOAD_CHAR:
      D->accum.type = DUNA_TYPE_CHAR;
      D->accum.value.chr = (uint8_t) EXTRACT_ARG(instr);
      break;

    case DUNA_OP_INC:
      D->accum.value.fixnum++;
      break;

    case DUNA_OP_DEC:
      D->accum.value.fixnum--;
      break;

    case DUNA_OP_FIXNUM_TO_CHAR:
      D->accum.type = DUNA_TYPE_CHAR;
      D->accum.value.chr = (uint8_t) D->accum.value.fixnum;
      break;

    case DUNA_OP_CHAR_TO_FIXNUM:
      D->accum.type = DUNA_TYPE_FIXNUM;
      D->accum.value.fixnum = (uint32_t) D->accum.value.chr;
      break;

    case DUNA_OP_NULL_P:
      DUNA_SET_BOOL(D->accum.type == DUNA_TYPE_NIL);
      break;

    case DUNA_OP_ZERO_P:
      DUNA_SET_BOOL(D->accum.value.fixnum == 0);
      break;

    case DUNA_OP_NOT:
      DUNA_SET_BOOL(D->accum.type == DUNA_TYPE_BOOL &&
		    D->accum.value.bool == 0);
      break;

    case DUNA_OP_BOOL_P:
      DUNA_SET_BOOL(D->accum.type == DUNA_TYPE_BOOL);
      break;

    case DUNA_OP_CHAR_P:
      DUNA_SET_BOOL(D->accum.type == DUNA_TYPE_CHAR);
      break;

    case DUNA_OP_FIXNUM_P:
      DUNA_SET_BOOL(D->accum.type == DUNA_TYPE_FIXNUM);
      break;

    case DUNA_OP_PUSH:
      D->stack[D->sp++] = D->accum;
      break;

    case DUNA_OP_PLUS:
      D->accum.value.fixnum += (D->stack[--D->sp]).value.fixnum;
      break;

    case DUNA_OP_MINUS:
      D->accum.value.fixnum -= (D->stack[--D->sp]).value.fixnum;
      break;

    case DUNA_OP_MULT:
      D->accum.value.fixnum *= (D->stack[--D->sp]).value.fixnum;
      break;

    case DUNA_OP_LOAD_0:
      D->accum = D->stack[D->fp-1];
      break;

    case DUNA_OP_LOAD_1:
      D->accum = D->stack[D->fp-2];
      break;

    case DUNA_OP_LOAD_2:
      D->accum = D->stack[D->fp-3];
      break;

    case DUNA_OP_LOAD_3:
      D->accum = D->stack[D->fp-4];
      break;

    case DUNA_OP_LOAD:
      D->accum = D->stack[D->fp-EXTRACT_ARG(instr)-1];
      break;

    case DUNA_OP_MAKE_CLOSURE:
      /* number of free variables */
      dw1 = EXTRACT_ARG(instr);

      tmp.type = DUNA_TYPE_CLOSURE;
      tmp.value.gc = (duna_GCObject*) alloc_closure(&D->store, dw1);
      check_alloc(D, tmp.value.gc);
      D->accum = tmp;

      /*
       * There is always a jump after this instruction, to jump over the
       * closure code. So the closure entry point is PC + 1
       */
      ((duna_Closure*)D->accum.value.gc)->entry_point = D->pc + 1;

      /* gathering free variables */
      for(i = 0; i < dw1; i++) {
	((duna_Closure*)D->accum.value.gc)->free_vars[i] = D->stack[D->sp-i-1];
      }
      D->sp -= dw1;
      break;

    case DUNA_OP_TAIL_CALL:
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
      memcpy(D->stack+i, D->stack+j, (dw1+1) * sizeof(duna_Object));

      /* fall through */

    case DUNA_OP_CALL:
      /* frame pointer */
      D->fp = D->sp - 1;

      /* setting current procedure */
      D->proc = D->accum;

      /* jumping to closure body */
      D->pc = ((duna_Closure*)D->proc.value.gc)->entry_point;
      break;

    case DUNA_OP_RETURN:
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

    case DUNA_OP_JMP_IF:
      if(!(D->accum.type == DUNA_TYPE_BOOL && D->accum.value.bool == 0)) {
	D->pc += EXTRACT_ARG(instr);
      }
      break;

    case DUNA_OP_JMP:
      D->pc += EXTRACT_ARG(instr);
      break;

    case DUNA_OP_LOAD_FREE:
      D->accum = ((duna_Closure*)D->proc.value.gc)->free_vars[EXTRACT_ARG(instr)];
      break;

    case DUNA_OP_SAVE_CONT:
      dw1 = D->sp * sizeof(duna_Object);

      tmp.type = DUNA_TYPE_CONTINUATION;
      tmp.value.gc = (duna_GCObject*) alloc_continuation(&D->store, D->sp);
      check_alloc(D, tmp.value.gc);
      D->accum = tmp;

      /* copying stack */
      memcpy(((duna_Continuation*)D->accum.value.gc)->stack, D->stack, dw1);

      /* removing number of arguments from the stack */
      D->sp--;
      break;

    case DUNA_OP_REST_CONT:
      /* return value is on the stack */
      tmp = D->stack[--D->sp];

      /* restoring stack */
      D->sp = ((duna_Continuation*)D->accum.value.gc)->size;
      memcpy(D->stack, ((duna_Continuation*)D->accum.value.gc)->stack, D->sp * sizeof(duna_Object));

      D->accum = tmp;
      break;

    case DUNA_OP_ASSIGN:
      assert((D->stack[D->fp-EXTRACT_ARG(instr)-1]).type == DUNA_TYPE_BOX);
      ((duna_Box*)(D->stack[D->fp-EXTRACT_ARG(instr)-1]).value.gc)->value = D->accum;
      break;

    case DUNA_OP_ASSIGN_FREE:
      assert((((duna_Closure*)D->proc.value.gc)->free_vars[EXTRACT_ARG(instr)]).type == DUNA_TYPE_BOX);
      ((duna_Box*)(((duna_Closure*)D->proc.value.gc)->free_vars[EXTRACT_ARG(instr)]).value.gc)->value = D->accum;
      break;

    case DUNA_OP_BOX:
      tmp.type = DUNA_TYPE_BOX;
      tmp.value.gc = (duna_GCObject*) alloc_box(&D->store);
      check_alloc(D, tmp.value.gc);

      ((duna_Box*)tmp.value.gc)->value = D->accum;
      D->accum = tmp;
      break;

    case DUNA_OP_OPEN_BOX:
      assert(D->accum.type == DUNA_TYPE_BOX);
      D->accum = ((duna_Box*)D->accum.value.gc)->value;
      break;

    case DUNA_OP_FRAME:
      /* pushing return address */
      (D->stack[D->sp  ]).type = DUNA_TYPE_FIXNUM;
      (D->stack[D->sp++]).value.fixnum = EXTRACT_ARG(instr);

      /* pushing current procedure */
      D->stack[D->sp++] = D->proc;

      /* pushing frame pointer */
      (D->stack[D->sp  ]).type = DUNA_TYPE_FIXNUM;
      (D->stack[D->sp++]).value.fixnum = D->fp;
      break;

    case DUNA_OP_HALT:
      write_obj(&D->accum);
      printf("\n");
      go_on = 0;
      break;

    case DUNA_OP_LOAD_LOCAL:
      D->accum = D->stack[D->fp+EXTRACT_ARG(instr)+1];
      break;

    case DUNA_OP_INSERT_BOX:
      i = D->fp-EXTRACT_ARG(instr)-1;

      tmp.type = DUNA_TYPE_BOX;
      tmp.value.gc = (duna_GCObject*) alloc_box(&D->store);
      check_alloc(D, tmp.value.gc);

      ((duna_Box*)tmp.value.gc)->value = D->stack[i];
      D->stack[i] = tmp;
      break;

    case DUNA_OP_ASSIGN_LOCAL:
      assert((D->stack[D->fp+EXTRACT_ARG(instr)+1]).type == DUNA_TYPE_BOX);
      ((duna_Box*)(D->stack[D->fp+EXTRACT_ARG(instr)+1]).value.gc)->value = D->accum;
      break;

    case DUNA_OP_POP:
      D->sp -= EXTRACT_ARG(instr);
      break;

    case DUNA_OP_CHECKED_GLOBAL_REF:
      if(D->global_env.vars[EXTRACT_ARG(instr)].value.type == DUNA_TYPE_UNDEF) {
	/* throw error */
      }
      /* fall through */

    case DUNA_OP_GLOBAL_REF:
      D->accum = D->global_env.vars[EXTRACT_ARG(instr)].value;
      break;

    case DUNA_OP_CHECKED_GLOBAL_SET:
      if(D->global_env.vars[EXTRACT_ARG(instr)].value.type == DUNA_TYPE_UNDEF) {
	/* throw error */
      }
      /* fall through */

    case DUNA_OP_GLOBAL_SET:
      D->global_env.vars[EXTRACT_ARG(instr)].value = D->accum;
      break;

    case DUNA_OP_LOAD_UNDEF:
      D->accum.type = DUNA_TYPE_UNDEF;
      break;

    case DUNA_OP_CONST:
      D->accum = D->consts[EXTRACT_ARG(instr)];
      break;

    case DUNA_OP_CONST_INIT:
      D->consts[EXTRACT_ARG(instr)] = D->accum;
      break;

    case DUNA_OP_STRING:
      /* string size */
      dw1 = D->accum.value.fixnum;

      tmp.type = DUNA_TYPE_STRING;
      tmp.value.gc = (duna_GCObject*)alloc_string(&D->store, DUNA_SIZE_OF_STRING(dw1));
      check_alloc(D, tmp.value.gc);

      D->accum = tmp;
      for(i = 0; i < dw1; i++) {
	((duna_String*)D->accum.value.gc)->chars[i] =
	  D->stack[--D->sp].value.fixnum;
      }
      break;

    case DUNA_OP_STRING_TO_SYMBOL:
      tmp = duna_make_symbol(D, (duna_String*)D->accum.value.gc);
      D->accum = tmp;
      break;

    case DUNA_OP_CONS:
      tmp.type = DUNA_TYPE_PAIR;
      tmp.value.gc = (duna_GCObject*) alloc_pair(&D->store);
      check_alloc(D, tmp.value.gc);

      ((duna_Pair*)tmp.value.gc)->car = D->accum;
      ((duna_Pair*)tmp.value.gc)->cdr = D->stack[--D->sp];
      D->accum = tmp;
      break;

    case DUNA_OP_CAR:
      D->accum = ((duna_Pair*)D->accum.value.gc)->car;
      break;

    case DUNA_OP_CDR:
      D->accum = ((duna_Pair*)D->accum.value.gc)->cdr;
      break;

    case DUNA_OP_NUM_EQ:
      /* TODO: throw error if args are not numbers */
      DUNA_SET_BOOL(D->accum.value.fixnum == D->stack[--D->sp].value.fixnum);
      break;

    case DUNA_OP_EQ:
      DUNA_SET_BOOL(D->accum.type == D->stack[D->sp-1].type &&
		    D->accum.value.symbol == D->stack[D->sp-1].value.symbol);
      --D->sp;
      break;

    case DUNA_OP_EQV:
      DUNA_SET_BOOL(D->accum.type == D->stack[D->sp-1].type &&
		    D->accum.value.symbol == D->stack[D->sp-1].value.symbol);
      --D->sp;
      break;
    }
  }

  return 1;
}

/*
 * loading
 */

typedef struct duna_Module_ duna_Module;

struct duna_Module_ {

  /* uninterned globals */
  uint32_t nr_globals;
  duna_String **globals;

  /* constants */
  uint32_t nr_consts;

  /* code */
  uint32_t *code, code_size;
};

static void duna_destroy_module(duna_Module *M)
{
  uint32_t i;

  free(M->code);

  for(i = 0; i < M->nr_globals; i++) {
    free(M->globals[i]);
  }
  free(M->globals);
}

static uint32_t duna_link_module(duna_State* D, duna_Module *mod)
{
  duna_Env env;
  duna_Env_Var *vars;
  duna_Object obj, *tmp;
  uint32_t *code;
  uint32_t i, j, dw, consts_base, code_base, growth;

  env.size = mod->nr_globals;
  env.vars = (duna_Env_Var*)malloc(env.size * sizeof(duna_Env_Var));
  /* TODO: test return and throw error */

  /*
   * adding global to environment
   * a little trick is used here, I use the value
   * of the global var to store the index where the
   * global will be mapped to when linking the code
   *
   */
  for(growth = 0, i = 0; i < env.size; i++) {
    obj = duna_make_symbol(D, mod->globals[i]);
    env.vars[i].symbol = obj.value.symbol;
    env.vars[i].value.type = DUNA_TYPE_FIXNUM;

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
    vars = (duna_Env_Var*)realloc(D->global_env.vars,
				  dw * sizeof(duna_Env_Var));
    /* TODO: test return and throw error */
    D->global_env.vars = vars;

    for(i = 0; i < env.size; i++) {
      j = env.vars[i].value.value.fixnum;
      if(D->global_env.size < j) {
	D->global_env.vars[j].symbol = env.vars[i].symbol;
	D->global_env.vars[j].value.type = DUNA_TYPE_UNDEF;
      }
    }
    D->global_env.size = dw;
  }

  /* enlarging constants */
  consts_base = D->nr_consts;
  dw = D->nr_consts + mod->nr_consts;
  tmp = (duna_Object*)realloc(D->consts, dw * sizeof(duna_Object));
  /* TODO: test return and throw error */
  D->consts = tmp;
  for(i = D->nr_consts; i < dw; i++) {
    D->consts[i].type = DUNA_TYPE_UNDEF;
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

    case DUNA_OP_FRAME:
      dw = EXTRACT_ARG(instr);
      dw += code_base;
      instr = ((uint32_t)op) | dw << 8;
      break;

    case DUNA_OP_CONST:
    case DUNA_OP_CONST_INIT:
      dw = EXTRACT_ARG(instr);
      dw += consts_base;
      instr = ((uint32_t)op) | dw << 8;
      break;

    case DUNA_OP_GLOBAL_REF:
    case DUNA_OP_CHECKED_GLOBAL_REF:
    case DUNA_OP_GLOBAL_SET:
    case DUNA_OP_CHECKED_GLOBAL_SET:
      dw = EXTRACT_ARG(instr);
      dw = env.vars[dw].value.value.fixnum;
      instr = ((uint32_t)op) | dw << 8;
      break;
    }

    D->code[growth+i] = instr;
  }

  free(env.vars);

  return growth;
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

static int get_string(FILE *f, duna_String **str)
{
  int ret;
  uint32_t i, dw1, dw2;

  /* string size */
  ret = get_fixnum(f, &dw1);
  if(!ret) {
    return 0;
  }

  *str = (duna_String*)malloc(DUNA_SIZE_OF_STRING(dw1));
  /* TODO: test return and throw error */
  (*str)->size  = dw1;
  (*str)->type = DUNA_TYPE_STRING;

  for(i = 0; i < dw1; i++) {
    ret = get_fixnum(f, &dw2);
    if(!ret) {
      free(*str);
      return 0;
    }
    (*str)->chars[i] = dw2;
  }

  return 1;
}

static int load_code_from_file(duna_Module *mod, const char* fname)
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
  dw2 = dw1 * sizeof(duna_String*);
  mod->globals = (duna_String**)malloc(dw2);
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
      duna_destroy_module(mod);
      fclose(f);
      return 0;
    }

    /* retrieve operands if any */
    if(IS_TYPE_B(instr)) {
      ret = get_fixnum(f, &dw1);
      if(!ret) {
	duna_destroy_module(mod);
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

int duna_load_file(duna_State* D, const char *fname)
{
  duna_Module mod;

  /* tries to load code into module */
  if(!load_code_from_file(&mod, fname)) {
    return 0;
  }

  D->pc = duna_link_module(D, &mod);
  duna_destroy_module(&mod);

  /* initial frame on stack with address of halt instruction */
  /* return address */
  (D->stack[D->sp  ]).type = DUNA_TYPE_FIXNUM;
  (D->stack[D->sp++]).value.fixnum = 0;

  /* saved procedure */
  D->stack[D->sp++] = D->proc;

  /* saved frame pointer */
  (D->stack[D->sp  ]).type = DUNA_TYPE_FIXNUM;
  (D->stack[D->sp++]).value.fixnum = D->fp;

  /* number of arguments */
  (D->stack[D->sp  ]).type = DUNA_TYPE_FIXNUM;
  (D->stack[D->sp++]).value.fixnum = 0;

  D->fp = D->sp - 1;

  return duna_vm_run(D);
}


/*
 * entry point
 */

int main(int argc, char *argv[])
{
  duna_State* D;

  if(argc != 2) {
    fprintf(stderr, "%s: Need file to run.\n", argv[0]);
    exit(13);
  }

  D = duna_init();

  /* tries to load initial environment */
  duna_load_file(D, "init.fasl");

  if(!duna_load_file(D, argv[1])) {
    printf("Error!\n");
  }

  duna_close(D);

  return 0;
}

