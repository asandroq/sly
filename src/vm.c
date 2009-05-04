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
typedef unsigned char uint8_t;
typedef unsigned int  uint32_t;
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
#define DUNA_OP_LOAD_NIL          1
#define DUNA_OP_LOAD_FALSE        2
#define DUNA_OP_LOAD_TRUE         3
#define DUNA_OP_LOAD_ZERO         4
#define DUNA_OP_LOAD_ONE          5
#define DUNA_OP_LOAD_FIXNUM       6
#define DUNA_OP_LOAD_CHAR         7
#define DUNA_OP_PUSH              8
#define DUNA_OP_LOAD_0            9
#define DUNA_OP_LOAD_1           10
#define DUNA_OP_LOAD_2           11
#define DUNA_OP_LOAD_3           12
#define DUNA_OP_LOAD             13
#define DUNA_OP_MAKE_CLOSURE     14
#define DUNA_OP_CALL             15
#define DUNA_OP_RETURN           16
#define DUNA_OP_JMP_IF           17
#define DUNA_OP_JMP              18
#define DUNA_OP_LOAD_FREE        19
#define DUNA_OP_SAVE_CONT        20
#define DUNA_OP_REST_CONT        21
#define DUNA_OP_ASSIGN           22
#define DUNA_OP_ASSIGN_FREE      23
#define DUNA_OP_BOX              24
#define DUNA_OP_OPEN_BOX         25
#define DUNA_OP_FRAME            26
#define DUNA_OP_TAIL_CALL        27
#define DUNA_OP_HALT             28
#define DUNA_OP_LOAD_LOCAL       29
#define DUNA_OP_INSERT_BOX       30
#define DUNA_OP_ASSIGN_LOCAL     31
#define DUNA_OP_POP              32

/* type predicates */
#define DUNA_OP_NULL_P           81
#define DUNA_OP_BOOL_P           82
#define DUNA_OP_CHAR_P           83
#define DUNA_OP_FIXNUM_P         84

/* primitives optimised as instructions */
#define DUNA_OP_INC              101
#define DUNA_OP_DEC              102
#define DUNA_OP_FIXNUM_TO_CHAR   103
#define DUNA_OP_CHAR_TO_FIXNUM   104
#define DUNA_OP_ZERO_P           105
#define DUNA_OP_NOT              106
#define DUNA_OP_PLUS             107
#define DUNA_OP_MINUS            108
#define DUNA_OP_MULT             109
#define DUNA_OP_CONS             110
#define DUNA_OP_CAR              111
#define DUNA_OP_CDR              112

/*
 * data types tags
 */
#define DUNA_TYPE_NIL            1
#define DUNA_TYPE_BOOL           2
#define DUNA_TYPE_FIXNUM         3
#define DUNA_TYPE_CHAR           4
#define DUNA_TYPE_SYMBOL         5
#define DUNA_TYPE_CLOSURE        6
#define DUNA_TYPE_PAIR           7
#define DUNA_TYPE_CONTINUATION   8
#define DUNA_TYPE_BOX            9

#define IS_TYPE_B(instr) \
   ((instr) == DUNA_OP_LOAD_FIXNUM ||  \
    (instr) == DUNA_OP_LOAD_CHAR ||    \
    (instr) == DUNA_OP_MAKE_CLOSURE || \
    (instr) == DUNA_OP_JMP_IF ||       \
    (instr) == DUNA_OP_JMP ||          \
    (instr) == DUNA_OP_LOAD_FREE ||    \
    (instr) == DUNA_OP_ASSIGN_FREE ||  \
    (instr) == DUNA_OP_FRAME ||        \
    (instr) == DUNA_OP_INSERT_BOX ||   \
    (instr) == DUNA_OP_ASSIGN_LOCAL || \
    (instr) == DUNA_OP_LOAD ||         \
    (instr) == DUNA_OP_ASSIGN ||       \
    (instr) == DUNA_OP_LOAD_LOCAL ||   \
    (instr) == DUNA_OP_POP)

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
  {0, NULL}
};

/*
 * Data types
 */

/* forward type declarations */
typedef struct duna_Object_ duna_Object;
typedef struct duna_GCObject_ duna_GCObject;

/*
 * entry in the symbol table
 * symbols are not collected
 * some hash functions:
 * http://burtleburtle.net/bob/c/lookup3.c
 */
typedef struct duna_Symbol_ duna_Symbol;

struct duna_Symbol_ {
  /* symbol textual representation */
  uint8_t *str;

  /* next symbol in chain */
  duna_Symbol *next;
};

/* value types */
struct duna_Object_ {

  /* the runtime type tag */
  uint8_t type;

  /* the value of this object */
  union {
    /* immediates */
    uint8_t bool;
    uint8_t chr;
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

typedef struct duna_Box_ duna_Box;

struct duna_Closure_ {
  DUNA_GC_BASE;
  uint32_t entry_point;
  uint32_t nr_free;
  duna_Object free_vars[1];
};

typedef struct duna_Closure_ duna_Closure;

struct duna_Pair_ {
  DUNA_GC_BASE;
  duna_Object car;
  duna_Object cdr;  
};

typedef struct duna_Pair_ duna_Pair;

struct duna_Continuation_ {
  DUNA_GC_BASE;
  uint32_t size;
  duna_Object stack[1];
};

typedef struct duna_Continuation_ duna_Continuation;

/*
 * the store, i.e., memory
 */

#define DUNA_INITIAL_SPACE_SIZE     ((uint32_t)(1 << 8))
#define DUNA_IMMEDIATE_P(o)         ((o)->type < DUNA_TYPE_CLOSURE)
#define DUNA_FORWARD_TAG            199

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
    size = ((duna_Closure*)obj)->nr_free;
    size = sizeof(duna_Closure) +
           (size == 0 ? 0 : size - 1) * sizeof(duna_Object);
    break;

  case DUNA_TYPE_PAIR:
    size = sizeof(duna_Pair);
    break;

  case DUNA_TYPE_CONTINUATION:
    size = sizeof(duna_Continuation) +
           (((duna_Continuation*)obj)->size-1) * sizeof(duna_Object);
    break;

  case DUNA_TYPE_BOX:
    size = sizeof(duna_Box);
    break;
  default:
    size = 0;
  };

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

    gcobj = scan;
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
    };

    scan += size;
  }

  /* swap spaces */
  scan = S->from_space;
  S->from_space = S->to_space;
  S->to_space = scan;

  printf("GC: before: %u after: %u\n", old_size, S->size);
}

static int expand_store(duna_Store* S)
{
  void *tmp;
  uint32_t old_size, size;

  old_size = S->capacity;

  /* new size is 30% larger, multiple of 4 */
  size = old_size * 4 / 3;
  size += size % 4;
  fprintf(stderr, "Expanding store from %u to %u\n", old_size, size);
  
  tmp = realloc(S->os_address, size * 2);
  if(tmp == NULL) {
    return 0;
  } else {
    if(S->os_address == S->to_space) {
      /* argh */
      memcpy(S->from_space, S->to_space, S->size);
    }
    S->capacity = size;
    S->os_address = tmp;
    S->from_space = tmp;
    S->to_space = tmp + size;
    return 1;
  }
}

static void* alloc_from_store(duna_Store *S, uint32_t size)
{
  void *ret;

  if(S->size + size > S->capacity) {
    /* not enough space, try to find some */
    collect_garbage(S);

    while(S->size + size > S->capacity) {
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

  ret = (duna_Box*)alloc_from_store(S, sizeof(duna_Box));
  if(ret) {
    ret->type = DUNA_TYPE_BOX;
  }

  return ret;
}

static duna_Closure *alloc_closure(duna_Store *S, uint32_t nr_vars)
{
  uint32_t size;
  duna_Closure *ret;

  size = sizeof(duna_Closure) +
         (nr_vars == 0 ? 0 : (nr_vars-1)) * sizeof(duna_Object);

  ret = (duna_Closure*) alloc_from_store(S, size);
  if(ret) {
    ret->type = DUNA_TYPE_CLOSURE;
    ret->nr_free = nr_vars;
  }

  return ret;
}

static duna_Pair *alloc_pair(duna_Store *S)
{
  duna_Pair *ret;

  ret = (duna_Pair*) alloc_from_store(S, sizeof(duna_Pair));
  if(ret) {
    ret->type = DUNA_TYPE_PAIR;
  }

  return ret;
}

static duna_Continuation *alloc_continuation(duna_Store *S, uint32_t stack_size)
{
  uint32_t size;
  duna_Continuation *ret;

  size = sizeof(duna_Continuation) + (stack_size-1) * sizeof(duna_Object);
  ret = (duna_Continuation*)alloc_from_store(S, size);
  if(ret) {
    ret->type = DUNA_TYPE_CONTINUATION;
    ret->size = stack_size;
  }

  return ret;
}

/*
 * the virtual machine
 */

/* an entry in a environment */
struct duna_Env_Item_ {
  /* entry in the symbol table */
  duna_Symbol *symbol;

  /* the actual value */
  duna_Object value;
};

typedef struct duna_Env_Item_ duna_Env_Item;

/* the state of the Duna interpreter */
typedef struct duna_State_ duna_State;

struct duna_State_ {

  /* the size of the bytecode vector used */
  uint32_t code_size;

  /* the total capacity of the bytecode vector */
  uint32_t code_capacity;

  /* stack allocated size */
  uint32_t stack_size;

  /* size of global environment */
  uint32_t global_env_size;

  /* where is the top of the stack */
  uint32_t sp;

  /* the frame pointer */
  uint32_t fp;

  /* the program counter */
  uint32_t pc;

  /* the bytecode to be interpreted */
  uint32_t *code;

  /* the machine stack */
  duna_Object *stack;

  /* global environment */
  duna_Env_Item *global_env;

  /* accumulator register */
  duna_Object accum;

  /* the current procedure */
  duna_Object proc;

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
      if(gc_data->count == gc_data->D->global_env_size) {
	gc_data->state++;
	gc_data->count = 0;
	continue;
      } else {
	return &gc_data->D->global_env[gc_data->count++].value;
      }
    } else if(gc_data->state == 2) {
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
  D->code_size = 0;
  D->code = (uint32_t*)malloc(sizeof(uint32_t) * 8192);
  if(D->code) {
    D->code_capacity = 8192;
  } else {
    free(D);
    return NULL;
  }
  /* instruction to halt the machine always at address 0 */
  D->code[0] = (uint32_t) DUNA_OP_HALT;
  D->code_size = 1;

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
  /* initial frame on stack with address of halt instruction */
  (D->stack[0]).type = DUNA_TYPE_FIXNUM;
  (D->stack[0]).value.fixnum = 0;
  (D->stack[1]).type = DUNA_TYPE_BOOL;
  (D->stack[1]).value.bool = 0;
  (D->stack[2]).type = DUNA_TYPE_FIXNUM;
  (D->stack[2]).value.fixnum = 0;
  (D->stack[3]).type = DUNA_TYPE_FIXNUM;
  (D->stack[3]).value.fixnum = 0;
  D->sp = 4;
  D->fp = 3;

  /* globals */
  D->global_env = NULL;
  D->global_env_size = 0;

  /* current procedure */
  D->proc.type = DUNA_TYPE_BOOL;
  D->proc.value.bool = 0;

  D->accum.type = DUNA_TYPE_NIL;

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
    free(D);
  }
}

/*
 * writer
 */

static void write_obj(duna_Object* obj)
{
  switch(obj->type) {
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
    printf("%s", obj->value.symbol->str);
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

    case DUNA_OP_CALL:
      /*
       * setting frame pointer
       */
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
      memcpy(D->stack+i, D->stack+j, (dw1+1) * sizeof(duna_Object));

      /*
       * setting stack and frame pointer
       */
      D->sp = i + dw1 + 1;
      D->fp = D->sp - 1;
      
      /* setting current procedure */
      D->proc = D->accum;
      
      /* jumping to closure body */
      D->pc = ((duna_Closure*)D->proc.value.gc)->entry_point;
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
    }
  }

  return 1;
}

/*
 * loading
 */

static int get_next(FILE* f, uint32_t *next)
{
  int ret;

  ret = fscanf(f, " %u", next);
  if(ret == EOF) {
    return -1;
  } else if(ret == 0) {
    /* maybe got to end? */
    ret = fscanf(f, ")");
    if(ret == EOF) {
      return -1;
    } else {
      return 0;
    }
  } else {
    return 1;
  }
}

static int get_fixnum(FILE* f, uint32_t *num)
{
  int ret;
  uint32_t b1, b2, b3, b4;

  ret = get_next(f, &b1);
  if(ret <= 0) {
    return -1;
  }
  ret = get_next(f, &b2);
  if(ret <= 0) {
    return -1;
  }
  ret = get_next(f, &b3);
  if(ret <= 0) {
    return -1;
  }
  ret = get_next(f, &b4);
  if(ret <= 0) {
    return -1;
  }

  *num = b1 | (b2 << 8) | (b3 << 16) | (b4 << 24);

  return 1;
}

static int load_code_from_file(duna_State* D, const char* fname)
{
  FILE *f;
  uint32_t pc;

  /* opening input file */
  f = fopen(fname, "r");
  if(!f) {
    return -1;
  }

  /* bytecode beginning */
  if(fscanf(f, " #(") == EOF) {
    fclose(f);
    return -1;
  }

  pc = D->code_size;

  /* reading actual code */
  while(1) {
    int ret;
    uint32_t instr, dw1;

    ret = get_next(f, &instr);
    if(ret == -1) {
      /* unexpected end */
      fclose(f);
      return -1;
    }
    if(ret == 0 || instr == 0) {
      /* real end */
      return pc;
    }

    /* retrieve operands if any */
    if(IS_TYPE_B(instr)) {
      ret = get_fixnum(f, &dw1);
      if(ret < 0) {
	fclose(f);
	return -1;
      }

      /*
       * if instruction is FRAME, the return address
       * encoded must be patched to account for
       * the current code offset
       */
      if(instr == DUNA_OP_FRAME) {
	dw1 += pc;
      }

      instr |= (dw1 << 8);

    }

    /* does the code vector has space? */
    if(D->code_size == D->code_capacity) {
      uint32_t *code, size;

      size = D->code_size * 3 / 2;
      code = (uint32_t*)realloc(D->code, size);
      if(!code) {
	fclose(f);
	return -1;
      }

      D->code = code;
      D->code_capacity = size;
    }

    /* adds new read byte to code vector */
    D->code[D->code_size++] = instr;
  }
}

int duna_load_file(duna_State* D, const char *fname)
{
  int pc;

  /* tries to load code into state */
  pc = load_code_from_file(D, fname);
  if(pc < 0) {
    return 0;
  }

  D->pc = pc;

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
  if(!duna_load_file(D, argv[1])) {
    printf("Error!\n");
  }

  duna_close(D);

  return 0;
}

