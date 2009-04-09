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
#define DUNA_TYPE_CLOSURE        5
#define DUNA_TYPE_PAIR           6
#define DUNA_TYPE_CONTINUATION   7
#define DUNA_TYPE_BOX            8

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
  
/* forward type declarations */
typedef struct duna_Object_ duna_Object;
typedef struct duna_GCObject_ duna_GCObject;

/* Duna data types */
struct duna_Object_ {

  /* the runtime type tag */
  uint8_t type;

  /* the value of this object */
  union {
    /* immediates */
    uint8_t bool;
    uint8_t chr;
    uint32_t fixnum;

    /* collectable objects */
    duna_GCObject *gc;
  } value;
};

struct duna_GCObject_ {
  uint8_t flags;

  union {
    struct {
      duna_Object value;
    } box;
    struct {
      uint32_t entry_point;
      duna_Object *free_vars;
    } closure;
    struct {
      duna_Object car;
      duna_Object cdr;
    } pair;
    struct {
      uint32_t size;
      duna_Object *stack;
    } continuation;
  } data;
};

#define DUNA_VISITED(o) (((o).value->gc.flags) & 0x01)

/* the state of the Duna interpreter */
struct duna_State_ {

  /* the bytecode to be interpreted */
  uint32_t *code;

  /* the size of the bytecode vector used */
  uint32_t code_size;

  /* the total capacity of the bytecode vector */
  uint32_t code_capacity;

  /* the program counter */
  uint32_t pc;

  /* accumulator register */
  duna_Object accum;

  /* the machine stack */
  duna_Object *stack;

  /* stack allocated size */
  uint32_t stack_size;

  /* where is the top of the stack */
  uint32_t sp;

  /* the frame pointer */
  uint32_t fp;

  /* the current procedure */
  duna_Object proc;
};

typedef struct duna_State_ duna_State;

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
  case DUNA_TYPE_CLOSURE:
    printf("<#closure %u>", obj->value.gc->data.closure.entry_point);
    break;
  case DUNA_TYPE_PAIR:
    printf("(");
    write_obj(&obj->value.gc->data.pair.car);
    printf(" . ");
    write_obj(&obj->value.gc->data.pair.cdr);
    printf(")");
    break;
  case DUNA_TYPE_CONTINUATION:
    printf("<#continuation %u>", obj->value.gc->data.continuation.size);
    break;
  case DUNA_TYPE_BOX:
    printf("#&");
    write_obj(&obj->value.gc->data.box.value);
    break;
  default:
    printf("Unknown type!");
  }
}

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

duna_State* duna_init(void)
{
  duna_State *D = NULL;

  D = (duna_State*)malloc(sizeof(duna_State));
  if(!D) {
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

  /* current procedure */
  D->proc.type = DUNA_TYPE_BOOL;
  D->proc.value.bool = 0;

  D->accum.type = DUNA_TYPE_NIL;

  return D;
}

void duna_close(duna_State* D)
{
  if(D) {
    if(D->code) {
      free(D->code);
    }
    if(D->stack) {
      free(D->stack);
    }
    free(D);
  }
}

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

      D->accum.type = DUNA_TYPE_CLOSURE;
      D->accum.value.gc = (duna_GCObject*) malloc(sizeof(duna_GCObject));

      /*
       * There is always a jump after this instruction, to jump over the
       * closure code. So the closure entry point is PC + 1
       */
      D->accum.value.gc->data.closure.entry_point = D->pc + 1;

      /* gathering free variables */
      D->accum.value.gc->data.closure.free_vars = (duna_Object*)malloc(dw1 * sizeof(duna_Object));
      for(i = 0; i < dw1; i++) {
	D->accum.value.gc->data.closure.free_vars[i] = D->stack[D->sp-i-1];
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
      D->pc = D->proc.value.gc->data.closure.entry_point;
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
      D->accum = D->proc.value.gc->data.closure.free_vars[EXTRACT_ARG(instr)];
      break;

    case DUNA_OP_SAVE_CONT:
      dw1 = D->sp * sizeof(duna_Object);

      D->accum.type = DUNA_TYPE_CONTINUATION;
      D->accum.value.gc = (duna_GCObject*)malloc(sizeof(duna_GCObject));

      /* copying stack */
      D->accum.value.gc->data.continuation.size = D->sp;
      D->accum.value.gc->data.continuation.stack = (duna_Object*)malloc(dw1);
      memcpy(D->accum.value.gc->data.continuation.stack, D->stack, dw1);

      /* removing number of arguments from the stack */
      D->sp--;
      break;

    case DUNA_OP_REST_CONT:
      /* return value is on the stack */
      tmp = D->stack[--D->sp];

      /* restoring stack */
      D->sp = D->accum.value.gc->data.continuation.size;
      memcpy(D->stack, D->accum.value.gc->data.continuation.stack, D->sp * sizeof(duna_Object));

      D->accum = tmp;
      break;

    case DUNA_OP_ASSIGN:
      assert((D->stack[D->fp-EXTRACT_ARG(instr)-1]).type == DUNA_TYPE_BOX);
      (D->stack[D->fp-EXTRACT_ARG(instr)-1]).value.gc->data.box.value = D->accum;
      break;

    case DUNA_OP_ASSIGN_FREE:
      assert((D->proc.value.gc->data.closure.free_vars[EXTRACT_ARG(instr)]).type == DUNA_TYPE_BOX);
      (D->proc.value.gc->data.closure.free_vars[EXTRACT_ARG(instr)]).value.gc->data.box.value = D->accum;
      break;

    case DUNA_OP_BOX:
      tmp.type = DUNA_TYPE_BOX;
      tmp.value.gc = (duna_GCObject*)malloc(sizeof(duna_GCObject));
      tmp.value.gc->data.box.value = D->accum;

      D->accum = tmp;
      break;

    case DUNA_OP_OPEN_BOX:
      assert(D->accum.type == DUNA_TYPE_BOX);
      D->accum = D->accum.value.gc->data.box.value;
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
      D->pc = D->proc.value.gc->data.closure.entry_point;
      break;

    case DUNA_OP_HALT:
      go_on = 0;
      break;

    case DUNA_OP_LOAD_LOCAL:
      D->accum = D->stack[D->fp+EXTRACT_ARG(instr)+1];
      break;

    case DUNA_OP_INSERT_BOX:
      i = D->fp-EXTRACT_ARG(instr)-1;

      tmp.type = DUNA_TYPE_BOX;
      tmp.value.gc = (duna_GCObject*)malloc(sizeof(duna_GCObject));
      tmp.value.gc->data.box.value = D->stack[i];
      D->stack[i] = tmp;
      break;

    case DUNA_OP_ASSIGN_LOCAL:
      assert((D->stack[D->fp+EXTRACT_ARG(instr)+1]).type == DUNA_TYPE_BOX);
      (D->stack[D->fp+EXTRACT_ARG(instr)+1]).value.gc->data.box.value = D->accum;
      break;

    case DUNA_OP_POP:
      D->sp -= EXTRACT_ARG(instr);
      break;

    case DUNA_OP_CONS:
      tmp = D->accum;
      D->accum.type = DUNA_TYPE_PAIR;
      D->accum.value.gc = (duna_GCObject*) malloc(sizeof(duna_GCObject));
      D->accum.value.gc->data.pair.car = tmp;
      D->accum.value.gc->data.pair.cdr = D->stack[--D->sp];
      break;

    case DUNA_OP_CAR:
      D->accum = D->accum.value.gc->data.pair.car;
      break;

    case DUNA_OP_CDR:
      D->accum = D->accum.value.gc->data.pair.cdr;
      break;
    }
  }

  return 1;
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

int main(int argc, char *argv[])
{
  duna_State* D;

  D = duna_init();
  if(!duna_load_file(D, argv[1])) {
    printf("Error!\n");
  }

  duna_close(D);

  return 0;
}

