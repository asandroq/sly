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

#include "sly.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "gc.h"
#include "io.h"
#include "object.h"

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

struct sly_State {

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
  if(sly_gc_init(&D->store, gc_callback, &gc_data) == 0) {
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

void sly_close(sly_State* S)
{
  if(S) {
    sly_gc_finish(&S->store);

    if(S->code) {
      free(S->code);
    }

    if(S->stack) {
      free(S->stack);
    }

    if(S->symbol_table) {
      sly_Symbol *sym;
      for(sym = S->symbol_table; sym != NULL;) {
	sly_Symbol *tmp = sym->next;
	free(sym->str);
	free(sym);
	sym = tmp;
      }
    }

    free(S);
  }
}

static int string_equal_p(sly_String *s1, sly_String *s2)
{
  if(s1->size != s2->size) {
    return 0;
  }

  return memcmp(s1->chars, s2->chars, s1->size * sizeof(sly_Char)) == 0;
}

static sly_String* string_copy(sly_State* S, sly_String* s)
{
  sly_String *ret;

  ret = sly_gc_alloc_string(&S->store, s->size);
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

static sly_Object sly_make_symbol(sly_State* S, sly_String *str)
{
  sly_Object obj;
  sly_Symbol *tmp;

  /* is the symbol already there? */
  for(tmp = S->symbol_table; tmp != NULL; tmp = tmp->next) {
    if(string_equal_p(tmp->str, str)) {
      break;
    }
  }

  if(tmp == NULL) {
    /* adding new symbol */
    tmp = (sly_Symbol*)malloc(sizeof(sly_Symbol));
    tmp->str = string_copy_extern(str);
    tmp->next = S->symbol_table;
    S->symbol_table = tmp;
  }

  obj.type = SLY_TYPE_SYMBOL;
  obj.value.symbol = tmp;

  return obj;
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

static void disassemble(sly_State* S)
{
  uint32_t i;

  printf("Code listing:\n");
  for(i = 0; i < S->code_size; i++) {
    printf("\t%u\t", i);
    dump_instr(S->code[i]);
    printf("\n");
  }
}

void sly_dump(sly_State* S)
{
  uint32_t i;

  printf("Instruction: ");
  dump_instr(S->code[S->pc]);
  printf("\n");

  printf("Registers:\n");
  printf("\taccum: "); sly_io_write(&S->accum); printf("\n");
  printf("\tclosure: "); sly_io_write(&S->proc); printf("\n");
  printf("\tPC: %d\n", S->pc);
  printf("\tFP: %d\n", S->fp);

  printf("Stack:");
  for(i = 0; i < S->sp; i++) {
    printf(" ");
    sly_io_write(S->stack + i);
  }
#if 0
  printf("\n\n");

  printf("Globals:");
  for(i = 0; i < S->global_env.size; i++) {
    sly_Env_Var var = S->global_env.vars[i];
    printf(" [");
    if(var.symbol) {
      write_string(var.symbol->str, 0);
    }
    printf(" . ");
    sly_io_write(&var.value);
    printf("]");
  }
  printf("\n\n");

  printf("Constants:");
  for(i = 0; i < S->nr_consts; i++) {
    printf(" ");
    sly_io_write(S->consts + i);
  }
#endif
  printf("\n\n");
}

static void sly_abort(sly_State *S)
{
  sly_dump(S);

  sly_close(S);
  abort();
}

static void check_alloc(sly_State *S, void* ptr)
{
  if(ptr == NULL) {
    fprintf(stderr, "sly: Out of memory!\n");
    sly_abort(S);
  }
}

#define SLY_SET_BOOL(cond)		\
  do {					\
    if(cond) {				\
      S->accum.type = SLY_TYPE_BOOL;	\
      S->accum.value.bool = 1;		\
    } else {				\
      S->accum.type = SLY_TYPE_BOOL;	\
      S->accum.value.bool = 0;		\
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
int sly_vm_run(sly_State* S)
{
  int go_on = 1, debug = 0;

  /*disassemble(S);*/

  while(go_on) {
    sly_Object tmp;
    register uint32_t instr;
    uint32_t i, j, dw1, dw2;

    if(debug) {
      sly_dump(S);
      getchar();
    }
    assert(S->pc < S->code_size);

    instr = S->code[S->pc++];

    switch(EXTRACT_OP(instr)) {

    case SLY_OP_LOAD_NIL:
      S->accum.type = SLY_TYPE_NIL;
      break;

    case SLY_OP_LOAD_FALSE:
      S->accum.type = SLY_TYPE_BOOL;
      S->accum.value.bool = 0;
      break;

    case SLY_OP_LOAD_TRUE:
      S->accum.type = SLY_TYPE_BOOL;
      S->accum.value.bool = 1;
      break;

    case SLY_OP_LOAD_ZERO:
      S->accum.type = SLY_TYPE_FIXNUM;
      S->accum.value.fixnum = 0;
      break;

    case SLY_OP_LOAD_ONE:
      S->accum.type = SLY_TYPE_FIXNUM;
      S->accum.value.fixnum = 1;
      break;

    case SLY_OP_LOAD_FIXNUM:
      S->accum.type = SLY_TYPE_FIXNUM;
      S->accum.value.fixnum = EXTRACT_ARG(instr);
      break;

    case SLY_OP_LOAD_CHAR:
      S->accum.type = SLY_TYPE_CHAR;
      S->accum.value.chr = (sly_Char) EXTRACT_ARG(instr);
      break;

    case SLY_OP_INC:
      S->accum.value.fixnum++;
      break;

    case SLY_OP_DEC:
      S->accum.value.fixnum--;
      break;

    case SLY_OP_FIXNUM_TO_CHAR:
      S->accum.type = SLY_TYPE_CHAR;
      S->accum.value.chr = (sly_Char) S->accum.value.fixnum;
      break;

    case SLY_OP_CHAR_TO_FIXNUM:
      S->accum.type = SLY_TYPE_FIXNUM;
      S->accum.value.fixnum = (uint32_t) S->accum.value.chr;
      break;

    case SLY_OP_NULL_P:
      SLY_SET_BOOL(S->accum.type == SLY_TYPE_NIL);
      break;

    case SLY_OP_ZERO_P:
      SLY_SET_BOOL(S->accum.value.fixnum == 0);
      break;

    case SLY_OP_NOT:
      SLY_SET_BOOL(S->accum.type == SLY_TYPE_BOOL &&
		    S->accum.value.bool == 0);
      break;

    case SLY_OP_BOOL_P:
      SLY_SET_BOOL(S->accum.type == SLY_TYPE_BOOL);
      break;

    case SLY_OP_CHAR_P:
      SLY_SET_BOOL(S->accum.type == SLY_TYPE_CHAR);
      break;

    case SLY_OP_FIXNUM_P:
      SLY_SET_BOOL(S->accum.type == SLY_TYPE_FIXNUM);
      break;

    case SLY_OP_PAIR_P:
      SLY_SET_BOOL(S->accum.type == SLY_TYPE_PAIR);
      break;

    case SLY_OP_PUSH:
      S->stack[S->sp++] = S->accum;
      break;

    case SLY_OP_PLUS:
      S->accum.value.fixnum =
	(S->stack[--S->sp]).value.fixnum + S->accum.value.fixnum;
      break;

    case SLY_OP_MINUS:
      S->accum.value.fixnum =
	(S->stack[--S->sp]).value.fixnum - S->accum.value.fixnum;
      break;

    case SLY_OP_MULT:
      S->accum.value.fixnum =
	(S->stack[--S->sp]).value.fixnum * S->accum.value.fixnum;
      break;

    case SLY_OP_LOAD_0:
      S->accum = S->stack[S->fp-1];
      break;

    case SLY_OP_LOAD_1:
      S->accum = S->stack[S->fp-2];
      break;

    case SLY_OP_LOAD_2:
      S->accum = S->stack[S->fp-3];
      break;

    case SLY_OP_LOAD_3:
      S->accum = S->stack[S->fp-4];
      break;

    case SLY_OP_LOAD:
      S->accum = S->stack[S->fp-EXTRACT_ARG(instr)-1];
      break;

    case SLY_OP_MAKE_CLOSURE:
      /* number of free variables */
      dw1 = EXTRACT_ARG(instr);

      tmp.type = SLY_TYPE_CLOSURE;
      tmp.value.gc = (sly_GCObject*) sly_gc_alloc_closure(&S->store, dw1);
      check_alloc(S, tmp.value.gc);
      S->accum = tmp;

      /*
       * There is always a jump after this instruction, to jump over the
       * closure code. So the closure entry point is PC + 1
       */
      ((sly_Closure*)S->accum.value.gc)->entry_point = S->pc + 1;

      /* gathering free variables */
      for(i = 0; i < dw1; i++) {
	((sly_Closure*)S->accum.value.gc)->free_vars[i] = S->stack[S->sp-i-1];
      }
      S->sp -= dw1;
      break;

    case SLY_OP_TAIL_CALL:
      /*
       * the arguments to the callee must be shifted down
       * removing the arguments of the caller
       */
      /* number of arguments newly pushed */
      dw1 = (S->stack[S->sp-1]).value.fixnum;

      /* number of arguments of old procedure */
      dw2 = (S->stack[S->fp]).value.fixnum;

      /* where to copy the new values */
      i = S->fp - dw2;

      /* where the values will be copied from */
      j = S->sp - dw1 - 1;

      S->sp = i + dw1 + 1;
      memcpy(S->stack+i, S->stack+j, (dw1+1) * sizeof(sly_Object));

      /* fall through */

    case SLY_OP_CALL:
      if(S->accum.type != SLY_TYPE_CLOSURE) {
	sly_abort(S);
      }

      /* frame pointer */
      S->fp = S->sp - 1;

      /* setting current procedure */
      S->proc = S->accum;

      /* jumping to closure body */
      S->pc = ((sly_Closure*)S->proc.value.gc)->entry_point;
      break;

    case SLY_OP_RETURN:
      /* removing number of arguments */
      dw1 = (S->stack[--S->sp]).value.fixnum;

      /* removing arguments */
      S->sp -= dw1;

      /* restoring previous frame pointer */
      S->fp = (S->stack[--S->sp]).value.fixnum;

      /* restoring previous procedure */
      S->proc = S->stack[--S->sp];

      /* jumping to return address */
      S->pc = (S->stack[--S->sp]).value.fixnum;
      break;

    case SLY_OP_JMP_IF:
      if(!(S->accum.type == SLY_TYPE_BOOL && S->accum.value.bool == 0)) {
	S->pc += EXTRACT_ARG(instr);
      }
      break;

    case SLY_OP_JMP:
      S->pc += EXTRACT_ARG(instr);
      break;

    case SLY_OP_LOAD_FREE:
      S->accum = ((sly_Closure*)S->proc.value.gc)->free_vars[EXTRACT_ARG(instr)];
      break;

    case SLY_OP_SAVE_CONT:
      dw1 = S->sp * sizeof(sly_Object);

      tmp.type = SLY_TYPE_CONTINUATION;
      tmp.value.gc = (sly_GCObject*) sly_gc_alloc_continuation(&S->store, S->sp);
      check_alloc(S, tmp.value.gc);
      S->accum = tmp;

      /* copying stack */
      memcpy(((sly_Continuation*)S->accum.value.gc)->stack, S->stack, dw1);

      /* removing number of arguments from the stack */
      S->sp--;
      break;

    case SLY_OP_REST_CONT:
      /* return value is on the stack */
      tmp = S->stack[--S->sp];

      /* restoring stack */
      S->sp = ((sly_Continuation*)S->accum.value.gc)->size;
      memcpy(S->stack, ((sly_Continuation*)S->accum.value.gc)->stack, S->sp * sizeof(sly_Object));

      S->accum = tmp;
      break;

    case SLY_OP_ASSIGN:
      assert((S->stack[S->fp-EXTRACT_ARG(instr)-1]).type == SLY_TYPE_BOX);
      ((sly_Box*)(S->stack[S->fp-EXTRACT_ARG(instr)-1]).value.gc)->value = S->accum;
      break;

    case SLY_OP_ASSIGN_FREE:
      assert((((sly_Closure*)S->proc.value.gc)->free_vars[EXTRACT_ARG(instr)]).type == SLY_TYPE_BOX);
      ((sly_Box*)(((sly_Closure*)S->proc.value.gc)->free_vars[EXTRACT_ARG(instr)]).value.gc)->value = S->accum;
      break;

    case SLY_OP_BOX:
      tmp.type = SLY_TYPE_BOX;
      tmp.value.gc = (sly_GCObject*) sly_gc_alloc_box(&S->store);
      check_alloc(S, tmp.value.gc);

      ((sly_Box*)tmp.value.gc)->value = S->accum;
      S->accum = tmp;
      break;

    case SLY_OP_OPEN_BOX:
      assert(S->accum.type == SLY_TYPE_BOX);
      S->accum = ((sly_Box*)S->accum.value.gc)->value;
      break;

    case SLY_OP_FRAME:
      /* pushing return address */
      (S->stack[S->sp  ]).type = SLY_TYPE_FIXNUM;
      (S->stack[S->sp++]).value.fixnum = EXTRACT_ARG(instr);

      /* pushing current procedure */
      S->stack[S->sp++] = S->proc;

      /* pushing frame pointer */
      (S->stack[S->sp  ]).type = SLY_TYPE_FIXNUM;
      (S->stack[S->sp++]).value.fixnum = S->fp;
      break;

    case SLY_OP_HALT:
      sly_io_write(&S->accum);
      printf("\n");
      go_on = 0;
      break;

    case SLY_OP_LOAD_LOCAL:
      S->accum = S->stack[S->fp+EXTRACT_ARG(instr)+1];
      break;

    case SLY_OP_INSERT_BOX:
      i = S->fp-EXTRACT_ARG(instr)-1;

      tmp.type = SLY_TYPE_BOX;
      tmp.value.gc = (sly_GCObject*) sly_gc_alloc_box(&S->store);
      check_alloc(S, tmp.value.gc);

      ((sly_Box*)tmp.value.gc)->value = S->stack[i];
      S->stack[i] = tmp;
      break;

    case SLY_OP_ASSIGN_LOCAL:
      assert((S->stack[S->fp+EXTRACT_ARG(instr)+1]).type == SLY_TYPE_BOX);
      ((sly_Box*)(S->stack[S->fp+EXTRACT_ARG(instr)+1]).value.gc)->value = S->accum;
      break;

    case SLY_OP_POP:
      S->sp -= EXTRACT_ARG(instr);
      break;

    case SLY_OP_CHECKED_GLOBAL_REF:
      if(S->global_env.vars[EXTRACT_ARG(instr)].value.type == SLY_TYPE_UNDEF) {
	printf("Undefined global referenced: ");
	/*write_string(S->global_env.vars[EXTRACT_ARG(instr)].symbol->str, 0);*/
	printf("\n");
	sly_abort(S);
      }
      /* fall through */

    case SLY_OP_GLOBAL_REF:
      S->accum = S->global_env.vars[EXTRACT_ARG(instr)].value;
      break;

    case SLY_OP_CHECKED_GLOBAL_SET:
      if(S->global_env.vars[EXTRACT_ARG(instr)].value.type == SLY_TYPE_UNDEF) {
	printf("Undefined global assigned: ");
	/*write_string(S->global_env.vars[EXTRACT_ARG(instr)].symbol->str, 0);*/
	printf("\n");
	sly_abort(S);
      }
      /* fall through */

    case SLY_OP_GLOBAL_SET:
      S->global_env.vars[EXTRACT_ARG(instr)].value = S->accum;
      break;

    case SLY_OP_LOAD_UNDEF:
      S->accum.type = SLY_TYPE_UNDEF;
      break;

    case SLY_OP_CONST:
      S->accum = S->consts[EXTRACT_ARG(instr)];
      break;

    case SLY_OP_CONST_INIT:
      S->consts[EXTRACT_ARG(instr)] = S->accum;
      break;

    case SLY_OP_ARITY_EQ:
      if(S->stack[S->fp].value.fixnum != EXTRACT_ARG(instr)) {
	printf("Arity mismatch!\n");
	sly_abort(S);
      }
      break;

    case SLY_OP_ARITY_GE:
      if(S->stack[S->fp].value.fixnum < EXTRACT_ARG(instr)) {
	printf("Variable arity mismatch!\n");
	sly_abort(S);
      }
      break;

    case SLY_OP_LISTIFY:
      /* number of fixed arguments */
      dw1 = EXTRACT_ARG(instr);

      /* number of variables arguments */
      dw2 = S->stack[S->fp].value.fixnum - dw1;

      /* consing */
      S->accum.type = SLY_TYPE_NIL;
      for(i = S->fp - 1; i > S->fp - (dw2 + 1); i--) {
	tmp.type = SLY_TYPE_PAIR;
	tmp.value.gc = (sly_GCObject*)sly_gc_alloc_pair(&S->store);
	check_alloc(S, tmp.value.gc);

	((sly_Pair*)tmp.value.gc)->car = S->stack[i];
	((sly_Pair*)tmp.value.gc)->cdr = S->accum;

	S->accum = tmp;
      }

      /* adjusting stack */
      S->stack[S->fp - dw2] = S->accum;
      S->fp -= dw2 - 1;
      S->stack[S->fp].type = SLY_TYPE_FIXNUM;
      S->stack[S->fp].value.fixnum = dw1 + 1;
      S->sp = S->fp + 1;
      break;

    case SLY_OP_ABORT:
      sly_abort(S);
      break;

    case SLY_OP_CONS:
      tmp.type = SLY_TYPE_PAIR;
      tmp.value.gc = (sly_GCObject*) sly_gc_alloc_pair(&S->store);
      check_alloc(S, tmp.value.gc);

      ((sly_Pair*)tmp.value.gc)->car = S->stack[--S->sp];
      ((sly_Pair*)tmp.value.gc)->cdr = S->accum;
      S->accum = tmp;
      break;

    case SLY_OP_CAR:
      S->accum = ((sly_Pair*)S->accum.value.gc)->car;
      break;

    case SLY_OP_CDR:
      S->accum = ((sly_Pair*)S->accum.value.gc)->cdr;
      break;

    case SLY_OP_NUM_EQ:
      /* TODO: throw error if args are not numbers */
      SLY_SET_BOOL(S->accum.value.fixnum == S->stack[--S->sp].value.fixnum);
      break;

    case SLY_OP_EQ:
      SLY_SET_BOOL(S->accum.type == S->stack[S->sp-1].type &&
		    S->accum.value.symbol == S->stack[S->sp-1].value.symbol);
      --S->sp;
      break;

    case SLY_OP_EQV:
      SLY_SET_BOOL(S->accum.type == S->stack[S->sp-1].type &&
		    S->accum.value.symbol == S->stack[S->sp-1].value.symbol);
      --S->sp;
      break;

    case SLY_OP_MAKE_STRING:
      /* string size */
      dw1 = S->accum.value.fixnum;

      tmp.type = SLY_TYPE_STRING;
      tmp.value.gc = (sly_GCObject*)sly_gc_alloc_string(&S->store, dw1);
      check_alloc(S, tmp.value.gc);

      S->accum = tmp;
      break;

    case SLY_OP_STRING_SET:
      dw1 = S->accum.value.chr;
      dw2 = S->stack[--S->sp].value.fixnum;
      S->accum = S->stack[--S->sp];
      ((sly_String*)S->accum.value.gc)->chars[dw2] = dw1;
      break;

    case SLY_OP_STRING_TO_SYMBOL:
      tmp = sly_make_symbol(S, (sly_String*)S->accum.value.gc);
      S->accum = tmp;
      break;

    case SLY_OP_MAKE_VECTOR:
      /* vector size */
      dw1 = S->accum.value.fixnum;

      tmp.type = SLY_TYPE_VECTOR;
      tmp.value.gc = (sly_GCObject*)sly_gc_alloc_vector(&S->store, dw1);
      check_alloc(S, tmp.value.gc);

      S->accum = tmp;
      break;

    case SLY_OP_VECTOR_SET:
      tmp = S->accum;
      dw1 = S->stack[--S->sp].value.fixnum;
      S->accum = S->stack[--S->sp];
      ((sly_Vector*)S->accum.value.gc)->data[dw1] = tmp;    
      break;

    case SLY_OP_WRITE:
      sly_io_write(&S->accum);
      break;

    case SLY_OP_DEBUG:
      if(S->accum.value.bool == 0) {
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

static uint32_t sly_link_module(sly_State* S, sly_Module *mod)
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
    obj = sly_make_symbol(S, mod->globals[i]);
    env.vars[i].symbol = obj.value.symbol;
    env.vars[i].value.type = SLY_TYPE_FIXNUM;

    for(j = 0; j < S->global_env.size; j++) {
      if(env.vars[i].symbol == S->global_env.vars[j].symbol) {
	env.vars[i].value.value.fixnum = j;
	break;
      }
    }

    if(j == S->global_env.size) {
      env.vars[i].value.value.fixnum = S->global_env.size + growth++;
    }
  }

  /* enlarging global environment */
  if(growth > 0) {
    dw = S->global_env.size + growth;
    vars = (sly_Env_Var*)realloc(S->global_env.vars,
				  dw * sizeof(sly_Env_Var));
    /* TODO: test return and throw error */
    S->global_env.vars = vars;

    for(i = 0; i < env.size; i++) {
      j = env.vars[i].value.value.fixnum;
      if(!(j < S->global_env.size)) {
	S->global_env.vars[j].symbol = env.vars[i].symbol;
	S->global_env.vars[j].value.type = SLY_TYPE_UNDEF;
      }
    }
    S->global_env.size = dw;
  }

  /* enlarging constants */
  consts_base = S->nr_consts;
  dw = S->nr_consts + mod->nr_consts;
  tmp = (sly_Object*)realloc(S->consts, dw * sizeof(sly_Object));
  /* TODO: test return and throw error */
  S->consts = tmp;
  for(i = S->nr_consts; i < dw; i++) {
    S->consts[i].type = SLY_TYPE_UNDEF;
  }
  S->nr_consts = dw;

  /* enlarging code */
  code_base = S->code_size;
  dw = S->code_size + mod->code_size;
  code = (uint32_t*)realloc(S->code, dw * sizeof(uint32_t));
  /* TODO: test return and throw error */
  S->code_size = dw;
  S->code = code;

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

    S->code[code_base+i] = instr;
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

int sly_load_file(sly_State* S, const char *fname)
{
  sly_Module mod;

  /* tries to load code into module */
  if(!load_code_from_file(&mod, fname)) {
    return 0;
  }

  S->pc = sly_link_module(S, &mod);
  sly_destroy_module(&mod);

  /* initial frame on stack with address of halt instruction */
  /* return address */
  (S->stack[S->sp  ]).type = SLY_TYPE_FIXNUM;
  (S->stack[S->sp++]).value.fixnum = 0;

  /* saved procedure */
  S->stack[S->sp++] = S->proc;

  /* saved frame pointer */
  (S->stack[S->sp  ]).type = SLY_TYPE_FIXNUM;
  (S->stack[S->sp++]).value.fixnum = S->fp;

  /* number of arguments */
  (S->stack[S->sp  ]).type = SLY_TYPE_FIXNUM;
  (S->stack[S->sp++]).value.fixnum = 0;

  S->fp = S->sp - 1;

  return sly_vm_run(S);
}

