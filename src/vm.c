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
#include "vm.h"
#include "object.h"
#include "state.h"

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

#define IS_TYPE_B(instr) ((instr) > 119)

#define EXTRACT_OP(instr)   ((uint8_t)((instr) & 0x000000ff))
#define EXTRACT_ARG(instr)  ((uint32_t)((instr) >> 8))

struct  sly_opcode_ {
  uint8_t op;
  const char *name;
  sly_symbol_t *sym;
};

typedef struct sly_opcode_ sly_opcode_t;

static sly_opcode_t global_opcodes[] = {
  {SLY_OP_LOAD_NIL,               "LOAD-NIL",           NULL},
  {SLY_OP_LOAD_FALSE,             "LOAD-FALSE",         NULL},
  {SLY_OP_LOAD_TRUE,              "LOAD-TRUE",          NULL},
  {SLY_OP_LOAD_UNDEF,             "LOAD-UNDEF",         NULL},
  {SLY_OP_LOAD_ZERO,              "LOAD-ZERO",          NULL},
  {SLY_OP_LOAD_ONE,               "LOAD-ONE",           NULL},
  {SLY_OP_PUSH,                   "PUSH",               NULL},
  {SLY_OP_LOAD_0,                 "LOAD0",              NULL},
  {SLY_OP_LOAD_1,                 "LOAD1",              NULL},
  {SLY_OP_LOAD_2,                 "LOAD2",              NULL},
  {SLY_OP_LOAD_3,                 "LOAD3",              NULL},
  {SLY_OP_RETURN,                 "RETURN",             NULL},
  {SLY_OP_SAVE_CONT,              "SAVE-CONT",          NULL},
  {SLY_OP_REST_CONT,              "REST-CONT",          NULL},
  {SLY_OP_BOX,                    "BOX",                NULL},
  {SLY_OP_OPEN_BOX,               "OPEN-BOX",           NULL},
  {SLY_OP_HALT,                   "HALT",               NULL},
  {SLY_OP_ABORT,                  "ABORT",              NULL},
  {SLY_OP_NULL_P,                 "NULL?",              NULL},
  {SLY_OP_BOOL_P,                 "BOOL?",              NULL},
  {SLY_OP_CHAR_P,                 "CHAR?",              NULL},
  {SLY_OP_FIXNUM_P,               "FIXNUM?",            NULL},
  {SLY_OP_PAIR_P,                 "PAIR?",              NULL},
  {SLY_OP_SYMBOL_P,               "SYMBOL?",            NULL},
  {SLY_OP_INC,                    "INC",                NULL},
  {SLY_OP_DEC,                    "DEC",                NULL},
  {SLY_OP_FIXNUM_TO_CHAR,         "FIXNUM->CHAR",       NULL},
  {SLY_OP_CHAR_TO_FIXNUM,         "CHAR->FIXNUM",       NULL},
  {SLY_OP_ZERO_P,                 "ZERO?",              NULL},
  {SLY_OP_NOT,                    "NOT",                NULL},
  {SLY_OP_PLUS,                   "PLUS",               NULL},
  {SLY_OP_MINUS,                  "MINUS",              NULL},
  {SLY_OP_MULT,                   "MULT",               NULL},
  {SLY_OP_CONS,                   "CONS",               NULL},
  {SLY_OP_CAR,                    "CAR",                NULL},
  {SLY_OP_CDR,                    "CDR",                NULL},
  {SLY_OP_NUM_EQ,                 "NUM-EQ",             NULL},
  {SLY_OP_EQ,                     "EQ?",                NULL},
  {SLY_OP_EQV,                    "EQV?",               NULL},
  {SLY_OP_MAKE_STRING,            "MAKE-STRING",        NULL},
  {SLY_OP_STRING_SET,             "STRING-SET",         NULL},
  {SLY_OP_STRING_TO_SYMBOL,       "STRING->SYMBOL",     NULL},
  {SLY_OP_MAKE_VECTOR,            "MAKE-VECTOR",        NULL},
  {SLY_OP_VECTOR_SET,             "VECTOR-SET",         NULL},
  {SLY_OP_DEBUG,                  "DEBUG",              NULL},
  {SLY_OP_LOAD_FIXNUM,            "LOAD-FIXNUM",        NULL},
  {SLY_OP_LOAD_CHAR,              "LOAD-CHAR",          NULL},
  {SLY_OP_LOAD,                   "LOAD",               NULL},
  {SLY_OP_MAKE_CLOSURE,           "MAKE-CLOSURE",       NULL},
  {SLY_OP_CALL,                   "CALL",               NULL},
  {SLY_OP_JMP_IF_NOT,             "JMP-IF-NOT",         NULL},
  {SLY_OP_JMP,                    "JMP",                NULL},
  {SLY_OP_LOAD_FREE,              "LOAD-FREE",          NULL},
  {SLY_OP_ASSIGN,                 "ASSIGN",             NULL},
  {SLY_OP_ASSIGN_FREE,            "ASSIGN-FREE",        NULL},
  {SLY_OP_FRAME,                  "FRAME",              NULL},
  {SLY_OP_TAIL_CALL,              "TAIL-CALL",          NULL},
  {SLY_OP_LOAD_LOCAL,             "LOAD-LOCAL",         NULL},
  {SLY_OP_INSERT_BOX,             "INSERT-BOX",         NULL},
  {SLY_OP_ASSIGN_LOCAL,           "ASSIGN-LOCAL",       NULL},
  {SLY_OP_POP,                    "POP",                NULL},
  {SLY_OP_GLOBAL_REF,             "GLOBAL-REF",         NULL},
  {SLY_OP_CHECKED_GLOBAL_REF,     "CHECKED-GLOBAL-REF", NULL},
  {SLY_OP_GLOBAL_SET,             "GLOBAL-SET",         NULL},
  {SLY_OP_CHECKED_GLOBAL_SET,     "CHECKED-GLOBAL-SET", NULL},
  {SLY_OP_CONST,                  "CONST",              NULL},
  {SLY_OP_CONST_INIT,             "CONST-INIT",         NULL},
  {SLY_OP_ARITY_EQ,               "ARITY=",             NULL},
  {SLY_OP_ARITY_GE,               "ARITY>=",            NULL},
  {SLY_OP_LISTIFY,                "LISTIFY",            NULL},
  {0, NULL, NULL}
};

void sly_vm_init(sly_state_t* S)
{
  sly_opcode_t *opc;
  sly_object_t sym;
  sly_gcobject_t *str;

  /* creating symbols for instructions */
  for(opc = global_opcodes; opc->name != NULL; opc++) {
    str = sly_create_string_from_ascii(S, opc->name);
    sym = sly_create_symbol(S, SLY_STRING(str));
    opc->sym = sym.value.symbol;
  }
}

/*
 * debugging
 */

static void dump_instr(sly_state_t *S, uint32_t instr, sly_object_t *port)
{
  uint8_t op;
  sly_opcode_t* dbg;
  char buf[64];

  op = EXTRACT_OP(instr);
  for(dbg = global_opcodes; dbg->name != NULL; dbg++) {
    if(dbg->op == op) {
      snprintf(buf, 64, "%u\t%s", (uint32_t)op, dbg->name);
      sly_io_write_c_string(S, buf, port);
      if(IS_TYPE_B(op)) {
	snprintf(buf, 64, " %u", EXTRACT_ARG(instr));
        sly_io_write_c_string(S, buf, port);
      }
      break;
    }
  }
}

/*
static void disassemble(sly_state_t* S)
{
  uint32_t i;

  printf("Code listing:\n");
  for(i = 0; i < S->code_size; i++) {
    printf("\t%u\t", i);
    dump_instr(S->code[i]);
    printf("\n");
  }
}
*/

void sly_vm_dump(sly_state_t* S)
{
  uint32_t i;
  char buf[64];
  sly_object_t port;

  port.type = SLY_TYPE_OUTPUT_PORT;
  port.value.gc = sly_io_create_stderr(S);

  sly_io_write_c_string(S, "Instruction: ", &port);
  dump_instr(S, S->code[S->pc], &port);
  sly_io_newline(S, &port);

  sly_io_write_c_string(S, "Registers:", &port);
  sly_io_newline(S, &port);
  sly_io_write_c_string(S, "\taccum: ", &port);
  sly_io_write(S, &S->accum, &port);
  sly_io_newline(S, &port);
  sly_io_write_c_string(S, "\tclosure: ", &port);
  sly_io_write(S, &S->proc, &port);
  sly_io_newline(S, &port);
  snprintf(buf, 64, "\tPC: %d", S->pc);
  sly_io_write_c_string(S, buf, &port);
  sly_io_newline(S, &port);
  snprintf(buf, 64, "\tFP: %d", S->fp);
  sly_io_write_c_string(S, buf, &port);
  sly_io_newline(S, &port);

  sly_io_write_c_string(S, "Stack:", &port);
  for(i = 0; i < S->sp; i++) {
    sly_io_write_c_string(S, " ", &port);
    sly_io_write(S, S->stack + i, &port);
  }

  sly_io_newline(S, &port);
  sly_io_newline(S, &port);

#if 0
  printf("Globals:\n");
  for(i = 0; i < S->global_env.size; i++) {
    sly_env_var_t var = S->global_env.vars[i];
    printf(" [ %d , ", i);
    fflush(NULL);
    if(var.symbol) {
      sly_io_write_symbol(S, var.symbol, &port);
    }
    printf(" , ");
    fflush(NULL);
    sly_io_write(S, &var.value, &port);
    printf("]\n");
    fflush(NULL);
  }
  printf("\n\n");
  fflush(NULL);

  printf("Constants:");
  for(i = 0; i < S->nr_consts; i++) {
    printf(" ");
    sly_io_write(S->consts + i);
  }
  printf("\n\n");
#endif
}

static void check_alloc(sly_state_t *S, void* ptr)
{
  S = S;

  if(ptr == NULL) {
    fprintf(stderr, "sly: Out of memory!\n");
    abort();
  }
}

static void return_from_call(sly_state_t* S)
{
  /* remove arguments and locals */
  S->sp = S->fp;
    
  /* restoring previous frame pointer */
  S->fp = (S->stack[--S->sp]).value.fixnum;

  /* restoring previous procedure */
  S->proc = S->stack[--S->sp];

  /* jumping to return address */
  S->pc = (S->stack[--S->sp]).value.fixnum;
}

static void call_c_closure(sly_state_t* S, sly_closure_t *clos)
{
  int ret;

  /* arguments are already in position, do the call */
  ret = (clos->entry_point.c)(S);

  /* result is on top of stack */
  if(ret) {
    S->accum = S->stack[S->sp-1];
  }

  /* now must do a 'return' */
  return_from_call(S);
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
 * SP ->
 *        +---------------------+
 *        | locals, returns etc.|
 *        +---------------------+
 *        |        arg N        |
 *        |          .          |
 *        |          .          |
 *        |          .          |
 *        |        arg 1        |
 *        +---------------------+
 * FP ->  | number of arguments |
 *        +---------------------+
 *        | saved frame pointer |
 *        +---------------------+
 *        |   saved procedure   !
 *        +---------------------+
 *        |    return address   |
 *        +=====================+
 *        |    previous frame   |
 */
static int sly_vm_run(sly_state_t* S)
{
  int go_on = 1, debug = 0;

  /*disassemble(S);*/

  while(go_on) {
    sly_object_t tmp;
    register uint32_t instr;
    uint32_t i, j, dw1, dw2;

    if(debug) {
      sly_vm_dump(S);
      /*getchar();*/
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
      S->accum.value.chr = (sly_char_t) EXTRACT_ARG(instr);
      break;

    case SLY_OP_INC:
      S->accum.value.fixnum++;
      break;

    case SLY_OP_DEC:
      S->accum.value.fixnum--;
      break;

    case SLY_OP_FIXNUM_TO_CHAR:
      S->accum.type = SLY_TYPE_CHAR;
      S->accum.value.chr = (sly_char_t) S->accum.value.fixnum;
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

    case SLY_OP_SYMBOL_P:
      SLY_SET_BOOL(S->accum.type == SLY_TYPE_SYMBOL);
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
      S->accum = S->stack[S->fp+1];
      break;

    case SLY_OP_LOAD_1:
      S->accum = S->stack[S->fp+2];
      break;

    case SLY_OP_LOAD_2:
      S->accum = S->stack[S->fp+3];
      break;

    case SLY_OP_LOAD_3:
      S->accum = S->stack[S->fp+4];
      break;

    case SLY_OP_LOAD:
      S->accum = S->stack[S->fp+EXTRACT_ARG(instr)+1];
      break;

    case SLY_OP_MAKE_CLOSURE:
      /*
       * There is always a jump after this instruction, to jump over the
       * closure code. So the closure entry point is PC + 1
       */
      dw1 = S->pc + 1;

      /* number of free variables */
      dw2 = EXTRACT_ARG(instr);

      tmp.type = SLY_TYPE_CLOSURE;
      tmp.value.gc = sly_create_sclosure(S, dw1, dw2);
      check_alloc(S, tmp.value.gc);
      S->accum = tmp;

      /* gathering free variables */
      for(i = 0; i < dw2; i++) {
	SLY_CLOSURE(S->accum.value.gc)->free_vars[i] = S->stack[S->sp-i-1];
      }
      S->sp -= dw2;
      break;

    case SLY_OP_TAIL_CALL:
      if(S->accum.type != SLY_TYPE_CLOSURE) {
        sly_push_string(S, "calling non-closure");
	sly_error(S, 1);
      }

      /*
       * the arguments to the callee must be shifted down
       * removing the arguments of the caller
       */

      /* number of arguments newly pushed */
      dw1 = EXTRACT_ARG(instr);

      /* where to copy the new values */
      i = S->fp + 1;

      /* where the values will be copied from */
      j = S->sp - dw1;

      memmove(S->stack+i, S->stack+j, dw1 * sizeof(sly_object_t));

      S->sp = S->fp + dw1 + 1;
      S->stack[S->fp].value.fixnum = dw1;

      /* setting active procedure */
      S->proc = S->accum;

      if(SLY_CLOSURE(S->proc.value.gc)->is_c) {
        call_c_closure(S, SLY_CLOSURE(S->proc.value.gc));
      } else {
        /* just jump to closure code */
        S->pc = SLY_CLOSURE(S->proc.value.gc)->entry_point.scm;
      }
      break;

    case SLY_OP_CALL:
      if(S->accum.type != SLY_TYPE_CLOSURE) {
	sly_push_string(S, "calling non-closure");
	sly_error(S, 1);
      }

      /* number of arguments newly pushed */
      dw1 = EXTRACT_ARG(instr);

      /* frame pointer */
      S->fp = S->sp - dw1 - 1;
      S->stack[S->fp].value.fixnum = dw1;

      /* setting active procedure */
      S->proc = S->accum;

      if(SLY_CLOSURE(S->proc.value.gc)->is_c) {
        call_c_closure(S, SLY_CLOSURE(S->proc.value.gc));
      } else {
        /* just jump to closure code */
        S->pc = SLY_CLOSURE(S->proc.value.gc)->entry_point.scm;
      }
      break;

    case SLY_OP_RETURN:
      return_from_call(S);
      break;

    case SLY_OP_JMP_IF_NOT:
      if(S->accum.type == SLY_TYPE_BOOL && S->accum.value.bool == 0) {
	S->pc += EXTRACT_ARG(instr);
      }
      break;

    case SLY_OP_JMP:
      S->pc += EXTRACT_ARG(instr);
      break;

    case SLY_OP_LOAD_FREE:
      S->accum = SLY_CLOSURE(S->proc.value.gc)->free_vars[EXTRACT_ARG(instr)];
      break;

    case SLY_OP_SAVE_CONT:
      dw1 = S->sp * sizeof(sly_object_t);

      tmp.type = SLY_TYPE_CONTI;
      tmp.value.gc = sly_create_conti(S, S->sp);
      check_alloc(S, tmp.value.gc);
      S->accum = tmp;

      /* copying stack */
      memcpy(SLY_CONTI(S->accum.value.gc)->stack, S->stack, dw1);
      break;

    case SLY_OP_REST_CONT:
      /* return value is on the stack */
      tmp = S->stack[--S->sp];

      /* restoring stack */
      S->sp = SLY_CONTI(S->accum.value.gc)->size;
      S->fp = S->sp - 1;
      memcpy(S->stack, SLY_CONTI(S->accum.value.gc)->stack, S->sp * sizeof(sly_object_t));

      S->accum = tmp;
      break;

    case SLY_OP_ASSIGN:
      assert((S->stack[S->fp+EXTRACT_ARG(instr)+1]).type == SLY_TYPE_BOX);
      SLY_BOX((S->stack[S->fp+EXTRACT_ARG(instr)+1]).value.gc)->value = S->accum;
      break;

    case SLY_OP_ASSIGN_FREE:
      assert((SLY_CLOSURE(S->proc.value.gc)->free_vars[EXTRACT_ARG(instr)]).type == SLY_TYPE_BOX);
      SLY_BOX((SLY_CLOSURE(S->proc.value.gc)->free_vars[EXTRACT_ARG(instr)]).value.gc)->value = S->accum;
      break;

    case SLY_OP_BOX:
      tmp.type = SLY_TYPE_BOX;
      tmp.value.gc = sly_create_box(S);
      check_alloc(S, tmp.value.gc);
      SLY_BOX(tmp.value.gc)->value = S->accum;

      S->accum = tmp;
      break;

    case SLY_OP_OPEN_BOX:
      assert(S->accum.type == SLY_TYPE_BOX);
      S->accum = SLY_BOX(S->accum.value.gc)->value;
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

      /* push slot where number of arguments will be set */
      (S->stack[S->sp  ]).type = SLY_TYPE_FIXNUM;
      (S->stack[S->sp++]).value.fixnum = 0;      
      break;

    case SLY_OP_HALT:
      go_on = 0;
      break;

    case SLY_OP_LOAD_LOCAL:
      S->accum = S->stack[S->fp+EXTRACT_ARG(instr)+1];
      break;

    case SLY_OP_INSERT_BOX:
      i = S->fp+EXTRACT_ARG(instr)+1;

      tmp.type = SLY_TYPE_BOX;
      tmp.value.gc = sly_create_box(S);
      check_alloc(S, tmp.value.gc);
      SLY_BOX(tmp.value.gc)->value = S->stack[i];

      S->stack[i] = tmp;
      break;

    case SLY_OP_ASSIGN_LOCAL:
      assert((S->stack[S->fp+EXTRACT_ARG(instr)+1]).type == SLY_TYPE_BOX);
      SLY_BOX((S->stack[S->fp+EXTRACT_ARG(instr)+1]).value.gc)->value = S->accum;
      break;

    case SLY_OP_POP:
      S->sp -= EXTRACT_ARG(instr);
      break;

    case SLY_OP_CHECKED_GLOBAL_REF:
      if(S->global_env.vars[EXTRACT_ARG(instr)].value.type == SLY_TYPE_UNDEF) {
	sly_push_string(S, "undefined global referenced: ");
	S->stack[S->sp].type = SLY_TYPE_SYMBOL;
        S->stack[S->sp++].value.symbol = S->global_env.vars[EXTRACT_ARG(instr)].symbol;
	sly_error(S, 2);
      }
      /* fall through */

    case SLY_OP_GLOBAL_REF:
      S->accum = S->global_env.vars[EXTRACT_ARG(instr)].value;
      break;

    case SLY_OP_CHECKED_GLOBAL_SET:
      if(S->global_env.vars[EXTRACT_ARG(instr)].value.type == SLY_TYPE_UNDEF) {
	sly_push_string(S, "undefined global assigned: ");
	S->stack[S->sp].type = SLY_TYPE_SYMBOL;
        S->stack[S->sp++].value.symbol = S->global_env.vars[EXTRACT_ARG(instr)].symbol;
	sly_error(S, 2);
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
	sly_push_string(S, "arity mismatch");
	sly_error(S, 1);
      }
      break;

    case SLY_OP_ARITY_GE:
      if(S->stack[S->fp].value.fixnum < EXTRACT_ARG(instr)) {
	sly_push_string(S, "variable arity mismatch");
	sly_error(S, 1);
      }
      break;

    case SLY_OP_LISTIFY:
      /* number of fixed arguments */
      dw1 = EXTRACT_ARG(instr);

      /* total number of arguments */
      dw2 = S->stack[S->fp].value.fixnum;

      /* consing */
      S->accum.type = SLY_TYPE_NIL;
      for(i = S->fp + dw2; i > S->fp + dw1; i--) {
	tmp.type = SLY_TYPE_PAIR;
	tmp.value.gc = sly_create_pair(S);
	check_alloc(S, tmp.value.gc);
	SLY_PAIR(tmp.value.gc)->car = S->stack[i];
	SLY_PAIR(tmp.value.gc)->cdr = S->accum;

	S->accum = tmp;
      }

      /* adjusting stack */
      S->stack[S->fp + ++dw1] = S->accum;
      S->stack[S->fp].value.fixnum = dw1;
      S->sp = S->fp + dw1 + 1;
      break;

    case SLY_OP_ABORT:
      sly_error(S, 0);
      break;

    case SLY_OP_CONS:
      tmp.type = SLY_TYPE_PAIR;
      tmp.value.gc = sly_create_pair(S);
      check_alloc(S, tmp.value.gc);
      SLY_PAIR(tmp.value.gc)->car = S->stack[--S->sp];
      SLY_PAIR(tmp.value.gc)->cdr = S->accum;

      S->accum = tmp;
      break;

    case SLY_OP_CAR:
      S->accum = SLY_PAIR(S->accum.value.gc)->car;
      break;

    case SLY_OP_CDR:
      S->accum = SLY_PAIR(S->accum.value.gc)->cdr;
      break;

    case SLY_OP_NUM_EQ:
      /* TODO: throw error if args are not numbers */
      SLY_SET_BOOL(S->accum.value.fixnum == S->stack[--S->sp].value.fixnum);
      break;

    case SLY_OP_EQ:
      SLY_SET_BOOL(SLY_OBJ_EQ(S->accum, S->stack[S->sp-1]));
      --S->sp;
      break;

    case SLY_OP_EQV:
      SLY_SET_BOOL(SLY_OBJ_EQ(S->accum, S->stack[S->sp-1]));
      --S->sp;
      break;

    case SLY_OP_MAKE_STRING:
      /* string size */
      dw1 = S->accum.value.fixnum;

      tmp.type = SLY_TYPE_STRING;
      tmp.value.gc = sly_create_string(S, NULL, dw1);
      check_alloc(S, tmp.value.gc);

      S->accum = tmp;
      break;

    case SLY_OP_STRING_SET:
      dw1 = S->accum.value.chr;
      dw2 = S->stack[--S->sp].value.fixnum;
      S->accum = S->stack[--S->sp];
      SLY_STRING(S->accum.value.gc)->chars[dw2] = dw1;
      break;

    case SLY_OP_STRING_TO_SYMBOL:
      tmp = sly_create_symbol(S, SLY_STRING(S->accum.value.gc));
      S->accum = tmp;
      break;

    case SLY_OP_MAKE_VECTOR:
      /* vector size */
      dw1 = S->accum.value.fixnum;

      tmp.type = SLY_TYPE_VECTOR;
      tmp.value.gc = sly_create_vector(S, dw1);
      check_alloc(S, tmp.value.gc);

      S->accum = tmp;
      break;

    case SLY_OP_VECTOR_SET:
      tmp = S->accum;
      dw1 = S->stack[--S->sp].value.fixnum;
      S->accum = S->stack[--S->sp];
      SLY_VECTOR(S->accum.value.gc)->data[dw1] = tmp;    
      break;

    case SLY_OP_DEBUG:
      if(S->accum.value.bool == 0) {
	debug = 0;
      } else {
	debug = 1;
      }
      break;

    default:
      abort();
    }
  }

  return 1;
}

void sly_vm_call(sly_state_t* S, sly_object_t proc, uint32_t nargs)
{
  int idx;

  /* must copy args "up" to create space for frame info */
  idx = S->sp - nargs;
  S->sp += 4;
  memmove(&S->stack[idx+4], &S->stack[idx], nargs * sizeof(sly_object_t));

  /* return address */
  (S->stack[idx  ]).type = SLY_TYPE_FIXNUM;
  (S->stack[idx++]).value.fixnum = SLY_HALT_ADDRESS;

  /* saved procedure */
  S->stack[idx++] = S->proc;
  S->proc = proc;

  /* saved frame pointer */
  (S->stack[idx  ]).type = SLY_TYPE_FIXNUM;
  (S->stack[idx++]).value.fixnum = S->fp;

  /* number of arguments */
  S->fp = idx;
  (S->stack[idx]).type = SLY_TYPE_FIXNUM;
  (S->stack[idx]).value.fixnum = nargs;

  if(SLY_CLOSURE(S->proc.value.gc)->is_c) {
    call_c_closure(S, SLY_CLOSURE(S->proc.value.gc));
  } else {
    /* reenter the virtual machine */
    S->pc = SLY_CLOSURE(S->proc.value.gc)->entry_point.scm;
    sly_vm_run(S);
  }
}

/*
 * loading
 */

typedef struct sly_module_t sly_module_t;

struct sly_module_t {

  /* uninterned globals */
  uint32_t nr_globals;
  sly_string_t **globals;

  /* constants */
  uint32_t nr_consts;

  /* code */
  uint32_t *code, code_size;
};

static void sly_destroy_module(sly_module_t *M)
{
  uint32_t i;

  free(M->code);

  for(i = 0; i < M->nr_globals; i++) {
    free(M->globals[i]);
  }
  free(M->globals);
}

static uint32_t sly_link_module(sly_state_t* S, sly_module_t *mod)
{
  sly_env_t env;
  sly_object_t obj, *tmp;
  uint32_t *code;
  int idx, old_size;
  uint32_t i, dw, consts_base, code_base, growth;

  env.size = mod->nr_globals;
  env.vars = (sly_env_var_t*)malloc(env.size * sizeof(sly_env_var_t));
  /* TODO: test return and throw error */

  /*
   * adding global to environment
   * a little trick is used here, I use the value
   * of the global var to store the index where the
   * global will be mapped to when linking the code
   *
   */
  for(growth = 0, i = 0; i < env.size; i++) {
    obj = sly_create_symbol(S, mod->globals[i]);
    env.vars[i].symbol = obj.value.symbol;
    env.vars[i].value.type = SLY_TYPE_FIXNUM;

    idx = sly_st_get_global_index(S, env.vars[i].symbol);
    if(idx < 0) {
      dw = S->global_env.size + growth++;
    } else {
      dw = idx;
    }

    env.vars[i].value.value.fixnum = dw;
  }

  /* enlarging global environment */
  if(growth > 0) {
    old_size = S->global_env.size;
    sly_st_enlarge_globals(S, growth);

    for(i = 0; i < env.size; i++) {
      idx = env.vars[i].value.value.fixnum;
      if(idx >= old_size) {
	S->global_env.vars[idx].symbol = env.vars[i].symbol;
	S->global_env.vars[idx].value.type = SLY_TYPE_UNDEF;
      }
    }
  }

  /* enlarging constants */
  consts_base = S->nr_consts;
  dw = S->nr_consts + mod->nr_consts;
  tmp = (sly_object_t*)realloc(S->consts, dw * sizeof(sly_object_t));
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

static int sly_link_run_module(sly_state_t* S, sly_module_t *mod)
{
  S->pc = sly_link_module(S, mod);
  sly_destroy_module(mod);

  /* initial frame on stack with address of halt instruction */
  /* return address */
  (S->stack[S->sp  ]).type = SLY_TYPE_FIXNUM;
  (S->stack[S->sp++]).value.fixnum = SLY_HALT_ADDRESS;

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

#define CAR(l) (SLY_PAIR((l).value.gc)->car)
#define CDR(l) (SLY_PAIR((l).value.gc)->cdr)
#define VECTOR_REF(v,i) (SLY_VECTOR((v).value.gc)->data[i])

static uint8_t get_opcode(sly_object_t sym)
{
  sly_opcode_t *opc;

  for(opc = global_opcodes; opc->name != NULL; opc++) {
    if(sym.value.symbol == opc->sym) {
      return opc->op;
    }
  }

  return 255;
}

static uint32_t list_length(sly_object_t lis)
{
  uint32_t i;

  if(lis.type == SLY_TYPE_NIL) {
    return 0;
  }

  for(i = 1;; i++, lis = CDR(lis)) {
    if((CDR(lis)).type == SLY_TYPE_NIL) {
      return i;
    }
  }
}

static void vector_to_module(sly_object_t vec, sly_module_t *mod)
{
  uint32_t sz;
  sly_object_t p;

  assert(vec.type == SLY_TYPE_VECTOR);

  /* globals */
  p = VECTOR_REF(vec, 0);
  sz = list_length(p);
  mod->nr_globals = sz;
  sz *= sizeof(sly_string_t*);
  mod->globals = (sly_string_t**)malloc(sz);
  memset(mod->globals, 0x00, sz);
  for(sz = 0;; p = CDR(p), sz++) {
    uint32_t size;
    sly_string_t *str;

    if(p.type == SLY_TYPE_NIL) {
      break;
    } else {
      size = SLY_SIZE_OF_STRING((CAR(p)).value.symbol->str->size);
      str = (sly_string_t*)malloc(size);
      memmove(str, (CAR(p)).value.symbol->str, size);

      mod->globals[sz] = str;
    }
  }

  /* constants */
  mod->nr_consts = list_length(VECTOR_REF(vec, 1));

  /* loading code */
  sz = (VECTOR_REF(vec, 2)).value.fixnum;
  mod->code_size = sz;
  mod->code = (uint32_t*)malloc(sz * sizeof(uint32_t));
  vec = VECTOR_REF(vec, 3);

  for(sz = 0; sz < mod->code_size; sz++) {
    uint32_t instr, dat;
    sly_object_t vec_instr;

    vec_instr = VECTOR_REF(vec, sz);
    instr = (uint32_t)get_opcode(VECTOR_REF(vec_instr, 0));

    /* retrieve operands if any */
    if(IS_TYPE_B(instr)) {
      dat = (VECTOR_REF(vec_instr, 1)).value.fixnum;
      instr |= (dat << 8);
    }

    mod->code[sz] = instr;
  }
}

int sly_vm_load(sly_state_t* S, sly_object_t vec)
{
  sly_module_t mod;

  assert(vec.type == SLY_TYPE_VECTOR);

  vector_to_module(vec, &mod);
  return sly_link_run_module(S, &mod);
}

#define FIXNUM(o, n) do {(o).type = SLY_TYPE_FIXNUM; (o).value.fixnum = (n);} while(0)

static void write_fixnum(sly_state_t *S, sly_object_t *fix, sly_object_t *port)
{
  uint8_t b;
  uint32_t val;
  sly_object_t o;

  val = fix->value.fixnum;

  b = val / (1 << 24);
  val %= 1 << 24;
  FIXNUM(o, b);
  sly_io_write(S, &o, port);
  sly_io_write_c_string(S, " ", port);

  b = val / (1 << 16);
  val %= 1 << 16;
  FIXNUM(o, b);
  sly_io_write(S, &o, port);
  sly_io_write_c_string(S, " ", port);

  b = val / (1 << 8);
  val %= 1 << 8;
  FIXNUM(o, b);
  sly_io_write(S, &o, port);
  sly_io_write_c_string(S, " ", port);

  FIXNUM(o, val);
  sly_io_write(S, &o, port);
  sly_io_write_c_string(S, " ", port);
}

void sly_vm_write_code(sly_state_t *S, sly_object_t vec, sly_object_t *port)
{
  uint32_t i;
  sly_module_t mod;
  sly_object_t o;

  vector_to_module(vec, &mod);

  sly_io_write_c_string(S, "#( ", port);
  FIXNUM(o, mod.nr_globals);
  write_fixnum(S, &o, port);

  for(i = 0; i < mod.nr_globals; i++) {
    uint32_t j;
    sly_string_t *str;

    str = mod.globals[i];
    FIXNUM(o, str->size);
    write_fixnum(S, &o, port);

    for(j = 0; j < str->size; j++) {
      FIXNUM(o, str->chars[j]);
      write_fixnum(S, &o, port);
    }
  }

  FIXNUM(o, mod.nr_consts);
  write_fixnum(S, &o, port);

  FIXNUM(o, mod.code_size);
  write_fixnum(S, &o, port);

  for(i = 0; i < mod.code_size; i++) {
    uint32_t instr;

    instr = mod.code[i];
    FIXNUM(o, EXTRACT_OP(instr));
    sly_io_write(S, &o, port);
    sly_io_write_c_string(S, " ", port);

    if(IS_TYPE_B(instr)) {
      FIXNUM(o, EXTRACT_ARG(instr));
      write_fixnum(S, &o, port);
    }
  }

  sly_io_write_c_string(S, ")", port);
  sly_io_newline(S, port);

  sly_destroy_module(&mod);
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

static int get_string(FILE *f, sly_string_t **str)
{
  int ret;
  uint32_t i, dw1, dw2;

  /* string size */
  ret = get_fixnum(f, &dw1);
  if(!ret) {
    return 0;
  }

  *str = (sly_string_t*)malloc(SLY_SIZE_OF_STRING(dw1));
  /* TODO: test return and throw error */
  (*str)->size  = dw1;
  SLY_GCOBJECT(*str)->type = SLY_TYPE_STRING;

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

static int load_code_from_file(sly_module_t *mod, const char* fname)
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
  dw2 = dw1 * sizeof(sly_string_t*);
  mod->globals = (sly_string_t**)malloc(dw2);
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

int sly_load_file(sly_state_t* S, const char *fname)
{
  sly_module_t mod;

#if 0
  /* tries to load code into module */
  if(!load_code_from_file(&mod, fname)) {
    return 0;
  }
#endif

  sly_get_global(S, "compile-from-port");
  sly_push_string(S, fname);
  sly_open_input_file(S);
  sly_call(S, 1);

  vector_to_module(S->stack[S->sp-1], &mod);
  S->sp -= 3;

  return sly_link_run_module(S, &mod);
}

static void get_fixnum_b(uint8_t **buf, uint32_t *num)
{
  uint32_t b1, b2, b3, b4;

  b1 = (*buf)[0];
  b2 = (*buf)[1];
  b3 = (*buf)[2];
  b4 = (*buf)[3];

  *num = b1 | (b2 << 8) | (b3 << 16) | (b4 << 24);

  *buf += 4;
}

static int get_string_b(uint8_t **buf, sly_string_t **str)
{
  uint32_t i, dw1, dw2;

  /* string size */
  get_fixnum_b(buf, &dw1);

  *str = (sly_string_t*)malloc(SLY_SIZE_OF_STRING(dw1));
  /* TODO: test return and throw error */
  (*str)->size  = dw1;
  SLY_GCOBJECT(*str)->type = SLY_TYPE_STRING;

  for(i = 0; i < dw1; i++) {
    get_fixnum_b(buf, &dw2);
    (*str)->chars[i] = dw2;
  }

  return 1;
}

static int load_code_from_buffer(sly_module_t *mod, const uint8_t *buffer)
{
  uint8_t *buf;
  uint32_t i, dw1, dw2;

  buf = (uint8_t*)buffer;

  /* reading number of globals */
  get_fixnum_b(&buf, &dw1);

  mod->nr_globals = dw1;
  dw2 = dw1 * sizeof(sly_string_t*);
  mod->globals = (sly_string_t**)malloc(dw2);
  /* TODO: test return and throw error */
  memset(mod->globals, 0x00, dw2);

  /* reading globals */
  for(i = 0; i < mod->nr_globals; i++) {
    get_string_b(&buf, &mod->globals[i]);
  }

  /* reading number of constants */
  get_fixnum_b(&buf, &dw1);
  mod->nr_consts = dw1;

  /* reading code size */
  get_fixnum_b(&buf, &dw1);

  mod->code_size = dw1;
  mod->code = (uint32_t*)malloc(dw1 * sizeof(uint32_t));
  /* TODO: test return and throw error */

  /* reading actual code */
  for(i = 0; i < mod->code_size; i++) {
    uint32_t instr;

    instr = (uint32_t)*(buf++);

    /* retrieve operands if any */
    if(IS_TYPE_B(instr)) {
      get_fixnum_b(&buf, &dw1);

      instr |= (dw1 << 8);
    }

    mod->code[i] = instr;
  }

  return 1;
}

int sly_load_buffer(sly_state_t* S, const uint8_t *buffer)
{
  sly_module_t mod;

  /* tries to load code into module */
  if(!load_code_from_buffer(&mod, buffer)) {
    return 0;
  }

  return sly_link_run_module(S, &mod);
}
