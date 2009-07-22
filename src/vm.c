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

/* debugging information */
struct  opcode_ {
  uint8_t op;
  const char *name;
};

typedef struct opcode_ opcode_t;

static opcode_t global_opcodes[] = {
  {SLY_OP_LOAD_NIL,               "LOAD-NIL"},
  {SLY_OP_LOAD_FALSE,             "LOAD-FALSE"},
  {SLY_OP_LOAD_TRUE,              "LOAD-TRUE"},
  {SLY_OP_LOAD_ZERO,              "LOAD-ZERO"},
  {SLY_OP_LOAD_ONE,               "LOAD-ONE"},
  {SLY_OP_LOAD_FIXNUM,            "LOAD-FIXNUM"},
  {SLY_OP_LOAD_CHAR,              "LOAD-CHAR"},
  {SLY_OP_PUSH,                   "PUSH"},
  {SLY_OP_LOAD_0,                 "LOAD0"},
  {SLY_OP_LOAD_1,                 "LOAD1"},
  {SLY_OP_LOAD_2,                 "LOAD2"},
  {SLY_OP_LOAD_3,                 "LOAD3"},
  {SLY_OP_LOAD,                   "LOAD"},
  {SLY_OP_MAKE_CLOSURE,           "MAKE-CLOSURE"},
  {SLY_OP_CALL,                   "CALL"},
  {SLY_OP_RETURN,                 "RETURN"},
  {SLY_OP_JMP_IF,                 "JMP-IF"},
  {SLY_OP_JMP,                    "JMP"},
  {SLY_OP_LOAD_FREE,              "LOAD-FREE"},
  {SLY_OP_SAVE_CONT,              "SAVE-CONT"},
  {SLY_OP_REST_CONT,              "REST-CONT"},
  {SLY_OP_ASSIGN,                 "ASSIGN"},
  {SLY_OP_ASSIGN_FREE,            "ASSIGN-FREE"},
  {SLY_OP_BOX,                    "BOX"},
  {SLY_OP_OPEN_BOX,               "OPEN-BOX"},
  {SLY_OP_FRAME,                  "FRAME"},
  {SLY_OP_TAIL_CALL,              "TAIL-CALL"},
  {SLY_OP_HALT,                   "HALT"},
  {SLY_OP_LOAD_LOCAL,             "LOAD-LOCAL"},
  {SLY_OP_INSERT_BOX,             "INSERT-BOX"},
  {SLY_OP_ASSIGN_LOCAL,           "ASSIGN-LOCAL"},
  {SLY_OP_POP,                    "POP"},
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
  {SLY_OP_SYMBOL_P,               "SYMBOL?"},
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
  {SLY_OP_DEBUG,                  "DEBUG"},
  {0, NULL}
};

/*
 * debugging
 */

static void dump_instr(sly_state_t *S, uint32_t instr, sly_oport_t *port)
{
  uint8_t op;
  opcode_t* dbg;
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
  sly_oport_t *port;

  port = SLY_OPORT(S->stack[2].value.gc);

  sly_io_write_c_string(S, "Instruction: ", port);
  dump_instr(S, S->code[S->pc], port);
  sly_io_newline(S, port);

  sly_io_write_c_string(S, "Registers:", port);
  sly_io_newline(S, port);
  sly_io_write_c_string(S, "\taccum: ", port);
  sly_io_write(S, &S->accum, port);
  sly_io_newline(S, port);
  sly_io_write_c_string(S, "\tclosure: ", port);
  sly_io_write(S, &S->proc, port);
  sly_io_newline(S, port);
  snprintf(buf, 64, "\tPC: %d", S->pc);
  sly_io_write_c_string(S, buf, port);
  sly_io_newline(S, port);
  snprintf(buf, 64, "\tFP: %d", S->fp);
  sly_io_write_c_string(S, buf, port);
  sly_io_newline(S, port);

  sly_io_write_c_string(S, "Stack:", port);
  for(i = 0; i < S->sp; i++) {
    sly_io_write_c_string(S, " ", port);
    sly_io_write(S, S->stack + i, port);
  }

  sly_io_newline(S, port);
  sly_io_newline(S, port);
#if 0
  printf("Globals:\n");
  for(i = 0; i < S->global_env.size; i++) {
    sly_env_var_t var = S->global_env.vars[i];
    printf(" [ %d , ", i);
    if(var.symbol) {
      sly_io_write_symbol(var.symbol);
    }
    printf(" , ");
    sly_io_write(&var.value);
    printf("]\n");
  }
  printf("\n\n");

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

      memcpy(S->stack+i, S->stack+j, dw1 * sizeof(sly_object_t));

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

    case SLY_OP_JMP_IF:
      if(!(S->accum.type == SLY_TYPE_BOOL && S->accum.value.bool == 0)) {
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
    }
  }

  return 1;
}

void sly_vm_call(sly_state_t* S)
{
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

static int is_sly_eval(sly_string_t *str)
{
  static sly_char_t test_str[] = {'s', 'l', 'y', '-', 'e', 'v', 'a', 'l'};

  return str->size == 8 && memcmp(str->chars, test_str, 8 * sizeof(sly_char_t)) == 0;
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

    /* caching the sly-eval procedure for the REPL */
    if(is_sly_eval(mod->globals[i])) {
      fprintf(stderr, "Got it at %d\n", dw);
      S->sly_eval = dw;
    }
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

  /* tries to load code into module */
  if(!load_code_from_file(&mod, fname)) {
    return 0;
  }

  S->pc = sly_link_module(S, &mod);
  sly_destroy_module(&mod);

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

