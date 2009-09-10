/*
 * The Sly Scheme virtual machine
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

#ifndef __SLY_VM_H__
#define __SLY_VM_H__

#include "sly.h"

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
#define SLY_OP_RETURN                 12
#define SLY_OP_SAVE_CONT              13
#define SLY_OP_REST_CONT              14
#define SLY_OP_BOX                    15
#define SLY_OP_OPEN_BOX               16
#define SLY_OP_HALT                   17
#define SLY_OP_ABORT                  18

#define SLY_OP_NULL_P                 40
#define SLY_OP_BOOL_P                 41
#define SLY_OP_CHAR_P                 42
#define SLY_OP_FIXNUM_P               43
#define SLY_OP_PAIR_P                 44
#define SLY_OP_SYMBOL_P               45

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
#define SLY_OP_DEBUG                  80

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
#define SLY_OP_CALL                  143
#define SLY_OP_TAIL_CALL             144

/* address of HALT instruction */
#define SLY_HALT_ADDRESS               0

/* initialises the virtual machine */
void sly_vm_init(sly_state_t* S);

/* dump the state of the VM to standard output */
void sly_vm_dump(sly_state_t* S);

/* performs the call protocol */
void sly_vm_call(sly_state_t* S, sly_object_t proc, uint32_t nargs);

/* loads compiled code into the vm */
int sly_vm_load(sly_state_t* S, sly_object_t mod);

#endif
