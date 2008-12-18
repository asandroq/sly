
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

/* OPCODES */
#define DUNA_OP_LOAD_NIL          1
#define DUNA_OP_LOAD_FALSE        2
#define DUNA_OP_LOAD_TRUE         3
#define DUNA_OP_LOAD_ZERO         4
#define DUNA_OP_LOAD_ONE          5
#define DUNA_OP_LOAD_FIXNUM       6
#define DUNA_OP_LOAD_CHAR         7
#define DUNA_OP_INC               8
#define DUNA_OP_DEC               9
#define DUNA_OP_FIXNUM_TO_CHAR   10
#define DUNA_OP_CHAR_TO_FIXNUM   11
#define DUNA_OP_NULL_P           12
#define DUNA_OP_ZERO_P           13
#define DUNA_OP_NOT              14
#define DUNA_OP_BOOL_P           15
#define DUNA_OP_CHAR_P           16
#define DUNA_OP_FIXNUM_P         17
#define DUNA_OP_PUSH             18
#define DUNA_OP_POP              19
#define DUNA_OP_PLUS             20
#define DUNA_OP_MINUS            21
#define DUNA_OP_MULT             22
#define DUNA_OP_LOAD_0           23
#define DUNA_OP_LOAD_1           24
#define DUNA_OP_LOAD_2           25
#define DUNA_OP_LOAD_3           26
#define DUNA_OP_LOAD             27
#define DUNA_OP_SET_FP           28
#define DUNA_OP_SAVE_FP          29
#define DUNA_OP_REST_FP          30

/* data types */
#define DUNA_TYPE_NIL            1
#define DUNA_TYPE_BOOL           2
#define DUNA_TYPE_FIXNUM         3
#define DUNA_TYPE_CHAR           4

/* Duna data types */
struct duna_Object_ {

  /* the runtime type tag */
  uint8_t type;

  /* the value of this object */
  union {
    uint8_t bool;
    uint8_t chr;
    uint32_t fixnum;
  } value;
};

typedef struct duna_Object_ duna_Object;

/* the state of the Duna interpreter */
struct duna_State_ {

  /* the bytecode to be interpreted */
  uint8_t *code;

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
  }
}

static int load_code_from_file(duna_State* D, const char* fname)
{
  FILE *f;
  int pc, ret;
  char buf[256];

  /* opening input file */
  f = fopen(fname, "r");
  if(!f) {
    return 0;
  }

  /* bytecode beginning */
  ret = fscanf(f, " %[^\n\t ]", buf);
  if(ret == EOF || ret == 0) {
    fclose(f);
    return -1;
  }
  if(buf[0] != '(' || buf[1] != '\0') {
    /* incorrect bytecode beginning */
    fclose(f);
    return -1;
  }

  pc = D->code_size;

  /* reading actual code */
  while(1) {
    ret = fscanf(f, " %[^\n\t ]", buf);
    if(ret == EOF || ret == 0) {
      fclose(f);
      return -1;
    }

    /* end of bytecode */
    if(buf[0] == ')' && buf[1] == '\0') {
      fclose(f);
      return pc;
    }

    ret = atoi(buf);
    if(ret > 255) {
      fclose(f);
      return -1;
    }

    /* does the code vector has space? */
    if(D->code_size == D->code_capacity) {
      uint8_t *code;
      uint32_t size;

      size = D->code_size * 3 / 2;
      code = (uint8_t*)realloc(D->code, size);
      if(!code) {
	fclose(f);
	return -1;
      }

      D->code = code;
      D->code_capacity = size;
    }

    /* adds new read byte to code vector */
    D->code[D->code_size++] = ret;
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
  D->code = (uint8_t*)malloc(sizeof(uint8_t) * 8192);
  if(D->code) {
    D->code_capacity = 8192;
  } else {
    free(D);
    return NULL;
  }

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

void duna_dump(duna_State* D)
{
  uint32_t i;

  printf("Registers:\n");
  printf("\taccum: "); write_obj(&D->accum); printf("\n");
  printf("\tPC: %d\n", D->pc);
  printf("\tFP: %d\n", D->fp);

  printf("Stack:");
  for(i = 0; i < D->sp; i++) {
    printf(" ");
    write_obj(D->stack + i);
  }
  printf("\n");
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

int duna_vm_run(duna_State* D)
{
  uint8_t b1, b2, b3, b4;
  uint32_t i, j, k, dw1, dw2;

  while(D->pc < D->code_size) {

    switch(D->code[D->pc++]) {

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
      b1 = D->code[D->pc++];
      b2 = D->code[D->pc++];
      b3 = D->code[D->pc++];
      b4 = D->code[D->pc++];
      dw1 = ((uint32_t)b1)       | ((uint32_t)b2 << 8) |
	    ((uint32_t)b3 << 16) | ((uint32_t)b4 << 24);

      D->accum.type = DUNA_TYPE_FIXNUM;
      D->accum.value.fixnum = dw1;
      break;

    case DUNA_OP_LOAD_CHAR:
      D->accum.type = DUNA_TYPE_CHAR;
      D->accum.value.chr = D->code[D->pc++];
      break;

    case DUNA_OP_INC:
      D->accum.value.fixnum++;
      break;

    case DUNA_OP_DEC:
      D->accum.value.fixnum--;
      break;

    case DUNA_OP_FIXNUM_TO_CHAR:
      b1 = (uint8_t) D->accum.value.fixnum;

      D->accum.type = DUNA_TYPE_CHAR;
      D->accum.value.chr = b1;
      break;

    case DUNA_OP_CHAR_TO_FIXNUM:
      dw1 = (uint32_t) D->accum.value.chr;

      D->accum.type = DUNA_TYPE_FIXNUM;
      D->accum.value.fixnum = dw1;
      break;

    case DUNA_OP_NULL_P:
      DUNA_SET_BOOL(D->accum.type == DUNA_TYPE_NIL);
      break;

    case DUNA_OP_ZERO_P:
      DUNA_SET_BOOL(D->accum.value.fixnum == 0);
      break;

    case DUNA_OP_NOT:
      DUNA_SET_BOOL(D->accum.value.bool == 0);
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

    case DUNA_OP_POP:
      /* removing number of arguments */
      dw1 = (D->stack[--D->sp]).value.fixnum;

      /* removing arguments */
      D->sp -= dw1;
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
      D->accum = D->stack[D->fp];
      break;

    case DUNA_OP_LOAD_1:
      D->accum = D->stack[D->fp-1];
      break;

    case DUNA_OP_LOAD_2:
      D->accum = D->stack[D->fp-2];
      break;

    case DUNA_OP_LOAD_3:
      D->accum = D->stack[D->fp-3];
      break;

    case DUNA_OP_LOAD:
      b1 = D->code[D->pc++];
      b2 = D->code[D->pc++];
      b3 = D->code[D->pc++];
      b4 = D->code[D->pc++];
      dw1 = ((uint32_t)b1)       | ((uint32_t)b2 << 8) |
	    ((uint32_t)b3 << 16) | ((uint32_t)b4 << 24);

      b1 = D->code[D->pc++];
      b2 = D->code[D->pc++];
      b3 = D->code[D->pc++];
      b4 = D->code[D->pc++];
      dw2 = ((uint32_t)b1)       | ((uint32_t)b2 << 8) |
	    ((uint32_t)b3 << 16) | ((uint32_t)b4 << 24);

      j = D->fp;
      for(i = 0; i < dw2; i++) {
	/* k has the number or arguments */
	k = (D->stack[j+1]).value.fixnum;

	/* j has the previous FP */
	j = (D->stack[j-k]).value.fixnum;
      }

      D->accum = D->stack[j-dw1];
      break;

    case DUNA_OP_SET_FP:
      D->fp = D->sp - 1;
      break;

    case DUNA_OP_SAVE_FP:
      (D->stack[D->sp  ]).type = DUNA_TYPE_FIXNUM;
      (D->stack[D->sp++]).value.fixnum = D->fp;
      break;

    case DUNA_OP_REST_FP:
      D->fp = (D->stack[--D->sp]).value.fixnum;
      break;
    }

    duna_dump(D);
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

