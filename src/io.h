/*
 * The Sly Scheme I/O
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

#ifndef __SLY_IO_H__
#define __SLY_IO_H__

#include "sly.h"
#include "object.h"

char* sly_strdup(const char* str);

typedef struct sly_inport_t  sly_inport_t;
typedef struct sly_outport_t sly_outport_t;

/*
 * A string buffer. This string buffer enlarges itself automatically to
 * accommodate new characters. It never shrinks.
 */

typedef struct sly_sbuffer_t sly_sbuffer_t;

sly_sbuffer_t* sly_sbuffer_new(void);
void sly_sbuffer_destroy(sly_sbuffer_t* buffer);
void sly_sbuffer_assign(sly_sbuffer_t* buffer, const char* str);
void sly_sbuffer_add(sly_sbuffer_t* buffer, char c);
const char* sly_sbuffer_string(sly_sbuffer_t* buffer);
int sly_sbuffer_equalp(sly_sbuffer_t* buffer, const char* str);

/*
 * writer
 */

void sly_io_write(sly_object_t* obj);
void sly_io_write_symbol(sly_symbol_t* sym);

void sly_io_display(sly_object_t* obj);

#endif
