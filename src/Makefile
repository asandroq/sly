##
## The Sly Scheme compiler
## Copyright (c) 2009 Alex Queiroz <asandroq@gmail.com>
##
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
## THE SOFTWARE.
##

CC = gcc
CFLAGS = -g3 -O0 -Wall -W -DSLY_DEBUG_API
LD = gcc
LDFLAGS =

TARGET = sly

SOURCES = \
	api.c \
	gc.c \
	io.c \
	lib.c \
	object.c \
	state.c \
	vm.c \
	sly.c

OBJECTS = $(SOURCES:.c=.o)

all: $(TARGET) fasl2h

$(TARGET): $(OBJECTS)
	$(LD) $(OBJECTS) $(LDFLAGS) -o $(TARGET)

fasl2h: fasl2h.o
	$(LD) fasl2h.o $(LDFLAGS) -o fasl2h

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

.PHONY: clean

clean:
	rm -f $(TARGET) fasl2h $(OBJECTS) *~ *.fasl
