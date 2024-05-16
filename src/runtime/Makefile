# Makefile for MiniML runtime
#
# CMSC 22600 --- Compilers for Computer Languages
# Spring 2024
# University of Chicago
#
# COPYRIGHT (c) 2024 John Reppy (https://cs.uchicago.edu/~jhr)
# All rights reserved.
#

SHELL		= /bin/sh
CC		= gcc -std=gnu99 -m64
CPPFLAGS	=
CFLAGS		= -g -O2

C_SRCS		= main.c basis.c gc.c statepoints.c
OBJS		= $(patsubst %.c,%.o,$(C_SRCS))

LIBDIR		= ../../lib

mml-rt.o:	$(OBJS)
	$(LD) -r -o mml-rt.o $(OBJS)

.PHONY:		install
install:	mml-rt.o
	mkdir -p $(LIBDIR)
	cp -p mml-rt.o $(LIBDIR)

main.o:		main.c mini-ml.h
basis.o:	basis.c mini-ml.h
gc.o:		statepoints.c mini-ml.h statepoints.h
statepoints.o:	statepoints.c statepoints.h

.PHONY:		clean
clean:
		rm -f *.o
