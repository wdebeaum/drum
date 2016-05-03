#
# Makefile for TRIPS convenience library
#
# George Ferguson, ferguson@cs.rochester.edu,  7 Mar 1996
# Time-stamp: <Wed Sep  3 20:37:24 EDT 2003 ferguson>
#

MODULE = trlib

LIB  = libtrlib
SRCS = input.c parse.c error.c send.c debugarg.c hostname.c
HDRS = $(SRCS:%.c=%.h)
MANS = $(LIB).man
XTRA = trlib_timestamp.h

EXTRA_CFLAGS = -I..

CONFIGDIR = ../config
include $(CONFIGDIR)/c/lib.mk

