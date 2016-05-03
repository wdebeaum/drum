/*
 * KQML_timestamp.h : Library timestamp header
 *
 * George Ferguson, ferguson@cs.rochester.edu, 16 Feb 1996
 * Time-stamp: <96/02/16 16:28:57 ferguson>
 *
 * This file is included by all files in the archive. It ensures that the
 * linker will include the timestamp in any executable that includes
 * any member of the archive.
 *
 * Note that a library timestamp's name is not `_timestamp', which would
 * conflict with the timestamp of an executable. The name of the
 * variable is set by the Makefile when timestamp.c is compiled.
 */

#ifndef _KQML_timestamp_h_gf
#define _KQML_timestamp_h_gf

extern char *_KQML_timestamp;

static void
_KQML_timestamp_func(void)
{
    char *dummy = _KQML_timestamp;
}


#endif
