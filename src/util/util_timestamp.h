/*
 * util_timestamp.h : Library timestamp header
 *
 * George Ferguson, ferguson@cs.rochester.edu, 16 Feb 1996
 * Time-stamp: <96/09/13 12:10:39 ferguson>
 *
 * This file is included by all files in the archive. It ensures that the
 * linker will include the timestamp in any executable that includes
 * any member of the archive.
 *
 * Note that a library timestamp's name is not `_timestamp', which would
 * conflict with the timestamp of an executable. The name of the
 * variable is set by the Makefile when timestamp.c is compiled.
 */

#ifndef _util_timestamp_h_gf
#define _util_timestamp_h_gf

#ifdef __cplusplus
extern "C" {
#endif

extern char *_util_timestamp;

static void
_util_timestamp_func(void)
{
    char *dummy = _util_timestamp;
}

#ifdef __cplusplus
}
#endif

#endif
