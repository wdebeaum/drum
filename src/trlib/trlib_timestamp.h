/*
 * trlib_timestamp.h : Library timestamp header
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Mar 1996
 * Time-stamp: <96/09/13 12:11:50 ferguson>
 *
 * This file is included by all files in the archive. It ensures that the
 * linker will include the timestamp in any executable that includes
 * any member of the archive.
 *
 * Note that a library timestamp's name is not `_timestamp', which would
 * conflict with the timestamp of an executable. The name of the
 * variable is set by the Makefile when timestamp.c is compiled.
 */

#ifndef _trlib_timestamp_h_gf
#define _trlib_timestamp_h_gf

#ifdef __cplusplus
extern "C" {
#endif

extern char *_trlib_timestamp;

static void
_trlib_timestamp_func(void)
{
    char *dummy = _trlib_timestamp;
}

#ifdef __cplusplus
}
#endif
#endif
