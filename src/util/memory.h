/*
 * memory.h
 *
 * George Ferguson, ferguson@cs.rochester.edu,  3 Nov 1995
 * Time-stamp: <Wed Sep 24 13:25:47 EDT 2003 ferguson>
 */

#ifndef _memory_h_gf
#define _memory_h_gf

#ifdef __cplusplus
extern "C" {
#endif

#define gfree(P)	if (P) free((char*)P)

extern char *gnewstr(char *s);
extern void gfreeall(char **strs);
extern char **gcopyall(char **strs);

#ifdef __cplusplus
}
#endif

#endif
