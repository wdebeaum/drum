/*
 * buffer.h
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Apr 1995
 * Time-stamp: <Wed Sep 24 13:25:39 EDT 2003 ferguson>
 */

#ifndef _buffer_h_gf
#define _buffer_h_gf

#ifdef __cplusplus
extern "C" {
#endif

/* Opaque type */
typedef struct _Buffer_st Buffer;

extern Buffer *bufferCreate(void);
extern void bufferDestroy(Buffer *this);
extern char *bufferData(Buffer *this);
extern int bufferDatalen(Buffer *this);
extern int bufferAvail(Buffer *this);
extern int bufferEmpty(Buffer *this);
extern int bufferAdd(Buffer *this, char *s, int len);
extern int bufferAddString(Buffer *this, char *s);
extern int bufferAddChar(Buffer *this, char c);
extern int bufferDiscard(Buffer *this, int len);
extern int bufferGet(Buffer *this, char *s, int len);
extern void bufferErase(Buffer *this);

extern int bufferIncRefCount(Buffer *this);
extern int bufferDecRefCount(Buffer *this);
extern int bufferRefCount(Buffer *this);

#ifdef __cplusplus
}
#endif
#endif
