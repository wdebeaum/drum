/*
 * buffer.c: Circular buffers for i/o buffering
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Apr 1995
 * Time-stamp: <96/01/29 18:02:26 ferguson>
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "error.h"
#undef DEBUG
#include "debug.h"

#ifndef memmove
# define memmove(T,S,N) bcopy(S,T,N)
#endif

typedef struct _Buffer_st {
    char *data;
    int len;
    char *next;
    int refcount;
} Buffer;

#define BUFFER_PAGESIZE    256
#define BUFFER_INITIAL_LEN BUFFER_PAGESIZE

/*
 * Functions defined here:
 */
Buffer *bufferCreate(void);
void bufferDestroy(Buffer *this);
char *bufferData(Buffer *this);
int bufferDatalen(Buffer *this);
int bufferAvail(Buffer *this);
int bufferEmpty(Buffer *this);
int bufferAdd(Buffer *this, char *s, int len);
int bufferAddString(Buffer *this, char *s);
int bufferAddChar(Buffer *this, char c);
int bufferDiscard(Buffer *this, int len);
int bufferGet(Buffer *this, char *s, int len);
void bufferErase(Buffer *this);
static int bufferRealloc(Buffer *this, int incr);
/* Reference counting routines for sharing buffers */
int bufferIncRefCount(Buffer *this);
int bufferDecRefCount(Buffer *this);
int bufferRefCount(Buffer *this);

/*	-	-	-	-	-	-	-	-	*/

Buffer *
bufferCreate(void)
{
    Buffer *this = (Buffer*)malloc(sizeof(Buffer));

    if (this == NULL) {
	SYSERR0("couldn't malloc Buffer");
	return NULL;
    }
    if ((this->data = malloc(BUFFER_INITIAL_LEN)) == NULL) {
	SYSERR0("couldn't malloc data");
	return NULL;
    }
    this->len = BUFFER_INITIAL_LEN;
    this->next = this->data;
    this->refcount = 1;
    return this;
}

void
bufferDestroy(Buffer *this)
{
    if (this == NULL) {
	return;
    }
    if (this->data != NULL) {
	free(this->data);
    }
    free((char*)this);
}

/*	-	-	-	-	-	-	-	-	*/

int
bufferEmpty(Buffer *this)
{
    if (this == NULL) {
	return 1;
    } else {
	return this->next == this->data;
    }
}

char *
bufferData(Buffer *this)
{
    if (this == NULL) {
	return NULL;
    } else {
	return this->data;
    }
}

int
bufferDatalen(Buffer *this)
{
    if (this == NULL) {
	return 0;
    } else {
	return this->next - this->data;
    }
}

int
bufferAvail(Buffer *this)
{
    if (this == NULL) {
	return 0;
    } else {
	return this->data + this->len - this->next;
    }
}

/*	-	-	-	-	-	-	-	-	*/

int
bufferAdd(Buffer *this, char *s, int len)
{
    DEBUG2("this=0x%lx, adding %d bytes", this, len);
    if (bufferAvail(this) < len) {
	DEBUG0("resizing buffer...");
	if (bufferRealloc(this, len) < 0) {
	    return -1;
	}
    }
    DEBUG1("calling memcpy, this->next=0x%lx", this->next);
    memcpy(this->next, s, len);
    this->next += len;
    DEBUG1("done, this->next=0x%lx", this->next);
    return 0;
}

int
bufferAddString(Buffer *this, char *s)
{
    return bufferAdd(this, s, strlen(s));
}

int
bufferAddChar(Buffer *this, char c)
{
    return bufferAdd(this, &c, 1);
}

int
bufferDiscard(Buffer *this, int len)
{
    int datalen = bufferDatalen(this);

    DEBUG2("this=0x%lx, discarding %d bytes", this, len);
    if (len <= 0) {
	return 0;
    } else if (len > datalen) {
	len = datalen;
    }
    /* Shift data down len bytes */
    /* Note: memmove is supposed to handle overlapping moves... */
    memmove(this->data, this->data + len, datalen-len);
    this->next -= len;
    DEBUG1("done, this->next=0x%lx", this->next);
    return 0;
}

int
bufferGet(Buffer *this, char *s, int len)
{
    memcpy(s, this->data, len);
    return bufferDiscard(this, len);
}

void
bufferErase(Buffer *this)
{
    this->next = this->data;
    *(this->data) = '\0';		/* for nice printing in gdb */
}

/*	-	-	-	-	-	-	-	-	*/

static int
bufferRealloc(Buffer *this, int incr)
{
    int currentlen, newlen;
    char *new;

    DEBUG3("this=0x%lx, incr=%d, len=%d", this, incr, this->len);
    currentlen = bufferDatalen(this);
    newlen = (((currentlen + incr) / BUFFER_PAGESIZE) + 1) * BUFFER_PAGESIZE;
    DEBUG2("currentlen=%d, newlen=%d", currentlen, newlen);
    if ((new = realloc(this->data, newlen)) == NULL) {
	SYSERR0("couldn't realloc buffer");
	return -1;
    }
    this->data = new;
    this->len = newlen;
    this->next = this->data + currentlen;
    DEBUG2("done: new data=0x%lx, new next=0x%lx", this->data, this->next);
    return 0;
}

/*	-	-	-	-	-	-	-	-	*/

int
bufferIncRefCount(Buffer *this)
{
    if (this == NULL) {
	return -1;
    }
    return ++(this->refcount);
}

int
bufferDecRefCount(Buffer *this)
{
    if (this == NULL) {
	return -1;
    }
    return --(this->refcount);
}

int
bufferRefCount(Buffer *this)
{
    if (this == NULL) {
	return -1;
    }
    return this->refcount;
}

