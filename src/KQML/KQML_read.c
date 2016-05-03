/*
 * KQML_read.c
 *
 * George Ferguson, ferguson@cs.rochester.edu,  8 Jan 1996
 * Time-stamp: <Fri Jan  8 10:29:05 EST 2010 ferguson>
 *
 * Most functions in this file are static, with just the high-level
 * ones exported. That could be changed if someone wanted lower-level
 * access.
 * gf: 8 Jan 2010: I needed lower-level access for the Objective-C
 * wrapper, so these functions are no longer static. As noted in KQML.h,
 * it would be better to use opaque types for, eg., the KQMLContext
 * definition.
 *
 * Note: On the Alpha, FIONREAD is not documented. Unfortunately, the thing
 * that is documented, I_NREAD in stropts.h, is defined as a void ioctl!
 * Perhaps the docs are wrong...
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
/* ioctl FIONREAD */
#if defined(__alpha) || defined(linux) || defined(__APPLE__)
# include <sys/ioctl.h>
#elif defined(__CYGWIN__)
#include <asm/socket.h>
#else
# include <sys/filio.h>
#endif
#include "util/error.h"
#include "util/debug.h"
#include "KQML.h"

#ifndef FD_SETSIZE
 FD_SETSIZE_MUST_BE DEFINED_SOMEHOW!
#endif

#define INBUF_INITIAL_SIZE	64
#define INBUF_REALLOC_FUNC(N)	(N + INBUF_INITIAL_SIZE)

/*
 * Functions defined here:
 */
KQMLContext *KQMLNewContext(void);
void KQMLDestroyContext(KQMLContext *ctx);
void KQMLResetContext(KQMLContext *ctx);
KQMLPerformative *KQMLInputChar(char c, KQMLContext *ctx, KQMLError *errorp);
KQMLPerformative *KQMLReadAndParse1(int fd, KQMLContext *ctx, KQMLError *errorp);
KQMLPerformative *KQMLRead(int fd, KQMLError *errorp, char **txtp);
KQMLPerformative *KQMLReadNoHang(int fd, KQMLError *errorp, char **txtp);
int gread1(int fd, char *dst);

/*
 * Data defined here:
 */
static KQMLContext *contexts[FD_SETSIZE];

/*	-	-	-	-	-	-	-	-	*/

KQMLContext *
KQMLNewContext(void)
{
    KQMLContext *ctx;
    
    if ((ctx = (KQMLContext*)malloc(sizeof(KQMLContext))) == NULL) {
	return NULL;
    }
    ctx->state = START;
    ctx->inbuf = NULL;
    ctx->inlen = ctx->incnt = ctx->hashnum = 0;
    return ctx;
}

void
KQMLDestroyContext(KQMLContext *ctx)
{
    if (ctx != NULL) {
	free((char*)ctx);
    }
}

void
KQMLResetContext(KQMLContext *ctx)
{
    if (ctx != NULL) {
	ctx->state = START;
	ctx->incnt = 0;
	ctx->hashnum = 0;
    }
}

/*	-	-	-	-	-	-	-	-	*/
/*
 * This routine implements the finite-state machine that determines when
 * we've seen a balanced set of parens outside of strings.
 * Returns: performative and *errorp = 1 when parsing is successfully done
 *	    NULL and *errorp = 0 while parsing is not finished
 *          NULL and *errorp < 0 when an error occurs
 */
KQMLPerformative *
KQMLInputChar(char c, KQMLContext *ctx, KQMLError *errorp)
{
    KQMLPerformative *p;

    /* Sanity check */
    if (ctx == NULL) {
	ERROR0("bad context!");
	*errorp = KQML_ERROR_BAD_CONTEXT;
	return NULL;
    }
    DEBUG2("processing char `%c', state=%d", c, ctx->state);
    /* Now, if we're in DONE state, reset the context */
    if (ctx->state == DONE) {
	DEBUG0("resetting context");
	KQMLResetContext(ctx);
    }
    /* Allocate initial buffer if needed */
    if (ctx->inbuf == NULL) {
	DEBUG1("allocating inbuf, size=%d", INBUF_INITIAL_SIZE);
	if ((ctx->inbuf = malloc(INBUF_INITIAL_SIZE)) == NULL) {
	    SYSERR0("couldn't malloc inbuf");
	    *errorp = KQML_ERROR_MALLOC_ERROR;
	    ctx->state = ERROR;
	    return NULL;
	}
	ctx->inlen = INBUF_INITIAL_SIZE;
	ctx->incnt = 0;
    }
    /* Save character in context inbuf (adding NUL here is useful later) */
    ctx->inbuf[ctx->incnt++] = c;
    ctx->inbuf[ctx->incnt] = '\0';
    DEBUG1("incnt now %d", ctx->incnt);
    /* Reallocate larger buffer if needed (always leave run for NUL) */
    if (ctx->incnt >= ctx->inlen-1) {
	ctx->inlen = INBUF_REALLOC_FUNC(ctx->inlen);
	DEBUG1("reallocating inbuf, size=%d", ctx->inlen);
	if ((ctx->inbuf = realloc(ctx->inbuf, ctx->inlen)) == NULL) {
	    SYSERR0("couldn't realloc inbuf");
	    *errorp = KQML_ERROR_REALLOC_ERROR;
	    ctx->state = ERROR;
	    return NULL;
	}
    }
    /* Adjust context state based on character */
    switch (ctx->state) {
      case START:			/* Waiting for open paren */
      case ERROR:
	switch (c) {
	  case '(':				/* Open paren */
	    DEBUG0("START: open paren: 1");
	    ctx->nparens = 1;
	    ctx->state = NORMAL;
	    break;
	  case ' ': case '\t': case '\n': case '\f': /* Skip whitespace */
	    /* Only skip leading whitespace */
	    if (ctx->state == START) {
		DEBUG0("START: skip whitespace");
		ctx->incnt -= 1;
		break;
	    }
	    /* Otherwise fall through to error case */
	  default:				/* Otherwise error */
	    DEBUG2("expected `(', got `%c' (%d) instead", c, (int)c);
	    *errorp = KQML_ERROR_BAD_START;
	    /* Subtle: backup so garbage isn't here later */
	    ctx->incnt -= 1;
	    ctx->state = ERROR;
	    return NULL;
	}
	break;
      case NORMAL:			/* Parsing when not in any string */
	switch (c) {
	  case '(':				/* Open paren */
	    ctx->nparens += 1;
	    DEBUG1("NORMAL: open paren: %d", ctx->nparens);
	    break;
	  case ')':				/* Close paren */
	    ctx->nparens -= 1;
	    DEBUG1("NORMAL: close paren: %d", ctx->nparens);
	    break;
	  case '"':				/* Start quoted string */
	    DEBUG0("NORMAL: quote");
	    ctx->state = QUOTED;
	    break;
	  case '#':				/* Start hashed string */
	    DEBUG0("NORMAL: hash");
	    ctx->state = HASH;
	    ctx->hashnum = 0;
	    break;
	}
	break;
      case QUOTED:			/* Inside quoted string */
	switch (c) {
	  case '\\':
	    DEBUG0("QUOTED: backslash");
	    ctx->state = BSLASH;		/* Backslash escapes next */
	    break;
	  case '"':				/* Quote ends string */
	    DEBUG0("QUOTED: quote");
	    ctx->state = NORMAL;
	    break;
	}
	break;
      case BSLASH:			/* After backslash... */
	DEBUG0("BSLASH: reverting to QUOTED");
	ctx->state = QUOTED;			/* ...back to quoted string */
	break;
      case HASH:			/* Number follows hash sign */
	if (isdigit(c)) {
	    ctx->hashnum = ctx->hashnum * 10 + c - 'a';
	    DEBUG1("HASH: digit: %d", ctx->hashnum);
	} else if (c == '"') {			/* Quote ends number */
	    DEBUG1("HASH: quote: %d", ctx->hashnum);
	    if (ctx->hashnum > 0) {
		ctx->state = HASHED;
	    } else {
		ctx->state = NORMAL;
	    }
	} else {				/* Otherwise error */
	    DEBUG2("expected digit or \", got `%c' (%d) instead", c, (int)c);
	    *errorp = KQML_ERROR_BAD_HASH;
	    /* Could be state=ERROR here, but that interferes with the way
	     * we handle leading errors (inter-message garbage) above. So
	     * instead we'll go to state NORMAL and just sort of plug along.
	     * Hard to say what the right thing to do is here... Eventually
	     * we'll try to parse and just fail.
	     */
	    ctx->state = NORMAL;
	    /*
	     * Making the incnt==0 here is a grotty hack, but it saves
	     * repeatedly printing the same garbage later.
	     */
	    ctx->incnt = 0;
	    return NULL;
	}
	break;
      case HASHED:			/* Inside hashed string */
	ctx->hashnum -= 1;
	DEBUG1("HASHED: %d", ctx->hashnum);
	if (ctx->hashnum == 0) {
	    ctx->state = NORMAL;
	}
	break;
      case DONE:
        /* do nothing */
	break;
    }
    DEBUG2("state=%d, nparens=%d", ctx->state, ctx->nparens);
    /* If after all this we have balanced parens, we are done */
    if (ctx->state == NORMAL && ctx->nparens == 0) {
	/* Parse performative */
	DEBUG1("parsing text \"%s\"", ctx->inbuf);
	p = KQMLParsePerformative(ctx->inbuf);
	/* Reset context buffer */
	ctx->incnt = 0;
	/* Return error or performative */
	if (p == NULL) {
	    DEBUG0("ERROR");
	    *errorp = KQML_ERROR_PARSE_ERROR;
	    ctx->state = ERROR;
	    return NULL;
	} else {
	    DEBUG0("DONE");
	    *errorp = 1;
	    ctx->state = DONE;
	    return p;
	}
    }
    /* Otherwise we're still processing */
    DEBUG0("CONTINUING");
    *errorp = 1;
    return NULL;
}

/*	-	-	-	-	-	-	-	-	*/
/*
 * This routine reads a single char from the given file descriptor,
 * and parses it.
 * Returns: performative and *errorp = 0 if successful
 *          NULL and *errorp > 0 if read is not yet complete
 *          NULL and *errorp = 0 on EOF
 *          NULL and *errorp < 0 on error
 * If the parse was not completed, calling this function again will
 * continue it (hence the KQMLContext* argument).
 */
KQMLPerformative *
KQMLReadAndParse1(int fd, KQMLContext *ctx, KQMLError *errorp)
{
    KQMLPerformative *p;
    int n;
    char c;

    /* Read one char */
    DEBUG1("reading from fd=%d", fd);
    if ((n = read(fd, &c, 1)) != 1) {
	/* Read failed, why? */
	if (n < 0) {
	    /* Error */
	    if (errno == EINTR) {
		DEBUG0("read interrupted");
		*errorp = KQML_ERROR_READ_INTERRUPTED;
	    } else {
		SYSERR0("read failed");
		*errorp = KQML_ERROR_READ_ERROR;
	    }
	} else {
	    /* EOF */
	    DEBUG0("read EOF");
	    *errorp = 0;
	}
	return NULL;
    }
    /* Read char ok */
    DEBUG2("read `%c' (%d)", c, c);
    /* Add char to parsing context */
    return KQMLInputChar(c, ctx, errorp);
}

/*
 * This routine reads a performative from the given file descriptor.
 * Returns: performative and *errorp = 0 if successful
 *          NULL and *errorp = 0 on EOF
 *          NULL and *errorp < 0 on error
 * The context used here needn't be static, but it saves malloc/freeing.
 * The text of the message (or text leading up to an error) is stored
 * in *txtp, if non-NULL, freshly malloc'd.
 */
KQMLPerformative *
KQMLRead(int fd, KQMLError *errorp, char **txtp)
{
    static KQMLContext *ctx = NULL;
    KQMLPerformative *p;
    int n;
    char c;

    DEBUG1("reading from fd=%d", fd);
    /* Create or reset parse context */
    if (ctx == NULL) {
	ctx = KQMLNewContext();
    } else {
	KQMLResetContext(ctx);
    }
    /* Keep trying to read until completed or error */
    while ((p = KQMLReadAndParse1(fd, ctx, errorp)) == NULL && *errorp > 0) {
	/*EMPTY*/
    }
    /* Return perf (or NULL) and *errorp is already set */
    DEBUG2("parse done or error: p=0x%lx, error=%d", p, *errorp);
    /* Save message text, if asked to do so */
    if (txtp) {
	if (ctx->inbuf) {
	    DEBUG1("saving message text: \"%s\"", ctx->inbuf);
	    if ((*txtp = malloc(strlen(ctx->inbuf)+1)) == NULL) {
		SYSERR0("couldn't malloc to save message text");
	    } else {
		strcpy(*txtp, ctx->inbuf);
	    }
	} else {
	    DEBUG0("no inbuf for message text");
	    *txtp = NULL;
	}
    }
    /* Ok, we're done */
    return p;
}

/*
 * This routine reads a performative from the given file descriptor
 * without blocking in read(2).
 * Returns: performative and *errorp = 0 if successful
 *          NULL and *errorp > 0 if read is not yet complete
 *          NULL and *errorp = 0 on EOF
 *          NULL and *errorp < 0 on error
 * The text of the message (or text leading up to an error) is stored
 * in *txtp, if non-NULL, freshly malloc'd. Note that this is not *only*
 * the text read *this* time--it is the text read *so far* on this message,
 * or up to this error.
 *
 * This function uses a static array of contexts indexed by file descriptor
 * so the caller doesn't have to worry about managing them. I wonder if this
 * is actually a good idea...
 */
KQMLPerformative *
KQMLReadNoHang(int fd, KQMLError *errorp, char **txtp)
{
    KQMLPerformative *p;
    int num;		/* SunOS4 manpage says long, header says int */
    int total, done;

    DEBUG1("reading from fd=%d", fd);
    /* Sanity check */
    if (fd < 0 || fd > FD_SETSIZE) {
	*errorp = KQML_ERROR_BAD_FILE;
	return NULL;
    }
    /* Create parse context if needed (but don't reset existing ones!) */
    if (contexts[fd] == NULL) {
	contexts[fd] = KQMLNewContext();
    }
    total = 0;
    done = 0;
    while (!done) {
	/* How many bytes are available? */
	DEBUG0("calling ioctl FIONREAD");
	if (ioctl(fd, FIONREAD, &num) < 0) {
	    SYSERR0("ioctl failed");
	    *errorp = KQML_ERROR_IOCTL_ERROR;
	    p = NULL;
	    break;
	}
	/* Nothing available -> stop reading and return total read */
	if (num <= 0) {
	    DEBUG1("nothing more to read, total=%d", total);
	    *errorp = total;
	    p = NULL;
	    break;
	}
	DEBUG1("%d chars available", num);
	/* Otherwise read up to that many */
	while (num > 0) {
	    DEBUG1("calling KQMLReadAndParse1, num=%d", num);
	    /* Try to read the next char */
	    if ((p = KQMLReadAndParse1(fd, contexts[fd], errorp)) != NULL ||
		*errorp <= 0) {
		/* Parse completed or error or EOF */
		DEBUG2("parse done or error: p=0x%lx, error=%d", p, *errorp);
		done = 1;
		break;
	    }
	    /* Read one char successfully and parse is continuing */
	    total += 1;
	    num -= 1;
	}
    }
    /* Done reading: save message text, if asked to do so */
    if (txtp) {
	if (contexts[fd]->inbuf) {
	    DEBUG1("saving message text: \"%s\"", contexts[fd]->inbuf);
	    if ((*txtp = malloc(strlen(contexts[fd]->inbuf)+1)) == NULL) {
		SYSERR0("couldn't malloc to save message text");
	    } else {
		strcpy(*txtp, contexts[fd]->inbuf);
	    }
	} else {
	    DEBUG0("no inbuf for message text");
	    *txtp = NULL;
	}
    }
    /* Ok, we're done */
    return p;
}

#ifdef undef
/*	-	-	-	-	-	-	-	-	*/
/*
 * This is just a simple buffering scheme to avoid calling read(2) all
 * the time. It is simplified by the fact that we know we're only reading
 * one char at a time. Otherwise we'd probably be better off just using
 * stdio via fdopen(3).
 */
typedef struct _fdbuf_s {
    char buf[BUFSIZ];
    char *next;
    int num;
} FdBuf;
static FdBuf *fdbuf[FD_SETSIZE];

int
gread1(int fd, char *dst)
{
    /* Do we need to allocate a buffer for this fd? */
    if (fdbuf[fd] == NULL) {
	/* Yes */
	DEBUG1("creating buffer for fd=%d", fd);
	if ((fdbuf[fd] = (FdBuf*)malloc(sizeof(FdBuf))) == NULL) {
	    SYSERR0("couldn't malloc for fdbuf");
	    return -1;
	}
	fdbuf[fd]->num = 0;
    }
    /* Do we need to try and fill the buffer? */
    if (fdbuf[fd]->num <= 0) {
	/* Originally, I just called read, but in fact we have to be smarter
	 * to avoid blocking, I think, despite the way we look after
	 * non-blocking in the KQML routines themselves. This is getting
	 * more and more complicated...
	 */
	int toread;
	DEBUG1("filling buffer for fd=%d", fd);
	if (ioctl(fd, FIONREAD, &toread) < 0) {
	    SYSERR0("ioctl failed");
	    return -1;
	}
	DEBUG1("FIONREAD said %d bytes ready", toread);
	if (toread > BUFSIZ) {
	    toread = BUFSIZ;
	}
	/* Call read(2) to fill the buffer */
	DEBUG0("calling read...");
	if ((fdbuf[fd]->num = read(fd, fdbuf[fd]->buf, toread)) <= 0) {
	    /* Error or EOF: return it and leave errno alone */
	    DEBUG1("read failed, returning %d", fdbuf[fd]->num);
	    return fdbuf[fd]->num;
	}
	/* Read ok, reset next pointer to start of buffer */
	DEBUG1("read ok, %d bytes", fdbuf[fd]->num);
	fdbuf[fd]->next = fdbuf[fd]->buf;
    }
    /* Store next character in destination buffer */
    *dst = *(fdbuf[fd]->next);
    /* Decrement count */
    fdbuf[fd]->next += 1;
    fdbuf[fd]->num -= 1;
    /* Return number of chars read (always 1 in this case) */
    return 1;
}
#endif
