/*
 * nonblockio.h
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Apr 1995
 * Time-stamp: <Wed Sep 24 13:25:49 EDT 2003 ferguson>
 *
 * Note: This is POSIX-style nonblocking IO, which seems to work well
 * on most systems (SunOS4 and Solaris, possibly also alphas).
 */

#ifndef _nonblockio_h_gf
#define _nonblockio_h_gf

#include <fcntl.h>

#define MAKE_NONBLOCKING(FD)	fcntl(FD, F_SETFL, O_NONBLOCK)
#define ISWOULDBLOCK(E)		((E) == EAGAIN)

/* gf: Added 1/18/96 for resetting on stdin */
#define MAKE_BLOCKING(FD)       { int flags = fcntl(FD, F_GETFL, 0); \
				  flags &= ~O_NONBLOCK;              \
				  fcntl(FD, F_SETFL, flags); }

#endif
