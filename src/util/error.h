/*
 * error.h
 *
 * George Ferguson, ferguson@cs.rochester.edu, 24 Mar 1995
 */

#ifndef _error_h_gf
#define _error_h_gf

#ifdef __cplusplus
extern "C" {
#endif

#include <string.h>
#include <errno.h>

#ifdef DEBUG

#include "debug.h"

#define ERROR0(S)       fprintf(stderr,_DFMT S "\n",_DFUNC); \
			DEBUG0(S)
#define ERROR1(S,A1)    fprintf(stderr,_DFMT S "\n",_DFUNC,A1); \
			DEBUG1(S,A1)
#define ERROR2(S,A1,A2) fprintf(stderr,_DFMT S "\n",_DFUNC,A1,A2); \
			DEBUG2(S,A1,A2)
#define ERROR3(S,A1,A2,A3) \
			fprintf(stderr,_DFMT S "\n",_DFUNC,A1,A2,A3);\
			DEBUG3(S,A1,A2,A3)

#define SYSERR0(S) \
	fprintf(stderr,_DFMT S ": %s\n", _DFUNC, strerror(errno)); \
	DEBUG1( S ": %s", strerror(errno))

#define SYSERR1(S,A1) \
	fprintf(stderr,_DFMT S ": %s\n", _DFUNC, A1, strerror(errno)); \
	DEBUG2( S ": %s", A1, strerror(errno))

#define SYSERR2(S,A1,A2) \
	fprintf(stderr,_DFMT S ": %s\n", _DFUNC, A1, A2, strerror(errno)); \
	DEBUG3( S ": %s", A1, A2, strerror(errno))

#else /*!DEBUG*/

extern FILE *debugfp;
extern char *progname;

#ifdef __GNUC__
# define _DFMT		"%s:%s: "
# define _DFUNC		progname,__FUNCTION__
#else
# define _DFMT		"%s:%s(%d): "
# define _DFUNC		progname,__FILE__,__LINE__
#endif

#define ERROR0(S)       fprintf(stderr,_DFMT S "\n",_DFUNC)
#define ERROR1(S,A1)    fprintf(stderr,_DFMT S "\n",_DFUNC,A1)
#define ERROR2(S,A1,A2) fprintf(stderr,_DFMT S "\n",_DFUNC,A1,A2)
#define ERROR3(S,A1,A2,A3) fprintf(stderr,_DFMT S "\n",_DFUNC,A1,A2,A3)

#define SYSERR0(S) \
	fprintf(stderr,_DFMT S ": %s\n", _DFUNC, strerror(errno))
#define SYSERR1(S,A1) \
	fprintf(stderr,_DFMT S ": %s\n", _DFUNC, A1, strerror(errno))
#define SYSERR2(S,A1,A2) \
	fprintf(stderr,_DFMT S ": %s\n", _DFUNC, A1, A2, strerror(errno))

#endif /*!DEBUG*/

#ifdef __cplusplus
}
#endif

#endif
