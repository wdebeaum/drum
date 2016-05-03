/*
 * debug.h: Debugging output macros
 *
 * George Ferguson, ferguson@cs.rochester.edu, 10 Feb 1995
 */

#ifndef _debug_h_gf
#define _debug_h_gf

#ifdef __cplusplus
extern "C" {
#endif

#ifdef DEBUG

extern FILE *debugfp;
extern char *progname;

#ifdef __GNUC__
# define _DFMT		"%s:%s: "
# define _DFUNC		progname,__FUNCTION__
#else
# define _DFMT		"%s:%s(%d): "
# define _DFUNC		progname,__FILE__,__LINE__
#endif

#define DEBUG0(S) \
	if (debugfp) fprintf(debugfp,_DFMT S "\n",_DFUNC)
#define DEBUG1(S,A1) \
	if (debugfp) fprintf(debugfp,_DFMT S "\n",_DFUNC,A1)
#define DEBUG2(S,A1,A2) \
	if (debugfp) fprintf(debugfp,_DFMT S "\n",_DFUNC,A1,A2)
#define DEBUG3(S,A1,A2,A3) \
	if (debugfp) fprintf(debugfp,_DFMT S "\n",_DFUNC,A1,A2,A3)
#define DEBUG4(S,A1,A2,A3,A4) \
	if (debugfp) fprintf(debugfp,_DFMT S "\n",_DFUNC,A1,A2,A3,A4)

#else /* !DEBUG */

#define DEBUG0(S)		/*EMPTY*/
#define DEBUG1(S,A1)		/*EMPTY*/
#define DEBUG2(S,A1,A2)		/*EMPTY*/
#define DEBUG3(S,A1,A2,A3)	/*EMPTY*/
#define DEBUG4(S,A1,A2,A3,A4)	/*EMPTY*/

#endif

#ifdef __cplusplus
}
#endif

#endif
