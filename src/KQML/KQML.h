/*
 * KQML.h
 *
 * George Ferguson, ferguson@cs.rochester.edu,  6 Nov 1995
 * Time-stamp: <Fri Mar 20 23:49:38 CDT 2020 lgalescu>
 *
 * The definition of parameters using a list means that modules need
 * not be re-linked if the set of possible parameters changes.
 *
 * gf: 8 Jan 2010: After 15 years... It would be better to implement
 * these things as opaque types rather than including the structure
 * defs in this header file. But many dependencies would need to be
 * checked.
 */

#ifndef _KQML_h_gf
#define _KQML_h_gf

#ifdef __cplusplus
extern "C" {
#endif

#include <strings.h>
  
/*
 * The KQML spec isn't clear about case-sensitivity. Although I would
 * expect that it should be case-sensitive, I suspect that it probably
 * isn't, since Lisp isn't. Anyway, making this be strcasecmp(3) makes
 * the parser case-insensitive; making this strcmp(3) makes it case-
 * sensitive.
 */
#ifndef STREQ
# define STREQ(S1,S2)	(strcasecmp(S1, S2) == 0)
#endif

typedef struct _KQMLParameter_s {
    char *key;
    char *value;
    struct _KQMLParameter_s *next;
} KQMLParameter;

typedef struct _KQMLPerformative_s {
    char *verb;
    KQMLParameter *parameters, *lastparm;
} KQMLPerformative;

#define KQML_VERB(P)		((P)->verb)

/*
 * Memory allocation and manipulation
 */
extern KQMLPerformative *KQMLNewPerformative(char *verb);
extern void KQMLFreePerformative(KQMLPerformative *perf);
extern char *KQMLGetParameter(KQMLPerformative *perf, char *key);
extern KQMLParameter *KQMLSetParameter(KQMLPerformative *perf,
				       char *key, char *value);

/*
 * Parsing KQML structures
 */
extern char *KQMLParseString(char *in);
extern char *KQMLParseQuotedString(char *in);
extern char *KQMLParseSharpedString(char *in);
extern char **KQMLParseList(char *in);
extern char **KQMLParseStringList(char *in);
extern char *KQMLParseThing(char *in);
extern char **KQMLParseThingList(char *in);
extern KQMLPerformative *KQMLParsePerformative(char *in);
extern int KQMLParseKeywordList(char *in, char **keys, char ***vals);

/*
 * KQML I/O: Errors <KQML_ERROR_SYSTEM_ERROR are system call failures.
 */
typedef enum {
    KQML_ERROR_BAD_CONTEXT = -1,
    KQML_ERROR_BAD_FILE = -2,
    KQML_ERROR_BAD_START = -3,
    KQML_ERROR_BAD_HASH = -4,
    KQML_ERROR_PARSE_ERROR = -5,
    KQML_ERROR_READ_INTERRUPTED = -6,
    KQML_ERROR_SYSTEM_ERROR = -100,
    KQML_ERROR_READ_ERROR = -101,
    KQML_ERROR_MALLOC_ERROR = -102,
    KQML_ERROR_REALLOC_ERROR = -103,
    KQML_ERROR_IOCTL_ERROR = -104
} KQMLError;

typedef enum {
    START, NORMAL, QUOTED, BSLASH, HASH, HASHED, ERROR, DONE
} KQMLParseState;

typedef struct _KQMLContext_s {
    KQMLParseState state;
    char *inbuf;
    int inlen;
    int incnt;
    int nparens;
    int hashnum;
} KQMLContext;

extern KQMLContext *KQMLNewContext(void);
extern void KQMLDestroyContext(KQMLContext *ctx);
extern void KQMLResetContext(KQMLContext *ctx);
extern KQMLPerformative *KQMLInputChar(char c, KQMLContext *ctx, KQMLError *errorp);
extern KQMLPerformative *KQMLReadAndParse1(int fd, KQMLContext *ctx, KQMLError *errorp);
extern KQMLPerformative *KQMLRead(int fd, KQMLError *errorp, char **txtp);
extern KQMLPerformative *KQMLReadNoHang(int fd, KQMLError *errorp, char **txtp);

extern char *KQMLErrorString(KQMLError num);

/*
 * Misc. KQML ops
 */
extern char *KQMLPerformativeToString(KQMLPerformative *perf);
extern KQMLPerformative *KQMLCopyPerformative(KQMLPerformative *perf);

#ifdef __cplusplus
}
#endif
#endif
