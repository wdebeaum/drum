/*
 * trlib_error.h
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Mar 1996
 * Time-stamp: <96/09/13 12:11:10 ferguson>
 */

#ifndef _trlib_error_h_gf
#define _trlib_error_h_gf

#ifdef __cplusplus
extern "C" {
#endif

#include "trlib_timestamp.h"
#include "KQML/KQML.h"

typedef enum {
    ERR_SYNTAX_ERROR = 500,
    ERR_MISSING_PARAMETER,
    ERR_MISSING_ARGUMENT,
    ERR_BAD_ARGUMENT,
    ERR_UNKNOWN_RECEIVER = 510,
    ERR_BAD_PERFORMATIVE = 520,
    ERR_BAD_CONTENT = 530,
    ERR_BAD_MODULE = 550,
    ERR_BAD_VALUE,
} TrlibErrorCode;

extern void trlibErrorReply(KQMLPerformative *perf,
			    TrlibErrorCode code, char *comment);

#ifdef __cplusplus
}
#endif
#endif
