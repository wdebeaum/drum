/*
 * trlib_parse.h
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Mar 1996
 * Time-stamp: <96/09/13 12:11:34 ferguson>
 */

#ifndef _trlib_parse_h_gf
#define _trlib_parse_h_gf

#ifdef __cplusplus
extern "C" {
#endif

#include "trlib_timestamp.h"
#include "KQML/KQML.h"

typedef void (*TrlibParseCallbackProc)(KQMLPerformative *perf,char **contents);

typedef struct _TrlibParseDef {
    char *verb;
    char *content0;
    TrlibParseCallbackProc cb;
} TrlibParseDef;

void trlibParsePerformative(KQMLPerformative *perf, TrlibParseDef *defs);

#ifdef __cplusplus
}
#endif
#endif
