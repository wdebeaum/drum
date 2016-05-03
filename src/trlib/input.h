/*
 * trlib_input.h
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Mar 1996
 * Time-stamp: <96/09/13 12:11:26 ferguson>
 */

#ifndef _trlib_input_h_gf
#define _trlib_input_h_gf

#ifdef __cplusplus
extern "C" {
#endif

#include "trlib_timestamp.h"
#include "KQML/KQML.h"

typedef void (*TrlibCallbackProc)(KQMLPerformative *perf);

enum {
    TRLIB_DONTBLOCK = 0,
    TRLIB_BLOCK = 1
};

extern int trlibInput(int fd, int block, TrlibCallbackProc cbp);

#ifdef __cplusplus
}
#endif
#endif
