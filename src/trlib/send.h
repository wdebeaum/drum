/*
 * trlib_send.h
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Mar 1996
 * Time-stamp: <96/10/10 15:32:58 ferguson>
 */

#ifndef _trlib_send_h_gf
#define _trlib_send_h_gf

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include "KQML/KQML.h"

extern void trlibSendPerformative(FILE *fp, KQMLPerformative *perf);

#ifdef __cplusplus
}
#endif
#endif
