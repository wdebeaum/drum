/*
 * hostname.h
 *
 * George Ferguson, ferguson@cs.rochester.edu,  2 Jan 1996
 * Time-stamp: <96/10/10 15:18:37 ferguson>
 */

#ifndef _hostname_h_gf
#define _hostname_h_gf

#ifdef __cplusplus
extern "C" {
#endif

#include "trlib_timestamp.h"

extern char *trlibGetHostname(void);

#ifdef __cplusplus
}
#endif
#endif
