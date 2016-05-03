/*
 * error_codes.h
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Nov 1995
 * Time-stamp: <96/08/02 13:15:00 ferguson>
 */

#ifndef _error_codes_h_gf
#define _error_codes_h_gf

typedef enum {
    ERR_SYNTAX_ERROR = 500,
    ERR_MISSING_PARAMETER,
    ERR_MISSING_ARGUMENT,
    ERR_BAD_ARGUMENT,
    ERR_UNKNOWN_RECEIVER = 510,
    ERR_BAD_PERFORMATIVE = 520,
    ERR_BAD_TELL,
    ERR_BAD_REQUEST,
    ERR_BAD_EVALUATE,
    ERR_BAD_MONITOR,
    ERR_BAD_UNMONITOR,
    ERR_BAD_REPLY,
    ERR_BAD_MODULE = 550,
    ERR_BAD_VALUE,
    ERR_BAD_CLASS,
} ErrorCodes;

extern char *errorCodeToString(int code);

#endif
