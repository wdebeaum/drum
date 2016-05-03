/*
 * KQML_error.c
 *
 * George Ferguson, ferguson@cs.rochester.edu,  9 Jan 1996
 * Time-stamp: <96/02/22 17:33:39 ferguson>
 */
#include "KQML.h"

/*
 * Functions defined here:
 */
char *KQMLErrorString(KQMLError num);

/*	-	-	-	-	-	-	-	-	*/
char *
KQMLErrorString(KQMLError num)
{
    char *s;

    switch (num) {
      case KQML_ERROR_BAD_CONTEXT: s = "bad parse context"; break;
      case KQML_ERROR_BAD_FILE: s = "bad file descriptor"; break;
      case KQML_ERROR_BAD_START: s = "expected `('"; break;
      case KQML_ERROR_BAD_HASH: s = "expected digit following `#'"; break;
      case KQML_ERROR_PARSE_ERROR: s = "parse error"; break;
      case KQML_ERROR_READ_INTERRUPTED: s = "read was interrupted"; break;
      case KQML_ERROR_SYSTEM_ERROR: s = "unspecified system error"; break;
      case KQML_ERROR_READ_ERROR: s = "read failed"; break;
      case KQML_ERROR_MALLOC_ERROR: s = "malloc failed"; break;
      case KQML_ERROR_REALLOC_ERROR: s = "realloc failed"; break;
      case KQML_ERROR_IOCTL_ERROR: s = "ioctl failed"; break;
      default: s = "!unknown error!";
    }
    return s;
}

