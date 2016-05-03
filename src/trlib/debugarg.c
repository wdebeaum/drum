/*
 * debugarg.c
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Mar 1996
 * Time-stamp: <96/03/12 10:58:00 ferguson>
 *
 * Very simple routine to look for -debug flag in options and do the
 * right thing to the global variable debugfp. The options are left
 * unchanged after this routine is finished.
 */
#include <stdio.h>
#include "util/error.h"
extern FILE *debugfp;

void
trlibDebugArg(int argc, char **argv)
{
    while (--argc > 0) {
	argv += 1;
	if (strcmp(argv[0], "-debug") == 0) {
	    if (argc < 2) {
		ERROR0("missing filename for -debug");
	    } else {
		if (strcmp(argv[1], "-") == 0) {
		    debugfp = stderr;
		} else if (*argv[1] == '|') {
		    if ((debugfp = popen(argv[1]+1, "w")) == NULL) {
			SYSERR1("couldn't open debug pipe \"%s\"", argv[1]+1);
		    } else {
			setbuf(debugfp, NULL);
		    }
		} else if ((debugfp = fopen(argv[1], "w")) == NULL) {
		    SYSERR1("couldn't open debug file \"%s\"", argv[1]);
		}
	    }
	    break;
	}
    }
}
