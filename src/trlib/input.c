/*
 * trlib_input.c
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Mar 1996
 * Time-stamp: <96/03/07 18:40:23 ferguson>
 */
#include <stdio.h>
#include "KQML/KQML.h"
#include "input.h"
#include "util/error.h"
#include "util/debug.h"

/*
 * Functions defined here:
 */
int trlibInput(int fd, int block, TrlibCallbackProc cbp);

/*	-	-	-	-	-	-	-	-	*/
/*
 * Returns <0 on serious error, 0 on EOF, otherwise 1.
 */
int
trlibInput(int fd, int block, TrlibCallbackProc cbp)
{
    KQMLPerformative *perf;
    KQMLError error;

    if (block) {
	DEBUG1("calling KQMLRead with fd=%d", fd);
	perf = KQMLRead(fd, &error, NULL);
    } else {
	DEBUG1("calling KQMLReadNoHang with fd=%d", fd);
	perf = KQMLReadNoHang(fd, &error, NULL);
    }
    if (error < 0) {
	ERROR2("KQML error %d (%s)", error, KQMLErrorString(error));
	if (error <= KQML_ERROR_SYSTEM_ERROR) {
	    return error;
	}
    } else if (error == 0) {
	DEBUG0("EOF!");
	return 0;
    } else if (perf != NULL) {
	/* Read complete */
	DEBUG0("processing msg");
	if (cbp) {
	    (*cbp)(perf);
	}
	/* Free performative */
	KQMLFreePerformative(perf);
    }
    return 1;
    DEBUG0("done");
}

