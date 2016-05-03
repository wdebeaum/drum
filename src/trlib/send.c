/*
 * trlib_send.c
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Mar 1996
 * Time-stamp: <96/03/07 12:05:56 ferguson>
 *
 * This a *very simple* send routine, suitable only when we don't care if
 * the output blocks.
 */
#include <stdio.h>
#include "send.h"
#include "util/debug.h"

/*
 * Functions defined here:
 */
void trlibSendPerformative(FILE *fp, KQMLPerformative *perf);

/*	-	-	-	-	-	-	-	-	*/

void
trlibSendPerformative(FILE *fp, KQMLPerformative *perf)
{
    KQMLParameter *q;

    fprintf(fp, "(%s", perf->verb);
    for (q=perf->parameters; q != NULL; q=q->next) {
	if (q->key && q->value) {
	    fprintf(fp, " %s %s", q->key, q->value);
	}
    }
    /* This newline is not strictly necessary, but may help some clients */
    fprintf(fp, ")\n");
    fflush(fp);
}
