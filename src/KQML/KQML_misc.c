/*
 * KQML_misc.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 22 Mar 1996
 * Time-stamp: <96/04/11 15:03:01 ferguson>
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "util/error.h"
#include "KQML.h"

/*
 * Functions defined here:
 */
char *KQMLPerformativeToString(KQMLPerformative *perf);
KQMLPerformative *KQMLCopyPerformative(KQMLPerformative *perf);

/*	-	-	-	-	-	-	-	-	*/

char *
KQMLPerformativeToString(KQMLPerformative *perf)
{
    KQMLParameter *q;
    char *str;
    int len;

    /* Sanity check */
    if (perf == NULL || perf->verb == NULL) {
	return NULL;
    }
    /* Length of string starts with verb */
    len = strlen(perf->verb);
    /* Then each of the parameters, key and value */
    for (q=perf->parameters; q != NULL; q=q->next) {
	if (q->key && q->value) {
	    /* Plus two for the spaces */
	    len += strlen(q->key) + strlen(q->value) + 2;
	}
    }
    /* Plus two for the parens, and one for the NUL */
    len += 3;
    /* Allocate that long a string */
    if ((str = malloc(len)) == NULL) {
	SYSERR0("couldn't malloc for string");
	return NULL;
    }
    /* And now format the performative into the string */
    sprintf(str, "(%s", perf->verb);
    for (q=perf->parameters; q != NULL; q=q->next) {
	if (q->key && q->value) {
	    sprintf(str+strlen(str), " %s %s", q->key, q->value);
	}
    }
    sprintf(str+strlen(str), ")");
    /* Return the string */
    return str;
}

/*	-	-	-	-	-	-	-	-	*/

KQMLPerformative *
KQMLCopyPerformative(KQMLPerformative *perf)
{
    KQMLPerformative *newperf;
    KQMLParameter *p;

    /* Sanity check */
    if (perf == NULL || perf->verb == NULL) {
	return NULL;
    }
    /* Allocate new perf with same verb */
    if ((newperf = KQMLNewPerformative(perf->verb)) == NULL) {
	return NULL;
    }
    /* Copy all parameters (will copy the strings) */
    for (p=perf->parameters; p != NULL; p = p->next) {
	if (p->key) {
	    KQMLSetParameter(newperf, p->key, p->value);
	}
    }
    /* Return new perf */
    return newperf;
}
