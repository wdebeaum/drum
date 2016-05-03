/*
 * KQML.c: Routines for allocating and manipulating KQML forms
 *
 * George Ferguson, ferguson@cs.rochester.edu,  6 Nov 1995
 * Time-stamp: <96/03/19 16:00:58 ferguson>
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "util/error.h"
#include "KQML.h"

/*
 * Functions defined here:
 */
KQMLPerformative *KQMLNewPerformative(char *verb);
void KQMLFreePerformative(KQMLPerformative *perf);
char *KQMLGetParameter(KQMLPerformative *perf, char *key);
KQMLParameter *KQMLSetParameter(KQMLPerformative *perf, char *key, char*value);
int KQMLParseKeywordList(char *in, char **keys, char ***vals);
static char *gnewstr(char *str);

/*	-	-	-	-	-	-	-	-	*/

KQMLPerformative *
KQMLNewPerformative(char *verb)
{
    KQMLPerformative *this;

    if ((this = (KQMLPerformative *)malloc(sizeof(KQMLPerformative))) == NULL){
	SYSERR0("couldn't malloc for performative");
	return NULL;
    }
    this->verb = gnewstr(verb);
    this->parameters = NULL;
    return this;
}

void
KQMLFreePerformative(KQMLPerformative *perf)
{
    KQMLParameter *p, *nextp;

    /* Sanity check */
    if (perf == NULL) {
	return;
    }
    /* Free parameters */
    for (p=perf->parameters; p != NULL; p = nextp) {
	nextp = p->next;
	if (p->key) {
	    free(p->key);
	}
	if (p->value) {
	    free(p->value);
	}
	free(p);
    }
    free(perf->verb);
    /* Free performative itself */
    free(perf);
}

char *
KQMLGetParameter(KQMLPerformative *perf, char *key)
{
    KQMLParameter *p;

    /* Sanity check */
    if (perf == NULL) {
	return NULL;
    }
    /* Scan for parameter */
    for (p=perf->parameters; p != NULL; p = p->next) {
	if (STREQ(key, p->key)) {
	    /* Found: return value */
	    return p->value;
	}
    }
    /* Not found */
    return NULL;
}

KQMLParameter *
KQMLSetParameter(KQMLPerformative *perf, char *key, char *value)
{
    KQMLParameter *p;

    /* Sanity check */
    if (perf == NULL || key == NULL) {
	return NULL;
    }
    /* Scan for parameter */
    for (p=perf->parameters; p != NULL; p = p->next) {
	if (STREQ(key, p->key)) {
	    /* Found: set new value */
	    if (p->value) {
		free(p->value);
	    }
	    p->value = gnewstr(value);
	    /* Done */
	    return p;
	}
    }
    /* If not found, need to create it */
    if ((p = (KQMLParameter*)malloc(sizeof(KQMLParameter))) == NULL) {
	SYSERR0("couldn't malloc for parameter");
	return NULL;
    }
    /* Set key and value */
    p->key = gnewstr(key);
    p->value = gnewstr(value);
    /* Add to list of parameters */
    if (perf->parameters == NULL) {
	/* First parameter in list */
	perf->parameters = perf->lastparm = p;
    } else {
	/* Add to end of list */
	perf->lastparm->next = p;
	perf->lastparm = p;
    }
    p->next = NULL;
    /* Return it */
    return p;
}

/*
 * This function parses a KQML list into a set of keyword/value pairs.
 * It fills in the pointers in the `vals' array appropriately.
 * It returns the number of keywords matched, or -1 on error.
 */
int
KQMLParseKeywordList(char *in, char **keys, char ***vals)
{
    char **strings;
    int i, k, num;

    /* Skip leading whitespace */
    while (isspace(*in)) {
	in += 1;
    }
    /* Break input into array of strings */
    if ((strings = KQMLParseList(in)) == NULL) {
	return -1;
    }
    /* Scan list of arguments (skipping first one) */
    num = 0;
    for (i=1; strings[i] != NULL; i+=2) {
	/* Check that we do indeed have a pair here */
	if (strings[i+1] == NULL) {
	    /* Nope, leftover thingo at end of list, free it and bail */
	    free(strings[i]);
	    break;
	}
	/* Scan list of keys */
	for (k=0; keys[k] != NULL; k++) {
	    /* Matched this key? */
	    if (STREQ(strings[i], keys[k])) {
		/* Yes; is there a place to store it? */
		if (vals[k] != NULL) {
		    /* Yes; store it there */
		    *(vals[k]) = strings[i+1];
		} else {
		    /* Otherwise just free string since we don't want it */
		    free(strings[i+1]);
		}
		num += 1;
		break;
	    }
	    /* Free the keyword */
	    free(strings[i]);
	}
    }
    /* Free array of strings */
    free(strings);
    /* return number of matches */
    return num;
}
    
/*	-	-	-	-	-	-	-	-	*/

char *
gnewstr(char *s)
{
    char *new;

    if (s == NULL) {
	return NULL;
    }
    if ((new = malloc(strlen(s)+1)) == NULL) {
	SYSERR0("couldn't malloc for string");
	return NULL;
    }
    strcpy(new, s);
    return new;
}
