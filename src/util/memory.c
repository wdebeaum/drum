/*
 * memory.c
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Nov 1995
 * Time-stamp: <96/04/16 09:46:02 ferguson>
 */
#include <stdio.h>
#include <stdlib.h>
#include "memory.h"
#include "error.h"

/*
 * Functions defiend here:
 */
char *gnewstr(char *s);
void gfreeall(char **strs);
char **gcopyall(char **strs);

/*	-	-	-	-	-	-	-	-	*/

char *
gnewstr(char *s)
{
    char *new;

    if (s == NULL) {
	return NULL;
    }
    if ((new = malloc(strlen(s)+1)) == NULL) {
	SYSERR0("couldn't malloc for new string");
	return NULL;
    }
    strcpy(new, s);
    return new;
}

void
gfreeall(char **strs)
{
    int i;

    if (strs == NULL) {
	return;
    }
    for (i=0; strs[i] != NULL; i++) {
	free(strs[i]);
    }
    free(strs);
}

char **
gcopyall(char **strs)
{
    char **new;
    int n;

    /* Sanity check */
    if (strs == NULL) {
	return NULL;
    }
    /* Count number of strings in array */
    for (n=0; strs[n] != NULL; n++) {
	/*empty*/
    }
    /* Alloc for new array */
    if ((new = (char**)calloc(n+1, sizeof(char*))) == NULL) {
	SYSERR0("couldn't calloc to copy strings");
	return NULL;
    }
    /* Alloc and copy strings into new array */
    for (n=0; strs[n] != NULL; n++) {
	if ((new[n] = malloc(strlen(strs[n])+1)) == NULL) {
	    SYSERR1("couldn't malloc for string %d", n);
	    break;
	}
	strcpy(new[n], strs[n]);
    }
    /* Last entry is NULL */
    new[n] = NULL;
    /* Return new array */
    return new;
}
