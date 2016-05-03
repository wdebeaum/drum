/*
 * trlib_parse.c
 *
 * George Ferguson, ferguson@cs.rochester.edu,  7 Mar 1996
 * Time-stamp: <96/07/31 15:17:09 ferguson>
 */
#include <stdio.h>
#include "parse.h"
#include "error.h"
#include "util/memory.h"
#include "util/debug.h"

/*
 * Functions defined here:
 */
void trlibParsePerformative(KQMLPerformative *perf, TrlibParseDef *defs);

/*	-	-	-	-	-	-	-	-	*/

void
trlibParsePerformative(KQMLPerformative *perf, TrlibParseDef *defs)
{
    char *content = NULL, **contents = NULL;

    if (perf == NULL || defs == NULL) {
	return;
    }
    DEBUG1("performative=%s", KQML_VERB(perf));
    while (defs->verb != NULL) {
	/* Look for performative verb in defs */
	if (STREQ(KQML_VERB(perf), defs->verb)) {
	    break;
	}
	defs += 1;
    }
    /* If not found, then this is a bad performative (for us) */
    if (defs->verb == NULL) {
	if (STREQ(KQML_VERB(perf), "error")) {
	    DEBUG0("ignoring error performative");
	} else {
	    trlibErrorReply(perf, ERR_BAD_PERFORMATIVE, KQML_VERB(perf));
	}
	return;
    }
    /* If found and don't care about content, call callback now (if any) */
    if (defs->content0 == NULL) {
	DEBUG0("don't care about content0");
	if (defs->cb != NULL) {
	    /* Call callback */
	    (*(defs->cb))(perf, NULL);
	}	
	return;
    }
    /* Otherwise we know we have to at least look at the content */
    if ((content = KQMLGetParameter(perf, ":content")) == NULL) {
	trlibErrorReply(perf, ERR_MISSING_PARAMETER, ":content");
    } else if ((contents = KQMLParseList(content)) == NULL) {
	trlibErrorReply(perf, ERR_SYNTAX_ERROR, content);
    } else if (contents[0] == NULL) {
	trlibErrorReply(perf, ERR_SYNTAX_ERROR, "empty contents");
    } else {
	/* Continue searching the defs[] array for a content match */
	while (defs->verb != NULL) {
	    /* Compare first word of contents */
	    if (STREQ(KQML_VERB(perf), defs->verb) &&
		STREQ(contents[0], defs->content0)) {
		/* Matched! */
		DEBUG2("match: verb=%s, content0=%s",
		       defs->verb, defs->content0);
		if (defs->cb != NULL) {
		    /* Call callback */
		    (*(defs->cb))(perf, contents);
		}
		/* Stop looking for matches */
		break;
	    }
	    /* Otherwise keep looking */
	    defs += 1;
	}
	/* If not found, then this content is bad */
	if (defs->verb == NULL) {
	    trlibErrorReply(perf, ERR_BAD_CONTENT, contents[0]);
	}
    }
    gfreeall(contents);
    DEBUG0("done");
}
