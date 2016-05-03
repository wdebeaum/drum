/*
 * KQML_parse.c: Parsing KQML structures
 *
 * George Ferguson, ferguson@cs.rochester.edu,  6 Nov 1995
 * Time-stamp: <Fri Apr 23 11:38:05 CDT 2010 lgalescu>
 *
 * Note that the KQML spec is very clear about where whitespace is and
 * isn't allowed, and these routines stick to the letter of the law.
 * For example, it's not allowed after an opening paren or before a
 * closing paren. Note also that `()' is not acceptable (use `nil').
 *
 * NOTE: We accept any `expression' as the first element of a list,
 *       whereas the KQML spec says that the first element must be a
 *	 `word'. This forbids lists of strings, for example.
 */
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "util/error.h"
#include "util/debug.h"
#include "KQML.h"

/*
 * This macro is used along with the ones from <ctype.h> to implement
 * the KQML character classes.
 */
#define isspecial(C)	((C)=='<' || (C)== '>' || (C)== '=' || (C) == '+' || \
			 (C)=='-' || (C)== '*' || (C)== '/' || (C) == '&' || \
			 (C)=='^' || (C)== '~' || (C)== '_' || (C) == '@' || \
			 (C)=='$' || (C)== '%' || (C)== ':' || (C) == '.' || \
			 (C)=='!' || (C)== '?' || (C)== '|')

/*
 * The KQML spec isn't clear about case-sensitivity. Although I would
 * expect that it should be case-sensitive, I suspect that it probably
 * isn't, since Lisp isn't. Anyway, making this be strcasecmp(3) makes
 * the parser case-insensitive; making this strcmp(3) makes it case-
 * sensitive.
 */
#define STREQ(S1,S2)	(strcasecmp(S1, S2) == 0)

/*
 * Handy macro
 */
#define SKIPWHITE(S)	while (isspace(*(S))) S += 1

/*
 * Functions defined here:
 */
static char *KQMLScanExpr(char *in, int comma);
static char *KQMLScanWord(char *in);
static char *KQMLScanPipedToken(char *in);
static char *KQMLScanString(char *in);
static char *KQMLScanQuotedString(char *in);
static char *KQMLScanSharpedString(char *in);
static char *KQMLScanQuotation(char *in, int comma);
static char *KQMLScanList(char *in, int comma);
char *KQMLParseString(char *in);
char *KQMLParseQuotedString(char *in);
char *KQMLParseSharpedString(char *in);
char **KQMLParseList(char *in);
char **KQMLParseStringList(char *in);
char *KQMLParseThing(char *in);
char **KQMLParseThingList(char *in);
KQMLPerformative *KQMLParsePerformative(char *in);
static char *strsave(char *start, char *end);

/*	-	-	-	-	-	-	-	-	*/
/*
 * These routines all return a pointer to the next character after the
 * relevant KQML object (expression, word, etc.). They do not allocate
 * any memory or modify the input string.
 */

static char *
KQMLScanExpr(char *in, int comma)
{
    switch (*in) {
      case '\'':
      case '`':
	return KQMLScanQuotation(in, comma);
      case '"':
      case '#':
	return KQMLScanString(in);
      case '(':
	return KQMLScanList(in, comma);
      case '|':
	return KQMLScanPipedToken(in);
      case ',':
	if (!comma) {
	    ERROR1("illegal comma outside backquote: \"%s\"", in);
	    return NULL;
	} else {
	    return KQMLScanExpr(in+1, comma);
	}
      default:
	return KQMLScanWord(in);
    }
}

static char *
KQMLScanWord(char *in)
{
    DEBUG1("\"%s\"", in);
    /* Sanity check */
    if (!isalnum(*in) && !isspecial(*in)) {
	ERROR1("expected alpha, digit, or special at start of word: \"%s\"",
	       in);
	return NULL;
    }
    /* Scan to end of word */
    while (isalnum(*in) || isspecial(*in)) {
	in += 1;
    }
    /* Return updated pointer */
    DEBUG1("returning \"%s\"", in);
    return in;
}

static char *
KQMLScanPipedToken(char *in)
{
    DEBUG1("\"%s\"", in);
    /* Sanity check */
    if (*in != '|') {
	ERROR1("expected `|' at start of piped token: \"%s\"", in);
	return NULL;
    }
    /* Skip opening pipe */
    in += 1;
    /* Scan string until next pipe is found (is this right?) */
    while (*in && *in != '|') {
        in += 1;
    }
    /* Check closing pipe */
    if (*in != '|') {
	ERROR1("expected `|' at end of piped token: \"%s\"", in);
	return NULL;
    }
    /* Skip closing pipe */
    in += 1;
    /* return updated pointer */
    DEBUG1("returning \"%s\"", in);
    return in;

}

static char *
KQMLScanString(char *in)
{
    if (*in == '"') {
	return KQMLScanQuotedString(in);
    } else if (*in == '#') {
	return KQMLScanSharpedString(in);
    } else {
	ERROR1("expected `\"' or `#' at start of string: \"%s\"", in);
	return NULL;
    }
}

static char *
KQMLScanQuotedString(char *in)
{
    DEBUG1("\"%s\"", in);
    /* Sanity check */
    if (*in != '"') {
	ERROR1("expected `\"' at start of quoted string: \"%s\"", in);
	return NULL;
    }
    /* Skip open quote */
    in += 1;
    /* Scan string to close quote */
    while (*in && *in != '"') {
	if (*in == '\\' && *(in+1) != '\0') {
	    in += 1;
	}
	in += 1;
    }
    /* Check close quote */
    if (*in != '"') {
	ERROR1("expected `\"' at end of quoted string: \"%s\"", in);
	return NULL;
    }
    /* Skip close quote */
    in += 1;
    /* return updated pointer */
    DEBUG1("returning \"%s\"", in);
    return in;
}

static char *
KQMLScanSharpedString(char *in)
{
    int n;

    DEBUG1("\"%s\"", in);
    /* Sanity check */
    if (*in != '#') {
	ERROR1("expected `#' at start of sharped string: \"%s\"", in);
	return NULL;
    }
    /* Skip sharp */
    in += 1;
    /* Scan leading digits */
    n = 0;
    while (isdigit(*in)) {
	n = n * 10 + *in - '0';
	in += 1;
    }
    /* Skip double-quote between number and text */
    if (*in != '"') {
	ERROR1("expected `\"' in sharped string: \"%s\"", in);
	return NULL;
    }
    in += 1;
    /* Skip ahead required number of characters */
    while (n-- > 0) {
	if (!*in) {
	    ERROR1("premature end of sharped string: \"%s\"", in);
	    return NULL;
	} else {
	    in += 1;
	}
    }
    /* Return updated pointer */
    DEBUG1("returning \"%s\"", in);
    return in;
}

static char *
KQMLScanQuotation(char *in, int comma)
{
    /* Sanity check */
    if (*in != '\'' && *in != '`') {
	ERROR1("expected `'' or ``' at start of quotation: \"%s\"", in);
	return NULL;
    }
    /* Scan subexpression */
    if (*in == '`') {
	return KQMLScanExpr(in+1, 1);
    } else {
	return KQMLScanExpr(in+1, comma);
    }
}

/* See also KQMLParseList() if this changes... */
static char *
KQMLScanList(char *in, int comma)
{
    char *end;

    DEBUG1("\"%s\"", in);
    /* Sanity check */
    if (*in != '(') {
	ERROR1("expected `(' at start of list: \"%s\"", in);
	return NULL;
    }
    /* Skip open paren */
    in += 1;
    /* Get initial word of list */
    /* NOTE: KQML spec says this has to be a word, however we want to accept
     * also lists of strings, for example, so we accept an expression here.
     */
    if ((end = KQMLScanExpr(in, comma)) == NULL) {
	ERROR1("bad expr at start of list: \"%s\"", in);
	return NULL;
    }
    /* Skip over first word */
    in = end;
    /* Check for no arguments */
    if (*in == ')') {
	return in+1;
    }
    /* Otherwise scan remaining elements (expressions) */
    while (*in && *in != ')') {
	/* Skip whitespace */
	SKIPWHITE(in);
	/* Get next expression */
	if ((end = KQMLScanExpr(in, comma)) == NULL) {
	    ERROR1("expected expression in list: \"%s\"", in);
	    return NULL;
	}
	/* Skip over expression */
	in = end;
    }
    /* Check close paren */
    if (*in != ')') {
	ERROR1("expected ')' at end of list: \"%s\"", in);
	return NULL;
    }
    /* Skip close paren */
    in += 1;
    /* Return updated pointer */
    DEBUG1("returning \"%s\"", in);
    return in;
}

/*	-	-	-	-	-	-	-	-	*/
/*
 * These functions are almost duplicates of the ones above, except that
 * they allocate storage and return various parsed results, rather than
 * just scanning the input.
 * They also skip leading whitespace, which the scanning functions don't
 * do since KQML is very picky about it.
 * It would be nice to not have to duplicate the scanning code...
 */

char *
KQMLParseString(char *in)
{
    /* Skip leading whitespace */
    SKIPWHITE(in);
    /* Go parse the right kind of string */
    if (*in == '"') {
	return KQMLParseQuotedString(in);
    } else if (*in == '#') {
	return KQMLParseSharpedString(in);
    } else {
	ERROR1("expected `\"' or `#' at start of string: \"%s\"", in);
	return NULL;
    }
}

char *
KQMLParseQuotedString(char *in)
{
    char *start = in;
    char *new, *s;
    int len = 0;

    /* Skip leading whitespace */
    SKIPWHITE(in);
    /* Sanity check */
    if (*in != '"') {
	ERROR1("expected `\"' at start of quoted string: \"%s\"", in);
	return NULL;
    }
    /* Skip open quote */
    in += 1;
    /* Scan string to close quote */
    while (*in && *in != '"') {
	if (*in == '\\' && *(in+1) != '\0') {
	    in += 1;
	}
	in += 1;
	len += 1;
    }
    /* Check close quote */
    if (*in != '"') {
	ERROR1("expected `\"' at end of quoted string: \"%s\"", in);
	return NULL;
    }
    /* Allocate space for new string */
    if ((new = malloc(len+1)) == NULL) {
	SYSERR0("couldn't malloc for new string");
	return NULL;
    }
    s = new;
    /* Rescan string copying into output (handling backslash) */
    start += 1;
    while (*start && *start != '"') {
	if (*start == '\\' && *(start+1) != '\0') {
	    start += 1;
	}
	*s++ = *start++;
    }
    /* NUL-terminate string */
    *s = '\0';
    /* Return new string */
    return new;
}

char *
KQMLParseSharpedString(char *in)
{
    char *s, *new;
    int n;

    /* Skip leading whitespace */
    SKIPWHITE(in);
    /* Sanity check */
    if (*in != '#') {
	ERROR1("expected `#' at start of sharped string: \"%s\"", in);
	return NULL;
    }
    /* Skip sharp */
    in += 1;
    /* Scan leading digits */
    n = 0;
    while (isdigit(*in)) {
	n = n * 10 + *in - '0';
	in += 1;
    }
    /* Skip double-quote between number and text */
    if (*in != '"') {
	ERROR1("expected `\"' in sharped string: \"%s\"", in);
	return NULL;
    }
    in += 1;
    /* Allocate space for new string */
    if ((new = malloc(n+1)) == NULL) {
	SYSERR0("couldn't malloc for new string");
	return NULL;
    }
    s = new;
    /* Skip ahead required number of characters */
    while (n-- > 0) {
	if (!*in) {
	    ERROR1("premature end of sharped string: \"%s\"", in);
	    free(new);
	    return NULL;
	} else {
	    *s++ = *in++;
	}
    }
    /* NUL-terminate string */
    *s = '\0';
    /* Return new string */
    return new;
}

char **
KQMLParseList(char *in)
{
    char *end;
    char **strings, **new;
    int n;

    DEBUG1("\"%s\"", in);
    /* Skip leading whitespace */
    SKIPWHITE(in);
    /* Sanity check */
    if (*in != '(') {
	ERROR1("expected `(' at start of list: \"%s\"", in);
	return NULL;
    }
    /* Skip open paren */
    in += 1;
    /* Get initial word of list */
    /* See note in KQMLScanList(); we accept an expression rather than
     * a word
     */
    if ((end = KQMLScanExpr(in, 0)) == NULL) {
	ERROR1("expected word at start of list: \"%s\"", in);
	return NULL;
    }
    /* Save initial element of list */
    if ((strings = (char**)calloc(2, sizeof(char*))) == NULL) {
	SYSERR0("couldn't calloc initial strings");
	return NULL;
    }
    strings[0] = strsave(in, end);
    strings[1] = NULL;
    n = 1;
    /* Skip over first word */
    in = end;
    /* Check for no arguments */
    if (*in == ')') {
	return strings;
    }
    /* Otherwise scan remaining elements (expressions) */
    while (*in && *in != ')') {
	/* Skip whitespace */
	SKIPWHITE(in);
	/* Get next expression */
	if ((end = KQMLScanExpr(in, 0)) == NULL) {
	    ERROR1("expected expression in list: \"%s\"", in);
	    return NULL;
	}
	/* Reallocate strings[] array and save this string */
	if ((new = (char**)realloc((char*)strings, (n+2)*sizeof(char*))) == NULL){
	    SYSERR0("couldn't realloc strings");
	    free(strings);
	    return NULL;
	} else {
	    strings = new;
	}
	strings[n] = strsave(in, end);
	strings[n+1] = NULL;
	n += 1;
	/* Skip over expression */
	in = end;
    }
    /* Check close paren */
    if (*in != ')') {
	ERROR1("expected ')' at end of list: \"%s\"", in);
	free(strings);
	return NULL;
    }
    /* Return strings */
    DEBUG1("done (%d strings)", n);
    return strings;
}

/*	-	-	-	-	-	-	-	-	*/
/*
 * This function returns an array of strings, each of which is itself
 * parsed as a KQML string.
 */
char **
KQMLParseStringList(char *in)
{
    char **strings, *old;
    int i;

    /* Skip leading whitespace */
    SKIPWHITE(in);
    /* Break input into array of strings */
    if ((strings = KQMLParseList(in)) == NULL) {
	return NULL;
    }
    /* Parse each element of the list as a string */
    for (i=0; strings[i] != NULL; i++) {
	old = strings[i];
	strings[i] = KQMLParseString(strings[i]);
	free(old);
    }
    /* Return array of strings */
    return strings;
}

/*
 * This function parses a string or copies its argument, which is assumed
 * to be a word.
 */
char *
KQMLParseThing(char *in)
{
    char *new;

    /* Skip leading whitespace */
    SKIPWHITE(in);
    /* Go parse the right kind of string, or just return the word */
    if (*in == '"') {
	return KQMLParseQuotedString(in);
    } else if (*in == '#') {
	return KQMLParseSharpedString(in);
    } else {
	if ((new = malloc(strlen(in)+1)) != NULL) {
	    strcpy(new, in);
	}
	return new;
    }
}

/*
 * This is like KQMLParseStringList(), but accepts words as well as strings.
 */
char **
KQMLParseThingList(char *in)
{
    char **strings, *old;
    int i;

    /* Skip leading whitespace */
    SKIPWHITE(in);
    /* Break input into array of strings */
    if ((strings = KQMLParseList(in)) == NULL) {
	return NULL;
    }
    /* Parse each element of the list as a string */
    for (i=0; strings[i] != NULL; i++) {
	old = strings[i];
	strings[i] = KQMLParseThing(strings[i]);
	free(old);
    }
    /* Return array of strings */
    return strings;
}

/*	-	-	-	-	-	-	-	-	*/
/*
 * This function returns a KQMLPerformative structure if the input can be
 * successfully parsed, otherwise NULL. The input string is not modified.
 * The elements of the returned structure are simply strings (ie., not
 * really parsed in any meaningful way except to ensure syntactic
 * correctness).
 */
KQMLPerformative *
KQMLParsePerformative(char *in)
{
    KQMLPerformative *perf;
    KQMLParameter *p, *lastp;
    char **strings;
    int n;

    /* Skip leading whitespace */
    SKIPWHITE(in);
    /* Break input into array of strings */
    if ((strings = KQMLParseList(in)) == NULL) {
	return NULL;
    }
    /* Check that first element of list is a "word" */
    if (strings[0] == NULL) {
	ERROR0("performative is empty");
	return NULL;
    } else if (!isalnum(*(strings[0])) && !isspecial(*(strings[0]))) {
	ERROR1("performative doesn't start with word: \"%s\"", strings[0]);
	return NULL;
    }
    /* Allocate performative: first element of list is verb of performative */
    if ((perf = KQMLNewPerformative(strings[0])) == NULL) {
	return NULL;
    }
    /* Process rest of strings */
    for (n=1; strings[n] != NULL; n+=2) {
	/* Check that there's a keyword/value pair */
	if (strings[n+1] == NULL) {
	    ERROR1("missing value for keyword: \"%s\"", strings[n]);
	    break;
	}
	/* Save key/value pair in performative parameter list */
	KQMLSetParameter(perf, strings[n], strings[n+1]);
    }
    /* Done; free strings and array */
    for (n=0; strings[n] != NULL; n++) {
	free(strings[n]);
    }
    free(strings);
    /* Return performative */
    return perf;
}

/*	-	-	-	-	-	-	-	-	*/

static char *
strsave(char *start, char *end)
{
    char *new, saved;

    saved = *end;
    *end = '\0';
    if ((new = malloc(strlen(start)+1)) == NULL) {
	SYSERR0("couldn't malloc new string");
	return NULL;
    }
    strcpy(new, start);
    *end = saved;
    return new;
}

