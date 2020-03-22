/*
 * hostname.c: Get local hostname
 *
 * George Ferguson, ferguson@cs.rochester.edu,  2 Jan 1996
 * Time-stamp: <Fri Mar 20 23:56:05 CDT 2020 lgalescu>
 *
 * This gets the hostname, suitable for use on the local network. For
 * more general use, you'd want to go via gethostbyname() to canonicalize
 * the name, and you wouldn't strip off the trailing components.
 */
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#ifdef SOLARIS
# include <sys/utsname.h>
#endif
#include "util/error.h"

char *
trlibGetHostname(void)
{
    static int initialized = 0;
    static char hostname[64];
    char *s;

    if (!initialized) {
#ifdef SOLARIS
	struct utsname info;
	if (uname(&info) < 0) {
	    SYSERR0("couldn't find hostname using uname");
	    strcpy(hostname, "localhost");
	} else {
	    strcpy(hostname, info.nodename);
	}
#else    
	gethostname(hostname, sizeof(hostname));
#endif
	if ((s = strchr(hostname, '.')) != NULL) {
	    *s = '\0';
	}
	initialized = 1;
    }
    return hostname;
}
