/*
 * ttrips_msg.c: Send cmd-line args as KQML message to Facilitator
 *
 * George Ferguson, ferguson@cs.rochester.edu, 23 Jan 1996
 * Time-stamp: <Wed Sep 24 14:26:03 EDT 2003 ferguson>
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "util/debug.h"
#include "util/error.h"

#define MAX_CONNECT_TRIES 100

/*
 * Functions defined here:
 */
int main(int argc, char **argv);
static void initArgs(int *argcp, char ***argvp);
static int initSocket(char *hostname, int port);

/*
 * Data defined here:
 */
static char hostname[256] = "localhost";
static int port = 6200;
/* Default this to stderr to have debugging on by default (if -DDEBUG) */
FILE *debugfp;
char *progname;

/*	-	-	-	-	-	-	-	-	*/

int
main(int argc, char **argv)
{
    int sock, error;
    FILE *sockfp;

    /* Initialize for debug */
    progname = argv[0];
#ifdef DEBUG
    debugfp = stderr;
#endif
    /* Parse command-line and environment */
    initArgs(&argc, &argv);
    /* Connect to IM */
    if ((sock = initSocket(hostname, port)) < 0) {
	exit(-1);
    }
    /* Create stdio stream for printing to IM */
    if ((sockfp = fdopen(sock, "w")) == NULL) {
	SYSERR0("fdopen failed");
	exit(-1);
    }
    /* Print remaining args as KQML message */
    if (argc > 0) {
	fprintf(sockfp, "(%s", *argv++);
	while (--argc) {
	    fprintf(sockfp, " %s", *argv++);
	}
	fprintf(sockfp, ")\n");
	fflush(sockfp);
    }
    /* Done */
    fclose(sockfp);
    exit(0);
}

static void
initArgs(int *argcp, char ***argvp)
{
    int argc = *argcp;
    char **argv = *argvp;
    char *s;

    /* Use environment variables for defaults */
    if ((s = getenv("TRIPS_SOCKET")) != NULL) {
	if (sscanf(s, "%[^:]:%d", hostname, &port) != 2) {
	    ERROR1("bad $TRIPS_SOCKET (should be HOST:PORT): \"%s\"", s);
	}
    }
    /* Parse options */
    while (--argc > 0) {
	argv += 1;
	if (strcmp(argv[0], "-socket") == 0) {
	    if (argc < 2) {
		ERROR0("missing argument for -socket");
	    } else {
		if (sscanf(argv[1], "%[^:]:%d", hostname, &port) != 2) {
		    ERROR1("bad -socket spec (should be HOST:PORT): \"%s\"",
			   argv[1]);
		}
		argc -= 1;
		argv += 1;
	    }
	} else {
	    break;
	}
    }
    *argcp = argc;
    *argvp = argv;
}

/*	-	-	-	-	-	-	-	-	*/

static int
initSocket(char *hostname, int port)
{
    struct hostent *hent;
    u_long haddr;
    struct sockaddr_in saddr;
    int sockfd, i;

    DEBUG2("connecting to host \"%s\", port %d", hostname, port);
    /* Lookup address */
    if ((hent = gethostbyname(hostname)) == NULL) {
	SYSERR1("couldn't find address for host \"%s\"", hostname);
	return -1;
    }
    DEBUG1("gethostbyname said hostname=\"%s\"", hent->h_name);
    haddr = *((u_long*)(hent->h_addr_list[0]));
    DEBUG4("haddr=%d.%d.%d.%d",
	    (haddr & 0xff000000) >> 24, (haddr & 0x00ff0000) >> 16,
	    (haddr & 0x0000ff00) >> 8, (haddr & 0x000000ff));
    for (i=0; i < MAX_CONNECT_TRIES; i++) {
	/* Create socket */
	DEBUG0("creating socket");
	if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	    SYSERR0("couldn't create socket");
	    return -1;
	}
	/* Connect to remote host/port */
	DEBUG1("sockfd=%d", sockfd);
	memset((char*)&saddr, '\0', sizeof(saddr));
	//saddr.sin_family = htons((short)AF_INET);
	saddr.sin_family = AF_INET;
	saddr.sin_port = htons((u_short)port);
	memcpy((char*)&(saddr.sin_addr),(char*)&haddr,sizeof(saddr.sin_addr));
	DEBUG1("connecting: port=%d", port);
	if (connect(sockfd,(struct sockaddr *)&saddr,sizeof(saddr)) >= 0) {
	    DEBUG0("connected!");
	    return sockfd;
	}
	SYSERR2("couldn't connect to %s:%d (will retry)", hostname, port);
	close(sockfd);
	port += 1;
    }
    ERROR1("failed to connect to %s", hostname);
    return -1;
}
