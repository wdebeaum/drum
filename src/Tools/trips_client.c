/*
 * trips_client.c: KQML messages from stdin to Facilitator and from
 *		   Facilitator to stdout
 *
 * George Ferguson, ferguson@cs.rochester.edu, 23 Jan 1996
 * Time-stamp: <Wed Sep 24 14:25:51 EDT 2003 ferguson>
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "KQML/KQML.h"
#include "util/debug.h"
#include "util/error.h"

#define MAX_CONNECT_TRIES 100

/*
 * Functions defined here:
 */
int main(int argc, char **argv);
static void processInput(int infd, FILE *outfp);
static void initArgs(int argc, char **argv);
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
    int sock;
    FILE *sockfp;
    fd_set rfds;

    /* Initialize for debug */
    progname = argv[0];
#ifdef DEBUG
    debugfp = stderr;
#endif
    /* Parse command-line and environment */
    initArgs(argc, argv);
    /* Connect to IM */
    if ((sock = initSocket(hostname, port)) < 0) {
	exit(-1);
    }
    /* Create stdio stream for printing to IM */
    if ((sockfp = fdopen(sock, "w")) == NULL) {
	SYSERR0("fdopen failed");
	exit(-1);
    }
    /* Loop until EOF (detected in processInput()) */
    while (1) {
	/* Setup select mask for stdin and im socket */
	FD_ZERO(&rfds);
	FD_SET(0, &rfds);
	FD_SET(sock, &rfds);
	/* Select waiting for input */
	DEBUG0("calling select...");
	if (select(sock+1, &rfds, NULL, NULL, NULL) < 0) {
	    /* Error */
	    SYSERR0("select failed");
	    exit(1);
	}
	/* Something is ready */
	if (FD_ISSET(0, &rfds)) {
	    DEBUG0("stdin ready for reading");
	    processInput(0, sockfp);
	}
	if (FD_ISSET(sock, &rfds)) {
	    DEBUG0("im ready for reading");
	    processInput(sock, stdout);
	}
    }
    /*NOTREACHED*/
}

static void
processInput(int infd, FILE *outfp)
{
    int error;
    KQMLPerformative *perf;
    KQMLParameter *parm;

    DEBUG1("infd=%d", infd);
    /* Read a performative from infd */
    perf = KQMLReadNoHang(infd, &error, NULL);
    if (error < 0) {
	/* Error */
	ERROR2("KQML error %d (%s)", error, KQMLErrorString(error));
    } else if (error == 0) {
	/* EOF */
	exit(0);
    } else if (perf != NULL) {
	/* Read done: print performative to IM */
	DEBUG0("sending to IM");
	fprintf(outfp, "(%s", perf->verb);
	for (parm=perf->parameters; parm != NULL; parm=parm->next) {
	    if (parm->key && parm->value) {
		fprintf(outfp, " %s %s", parm->key, parm->value);
	    }
	}
	fprintf(outfp, ")\n");
	fflush(outfp);
	/* Free performative */
	KQMLFreePerformative(perf);
    }
    /* Otherwise still reading */
}

/*	-	-	-	-	-	-	-	-	*/

static void
initArgs(int argc, char **argv)
{
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
	    ERROR1("unknown option: \"%s\"", argv[0]);
	}
    }
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
