/*
 * trips_exec.c: Exec a program with stdin/stdout connected to Facilitator
 *
 * George Ferguson, ferguson@cs.rochester.edu, 25 Jan 1996
 * Time-stamp: <Wed Sep 24 14:20:08 EDT 2003 ferguson>
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <signal.h>
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
static int doRegister = 0;
static char *name = NULL;
/* Default this to stderr to have debugging on by default (if -DDEBUG) */
FILE *debugfp;
char *progname;

/*	-	-	-	-	-	-	-	-	*/

int
main(int argc, char **argv)
{
    int sock;

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
    /* Register if we're supposed to */
    if (doRegister && name) {
	char buf[256];
	sprintf(buf, "(register :name %s)\n", name);
	size_t buflen = strlen(buf);
	ssize_t written = write(sock, buf, strlen(buf));
	if (written < buflen) {
	  SYSERR2("failed to write %zd bytes; write returned %zd", buflen, written);
	  exit(-1);
	}
    }
    /* Make stdin and stdout the IM connection */
    dup2(sock, 0);
    dup2(sock, 1);
    close(sock);
    /* Exec */
    if (execvp(*argv, argv) < 0) {
	SYSERR1("couldn't exec \"%s\"", *argv);
	exit(-1);
    }
    /*NOTREACHED*/
    exit(0);
}

/*	-	-	-	-	-	-	-	-	*/

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
    argv += 1;
    while (--argc > 0) {
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
	} else if (strcmp(argv[0], "-register") == 0) {
	    if (argc < 2) {
		ERROR0("missing argument for -register");
	    } else {
		if (strcasecmp(argv[1], "t") == 0 ||
		    strcasecmp(argv[1], "true") == 0 ||
		    strcasecmp(argv[1], "yes") == 0) {
		    doRegister = 1;
		} else {
		    doRegister = 0;
		}
		argc -= 1;
		argv += 1;
	    }
	} else if (strcmp(argv[0], "-name") == 0) {
	    if (argc < 2) {
		ERROR0("missing argument for -name");
	    } else {
		name = argv[1];
		argc -= 1;
		argv += 1;
	    }
	} else {
	    break;
	}
	argv += 1;
    }
    if (doRegister && !name) {
	ERROR0("can't have -register without -name");
	exit(1);
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
