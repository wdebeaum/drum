/*
 * StandardTripsModule.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 11 Dec 2000
 * $Id: StandardTripsModule.java,v 1.8 2011/11/13 02:15:08 blaylock Exp $
 */

package TRIPS.TripsModule;

import java.io.*;
import java.net.*;
import TRIPS.KQML.*;

/**
 * StandardTripsModule is the base class for all TRIPS components. It
 * provides functionality common to all modules.
 */
abstract public class StandardTripsModule implements TripsModule {
    //
    // Constants
    //
    /**
     * The default host at which to contact the Facilitator (``localhost'').
     */
    protected static final String DEFAULT_HOST = "localhost";
    /**
     * The default port at which to contact the Facilitator (6200).
     */
    protected static final int DEFAULT_PORT = 6200;
    /**
     * Number of ports to scan looking for Facilitator (100).
     */
    protected static final int MAX_PORT_TRIES = 6200;
    //
    // Fields
    //
    /**
     * List of arguments passed in to constructor.
     */
    protected String[] argv;
    /**
     * True if this module is running as an application.
     */
    protected boolean isApplication;
    /**
     * Host at which to contact facilitator.
     * Set by -connect argument; default is DEFAULT_HOST value.
     */
    protected String host = DEFAULT_HOST;
    /**
     * Port at which to contact facilitator.
     * Set by -connect argument; default is DEFAULT_PORT value.
     */
    protected int port = DEFAULT_PORT;
    /**
     * Whether to connect to facilitator.
     * Set by -connect argument; default is true.
     */
    protected boolean autoConnect = true;
    /**
     * the socket we use to connect to the Facilitator
     */
    protected Socket socket;
    /**
     * Name of this module (if we know it).
     * Set by -name argument.
     */
    protected String name;
    /**
     * Group name (or names) to which this module belongs.
     * Set by -group argument.
     */
    protected String groupName;
    /**
     * Should we scan for a facilitator port or just use what we're given.
     */
    protected boolean scanForPort = false;
    /**
     * Channel on which we receive KQML messages.
     */
    protected KQMLReader in;
    /**
     * Channel on which we send KQML messages.
     */
    protected PrintWriter out;
    /**
     * KQMLDispatcher that handles message dispatching for us.
     */
    protected KQMLDispatcher dispatcher;
    /**
     * True if warning messages are enabled.
     */
    protected boolean warningEnabled = true;
    /**
     * True if debugging messages are enabled.
     */
    protected boolean debuggingEnabled = false;
    //
    // Constructors
    //
    /**
     * Create a new StandardTripsModule with String arguments ARGV.
     * If boolean isApplication is true, then this module is running
     * as an application (which, for example, means that calls to
     * exit() will terminate the application).
     */
    public StandardTripsModule(String argv[], boolean isApplication) {
	this.argv = argv;
	this.isApplication = isApplication;
    }
    /**
     * Create a new StandardTripsModule with String arguments ARGV
     * and isApplication set to false.
     */
    public StandardTripsModule(String argv[]) {
	this(argv, false);
    }
    //
    // Runnable method
    //
    /**
     * This is the main Runnable method for StandardTripsModule.
     * <p>
     * Unless overridden, we simply initialize our I/O connections by
     * calling init(), then run our KQMLDispatcher by calling
     * dispatcher.start().
     * <p>
     * If you override this method, you probably want to call super.init()
     * first and dispatcher.start() last.
     */
    public void run() {
	init();
	dispatcher.start();
    }
    //
    // Additional methods useful to StandardTripsModules
    //
    /**
     * Standard initialization for TRIPS modules. Handles common
     * command-line args, connects to facilitator (optionally), sets
     * up KQML IO streams, starts KQMLDispatcher, and sends register
     * message (if name is non-null).
     */
    public void init() {
	// Read common parameters
	handleCommonParameters();
	// Connect to server
	if (autoConnect) {
	    if (!connect(host, port)) {
		exit(-1);
		// NB: 11/12/2011 
		// I put this line in because, on tomcat as web service, hit a case where
		// the facilitator didn't connect, but this exit call returned after
		// trying to shut down the dispatcher (see the exit() function below)
		// so, the code kept running until it tried to register and didn't have
		// a value for the 'out' variable.  (We may want to put this behavior in
		// exit(), not sure....
		throw new RuntimeException("Not able to connect to facilitator. Exit attempt failed.");
	    }
	} else {
	    out = new PrintWriter(System.out, true);
	    in  = new KQMLReader(System.in);
	}
	// Initialize dispatcher on this connection and start it
	dispatcher = new KQMLDispatcher(this, in);
	// Register with the Facilitator
	if (name != null) {
	    register();
	}
    }

    /**
     * Checks if this module is connected to the Facilitator.
     * Right now only checks if socket is connected.
     */
    public boolean isConnected() {
	if (socket == null) {
	    return false; // if we haven't instantiated a socket, we're not connected
	} else {
	    return socket.isConnected();
	}
    } // end connected

    /**
     * Process parameters common to all StandardTripsModules.
     * This should be done before calling connect() or register() if
     * you want the parameters (like -name and -connect) to get used
     * properly.
     */
    protected void handleCommonParameters() {
	String value;
	if ((value = getParameter("-connect")) != null) {
	    if (value.equalsIgnoreCase("true") ||
		value.equalsIgnoreCase("t") ||
		value.equalsIgnoreCase("yes")) {
		autoConnect = true;
	    } else if (value.equalsIgnoreCase("false") ||
		value.equalsIgnoreCase("nil") ||
		value.equalsIgnoreCase("no")) {
		autoConnect = false;
	    } else {
		int colon;
		if ((colon = value.indexOf(':')) > -1) {
		    host = value.substring(0, colon);
		    port = new Integer(value.substring(colon+1)).intValue();
		} else {
		    host = value;
		    port = DEFAULT_PORT;
		}
	    }
	}
	if ((value = getParameter("-name")) != null) {
	    name = value;
	}
	if ((value = getParameter("-group")) != null) {
	    groupName = value;
	}
	if ((value = getParameter("-scan")) != null) {
	    scanForPort = value.equalsIgnoreCase("true") ||
		value.equalsIgnoreCase("t") ||
		value.equalsIgnoreCase("yes");
	}
	if ((value = getParameter("-debug")) != null) {
	    if (value.equalsIgnoreCase("true") ||
		value.equalsIgnoreCase("t") ||
		value.equalsIgnoreCase("yes")) {
		setDebuggingEnabled(true);
	    } else {
		setDebuggingEnabled(false);
	    }
	}
    }
    /**
     * Connect to Facilitator using standard protocol for finding port.
     * This version of the method uses the instance variables set by
     * parameters processed by handleCommonParameters(), and is not
     * verbose.
     */
    protected boolean connect() {
	return connect(host, port);
    }
    /**
     * Connect to server using standard protocol for finding port.
     * This is the most general version of the method.
     * <p>
     * The standard protocol is to scan upwards from the given port
     * number looking for the Facilitator, up to a maximum of
     * MAX_PORT_TRIES from the start. If a connection is established,
     * the ``in'' and ``out'' fields are setup on the connection,
     * otherwise an error message is printed (should perhaps just signal
     * the error...).
     * <p>
     * See also connect1() which doesn't scan.
     */
    protected boolean connect(String host, int startport) {
	if (!scanForPort) {
	    return connect1(host, startport, true);
	} else {
	    int maxtries = 100;
	    // Scan ports looking for server
	    int port;
	    for (port=startport; port < startport + maxtries; port++) {
		if (connect1(host, port, false)) {
		    return true;
		}
	    }
	    error("connect: failed to connect to " + host + ":" + startport + "- " + port);
	    return false;
	}
    }
    /**
     * Connect to server using the host and port provided without
     * scanning for a free port.
     */
    protected boolean connect1(String host, int port, boolean verbose) {
	try {
	    socket = new Socket(host, port);
	    out = new PrintWriter(socket.getOutputStream(), true);
	    in = new KQMLReader(socket.getInputStream());
	    return true;
	} catch (IOException ex) {
	    if (verbose) {
		error("connect: " + host + ":" + port + ": " + ex.getMessage());
	    }
	}
	return false;
    }
    /**
     * Connect to server using the host and port provided without
     * scanning for a free port and be verbose about errors.
     */
    protected boolean connect1(String host, int port) {
	return connect1(host, port, true);
    }
    /**
     * Send REGISTER message to Facilitator.
     * This method uses the ``name'' and ``group'' fields, which
     * should have been set by handleCommonParameters(). If ``name''
     * is null, no message is sent.
     */
    protected void register() {
	if (name != null) {
	    KQMLPerformative perf = new KQMLPerformative("register");
	    perf.setParameter(":name", name);
	    if (groupName != null) {
		try {
		    KQMLObject group;
		    if (groupName.startsWith("(")) {
			group = KQMLList.fromString(groupName);
		    } else {
			group = new KQMLToken(groupName);
		    }
		    perf.setParameter(":group", group);
		} catch (IOException ex) {
		    System.err.println("bad groupName: " + groupName);
		}
	    }
	    send(perf);
	}
    }
    /**
     * Send (MODULE-STATUS READY) message.
     */
    protected void ready() {
	KQMLPerformative perf = new KQMLPerformative("tell");
	KQMLList content = new KQMLList();
	content.add("module-status");
	content.add("ready");
	perf.setParameter(":content", content);
	send(perf);
    }
    /**
     * Indicates that this module wants to exit.
     * <p>
     * If isApplication is true, then this calls System.exit().
     * <p>
     * If not, then this doesn't really cause an exit, since we don't want
     * to bring down the whole VM.
     * <p>
     * In any event, there's a major problem with Java's threads in that
     * threads blocked on I/O cannot (reliably) be interrupted or killed.
     * Since all StandardTripsModules have a KQMLDispatcher thread
     * associated with them, we are kind of screwed. If we're an application,
     * then it doesn't really matter since System.exit() will bring everything
     * down. But if not, we cannot realiably terminate just this module.
     */
    protected void exit(int n) {
	if (isApplication) {
	    System.exit(n);
	} else {
	    // Try to at least bring down the KQMLDispatcher
	    // Should we maybe just close our socket?
	    if (dispatcher != null)
		dispatcher.shutdown();
	}
    }
    /**
     * Return named parameter of module, or null.
     * This method is useful for modules that want to process additional
     * parameters beyond those handled by handleCommonParameters().
     */
    protected String getParameter(String parm) {
	for (int i=0; i < argv.length - 1; i++) {
	    if (argv[i].equals(parm)) {
		return argv[i+1];
	    }
	}
	return null;
    }
    //
    // KQMLReceiver methods (will be overridden by subclasses as needed)
    //
    /**
     * KQMLReceiver method called when EOF is detected on the input stream.
     * Unless overridden, this method calls exit().
     * <p>
     * Subclasses should override this method if they want to cleanup
     * before exiting.
     */
    public void receiveEOF() {
	exit(0);
    }
    /**
     * KQMLReceiver method called when a message arrives with no verb.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveMessageMissingVerb(KQMLPerformative msg) {
	errorReply(msg, "missing verb in performative");
    }
    /**
     * KQMLReceiver method called when a message arrives with no :content
     * (for a performative that expects one).
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveMessageMissingContent(KQMLPerformative msg) {
	errorReply(msg, "missing content in performative");
    }
    /**
     * KQMLReceiver method called when an ASK-IF message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveAskIf(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: ask-if");
    }
    /**
     * KQMLReceiver method called when an ASK-ALL message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveAskAll(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: ask-all");
    }
    /**
     * KQMLReceiver method called when an ASK-ONE message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveAskOne(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: ask-one");
    }
    /**
     * KQMLReceiver method called when an STREAM-ALL message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveStreamAll(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: stream-all");
    }
    /**
     * KQMLReceiver method called when a TELL message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveTell(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: tell");
    }
    /**
     * KQMLReceiver method called when an UNTELL message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveUntell(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: untell");
    }
    /**
     * KQMLReceiver method called when a DENY message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveDeny(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: deny");
    }
    /**
     * KQMLReceiver method called when an INSERT message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveInsert(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: insert");
    }
    /**
     * KQMLReceiver method called when an UNINSERT message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveUninsert(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: uninsert");
    }
    /**
     * KQMLReceiver method called when a DELETE-ONE message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveDeleteOne(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: delete-one");
    }
    /**
     * KQMLReceiver method called when a DELETE-ALL message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveDeleteAll(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: delete-all");
    }
    /**
     * KQMLReceiver method called when an UNDELETE message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveUndelete(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: undelete");
    }
    /**
     * KQMLReceiver method called when an ACHIEVE message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveAchieve(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: achieve");
    }
    /**
     * KQMLReceiver method called when an UNACHIEVE message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveUnachieve(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: unachieve");
    }
    /**
     * KQMLReceiver method called when an ADVERTISE message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveAdvertise(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: advertise");
    }
    /**
     * KQMLReceiver method called when an UNADVERTISE message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveUnadvertise(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: unadvertise");
    }
    /**
     * KQMLReceiver method called when a SUBSCRIBE message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveSubscribe(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: subscribe");
    }
    /**
     * KQMLReceiver method called when a STANDBY message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveStandby(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: standby");
    }
    /**
     * KQMLReceiver method called when a REGISTER message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveRegister(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: register");
    }
    /**
     * KQMLReceiver method called when a FORWARD message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveForward(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: forward");
    }
    /**
     * KQMLReceiver method called when a BROADCAST message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveBroadcast(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: broadcast");
    }
    /**
     * KQMLReceiver method called when a TRANSPORT-ADDRESS message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveTransportAddress(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: transport-address");
    }
    /**
     * KQMLReceiver method called when a BROKER-ONE message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveBrokerOne(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: broker-one");
    }
    /**
     * KQMLReceiver method called when a BROKER-ALL message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveBrokerAll(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: broker-all");
    }
    /**
     * KQMLReceiver method called when a RECOMMEND-ONE message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveRecommendOne(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: recommend-one");
    }
    /**
     * KQMLReceiver method called when a RECOMMEND-ALL message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveRecommendAll(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: recommend-all");
    }
    /**
     * KQMLReceiver method called when a RECRUIT-ONE message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveRecruitOne(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: recruit-one");
    }
    /**
     * KQMLReceiver method called when a RECRUIT-ALL message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveRecruitAll(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: recruit-all");
    }
    /**
     * KQMLReceiver method called when a REPLY message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveReply(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: reply");
    }
    /**
     * KQMLReceiver method called when a REQUEST message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveRequest(KQMLPerformative msg, Object content) {
	errorReply(msg, "unexpected performative: request");
    }
    /**
     * KQMLReceiver method called when an EOS message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveEos(KQMLPerformative msg) {
	errorReply(msg, "unexpected performative: eos");
    }
    /**
     * KQMLReceiver method called when an ERROR message arrives.
     * Unless this method is overriden, it does nothing.
     * In particular, it does not generate an ERROR reply (to avoid
     * ERROR loops).
     */
    public void receiveError(KQMLPerformative msg) {
    }
    /**
     * KQMLReceiver method called when a SORRY message arrives.
     * Unless this method is overriden, it does nothing.
     * In particular, it does not generate an ERROR reply (to avoid
     * ERROR loops).
     */
    public void receiveSorry(KQMLPerformative msg) {
    }
    /**
     * KQMLReceiver method called when a READY message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveReady(KQMLPerformative msg) {
	errorReply(msg, "unexpected performative: ready");
    }
    /**
     * KQMLReceiver method called when a NEXT message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveNext(KQMLPerformative msg) {
	errorReply(msg, "unexpected performative: next");
    }
    /**
     * KQMLReceiver method called when a REST message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveRest(KQMLPerformative msg) {
	errorReply(msg, "unexpected performative: rest");
    }
    /**
     * KQMLReceiver method called when a DISCARD message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveDiscard(KQMLPerformative msg) {
	errorReply(msg, "unexpected performative: discard");
    }
    /**
     * KQMLReceiver method called when an UNREGISTER message arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveUnregister(KQMLPerformative msg) {
	errorReply(msg, "unexpected performative: unregister");
    }
    /**
     * KQMLReceiver method called when any other (non-standard) message
     * arrives.
     * Unless this method is overriden, it generates an ERROR reply.
     */
    public void receiveOtherPerformative(KQMLPerformative msg) {
	errorReply(msg, "unexpected performative: " + msg);
    }
    /**
     * KQMLReceiver method called when an IOException occurs in the
     * KQMLDispatcher for this module (usually KQMLException).
     */
    public void handleException(IOException ex) {
	System.err.println(name + ": " + ex);
    }
    //
    // Output functions
    //
    /**
     * Sends KQMLPerformative MSG out via our ``out'' connection.
     * gf: 1 Jun 2007: Historically we didn't have to check for IOException
     * here, so now that I've switched to the write() API, I'm stuck eating it
     * if it occurs. Ah well...
     */
    protected synchronized void send(KQMLPerformative msg) {
	try {
	    msg.write(out);
	} catch (IOException ex) {
	}
	out.println();
    }
    /** For use by sendWithContinuation. */
    protected static int replyIDCounter = 1;
    /**
     * Sends a message, adding a :reply-with argument, and sets up the
     * continuation to be called when we receive a message with a corresponding
     * :in-reply-to argument.
     */
    protected synchronized void sendWithContinuation(KQMLPerformative msg, KQMLContinuation cont) {
	String replyIDBase = "IO-";
	if (name != null)
	    replyIDBase = name + "-";
	KQMLToken replyID = new KQMLToken(replyIDBase + Integer.toString(replyIDCounter++));
	msg.setParameter(":reply-with", replyID);
	dispatcher.addReplyContinuation(replyID.stringValue(), cont);
	send(msg);
    }
    /**
     * Replies to KQMLPerformative MSG with the given performative REPLYMSG.
     * The :receiver and :in-reply-to parameters are adjusted to match
     * the :sender and :reply-with of MSG, respectively, if present.
     */
    protected void reply(KQMLPerformative msg, KQMLPerformative replymsg) {
	// Set sender
	KQMLObject sender = msg.getParameter(":sender");
	if (sender != null) {
	    replymsg.setParameter(":receiver", sender);
	}
	// Set reply tag
	KQMLObject reply_with = msg.getParameter(":reply-with");
	if (reply_with != null) {
	    replymsg.setParameter(":in-reply-to", reply_with);
	}
	// Off it goes...
	send(replymsg);
    }
    /**
     * Replies to KQMLPerformative MSG with a message whose performative
     * is ERROR and whose :comment is COMMENT.
     */
    protected void errorReply(KQMLPerformative msg, String comment) {
	// New performative
	KQMLPerformative replymsg = new KQMLPerformative("error");
	// Set comment as KQML string
	replymsg.setParameter(":comment", new KQMLString(comment));
	// Off it goes...
	reply(msg, replymsg);
    }
    //
    // Debugging and error messages
    //
    /**
     * Prints MSG to stderr unconditionally.
     */
    protected void error(String msg) {
	System.err.println(msg);
    }
    /**
     * If warnings are enabled, prints MSG to stderr.
     */
    protected void warn(String msg) {
	if (warningEnabled) {
	    System.err.println(msg);
	}
    }
    /**
     * Enables and disables warning messages.
     */
    public void setWarningEnabled(boolean bool) {
	warningEnabled = bool;
    }
    /**
     * If debugging is enabled, prints MSG to stderr.
     */
    protected void debug(String msg) {
	if (debuggingEnabled) {
	    System.err.println(msg);
	}
    }
    /**
     * Enables and disables debugging messages.
     */
    public void setDebuggingEnabled(boolean bool) {
	debuggingEnabled = bool;
    }
}
