/*
 * @(#)Facilitator.java
 *
 * David Costello, costello@cs.rochester.edu,  8 Jul 1999
 * $Id: Facilitator.java,v 1.8 2010/04/22 03:20:20 lgalescu Exp $
 */
package TRIPS.Facilitator;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.ServerSocket;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Vector;
import java.util.Hashtable;
import java.util.Enumeration;
import java.util.ListIterator;
import java.util.NoSuchElementException;
import java.util.Arrays;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLToken;
import TRIPS.KQML.KQMLString;
import TRIPS.KQML.KQMLBadPerformativeException;
import TRIPS.util.StringUtils;
import TRIPS.util.Misc;

/**
 * The TRIPS Facilitator has several duties, each of which are performed by
 * separate sections of this code.
 *
 * First, it acts as a network server, accepting connections on a "well
 * known" socket and spawning clients to handle the message traffic on
 * the connections. This is performed by initServer() and the run()
 * method.
 *
 * Second, it manages the list of clients, the registry of client names, and
 * the display of clients. This is performed by the add(), register(),
 * unregister() methods.
 *
 * Third, it does some analysis of incoming messages and routes them to the
 * correct recipients or to the SubscriptionManager. This is performed by
 * handleClientMessage(). Note that this function will be executed by the
 * thread corresponding to the client from which the message was received.
 *
 * Fourth, the Facilitator can itself send and, more importantly, receive
 * messages. These have to be further decoded and acted upon. This function
 * is performed by code starting with receive(), which receives messages from
 * other clients via the Registry (directly addressed messages) or the
 * SubscriptionManager (for messages matching the Facilitator's own
 * subscriptions). The Facilitator's own subscriptions are setup in
 * initRegistry(), by the way.
 *
 * To understand this code, be sure to keep separate the methods in a class
 * and the threads executing those methods. In many cases, methods in the
 * main Facilitator class will be executed by threads corresponding to clients.
 */
public class Facilitator extends Thread implements Sendable {
    //
    // Constants
    //
    private final static int DEFAULT_PORT = 6200;
    private final static String USAGE = "usage: java TRIPS.Facilitator.Facilitator [-display CLASS] [-port NUM] [-scan BOOL] [-log BOOL] [-debug BOOL]";
    //
    // Fields
    //
    protected String[] argv;
    protected ServerSocket serverSocket;
    protected boolean serverRunning;
    protected Registry registry;
    protected Log log;
    protected FacilitatorDisplay display = new NullDisplay();
    protected SubscriptionManager subscriptionManager;
    protected Vector clients = new Vector();
    protected int startPort = DEFAULT_PORT;
    protected boolean scanForPort = false;
    protected String displayClassName = "TRIPS.Facilitator.ModemDisplay";
    protected boolean doLog = true;
    protected Boolean hideTraffic = Boolean.FALSE;
    protected Launcher launcher;
    //
    // Slot accessors
    //
    public Registry getRegistry() {
	return registry;
    }
    //
    // Constructors
    //
    public Facilitator(String argv[]) {
	this.argv = argv;
	handleParameters(argv);
    }
    //
    // Runnable method
    //
    public void run() {
    	// Set thread name for debugging and other uses
    	setName("FACILITATOR");
	// Initialize socket server
	initServer();
	// Initialize client registry
	initRegistry();
	// Initialize display
	initDisplay();
	// Initialize log
	initLog();
	// Initialize subscriptions
	initSubscriptionManager();
	// Initialize internal launcher
	initLauncher();
	// Initialize stdin
	initStdin();
	// Off we go
	serverRunning = true;
	while (serverRunning) {
	    try {
		Debug.debug("Facilitator.run: accepting connections...");
		Socket clientsock = serverSocket.accept();
		Debug.debug("Facilitator.run: accepted: " + clientsock);
		SocketClient client = new SocketClient(this, clientsock);
		Debug.debug("Facilitator.run: created client: " + client);
		add(client);
		// Off it goes...
		client.start();
	    } catch (IOException ex) {
		Debug.warn("accept failed: " + ex);
	    }
	}
    }
    public void stopSafely() {
	// This method will be called from another thread, obviously...
	serverRunning = false;
	// Try this even though it won't interrupt an IO sleep...
	this.interrupt();
    }
    //
    // Helper methods
    //
    private void handleParameters(String[] argv) {
	String value;
	if ((value = getParameter("-display")) != null) {
	    // Handle a few shorthands for classnames...
	    if (value.equalsIgnoreCase("tty")) {
		displayClassName = "TRIPS.Facilitator.TTYDisplay";
	    } else if (value.equalsIgnoreCase("null") ||
		       value.equalsIgnoreCase("none")) {
		displayClassName = "TRIPS.Facilitator.NullDisplay";
	    } else {
		displayClassName = value;
	    }
	}
	if ( parameterExists("-notraffic") ) {
	    // Don't show the message traffic window in the Modem display
	    hideTraffic = Boolean.TRUE;
	}
	if ((value = getParameter("-port")) != null) {
	    startPort = new Integer(value).intValue();
	}
	if ((value = getParameter("-scan")) != null) {
	    scanForPort = StringUtils.stringToBoolean(value);
	}
	if ((value = getParameter("-log")) != null) {
	    doLog = StringUtils.stringToBoolean(value);
	}
	if ((value = getParameter("-debug")) != null) {
	    Debug.setDebuggingEnabled(StringUtils.stringToBoolean(value));
	}
	if ((value = getParameter("-help")) != null) {
	    System.err.println(USAGE);
	    exit(0);
	}
    }
    // Public for use by display classes to get further args
    public boolean parameterExists(String parm) {
	for (int i=0; i < argv.length; i++) {
	    if (argv[i].equals(parm)) return true;
	}
	return false;
    }
    // Public for use by display classes to get further args
    public String getParameter(String parm) {
	for (int i=0; i < argv.length - 1; i++) {
	    if (argv[i].equals(parm)) {
		return argv[i+1];
	    }
	}
	return null;
    }
    protected void initServer() {
	int port = startPort;
	if (!scanForPort) {
	    // Just use the port we're given or die
	    try {
		serverSocket = createServerSocket(port);
		Debug.warn("listening on port " + port);
	    } catch (IOException ex) {
		Debug.error("bind: port " + port + ": " + ex.getMessage());
		System.exit(1);
	    }
	} else {
	    // Old-school mode: find an open port
	    while (port < startPort + 100) {
		try {
		    serverSocket = createServerSocket(port);
		    Debug.warn("listening on port " + port);
		    break;
		} catch (IOException ex) {
		    Debug.warn("bind: port " + port + ": warning: " + ex.getMessage());
		    port += 1;
		}
	    }
	    if (serverSocket == null) {
		Debug.warn("bind: failed to find any port");
		System.exit(0);
	    }
	}
    }
    /*
     * Create and return a new ServerSocket bound to the given port.
     * This method exists to allow setting socket options before
     * binding.
     * @see java.net.ServerSocket
     */
    protected ServerSocket createServerSocket(int port) throws IOException {
	ServerSocket s = new ServerSocket();
	// Enable SO_REUSEADDR
	s.setReuseAddress(true);
	// Finally bind to port and return socket
	s.bind(new InetSocketAddress(port));
	return s;
    }
    protected void initRegistry() {
	registry = new Registry();
	// Add ourselves to the registry to receive messages
	registry.add(getName(), this);
    }
    protected void initDisplay() {
	Debug.debug("initDisplay: displayClassName=" + displayClassName);
	if (displayClassName == null) {
	    Debug.warn("yow! displayClassName shouldn't be null!");
	    return;
	}
	Class displayClass;
	try {
	    displayClass = Class.forName(displayClassName);
	} catch (ClassNotFoundException ex) {
	    Debug.warn("display class not found: " + displayClassName);
	    return;
	}
	if (!FacilitatorDisplay.class.isAssignableFrom(displayClass)) {
	    Debug.warn("display class \"" + displayClassName + "\" is not a FacilitatorDisplay");
	    return;
	}
	Constructor cons;
	try {
	    // Get the constructor that takes a Facilitator as argument
	    Class[] argtypes = { Facilitator.class, Boolean.class };
	    cons = displayClass.getConstructor(argtypes);
	    Debug.debug("initDisplay: cons=" + cons);
	} catch (NoSuchMethodException ex) {
	    Debug.warn("can't locate display constructor: " + ex);
	    return;
	} catch (SecurityException ex) {
	    Debug.warn("denied access to display constructor: " + ex);
	    return;
	}
	try {
	    // Create an instance of the display using the constructor
	    Object[] args = { this, hideTraffic };
	    display = (FacilitatorDisplay)cons.newInstance(args);
	    Debug.debug("initDisplay: display=" + display);
	} catch (InstantiationException ex) {
	    Debug.warn("display class is abstract: " + ex);
	    return;
	} catch (IllegalAccessException ex) {
	    Debug.warn("display constructor inaccessible: " + ex);
	    return;
	} catch (IllegalArgumentException ex) {
	    Debug.warn("illegal argument to display constructor: " + ex);
	    return;
	} catch (InvocationTargetException ex) {
	    Debug.warn("exception during display constructor: " + ex);
	    return;
	}
	// Add facilitator itself to the display
	display.add(getName());
	display.indicateStatus(getName(), "READY");
	Debug.debug("initDisplay: done");
    }
    protected void initLog() {
	if (doLog) {
	    try {
		log = new Log();
	    } catch (IOException ex) {
		Debug.warn("error opening log file: " + ex);
	    }
	}
    }
    protected void initSubscriptionManager() {
	Debug.debug("initSubscriptionManager: creating subscriptionManager");
	subscriptionManager = new SubscriptionManager(registry);
	Debug.debug("initSubscriptionManager: subscriptionManager=" + subscriptionManager);
	try {
	    KQMLList msg;
	    msg = KQMLList.fromString("(register . *)");
	    subscriptionManager.subscribe(this, msg);
	    msg = KQMLList.fromString("(unregister . *)");
	    subscriptionManager.subscribe(this, msg);
	    msg = KQMLList.fromString("(broadcast . *)");
	    subscriptionManager.subscribe(this, msg);
	    msg = KQMLList.fromString("(subscribe . *)");
	    subscriptionManager.subscribe(this, msg);
	    msg = KQMLList.fromString("(tell &key :content (module-status . *))");
	    subscriptionManager.subscribe(this, msg);
	    msg = KQMLList.fromString("(tell &key :content (component-status . *))");
	    subscriptionManager.subscribe(this, msg);
	} catch (IOException ex) {
	    Debug.warn("yow! bogus subscription in initSubscriptionManager: " + ex);
	}
	Debug.debug("initSubscriptionManager: done");
    }
    void initLauncher() {
	launcher = new Launcher(this);
    }
    void initStdin() {
	StdinClient stdin = new StdinClient(this);
	add(stdin);
	stdin.start();
    }
    //
    // Methods for handling clients
    //
    /**
     * Adding a client for the first time adds a the name to the registry and
     * adds a display item for it also.
     */
    public synchronized void add(Sendable client) {
	Debug.debug("Facilitator.add: client=" + client);
	String name = client.getName().toUpperCase();
	display.add(name);
	display.indicateStatus(name, "CONNECTED");
	Debug.debug("Facilitator.add: done");
    }
    /**
     * Registering a name for a client adds the new name to the registry
     * and creates a display entry for that name.
     */
    public synchronized void register(Sendable client, String name) {
	name = name.toUpperCase();
	Debug.debug("Facilitator.register: client=" + client + ", name=" + name);
	// If first registration for this client
	if (registry.lookupNameOfClient(client) == null) {

	    // Then remove the display item for its internal name
	    display.remove(client.getName());
	}
	// Check for previous registration of this name
	Sendable oldclient = registry.lookupClientByName(name);
	if (oldclient == client) {
	    Debug.warn("Facilitator.register: " + client + ": already registered as " + name);
	} else if (oldclient != null) {
	    Debug.warn("Facilitator.register: " + client + ": name alreday registered: " + name + " (by " + oldclient + ")");
	} else {
	    // Ok to register
	    Debug.debug("Facilitator.register: registering name " + name);
	    registry.add(name, client);
	    Debug.debug("Facilitator.register: adding display for " + name);
	    display.add(name);
	    display.indicateStatus(name, "CONNECTED");
	}
	Debug.debug("Facilitator.register: done");
    }
    /**
     * Unregistering a name for a client removes that name's entry from the
     * display and, if that was the last name for the client, adds back the
     * display entry with the internal name.
     */
    public synchronized void unregister(Sendable client, String name) {
	Debug.debug("Facilitator.unregister: client=" + client + ", name=" + name);
	// Check that we are legitimate registrees of this name
	if (registry.lookupClientByName(name) != client) {
	    Debug.warn("Facilitator.unregister: " + client + ": name not registered: " + name);
	} else {
	    // Ok to unregister
	    Debug.debug("Facilitator.register: unregistering name " + name);
	    registry.remove(name, client);
	    Debug.debug("Facilitator.register: removing display for " + name);
	    display.remove(name);
	    // Create temporary display for this guy (if one is needed)
	    if (registry.lookupNameOfClient(client) == null) {
		name = client.getName();
	    	display.add(name);
		display.indicateStatus(name, "CONNECTED");
	    }
	}
	Debug.debug("Facilitator.unregister: done");
    }    
    public synchronized void remove(Sendable client) {
	Debug.debug("Facilitator.remove: client=" + client);
	String name;
	// If no names registered...
	if (registry.lookupNameOfClient(client) == null) {
	    // ...Then we have just our internal name in the display
	    name = client.getName().toUpperCase();
	    Debug.debug("Facilitator.remove: removing internal name " + name);
	    display.remove(name);
	} else {
	    // Otherwise remove real display elements
	    Enumeration names = registry.enumerateNamesOfClient(client);
	    try {
	    	while (names.hasMoreElements()) {
	    	    name = (String)names.nextElement();
		    Debug.debug("Facilitator.remove: removing display item for " + name);
	    	    display.remove(name);
	    	}
	    } catch (NoSuchElementException ex) {
	    }
	    name = registry.lookupNameOfClient(client);
	}
	log.logExit(name);
	// Special case to be quieter about stdin
	if (client.getClass() != StdinClient.class) {
	    Debug.warn("client exited: " + name);
	}
	// Remove from any groups
	Debug.debug("Facilitator.remove: removing " + client + " from groups");
	Enumeration clients = registry.enumerateClients();
	try {
	    while (clients.hasMoreElements()) {
		Sendable s = (Sendable)clients.nextElement();
		if (s instanceof ClientGroup) {
		    ClientGroup g = (ClientGroup)s;
		    if (g.contains(this)) {
			g.remove(this);
		    }
	    	}
	    }
	} catch (NoSuchElementException ex) {
	}
	// Remove from registry also
	Debug.debug("Facilitator.remove: removing client " + client);
	registry.remove(client);
	Debug.debug("Facilitator.remove: done");
    }
    //
    // Methods for displaying message traffic
    //
    public void logSend(Sendable toWhom, KQMLPerformative msg) {
	String name = registry.lookupNameOfClient(toWhom);
	if (name == null) {
	    name = Thread.currentThread().getName();
	}
	if (log != null) {
	    log.logSend(name, msg);
	}
	if (display != null) {
	    display.indicateMessageReceived(name, msg);
	}
    }
    public void logReceive(Sendable fromWhom, KQMLPerformative msg) {
	String name = registry.lookupNameOfClient(fromWhom);
	if (name == null) {
	    name = Thread.currentThread().getName();
	}
	if (log != null) {
	    log.logReceive(name, msg);
	}
	if (display != null) {
	    display.indicateMessageSent(name, msg);
	}
    }
    public void logError(Sendable fromWhom, String msg) {
	String name = registry.lookupNameOfClient(fromWhom);
	if (name == null) {
	    name = Thread.currentThread().getName();
	}
	if (log != null) {
	    log.logError(name, msg);
	}
    }
    //
    // Methods useful for debugging (maybe invoked from a display menu or something)
    //
    public void dumpGroups() {
	System.out.println("---------- Facilitator Groups ----------");
	Enumeration clients = registry.enumerateClients();
	try {
	    while (clients.hasMoreElements()) {
		Sendable client = (Sendable)clients.nextElement();
		if (client instanceof ClientGroup) {
		    ClientGroup g = (ClientGroup)client;
		    g.dump();
		}
	    }
	} catch (NoSuchElementException ex) {
	}
	System.out.println("-----------------------------------------");
    }
    public void dumpRegistry() {
	System.out.println("---------- Facilitator Registry ----------");
	Enumeration names = registry.enumerateNames();
	try {
	    while (names.hasMoreElements()) {
		String name = (String)names.nextElement();
		System.out.println(name + ": " + registry.lookupClientByName(name));
	    }
	} catch (NoSuchElementException ex) {
	}
	System.out.println("------------------------------------------");
    }
    public void dumpSubscriptions() {
	System.out.println("---------- Facilitator Subscriptions ----------");
	Enumeration subs = subscriptionManager.enumerateSubscriptions();
	try {
	    while (subs.hasMoreElements()) {
		Subscription sub = (Subscription)subs.nextElement();
		Sendable client = sub.getClient();
		String name = registry.lookupNameOfClient(client);
		if (name == null) {
		    name = client.toString();
		}
		System.out.println(name + ": " + sub.getSubscriptionPattern());
	    }
	} catch (NoSuchElementException ex) {
	}
	System.out.println("-----------------------------------------------");
    }
    //
    // Main method for processing message that has arrived from a client
    //
    // Note that this method will be invoked by the thread corresponding
    // to the ClientSocket on which the message arrived.
    //
    public void handleClientMessage(KQMLPerformative msg, Sendable fromWhom) {
	Debug.debug("Facilitator.handleClientMessage: msg=" + msg);
	Sendable sender = fromWhom;
	Sendable receiver;
	// First see if :sender was given
	Object senderobj = msg.getParameter(":sender");
	Debug.debug("Facilitator.handleClientMessage: senderobj=" + senderobj);
	if (senderobj != null) {
	    // Yes, :sender given
	    String senderstr = senderobj.toString();
	    sender = registry.lookupClientByName(senderstr);
	    Debug.debug("Facilitator.handleClientMessage: looked up sender=" + sender);
	    if (sender == null) {
		// Hmm, :sender doesn't exist
		Debug.warn("Facilitator.handleClientMessage: warning: sender \"" + senderstr + "\" not registered");
		// Better go back to using original client as the sender
		sender = fromWhom;
	    }
	} else {
	    // No :sender given => use client name if we have one
	    String clientname = registry.lookupNameOfClient(fromWhom);
	    if (clientname == null) {
		if (!msg.getVerb().equalsIgnoreCase("REGISTER")) {
		    Debug.warn("Facilitator.handleClientMessage: warning: no :sender and no name for sending client");
		}
	    } else {
		msg.setParameter(":sender", clientname);
	    }
        }
	Debug.debug("Facilitator.handleClientMessage: sender=" + sender);
	// Now see if :receiver was given
	Object receiverobj = msg.getParameter(":receiver");
	Debug.debug("Facilitator.handleClientMessage: receiverobj=" + receiverobj);
	if (receiverobj != null) {
	    // Yes, :receiver given
	    String receiverstr = receiverobj.toString();
	    receiver = registry.lookupClientByName(receiverstr);
	    Debug.debug("Facilitator.handleClientMessage: receiver=" + receiver);
	    if (receiver == null) {
		// Receiver not found => error (unless already error/sorry)
		String verb = msg.getVerb();
		if (!(verb.equalsIgnoreCase("error") || verb.equalsIgnoreCase("sorry"))) {
		    errorReply(msg, sender, "unknown receiver: " + receiverstr);
		} else {
		    Debug.warn("Facilitator.handleClientMessage: unknown receiver in ERROR/SORRY: " +
		    	       receiverstr + "; dropping message");
		}
	    } else {
		// Receiver found ok: send it the message
		receiver.sendTo(msg, sender);
	    }
	} else {
	    // No :receiver in message => send to message board
	    subscriptionManager.broadcast(msg, sender);
	}
    }
    //
    // Sendable method: Message handling for the Facilitator itself
    // (see initSubscriptionManager() for the subscriptions that result in
    // these message being delivered, in addition to explicitly-addressed
    // messages).
    //
    // Since this is invoked from handleClientMessage(), above, it will also
    // typically be executed by the thread corresponding to the sending client.
    //
    public void sendTo(KQMLPerformative msg, Sendable fromWhom) {
	Debug.debug("Facilitator.receive: " + msg);
	String verb = msg.getVerb();
	if (verb.equalsIgnoreCase("register")) {
	    receiveRegister(msg, fromWhom);
	} else if (verb.equalsIgnoreCase("unregister")) {
	    receiveUnregister(msg, fromWhom);
	} else if (verb.equalsIgnoreCase("tell")) {
	    receiveTell(msg, fromWhom);
	} else if (verb.equalsIgnoreCase("request")) {
	    receiveRequest(msg, fromWhom);
	} else if (verb.equalsIgnoreCase("broadcast")) {
	    receiveBroadcast(msg, fromWhom);
	} else if (verb.equalsIgnoreCase("subscribe")) {
	    receiveSubscribe(msg, fromWhom);
	} else {
	    errorReply(msg, fromWhom, "unknown performative: " + verb);
	}
	Debug.debug("Facilitator.receive: done");
    }
    public void receiveRegister(KQMLPerformative msg, Sendable fromWhom) {
	// Handle :name parameter
	Object namearg = msg.getParameter(":name");
	if (namearg == null) {
	    errorReply(msg, fromWhom, "REGISTER: missing :name");
	    return;
	} 
	String name = null;
	try {
	    name = ((KQMLToken)namearg).stringValue();
	} catch (ClassCastException ex) {
	    errorReply(msg, fromWhom, "REGISTER: :name is not a token");
	    return;
	}
	// Add registration for client
	register(fromWhom, name);
	// Now check :group (or :class) argument
	Object grouparg = msg.getParameter(":group");
	if (grouparg != null) {
	    // We have a :group argument
	    if (grouparg instanceof KQMLToken) {
		// Belongs to one group
		String gname = grouparg.toString();
		Sendable gs = registry.lookupClientByName(gname);
		if (gs == null) {
		    // If not found, create group
		    gs = new ClientGroup(gname);
		    registry.add(gname, gs);
		} else if (!(gs instanceof ClientGroup)) {
		    errorReply(msg, fromWhom, "not a group: " + gname + " (ignored)");
		} else {
		    ClientGroup g = (ClientGroup)gs;
		    g.add(fromWhom);
		}
	    } else if (grouparg instanceof KQMLList) {
		// Belongs to several groups
		ListIterator names = ((KQMLList)grouparg).listIterator();
		try {
		    while (names.hasNext()) {
			String gname = names.next().toString();
			Sendable gs = registry.lookupClientByName(gname);
			if (gs == null) {
			    // If not found, create group
			    gs = new ClientGroup(gname);
			    registry.add(gname, gs);
			} else if (!(gs instanceof ClientGroup)) {
			    errorReply(msg, fromWhom, ":group not a group (ignored)" + gname);
			} else {
			    ClientGroup g = (ClientGroup)gs;
			    g.add(fromWhom);
			}
		    }
		} catch (NoSuchElementException ex) {
		}
	    } else {
		// Bogus
		errorReply(msg, fromWhom, ":group neither token nor list (ignored)" + grouparg);
	    }
	}
    }
    public void receiveUnregister(KQMLPerformative msg, Sendable fromWhom) {
	// Handle :name parameter
	Object namearg = msg.getParameter(":name");
	if (namearg == null) {
	    errorReply(msg, fromWhom, "UNREGISTER: missing :name");
	    return;
	} 
	String name = null;
	try {
	    name = ((KQMLToken)namearg).stringValue();
	} catch (ClassCastException ex) {
	    errorReply(msg, fromWhom, "UNREGISTER: :name is not a token");
	    return;
	}
	// Remove registration for client
	unregister(fromWhom, name);
    }
    public void receiveTell(KQMLPerformative msg, Sendable fromWhom) {
	Object contentobj = msg.getParameter(":content");
	if (contentobj == null) {
	    errorReply(msg, fromWhom, "missing content for TELL");
	    return;
	} else if (!(contentobj instanceof KQMLList)) {
	    errorReply(msg, fromWhom, "content of TELL is not a list");
	    return;
	}
	KQMLList content = (KQMLList)contentobj;
	KQMLObject car = content.get(0);
	if (car == null || !(car instanceof KQMLToken)) {
	    errorReply(msg, fromWhom, "TELL: car of :content is not a token");
	} else {
	    String carstr = car.toString();
	    if (carstr.equalsIgnoreCase("module-status")) {
		// Old non-infrastructure sends module-status
		receiveTellModuleStatus(msg, fromWhom, content);
	    } else if (carstr.equalsIgnoreCase("component-status")) {
		// Old defcomponent infrastructure sends component-status
		receiveTellComponentStatus(msg, fromWhom, content);
	    } else if (carstr.equalsIgnoreCase("start-conversation") ||
		       carstr.equalsIgnoreCase("end-conversation")) {
		// Ignore
	    } else {
		errorReply(msg, fromWhom, "unknown tell: " + carstr);
	    }
	}
    }
    public void receiveTellModuleStatus(KQMLPerformative msg,
				  Sendable fromWhom,
				  KQMLList content) {
	if (content.size() != 2) {
	    errorReply(msg, fromWhom, "TELL MODULE-STATUS: :content not length 2");
	    return;
	}
	// Don't use canonical name of client, use the :sender name (if any)
	KQMLObject senderobj = msg.getParameter(":sender");
	String sendername;
	if (senderobj != null) {
	    sendername = senderobj.toString();
	} else {
	    sendername = registry.lookupNameOfClient(fromWhom);
	    if (sendername == null) {
	    	errorReply(msg, fromWhom, "TELL MODULE-STATUS: no :sender and client not registered");
	    	return;
	    }
	}
	KQMLObject statusobj = content.get(1);
	if (statusobj == null || !(statusobj instanceof KQMLToken)) {
	    errorReply(msg, fromWhom, "TELL MODULE-STATUS: second of :content is not a token");
	    return;
	}
	String status = statusobj.toString();
	// Show status on display
	display.indicateStatus(sendername, status);
    }
    public void receiveTellComponentStatus(KQMLPerformative msg,
					   Sendable fromWhom,
					   KQMLList content) {
	KQMLObject whoobj = content.getKeywordArg(":who");
	if (whoobj == null || !(whoobj instanceof KQMLToken)) {
	    errorReply(msg, fromWhom, "TELL COMPONENT-STATUS: :who is not a token");
	    return;
	}
	String who = whoobj.toString();
	KQMLObject whatobj = content.getKeywordArg(":what");
	if (whatobj == null || !(whatobj instanceof KQMLToken)) {
	    errorReply(msg, fromWhom, "TELL COMPONENT-STATUS: :what is not a token");
	    return;
	}
	String what = whatobj.toString();
	// Show status on display
	display.indicateStatus(who, what);
    }
    public void receiveRequest(KQMLPerformative msg, Sendable fromWhom) {
	Object contentobj = msg.getParameter(":content");
	if (contentobj == null) {
	    errorReply(msg, fromWhom, "missing content for REQUEST");
	    return;
	} else if (!(contentobj instanceof KQMLList)) {
	    errorReply(msg, fromWhom, "content of REQUEST is not a list");
	    return;
	}
	KQMLList content = (KQMLList)contentobj;
	Object car = content.get(0);
	if (car == null || !(car instanceof KQMLToken)) {
	    errorReply(msg, fromWhom, "car of :content is not a token");
	} else {
	    String carstr = ((KQMLToken)car).stringValue();
	    if (carstr.equalsIgnoreCase("exit")) {
		receiveRequestExit(msg, fromWhom, content);
	    } else if (carstr.equalsIgnoreCase("chdir")) {
		receiveRequestChdir(msg, fromWhom, content);
	    } else if (carstr.equalsIgnoreCase("hide-window")) {
		display.hideWindow();
	    } else if (carstr.equalsIgnoreCase("show-window")) {
		display.showWindow();
	    } else if (carstr.equalsIgnoreCase("define-group")) {
		receiveRequestDefineGroup(msg, fromWhom, content);
	    } else if (carstr.equalsIgnoreCase("dump-registry")) {
		dumpRegistry();
	    } else if (carstr.equalsIgnoreCase("dump-subscriptions")) {
		dumpSubscriptions();
	    } else if (carstr.equalsIgnoreCase("start-module")) {
		try {
		    launcher.receiveRequestStartModule(content);
		} catch (LauncherException ex) {
		    errorReply(msg, fromWhom, ex.toString() + Misc.getStackTraceString(ex));
		}
	    } else if (carstr.equalsIgnoreCase("stop-module")) {
		try {
		    launcher.receiveRequestStopModule(content);
		} catch (LauncherException ex) {
		    errorReply(msg, fromWhom, ex.getMessage());
		}
	    } else {
		errorReply(msg, fromWhom, "unknown request: " + carstr);
	    }
	}
    }
    public void receiveRequestExit(KQMLPerformative msg, Sendable fromWhom,
				   KQMLList content) {
	if (content.size() == 1) {
	    // No exit value
	    exit(0);
	} else {
	    // Something specified for exit value
	    Object second = content.get(1);
	    if (!(second instanceof KQMLToken)) {
		errorReply(msg, fromWhom, "REQUEST EXIT: bogus exit value: " + second);
		return;
	    }
	    String valuestr = ((KQMLToken)second).stringValue();
	    int value = Integer.parseInt(valuestr);
	    exit(value);
	}
    }
    public void receiveRequestChdir(KQMLPerformative msg, Sendable fromWhom,
				    KQMLList content) {
	// If not logging, we can ignore this
	if (!doLog) {
	    return;
	}
	// Otherwise get the directory name
	String dirname = content.get(1).toString();
	// Strip off quotes if necessary
	if (dirname.startsWith("\"")) {
	    dirname = dirname.substring(1, dirname.length()-1);
	}
	try {
	    // Open new log first
	    Log newlog = new Log(dirname);
	    // Then close current log
	    log.close();
	    // Make new log current
	    log = newlog;
	} catch (IOException ex) {
	    Debug.warn("error opening log file: " + ex);
	    return;
	}
    }
    public void receiveRequestDefineGroup(KQMLPerformative msg,
					  Sendable fromWhom,
					  KQMLList content) {
	// Always have group name
	String name = content.get(1).toString();
	// May or may not have parent group
	ClientGroup pg = null;
	KQMLObject pobj = content.getKeywordArg(":parent");
	if (pobj != null) {
	    String pname = pobj.toString();
	    Sendable ps = registry.lookupClientByName(pname);
	    if (ps == null) {
		errorReply(msg, fromWhom, "unknown parent for DEFINE-GROUP: " + pname);
		return;
	    } else if (!(ps instanceof ClientGroup)) {
		errorReply(msg, fromWhom, "DEFINE-GROUP parent not a group: " + pname);
		return;
	    } else {
		pg = (ClientGroup)ps;
	    }
	}
	// Create group
	ClientGroup g = new ClientGroup(name, pg);
	// Add to registry (so it can be found as a receiver)
	registry.add(name, g);
    }		
    public void receiveBroadcast(KQMLPerformative msg,
				 Sendable fromWhom) {
	Debug.debug("Facilitator.receiveBroadcast: msg=" + msg);
	Object contentobj = msg.getParameter(":content");
	if (contentobj == null) {
	    errorReply(msg, fromWhom, "missing content for BROADCAST");
	    return;
	} else if (!(contentobj instanceof KQMLList)) {
	    errorReply(msg, fromWhom, "content of BROADCAST is not a list");
	    return;
	}
	KQMLList content = (KQMLList)contentobj;
	// Check that content is in fact a performative
	KQMLPerformative contentmsg = null;
	try {
	    contentmsg = new KQMLPerformative(content);
	} catch (KQMLBadPerformativeException ex) {
	    errorReply(msg, fromWhom, "bad content for broadcast: " + ex.getMessage());
	    return;
	}
	// If no :sender, fill it in
	if (contentmsg.getParameter(":sender") == null) {
	    String clientname = registry.lookupNameOfClient(fromWhom);
	    if (clientname == null) {
		Debug.warn("Facilitator.receiveBroadcast: warning: no :sender and no name for sending client");
	    } else {
		contentmsg.setParameter(":sender", clientname);
	    }
	}
	// Send to each client named in registry
	Enumeration names = registry.enumerateNames();
	try {
	    while (names.hasMoreElements()) {
		String name = (String)names.nextElement();
		Sendable client = registry.lookupClientByName(name);
		// Sort of strange to have to block groups here, but...
		if (!(client instanceof ClientGroup)) {
		    Debug.debug("Facilitator.receiveBroadcast: sending to client " + client);
		    contentmsg.setParameter(":receiver", name);
		    client.sendTo(contentmsg, fromWhom);
		}
	    }
	} catch (NoSuchElementException ex) {
	}
	Debug.debug("Facilitator.receiveBroadcast: done");
    }
    public void receiveSubscribe(KQMLPerformative msg, Sendable fromWhom) {
	Object contentobj = msg.getParameter(":content");
	if (contentobj == null) {
	    errorReply(msg, fromWhom, "missing content for SUBSCRIBE");
	    return;
	} else if (!(contentobj instanceof KQMLList)) {
	    errorReply(msg, fromWhom, "content of SUBSCRIBE is not a list");
	    return;
	}
	KQMLList content = (KQMLList)contentobj;
	Debug.debug("Facilitator.receiveSubscribe: fromWhom=" + fromWhom + ", content=" + content);
	subscriptionManager.subscribe(fromWhom, content);
	Debug.debug("Facilitator.receiveSubscribe: done");
    }
    //
    // Output functions
    //
    protected void send(KQMLPerformative msg, Sendable toWhom) {
	toWhom.sendTo(msg, this);
    }
    protected void reply(String verb, KQMLList content,
			 KQMLPerformative msg, Sendable toWhom) {
	// New performative
	KQMLPerformative replymsg = new KQMLPerformative(verb);
	// Set sender (to us)
	replymsg.setParameter(":sender", getName());
	// Set receiver (to them)
	KQMLObject sender = msg.getParameter(":sender");
	if (sender != null) {
	    replymsg.setParameter(":receiver", sender);
	}
	// Set in-reply-to tag (if needed)
	KQMLObject reply_with = msg.getParameter(":reply-with");
	if (reply_with != null) {
	    replymsg.setParameter(":in-reply-to", reply_with);
	}
	// Set content
	replymsg.setParameter(":content", content);
	// Off it goes...
	send(replymsg, toWhom);
    }
    protected void errorReply(KQMLPerformative msg, Sendable toWhom,
			      String comment) {
	// New performative
	KQMLPerformative replymsg = new KQMLPerformative("error");
	// Set sender (to us)
	replymsg.setParameter(":sender", getName());
	// Set receiver (to them)
	KQMLObject sender = msg.getParameter(":sender");
	if (sender != null) {
	    replymsg.setParameter(":receiver", sender);
	}
	// Set in-reply-to tag (if needed)
	KQMLObject reply_with = msg.getParameter(":reply-with");
	if (reply_with != null) {
	    replymsg.setParameter(":in-reply-to", reply_with);
	}
	// Set comment as KQML string
	replymsg.setParameter(":comment", new KQMLString(comment));
	// Off it goes...
	send(replymsg, toWhom);
    }
    //
    // Miscellaneous
    //
    protected void exit(int n) {
	// Not sure this right...
	// Should rely on clients closing down and readers being daemons
	// so we exit when we're an application, but can't rely on it.
	System.exit(n);
    }
    //
    // When run as an application
    //
    public static void main(String argv[]) {
	new Facilitator(argv).run();
    }
}

