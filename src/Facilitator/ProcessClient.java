/*
 * @(#)ProcessClient.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 14 May 2001
 * $Id: ProcessClient.java,v 1.1.1.1 2005/01/14 19:48:08 ferguson Exp $
 *
 * Represents a client that is a Process launched by us and that we
 * have connected (some Processes started by the JavaLauncher code
 * might connect themselves, which will make the SocketClients when
 * they make the connection).
 *
 * This code is derived from SocketClient. Looking at them both now,
 * of course, the common functionality should be abstracted into a
 * class from which they both inherit. Oh well.
 *
 */

package TRIPS.Facilitator;

import java.io.IOException;
import java.io.EOFException;
import java.io.PrintWriter;
import TRIPS.KQML.KQMLReader;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLException;

public class ProcessClient extends Thread implements Sendable {
    //
    // Class fields
    //
    protected static int counter = 0;
    //
    // Fields
    //
    protected Facilitator facilitator;
    protected Process proc;
    protected boolean running;
    protected KQMLReader in;
    protected PrintWriter out;
    //
    // Constructors
    //
    public ProcessClient(Facilitator facilitator, Process proc) {
	Debug.debug("ProcessClient.<init>: proc=" + proc);
	this.facilitator = facilitator;
	this.proc = proc;
	counter += 1;
	// Set thread name for debugging
	setName("ProcessClient-" + counter);
    }
    //
    // Thread method: Process messages on our socket (forever)
    //
    public void run() {
	//Debug.debug("ProcessClient.run: starting");
	// Get I/O streams from the socket
	Debug.debug("ProcessClient.run[" + getName() + "]: proc=" + proc);
	Debug.debug("ProcessClient.run[" + getName() + "]: opening output PrintWriter");
	out = new PrintWriter(proc.getOutputStream(), true);
	Debug.debug("ProcessClient.run[" + getName() + "]: opening input KQMLReader");
	in = new KQMLReader(proc.getInputStream());
	try {
	    // Then read messages forever (until EOF or error)
	    running = true;
	    while (running) {
		try {
		    // Read from socket
		    KQMLPerformative msg = in.readPerformative();
		    Debug.debug("ProcessClient.run[" + getName() + "]: msg=" + msg);
		    facilitator.logReceive(this, msg);
		    facilitator.handleClientMessage(msg, this);
		} catch(KQMLException ex) {
		    String msg = ex.toString();
		    Debug.warn("from " + getName() + ": " + msg);
		    facilitator.logError(this, msg);
		}
	    }
	} catch(EOFException ex) {
	} catch(IOException ex) {
	    Debug.warn("IO error: " + ex);
	} finally {
	    Debug.debug("ProcessClient.run[" + getName() + "]: finally clause");
	    proc.destroy();
	    // Get rid of this clients registrations, etc.
	    Debug.debug("ProcessClient.run[" + getName() + "]: cleaning up");
	    facilitator.remove(this);
	    Debug.debug("ProcessClient.run[" + getName() + "]: done");
	}
    }
    public void stopSafely(){
	running = false;
	this.interrupt();
    }
    //
    // Sendable method: Message for us goes down our socket
    //
    // Note that this method will typically be executed by the thread
    // corresponding to the sender of the message.
    //
    public void sendTo(KQMLPerformative msg, Sendable fromWhome) {
    	// Log output
	facilitator.logSend(this, msg);
	// Print to socket
	out.println(msg.toString());
    }
    //
    // Pretty-printing
    //
    public String toString() {
	return "[" + getName() + "]";
    }
}
