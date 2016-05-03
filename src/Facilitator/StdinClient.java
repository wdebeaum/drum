/*
 * @(#)StdinClient.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 14 May 2001
 * $Id: StdinClient.java,v 1.1.1.1 2005/01/14 19:48:08 ferguson Exp $
 *
 * Reads messages from stdin and hands to Facilitator. Used for bootstrapping
 * the Facilitator.
 */

package TRIPS.Facilitator;

import java.io.EOFException;
import java.io.IOException;
import TRIPS.KQML.KQMLReader;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLException;

public class StdinClient extends Thread implements Sendable {
    //
    // Fields
    //
    protected Facilitator facilitator;
    protected boolean running;
    protected KQMLReader in;
    //
    // Constructors
    //
    public StdinClient(Facilitator facilitator) {
	Debug.debug("StdinClient.<init>");
	this.facilitator = facilitator;
	// Set thread name for debugging
	setName("stdin");
    }
    //
    // Thread method: Process messages on stdin (forever until EOF)
    //
    public void run() {
	Debug.debug("StdinClient.run: opening stdin KQMLReader");
	in = new KQMLReader(System.in);
	try {
	    // Then read messages forever (until EOF or error)
	    running = true;
	    while (running) {
		try {
		    // Read from socket
		    KQMLPerformative msg = in.readPerformative();
		    Debug.debug("StdinClient.run: msg=" + msg);
		    facilitator.logReceive(this, msg);
		    // We could call sendTo() here, but then we couldn't
		    // use stdin to inject messages, which might be useful
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
	    //Debug.warn("stdin connection closed");
	    facilitator.remove(this);
	}
    }
    public void stopSafely(){
	running = false;
	this.interrupt();
    }
    //
    // Sendable method: Message for us goes to stderr
    //
    public void sendTo(KQMLPerformative msg, Sendable fromWhome) {
    	// Log output
	facilitator.logSend(this, msg);
	// Print to stderr
	System.err.println(msg.toString());
    }
    //
    // Pretty-printing
    //
    public String toString() {
	return "[" + getName() + "]";
    }
}
