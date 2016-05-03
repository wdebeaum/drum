/*
 * @(#)SocketClient.java
 *
 * David Costello, costello@cs.rochester.edu,  8 Jul 1999
 * $Id: SocketClient.java,v 1.4 2007/11/15 14:40:16 wdebeaum Exp $
 *
 * Manages communications for a specific client connected to
 * the input manager hub by bridging messages to and from it's
 * client module and the facilitator.
 *
 * SocketClients can be associated with multiple names through multiple
 * REGISTER messages.
 */

package TRIPS.Facilitator;

import java.io.PrintWriter;
import java.io.EOFException;
import java.io.IOException;
import java.net.Socket;
import TRIPS.KQML.KQMLReader;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLException;

public class SocketClient extends Thread implements Sendable {
    //
    // Class fields
    //
    protected static int counter = 0;
    //
    // Fields
    //
    protected Facilitator facilitator;
    protected Socket sock;
    protected boolean running;
    protected KQMLReader in;
    protected PrintWriter out;
    //
    // Constructors
    //
    public SocketClient(Facilitator facilitator, Socket sock) {
	Debug.debug("SocketClient.<init>: sock=" + sock);
	this.facilitator = facilitator;
	this.sock = sock;
	counter += 1;
	// Set thread name for debugging
	setName("SocketClient-" + counter);
    }
    //
    // Thread method: Process messages on our socket (forever)
    //
    public void run() {
	//Debug.debug("SocketClient.run: starting");
	// Get I/O streams from the socket
	Debug.debug("SocketClient.run[" + getName() + "]: sock=" + sock);
	try {
	    Debug.debug("SocketClient.run[" + getName() + "]: opening output PrintWriter");
	    out = new PrintWriter(sock.getOutputStream(), true);
	} catch (IOException ex) {
	    Debug.warn("couldn't get OutputStream for socket " + sock + ": " + ex);
	    return;
	}
	try {
	    Debug.debug("SocketClient.run[" + getName() + "]: opening input KQMLReader");
	    in = new KQMLReader(sock.getInputStream());
	} catch (IOException ex) {
	    Debug.warn("couldn't get InputStream for socket " + sock + ": " + ex);
	    return;
	}
	try {
	    // Then read messages forever (until EOF or error)
	    running = true;
	    while (running) {
		try {
		    // Read from socket
		    Debug.debug("SocketClient.run[" + getName() + "]: waiting for a msg...");
		    KQMLPerformative msg = in.readPerformative();
		    Debug.debug("SocketClient.run[" + getName() + "]: msg=" + msg);
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
	    Debug.debug("SocketClient.run[" + getName() + "]: finally clause");
	    try {
		sock.close();
	    } catch (IOException ex) {
		// Nothing we can do
	    }
	    // Get rid of this clients registrations, etc.
	    Debug.debug("SocketClient.run[" + getName() + "]: cleaning up");
	    facilitator.remove(this);
	    Debug.debug("SocketClient.run[" + getName() + "]: done");
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
    synchronized public void sendTo(KQMLPerformative msg, Sendable fromWhome) {
    	// Log output
	facilitator.logSend(this, msg);
	// Write to socket
	try {
	    msg.write(out);
	    out.println(); // Shouldn't be needed (but is)!
	    out.flush();
	} catch (IOException ex) {
	    System.err.println("facilitator: " + getName() + ": " + ex.getMessage());
	}
    }
    //
    // Pretty-printing
    //
    public String toString() {
	return "[" + getName() + "]";
    }
}
