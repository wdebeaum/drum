/*
 * TripsConnection.java
 *
 * George Ferguson, ferguson@cs.rochester.edu,  5 Nov 2003
 * Time-stamp: <Wed Nov  5 16:12:29 EST 2003 ferguson>
 */

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.ConnectException;

/**
 * A two-way connection to the TRIPS Facilitator.
 */
public class TripsConnection {
    /**
     * The socket underlying this TripsConnection.
     */
    Socket socket;

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
    protected static final int MAX_PORT_TRIES = 100;

    /**
     * Construct a new connection to the TRIPS Facilitator at the given
     * host and port.
     */
    public TripsConnection(String host, int port) throws IOException {
	socket = connect(host, port);
	if (socket == null) {
	    throw new IOException("couldn't connect to Facilitator");
	}
    }
    /**
     * Construct a new connection to the TRIPS Facilitator at the given
     * host and the default port.
     */
    public TripsConnection(String host) throws IOException {
	this(host, DEFAULT_PORT);
    }
    /**
     * Construct a new connection to the TRIPS Facilitator at the default
     * host and port.
     */
    public TripsConnection() throws IOException {
	this(DEFAULT_HOST, DEFAULT_PORT);
    }

    /**
     * Connect to server using standard protocol for finding port.
     * Returns the connected Socket if successful, else null.
     * <p>
     * The standard protocol is to scan upwards from the given port
     * number looking for the Facilitator, up to a maximum of
     * MAX_PORT_TRIES from the start.
     */
    protected Socket connect(String host, int startport) throws IOException {
	int maxtries = MAX_PORT_TRIES;
	for (int port=startport; port < startport + maxtries; port++) {
	    try {
		return new Socket(host, port);
	    } catch (ConnectException ex) {
	    }
	}
	return null;
    }

    /**
     * Returns the InputStream from this TripsConnection.
     */
    public InputStream getInputStream() throws IOException {
	return socket.getInputStream();
    }

    /**
     * Returns the OutputStream from this TripsConnection.
     */
    public OutputStream getOutputStream() throws IOException {
	return socket.getOutputStream();
    }

    /**
     * Test program for TripsConnection.
     */
    public static void main(String[] argv) throws IOException {
	TripsConnection conn = new TripsConnection();
	new StreamWatcher(conn.getInputStream()).start();
	PrintWriter toTrips = new PrintWriter(conn.getOutputStream(), true);
	BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));
	while (true) {
	    String text = stdin.readLine();
	    if (text == null || text.equalsIgnoreCase("quit")) {
		System.exit(0);
	    }
	    toTrips.println(text);
	}
    }
}

class StreamWatcher extends Thread {
    BufferedReader in;
    public StreamWatcher(InputStream in) {
	this.in = new BufferedReader(new InputStreamReader(in));
    }
    public void run() {
	try {
	    while (true) {
		String text = in.readLine();
		if (text == null) {
		    break;
		}
		System.out.println(text);
	    }
	} catch (IOException ex) {
	    ex.printStackTrace();
	}
    }
}
