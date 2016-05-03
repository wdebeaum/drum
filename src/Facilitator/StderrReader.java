/*
 * StderrReader.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 14 May 2001
 * $Id: StderrReader.java,v 1.1.1.1 2005/01/14 19:48:08 ferguson Exp $
 */

package TRIPS.Facilitator;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;

/**
 * This class is simply a Thread that is handed an InputStream when
 * it's created. It copies from that stream to System.err until EOF, at
 * which point it exits.
 */
public class StderrReader extends Thread {
    BufferedReader in;
    public StderrReader(InputStream stream) {
	this.in = new BufferedReader(new InputStreamReader(stream));
    }
    public void run() {
	while (true) {
	    try {
		String line = in.readLine();
		if (line == null) {
		    break;
		}
		System.err.println(line);
	    } catch (IOException ex) {
		// Don't bother reporting problems...
		break;
	    }
	}
    }
}
