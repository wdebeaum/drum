/*
 * Debug.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 14 Dec 2000
 * $Id: Debug.java,v 1.1.1.1 2005/01/14 19:48:08 ferguson Exp $
 */

package TRIPS.Facilitator;

public class Debug {
    static boolean debuggingEnabled = false;
    public static void setDebuggingEnabled(boolean flag) {
	debuggingEnabled = flag;
    }
    public static void debug(String s) {
	if (debuggingEnabled) {
	    System.err.println("facilitator: " + s);
	}
    }
    public static void warn(String s) {
	System.err.println("facilitator: " + s);
    }
    public static void error(String s) {
	System.err.println("facilitator: " + s);
    }
}
