/*
 * LauncherException.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 14 May 2001
 * $Id: LauncherException.java,v 1.2 2007/07/13 17:09:00 blaylock Exp $
 */

package TRIPS.Facilitator;

/**
 * Exception thrown by Launcher routines.
 */
public class LauncherException extends Exception {
    LauncherException(String s, Throwable cause) { super(s,cause); }
    LauncherException(String s) { super(s); }
}
