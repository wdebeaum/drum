/*
 * Debug.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 14 Dec 2000
 * Time-stamp: <Thu Dec 21 13:04:26 EST 2000 ferguson>
 *
 * Since modified by William Taysom Feb. 3, 2005.
 */

package TRIPS.util;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

/**
 * An superclass for adding debug message printing for Trips modules.
 * Individual modules should contain a subclass which overrides the
 * setDebugLevel(String) and getDebugLevel() methods.
 */
public class Debug {

	/*** Subclasses should override. ***/
	
	private static String _debugLevel = "off";

	public static String getDebugLevel() {
		return _debugLevel;
	}
	
	/**
	 * "off" prints no messages.
	 * "debug" prints all messages.
	 *
	 * At other values, only messages of that level will be printed.
	 */
	public static void setDebugLevel(String level) {
		_debugLevel = level;
	}
	
	/*** Initialization Functions ***/
	
	public static void updateLevelFromRequestContent(KQMLList content) {
		if (content.length() == 0) return;
		String request = content.nth(0).stringValue();
		if (!request.equalsIgnoreCase("set-debug")) return;
		KQMLObject level = content.getKeywordArg(":LEVEL");
		if (level == null) return;
		setDebugLevel(level.stringValue());
	}
	
	public static void updateLevelFromArgs(String[] args) {
		for (int i = 0; i < args.length; ++i) {
			if ("-debug".equalsIgnoreCase(args[i])) {
				if (i + 1 == args.length || args[i + 1].charAt(0) == '-') {
					setDebugLevel("debug");
				} else {
					setDebugLevel(args[i + 1]);
				}
			}
		}
	}
	
	/**
	 * Clients should call when they want to print a debug message.
	 */
	public static void log(String level, Object message) {
		if ("off".equalsIgnoreCase(getDebugLevel())) return;
		if (getDebugLevel().equalsIgnoreCase(level)
				||  "debug".equalsIgnoreCase(getDebugLevel()))
			System.out.println(message);
	}
	
	/*** Convenience Methods ***/
	
	public static void info(Object message) {
		log("info", message);
	}
	
	public static void err(Object message) {
		log("error", message);
	}
	
	public static void bug(Object message) {
		log("debug", message);
	}
	
	/*** Old Methods ***/
	
	public static void debug(String s) {
	System.err.println(s);
    }
    public static void warn(String s) {
	System.err.println(s);
    }
    public static void error(String s) {
	System.err.println(s);
    }
}
