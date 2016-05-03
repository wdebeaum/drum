/*
 * Debug.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 14 Dec 2000
 * Time-stamp: <Thu Jun 25 23:42:45 CDT 2015 lgalescu>
 *
 * Since modified by William Taysom Feb. 3, 2005.
 * Modified by Lucian Galescu, 26 Jun 2015.
 */

package TRIPS.DrumGUI;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

/**
 * Class for printing debug information to STDERR.
 */
public class Debug {

    /** Debugging levels */
    public enum Level {
        OFF, FATAL, ERROR, INFO, WARNING, DEBUG, ALL;

        public boolean equals(String level) {
            return name().equalsIgnoreCase(level);
        }

    };

    private static Level _debugLevel = Level.INFO;

    public static Level getDebugLevel() {
        return _debugLevel;
    }

    /**
     * Sets {@link #_debugLevel}.
     * <p>
     * Possible values:
     * <ul>
     * <li>"off" prints no messages
     * <li>"info" prints only informational messages
     * <li>"warning" also prints warnings
     * <li>"debug" also prints debugging information
     * <li>"all" prints all messages
     * </ul>
     *
     * @param level
     */
    public static void setDebugLevel(String level) throws IllegalArgumentException {
        _debugLevel = Level.valueOf(level.toUpperCase());
    }

    /**
     * Sets {@link #_debugLevel}.
     * 
     * @param level
     */
    public static void setDebugLevel(Level level) {
        _debugLevel = level;
    }

    /**
     * Sets {@link #_debugLevel} from a TRIPS message.
     * 
     * @param content
     */
    public static void setDebugLevel(KQMLList content) throws IllegalArgumentException {
        if (content.size() == 0)
            return;
        String request = content.get(0).stringValue();
        if (!request.equalsIgnoreCase("set-debug"))
            return;
        KQMLObject level = content.getKeywordArg(":LEVEL");
        if (level == null)
            return;
        setDebugLevel(level.stringValue());
    }

    /**
     * Prints a message.
     */
    public static void log(String level, Object message) {
        if (getDebugLevel().equals(Level.OFF))
            return;
        if (getDebugLevel().equals(Level.ALL) || (getDebugLevel().compareTo(Level.valueOf(level)) >= 0)) {
            System.err.println("[" + level + "] " + message);
        }
    }

    /*** Convenience Methods ***/

    public static void info(Object message) {
        log("INFO", message);
    }

    public static void warn(Object message) {
        log("WARNING", message);
    }

    public static void debug(Object message) {
        log("DEBUG", message);
    }

    public static void error(Object message) {
        log("ERROR", message);
    }

    public static void fatal(Object message) {
        log("FATAL", message);
        System.exit(-1);
    }

}
