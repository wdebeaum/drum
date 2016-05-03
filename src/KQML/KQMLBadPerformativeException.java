/*
 * KQMLBadPerformativeException.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 17 Feb 1998
 * Time-stamp: <Wed Jan 28 11:04:18 EST 2004 ferguson>
 *
 */
package TRIPS.KQML;

/**
 * Thrown when the expression read is not a performative (or actually,
 * not a list, since we don't check that it's actually a verb followed
 * by keyword/value pairs).
 *
 * @see KQMLReader
 */
public class KQMLBadPerformativeException extends KQMLException {
    public KQMLBadPerformativeException(String s) { super(s); }
    public String toString() {
	return "bad performative: " + getMessage();
    }
}
