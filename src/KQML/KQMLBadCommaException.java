/*
 * KQMLBadCommaException.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 17 Feb 1998
 * Time-stamp: <Wed Jan 28 11:03:54 EST 2004 ferguson>
 *
 */
package TRIPS.KQML;

/**
 * Thrown when a comma is read outside of a backquoted expression.
 *
 * @see KQMLReader
 */
public class KQMLBadCommaException extends KQMLException {
    public KQMLBadCommaException(String s) { super(s); }
    public String toString() {
	return "comma outside of backquote: " + getMessage();
    }
}
