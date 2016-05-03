/*
 * KQMLExpectedWhitespaceException.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 17 Feb 1998
 * Time-stamp: <Wed Jan 28 11:04:42 EST 2004 ferguson>
 *
 */
package TRIPS.KQML;

/**
 * Thrown when whitespace is expected but something else is read.
 *
 * @see KQMLReader
 */
public class KQMLExpectedWhitespaceException extends KQMLException {
    public KQMLExpectedWhitespaceException(String s) { super(s); }
    public String toString() {
	return "expected whitespace: " + getMessage();
    }
}
