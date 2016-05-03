/*
 * KQMLBadOpenException.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 17 Feb 1998
 * Time-stamp: <Wed Jan 28 11:04:08 EST 2004 ferguson>
 *
 */
package TRIPS.KQML;

/**
 * Thrown when an open parenthesis was read when one was not expected.
 * (In fact, this should never be thrown...)
 *
 * @see KQMLReader
 */
public class KQMLBadOpenException extends KQMLException {
    public KQMLBadOpenException(String s) { super(s); }
    public String toString() {
	return "expected '(': " + getMessage();
    }
}
