/*
 * KQMLBadCloseException.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 17 Feb 1998
 * Time-stamp: <Wed Jan 28 11:03:46 EST 2004 ferguson>
 *
 */
package TRIPS.KQML;

/**
 * Thrown when a closing parenthesis was expected but not read. (In fact,
 * this should never be thrown, but...)
 *
 * @see KQMLReader
 */
public class KQMLBadCloseException extends KQMLException {
    public KQMLBadCloseException(String s) { super(s); }
    public String toString() {
	return "expected ')': " + getMessage();
    }
}
