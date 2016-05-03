/*
 * KQMLExpectedListException.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 16 Mar 2000
 * Time-stamp: <Wed Jan 28 11:03:23 EST 2004 ferguson>
 *
 */
package TRIPS.KQML;

/**
 * Thrown when the expression read is not a list.
 *
 * @see KQMLReader
 */
public class KQMLExpectedListException extends KQMLException {
    public KQMLExpectedListException(String s) { super(s); }
    public String toString() {
	return "expected list: " + getMessage();
    }
}
