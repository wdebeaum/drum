/*
 * KQMLBadCharacterException.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 17 Feb 1998
 * Time-stamp: <Wed Jan 28 11:03:37 EST 2004 ferguson>
 *
 */
package TRIPS.KQML;

/**
 * Thrown when a non-KQML character is read.
 *
 * @see KQMLReader
 */
public class KQMLBadCharacterException extends KQMLException {
    public KQMLBadCharacterException(String s) { super(s); }
    public String toString() {
	return "illegal character: " + getMessage();
    }
}
