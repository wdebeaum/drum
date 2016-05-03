/*
 * KQMLBadHashException.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 17 Feb 1998
 * Time-stamp: <Wed Jan 28 11:04:00 EST 2004 ferguson>
 *
 */
package TRIPS.KQML;

/**
 * Thrown when an illegal ``hashed string'' syntax is detected (it should
 * be ``#<digits>"<chars>''.
 * This is
 * usually caused by a hash (pound) character getting into the input by
 * accident, since hashed strings are rarely used. They can be printed
 * by Lisp, for example, when printing structures without print functions.
 *
 * @see KQMLReader
 */
public class KQMLBadHashException extends KQMLException {
    public KQMLBadHashException(String s) { super(s); }
    public String toString() {
	return "expected digit or `\"': " + getMessage();
    }
}
