/*
 * KQMLException.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 17 Feb 1998
 * Time-stamp: <Wed Jan 28 11:04:26 EST 2004 ferguson>
 *
 */
package TRIPS.KQML;

import java.io.IOException;

/**
 * Parent class of all exceptions thrown during KQML I/O. This is a
 * subclass of IOException so that applications that don't care about
 * the details of an error can just catch them all.
 *
 * @see KQMLReader
 */
public class KQMLException extends IOException {
    public KQMLException(String s) { super(s); }
}
