/*
 * KQMLObject.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 12 Feb 1999
 * Time-stamp: <Fri Jun  1 13:42:26 EDT 2007 ferguson>
 */

package TRIPS.KQML;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.io.Serializable;

/**
 * Base class for all KQML objects (KQMLPerformative, KQMLList, etc.).
 *
 * @see KQMLReader
 */
abstract public class KQMLObject implements Serializable {
    /**
     * version# for serialization
     */
    protected static final long serialVersionUID = 2;


    /**
     * Writes this KQMLToken to the given Writer.
     */
    abstract public void write(Writer out) throws IOException;
    /**
     * This method simply calls the object's toString() method. However,
     * subclasses, such as KQMLString(), can override this definition
     * in order to provide a different ``string value'', in cases where
     * this differs from the printed representation of the object (as
     * it does with strings, which print with quotes and escapes, but whose
     * ``value'' does not include such things).
     */
    public String stringValue() {
	return toString();
    }
}
