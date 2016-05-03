/*
 * KQMLQuotation.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 17 Feb 1998
 * Time-stamp: <Fri Jun  1 13:44:56 EDT 2007 ferguson>
 *
 */

package TRIPS.KQML;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

/**
 * A class representation quotations in KQML. These are expressions
 * preceded by a quote, backquote, or comma (the ``type'' of the quotation).
 *
 * @see KQMLPerformative
 * @see KQMLReader
 */
public class KQMLQuotation extends KQMLObject {
    // Slots
    private char type;
    private KQMLObject object;
    // Constructors
    /**
     * Returns a new KQMLQuotation consisting of the given elemnts.
     *
     * @param    t  Type of quotation (quote, backquote, or comma)
     * @param    obj  KQMLObject being quoted
     */
    public KQMLQuotation(char t, KQMLObject obj) {
	type = t;
	object = obj;
    }
    // Methods
    /**
     * Returns the type of the quotation.
     *
     * @return    Type of quotation (quote, backquote, or comma)
     */
    public char getType() {
	return type;
    }
    /**
     * Returns the object being quoted.
     *
     * @return   KQMLObject being quoted
     */
    public KQMLObject getObject() {
	return object;
    }
    /**
     * Write this KQMLQuotation to given Writer in KQML syntax.
     */
    public void write(Writer out) throws IOException {
	out.write(String.valueOf(type));
	object.write(out);
    }
    /**
     * Returns a KQMLQuotation as a String in KQML syntax.
     *
     * @return   String denoting KQMLQuotation
     */
    public String toString() {
	StringWriter out = new StringWriter();
	try {
	    write(out);
	} catch (IOException ex) {
	}
	return out.toString();
    }
}
