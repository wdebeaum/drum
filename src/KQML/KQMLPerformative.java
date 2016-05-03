/*
 * KQMLPerformative.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 17 Feb 1998
 * Time-stamp: <Tue Mar  4 17:56:04 CST 2008 lgalescu>
 *
 * A KQMLPerformative is really just a list with accessors for getting
 * the verb and parameters of the performative.
 */
package TRIPS.KQML;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;


/**
 * A class representing KQML performatives. This is really just a Vector
 * with methods for getting at the verb and parameters of the performative.
 *
 * @see KQMLReader
 */
public class KQMLPerformative extends KQMLObject {
    // Slots
    private KQMLList data;
    // Constructors
    /**
     * Creates a new performative with the given verb (and no parameters).
     *
     * @param	str	The verb of the performative
     */
    public KQMLPerformative(String verb) {
	data = new KQMLList();
	data.add(new KQMLToken(verb));
    }
    /**
     * Creates a new performative from the given list. Note that this
     * constructor checks that the elements of the
     * list are in fact a verb followed by keyword/value pairs.
     *
     * @param	list	KQMLList containing elements of the performative.
     */
    public KQMLPerformative(KQMLList list) throws KQMLBadPerformativeException {
	// Sanity checks
	int len = list.length();
	if (len == 0) {
	    throw new KQMLBadPerformativeException("list has no elements");
	} else if (!(list.nth(0) instanceof KQMLToken)) {
	    throw new KQMLBadPerformativeException("list doesn't start with KQMLToken: " + list.nth(0));
	} else {
	    for (int i=1; i < len; i++) {
		if (!(list.nth(i) instanceof KQMLToken) ||
		    ((KQMLToken)(list.nth(i))).charAt(0) != ':') {
		    throw new KQMLBadPerformativeException("performative element not a keyword: " + list.nth(i));
		}
		i += 1;
		if (i == len) {
		    throw new KQMLBadPerformativeException("missing value for keyword: " + list.nth(i-1));
		}
	    }
	}
	// Ok, made it this far, save the list
	data = list;
    }
    // Methods
    /**
     * Returns the verb of the performative as a String.
     *
     * @return    Verb of performative
     */
    public String getVerb() {
	return data.nth(0).toString();
    }
    /**
     * Returns the requested parameter of the performative. The case of
     * the given keyword is ignored.
     *
     * @param     keyword Name of parameter (including colon)
     * @return    Value of parameter (KQMLToken, KQMLString, KQMLQuotation,
     *            or KQMLList)
     * @see       java.lang.String
     * @see       KQMLString
     * @see       KQMLQuotation
     * @see       KQMLList
     */
    public KQMLObject getParameter(String keyword) {
	int i;
	for (i=1; i < data.length()-1; i++) {
	    String key = data.nth(i).toString();
	    if (key.equalsIgnoreCase(keyword)) {
		// This should be a safe cast...
		return (KQMLObject)data.nth(i+1);
	    }
	}
	return null;
    }
    /**
     * Sets the given parameter of the performative.
     *
     * @param     keyword Name of parameter (including colon)
     * @param     value Value of parameter (KQMLToken, KQMLString, KQMLQuotation,
     *            or KQMLList)
     * @see       java.lang.String
     * @see       KQMLString
     * @see       KQMLQuotation
     * @see       KQMLList
     */
    public void setParameter(String keyword, KQMLObject value) {
	boolean found = false;
	int i;
	for (i=1; !found && i < data.length()-1; i++) {
	    String key = data.nth(i).toString();
	    if (key.equalsIgnoreCase(keyword)) {
		data.removeAt(i+1);
		data.insertAt(value, i+1);
		found = true;
	    }
	}
	if (!found) {
	    data.add(keyword);
	    data.add(value);
	}
    }
    /**
     * Sets the given parameter of the performative. The string parameter
     * will be converted to a KQMLToken.
     *
     * @param     keyword Name of parameter (including colon)
     * @param     str Value of parameter
     */
    public void setParameter(String keyword, String value) {
	setParameter(keyword, new KQMLToken(value));
    }
    /**
     * Removes the requested parameter of the performative and the associated 
     * value. 
     *
     * @param     keyword Name of parameter (including colon)
     * @see       java.lang.String
     * @see       KQMLString
     * @see       KQMLQuotation
     * @see       KQMLList
     */
    public void removeParameter(String keyword) {
	int i;
	for (i=1; i < data.length()-1; i++) {
	    String key = data.nth(i).toString();
	    if (key.equalsIgnoreCase(keyword)) {
		data.removeAt(i);
		data.removeAt(i);
	    }
	}
	return;
    }
    /**
     * Returns the performative as a KQMLList.
     * Note that you should copy this list before modifying it.
     *
     * @return    KQMLList with same contents as performative
     */
    public KQMLList toList() {
	return data;
    }
    /**
     * Write this KQMLPerformative to the given Writer.
     */
    public void write(Writer out) throws IOException {
	data.write(out);
    }
    /**
     * Returns the performative as a String.
     *
     * @return    String suitable for printing as KQML
     */
    public String toString() {
	return data.toString();
    }
    /**
     * Reads a KQMLPerforative from a string.
     *
     * @return    KQMLPerformative described by string
     * @exception KQMLException If the input is not proper KQML.
     * @exception java.io.EOFException If end-of-string is reached.
     * @exception java.io.IOException For any other I/O error.
     */
    public static KQMLPerformative fromString(String s) throws IOException {
	StringReader sreader = new StringReader(s);
	KQMLReader kreader = new KQMLReader(sreader);
	return new KQMLPerformative(kreader.readList());
    }
}
