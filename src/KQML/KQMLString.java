/*
 * KQMLString.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 17 Feb 1998
 * Time-stamp: <Sat Jun 13 13:03:07 CDT 2015 lgalescu>
 *
 * KQMLString is a normal string except that it prints itself surrounded
 * by double-quotes, with internal double-quotes escaped.
 *
 * 3/9/2000: gf: Make sure backslashes get escaped in toString().
 *
 */

package TRIPS.KQML;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

/**
 * A class representing KQML strings. These are just regular strings
 * that print themselves using KQML syntax.
 *
 * @see KQMLPerformative
 * @see KQMLReader
 */
public class KQMLString extends KQMLObject {
    // Slots
    private String data;
    // Contructor
    /**
     * Creates a new empty KQMLString.
     *
     */
    public KQMLString() {
	data = new String();
    }
    /**
     * Creates a new KQMLString with the given contents.
     *
     * @param  s  Contents of string.
     */
    public KQMLString(String s) {
	data = s;
    }
    // Methods
    /**
     * Returns the number of characters in a KQMLString.
     *
     * @return    Length of KQMLString
     */
    public int length() {
	return data.length();
    }
    /**
     * Returns the character at a given index in a KQMLString.
     *
     * @param  n  Index of character
     * @return    Character at that index
     */
    public int charAt(int n) {
	return data.charAt(n);
    }
    /**
     * Compares this string to the specified object.
     *
     * @param  obj  the Object against which to compare.
     * @return    true if the Strings are equal; false otherwise.
     * @see     java.lang.String.equals()
     */
    @Override
    public boolean equals(Object obj) {
        // Might want equalsIgnoreCase here (or do it yourself)
        if (!(obj instanceof KQMLString))
            return false;
        return data.equals(((KQMLString)obj).data);
    }
    /**
     * Write this KQMLString to given Writer in KQML syntax.
     */
    public void write(Writer out) throws IOException {
	out.write("\"");
	int len = data.length();
	int i;
	for (i=0; i < len; i++) {
	    char ch = data.charAt(i);
	    if (ch == '\"' || ch == '\\') {
		out.write("\\");
	    }
	    out.write(ch);
	}
	out.write("\"");
    }
    /**
     * Returns a KQMLString as a String in KQML syntax.
     *
     * @return  String denoting KQMLString
     */
    public String toString() {
	StringWriter out = new StringWriter();
	try {
	    write(out);
	} catch (IOException ex) {
	}
	return out.toString();
    }
    /**
     * Returns the String content of a KQMLString (no extra quotes).
     *
     * @return  String contents of KQMLString
     */
    public String stringValue() {
	return data;
    }
}
