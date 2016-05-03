/*
 * KQMLToken.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 25 Apr 2000
 * Time-stamp: <Fri Jun  1 13:42:41 EDT 2007 ferguson>
 *
 * A KQMLToken is simply a string (although not a KQMLString). It prints
 * itself without any funny business.
 */

package TRIPS.KQML;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.*;

/**
 * A class representing KQML tokens. These are just strings and print
 * themselves literally.
 *
 * @see KQMLPerformative
 * @see KQMLReader
 */
public class KQMLToken extends KQMLObject {
    /**
     * the NIL token -- used for comparisons (e.g., NIL.equalsIgnoreCase(otherToken)
     */
    public static final KQMLToken NIL = new KQMLToken("NIL");

    // Slots
    private String data;
    // Contructor
    /**
     * Creates a new empty KQMLToken.
     *
     */
    public KQMLToken() {
	data = new String();
    }
    /**
     * Creates a new KQMLToken with the given contents.
     *
     * @param  s  Contents of string.
     */
    public KQMLToken(String s) {
	data = s;
    }
    // Methods
    /**
     * Returns the number of characters in a KQMLToken.
     *
     * @return    Length of KQMLToken
     */
    public int length() {
	return data.length();
    }
    /**
     * Returns the character at a given index in a KQMLToken.
     *
     * @param  n  Index of character
     * @return    Character at that index
     */
    public int charAt(int n) {
	return data.charAt(n);
    }
    /**
     * Compares this token to the specified object.
     *
     * @param  obj  the Object against which to compare.
     * @return    true if the Strings are equal; false otherwise.
     * @see	java.lang.String.equals()
     */
    @Override
    public boolean equals(Object obj) {
	// Might want equalsIgnoreCase here (or do it yourself)
	if (!(obj instanceof KQMLToken))
	    return false;
	return data.equals(((KQMLToken)obj).data);
    }
    /**
     * Compares this token to the specified String ignoring case.
     *
     * @param  str  the Object against which to compare.
     * @return    true if the Strings are equal; false otherwise.
     * @see	java.lang.String.equalsIgnoreCase()
     */
    public boolean equalsIgnoreCase(String str) {
	return data.equalsIgnoreCase(str);
    }

    /**
     * Compares these tokens ignoring their case
     */
    public boolean equalsIgnoreCase(KQMLToken token) {
	return equalsIgnoreCase(token.toString());
    }

    /**
     * Writes this KQMLToken to the given Writer.
     */
    public void write(Writer out) throws IOException {
	out.write(data);
    }
    /**
     * Returns a KQMLToken as a String.
     *
     * @return  String denoting KQMLToken
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
     * Returns the String content of a KQMLToken (same as toString()).
     *
     * @return  String contents of KQMLToken
     */
    public String stringValue() {
	return data;
    }

    /**
     * returns the name of the (LISP) package of this token, if it were taken
     * as a lisp symbol.  If there is no package reference (i.e., no colons)
     * returns null.  If the token is in the KEYWORD package (starts with a :)
     * returns "KEYWORD".
     */
    public String getPackage() {
	String[] results = parsePackage();
	return results[0];
    }

    /**
     * if this token has a package part (including KEYWORD), returns true
     */
    public boolean hasPackage() {
	String pkg = getPackage();
	return (pkg != null);
    }

    /**
     * returns the (bare) name of the token with its package reference stripped off
     * (considering it as if it were a lisp symbol)
     */
    public String getName() {
	String[] results = parsePackage();
	return results[1];
    }

    /**
     * the string we return when something is in the keyword package (see isKeyword())
     */
    public static final String KEYWORD_PACKAGE_NAME = "KEYWORD";

    /**
     * returns true if this token is in the KEYWORD package (taken as if it were a lisp symbol)
     */
    public boolean isKeyword() {
	String packageName = getPackage();
	return KEYWORD_PACKAGE_NAME.equalsIgnoreCase(packageName);
    }

    /**
     * parses the string for this token into its package and name
     * used by getPackage() and getName()
     * the package part is returned as described in getPackage()
     */
    protected String[] parsePackage() {
	String[] retval = new String[2];
	String packageName = null;
	String bareName = "";

	boolean quoted = false;
	String[] chars = data.split("");
	ListIterator<String> iter = Arrays.asList(chars).listIterator();
	while (iter.hasNext()) {
	    String thisChar = iter.next();
	    if (thisChar.equals("|")) {
		quoted = !quoted;
		bareName += thisChar;
	    } else if (!quoted && (thisChar.equals(":"))) { // has a package
		if (bareName.equals("")) { // nothing there yet, keyword
		    packageName = KEYWORD_PACKAGE_NAME;
		} else {
		    packageName = bareName;
		    bareName = "";
		}
		if (!iter.hasNext())
		    throw new RuntimeException("token ends in a colon: " + data);
		thisChar = iter.next();
		if (thisChar.equals(":")) { // has a second colon
		    if (!iter.hasNext())
			throw new RuntimeException("token ends in a colon: " + data);
		} else {
		    iter.previous(); // backtrack
		}
	    } else {
		bareName += thisChar;
	    }

	} // end for each char
	if (quoted)
	    throw new RuntimeException("no closing bar: " + data);

	retval[0] = packageName;
	retval[1] = bareName;
	return retval;
    } // end parsePackage

}
