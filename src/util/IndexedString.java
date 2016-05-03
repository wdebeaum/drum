/*
 * IndexedString.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 11 Mar 1998
 * Time-stamp: <Tue Feb 16 17:36:56 EST 1999 ferguson>
 */

package TRIPS.util;

/**
 * A class for scanning through a string using an index.
 */
public class IndexedString {
    // Members
    private String str;
    private int length;
    private int index;
    // Constructor
    /**
     * Creates a new indexed string with the given contents and the index
     * at position 0.
     *
     * @param	s	String contents
     */
    IndexedString(String s) {
	str = s;
	length = s.length();
	index = 0;
    }
    // Methods
    /**
     * Increments an indexed string's index until it points to the
     * first non-whitespace character.
     *
     * @see java.lang.Character.isWhitespace
     */
    public void skipWhitespace() {
	while (index < length && Character.isWhitespace(str.charAt(index))) {
	    index += 1;
	}
    }
    /**
     * Returns the character at the current index of the indexed string
     * and increments the index. Returns '\0' at the end of the string.
     *
     * @return Current character in indexed string
     */
    public char nextChar() {
	return (index < length) ? str.charAt(index++) : '\0';
    }
    /**
     * Returns the character at the current index of the indexed string
     * but does not increment the index. Returns '\0' at the end of the string.
     *
     * @return Current character in indexed string
     */
    public char peekChar() {
	return (index < length) ? str.charAt(index) : '\0';
    }
    /**
     * Reads digits from the indexed string and returns the number they
     * represent. The index of teh indexed string is left pointing at the
     * next character (or the end of the string).
     *
     * @return Integer value of leading digits in indexed string
     */
    public int nextInt() {
	int n = 0;
	while (index < length) {
	    char ch = str.charAt(index);
	    if (Character.isDigit(ch)) {
		n = n * 10 + Character.digit(ch, 10);
		index += 1;
	    } else {
		break;
	    }
	}
	return n;
    }
    /**
     * Tests if the index of an indexed string is at the end of the string.
     *
     * @return True if at end of string, otherwise false.
     */
    public boolean atEnd() {
	return (index == length);
    }
}

