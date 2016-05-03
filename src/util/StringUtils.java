/*
 * StringUtils.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 11 Mar 1998
 * Time-stamp: <Sat Apr  1 16:44:24 EST 2000 ferguson>
 */

package TRIPS.util;

import java.io.StringWriter;
import java.io.PrintWriter;
import java.util.Iterator;

public class StringUtils {
    public static int atoi(String s) {
	int i = 0;
	int n = 0;
	// Skip whitespace
	while (i < s.length() && Character.isWhitespace(s.charAt(i))) {
	    i += 1;
	}
	// Possible leading sign
	int polarity = 1;
	if (s.charAt(i) == '-') {
	    polarity = -1;
	    i += 1;
	} else if (s.charAt(i) == '+') {
	    polarity = 1;
	    i += 1;
	}
	// Then scan digits
	while (i < s.length()) {
	    char ch = s.charAt(i);
	    if (Character.isDigit(ch)) {
		n = n * 10 + Character.digit(ch, 10);
		i += 1;
	    } else {
		break;
	    }
	}
	// Return integer value
	return polarity * n;
    }    
    public static String itoa(int i, int n) {
	StringBuffer buf = new StringBuffer();
	while (i > 0 || n > 0) {
	    int digit = i % 10;
	    buf.append(digit);
	    i /= 10;
	    n -= 1;
	}
	return buf.reverse().toString();
    }
    public static boolean stringToBoolean(String s) {
	return (s.equalsIgnoreCase("true") ||
		s.equalsIgnoreCase("t") ||
		s.equalsIgnoreCase("yes") ||
		s.equalsIgnoreCase("1"));
    }
    public static String paddedInt(int n, int len) {
	char[] buf = new char[len];
	for (int i=0; i < len; i++) {
	    buf[len-i-1] = (char)('0' + (n % 10));
	    n = n / 10;
	}
	return new String(buf);
    }

    /**
     * a perl-like join function: concats all elements of the list together with 
     * the 'middle' string inbetween each one
     */
    public static String join(String middle, Iterable<?> lst) {
	String retval = "";

	Iterator<?> iter = lst.iterator();
	while (iter.hasNext()) {
	    retval += iter.next().toString();

	    if (iter.hasNext())
		retval += middle;
	} // end for each obj

	return retval;
    } // end join
    
}
