package TRIPS.util;

import java.io.*;
import java.util.*;

/**
 * a set of miscellaneous methods that don't fit anywhere else
 */
public class Misc {

    /**
     * this takes a Throwable (e.g., an Exception or Error) and returns as
     * a String what it would have printed to System.err with throwable.printStackTrace()
     * note that - this gives you more than just turning the results of getStackTrace
     * into a string, since it recursively adds on causes
     * (I wish they'd add this as a method to Throwable!)
     * author: blaylock July 2007
     */
    public static String getStackTraceString(Throwable throwable) {
	StringWriter sw = new StringWriter();
	PrintWriter pw = new PrintWriter(sw);
	throwable.printStackTrace(pw);
	return sw.toString();
    } // end getStackTraceString

    /**
     * returns a list of the chained causes from this throwable
     * optionally allows this throwable itself to be included (at the front)
     */
    public static ArrayList<Throwable> getExceptionChain(Throwable throwable, 
						    boolean includeThisThrowable) {
	ArrayList<Throwable> retval;

	Throwable cause = throwable.getCause();
	if (cause != null) { // has a cause
	    retval = getExceptionChain(cause,true);
	} else { // no cause
	    retval = new ArrayList<Throwable>();
	}

	if (includeThisThrowable) { 
	    retval.add(0,throwable); // add this on front
	}

	return retval;
    } // end getExceptionChain

    /**
     * calls getExceptionChain(true) which includes this throwable at the start of the chain
     */
    public static ArrayList<Throwable> getExceptionChain(Throwable throwable) {
	return getExceptionChain(throwable,true);
    }

}
