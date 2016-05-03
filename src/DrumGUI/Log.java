/*
 * Log.java
 *
 * Lucian Galescu <lgalescu@ihmc.us> 5 May 2006
 * Dave Costello, costello@cs.rochester.edu, Jun 1998
 * $Id: Log.java,v 1.3 2016/02/19 00:11:44 lgalescu Exp $
 *
 */

package TRIPS.DrumGUI;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class Log {
    //
    // Constants
    //
    public static final String DEFAULT_FILENAME = "DrumGUI.log";    
    //
    // Fields
    //
    protected PrintWriter writer; 
    //
    // Constructors
    //
    public Log() throws IOException {
	this(DEFAULT_FILENAME);
    }
    public Log(String pathname) throws IOException {
	File f = new File(pathname);
	if (f.exists() && f.isDirectory()) {
	    pathname = pathname + File.separator + DEFAULT_FILENAME;
	}
	writer = new PrintWriter(new FileWriter(pathname), true);
	// Start log file with LOG entry
        DateFormat dateFmt = DateFormat.getDateInstance(DateFormat.SHORT);
	DateFormat timeFmt = DateFormat.getTimeInstance(DateFormat.SHORT);
	Date now = new Date();
	writer.println("<log " +
		       "date=\"" + dateFmt.format(now) + "\" " +
		       "time=\"" + timeFmt.format(now) + "\" " +
		       "file=\"" + pathname + "\">\n");
    }
    //
    // Methods
    //
    /** Write string to log in XML format.
     */
    public void log(String tag, String text) {
	TimeStamp timeStamp = new TimeStamp();
	writer.println("<" + tag + " t=\"" + timeStamp.toString() + "\">\n" 
		       + text 
		       + "\n</" + tag + ">\n");
    }
    /** Write string to log in XML format; use generic <item> tag.
     */
    public void log(String text) {
	log("item", text);
    }
    public void close() {
	writer.println("</log>");
	writer.close();
    }

    /**
     * TimeStamp
     */
    static class TimeStamp {

	private Date now;
	private SimpleDateFormat format = new SimpleDateFormat("HH:mm:ss.SSS");

	public TimeStamp() {
	    now = new Date();
	}

	/**
	 * Returns time stamp as String
	 */
	public String toString() {
	    return format.format(now);
	}
    }

}
