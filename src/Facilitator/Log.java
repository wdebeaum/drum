/*
 * Log.java
 *
 * Dave Costello  , costello@cs.rochester.edu, Jun 1998
 * $Id: Log.java,v 1.4 2007/07/13 20:06:24 wdebeaum Exp $
 *
 */

package TRIPS.Facilitator;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.Date;
import TRIPS.KQML.KQMLPerformative;

public class Log {
    //
    // Constants
    //
    public static final String DEFAULT_FILENAME = "facilitator.log";    
    //
    // Fields
    //
    protected PrintWriter out; 
    protected TimeStamper timestamper;
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
	out = new PrintWriter(new FileOutputStream(pathname));
	timestamper = new TimeStamper();
	// Start log file with LOG entry
	DateFormat dateFmt = DateFormat.getDateInstance(DateFormat.SHORT);
        DateFormat timeFmt = DateFormat.getTimeInstance(DateFormat.SHORT);
	Date now = new Date();
	out.println("<LOG " +
		       "TIME=\"" + timeFmt.format(now) + "\" " +
		       "DATE=\"" + dateFmt.format(now) + "\" " +
		       "FILE=\"" + pathname + "\">\n");
    }
    //
    // Methods
    //
    protected synchronized void logEntry(String tag, String attr, String name, KQMLPerformative msg) {
	out.write("<");
	out.write(tag);
	out.write(" T=\"");
	out.write(timestamper.currentTimeStamp());
	out.write("\" ");
	out.write(attr);
	out.write("=\"");
	out.write(name);
	out.write("\">\n");
	try {
	    msg.write(out);
	} catch (IOException ex) {
	}
	out.write("\n</");
	out.write(tag);
	out.write(">\n");
	// Flush output (could be configurable)
	out.flush();
    }
    public void logSend(String toWhom, KQMLPerformative msg) {
	logEntry("S", "R", toWhom, msg);
    }
    public void logReceive(String fromWhom, KQMLPerformative msg) {
	logEntry("R", "S", fromWhom, msg);
    }
    public void logError(String fromWhom, String msg) {
 	out.println("<ERROR T=\"" + timestamper.currentTimeStamp() + "\" " +
 		       "S=\"" + fromWhom + "\">\n" +
 		       msg + "\n" +
 		       "</ERROR>\n");
    }
    public void logExit(String fromWhom) {
 	out.println("<EXIT T=\"" + timestamper.currentTimeStamp() + "\" " +
 		       "S=\"" + fromWhom + "\" />\n");
    }
    public void close() {
	out.println("</LOG>");
	out.close();
    }
}

class TimeStamper {
    //
    // Fields
    //
    private long startTime;
    //
    // Constructors
    //
    public TimeStamper() {
	startTime = new Date().getTime();
    }
    //
    // Methods
    //
    public String currentTimeStamp() {
	long now = new Date().getTime();
	long elapsed = now - startTime;
	int hrs = (int)((double)elapsed / 3600000);
	int mins = (int)(((double)elapsed % 3600000)/60000);
	int secs = (int)((double)elapsed % 60000)/1000;
	int hundredths = (int)((double)elapsed % 1000)/10;  	
 	return new String(padNum(hrs) + ":" +
			  padNum(mins) + ":" +
			  padNum(secs) + "." +
			  padNum(hundredths));
    }
    private String padNum(int i) {
	if (i < 10) {
	    return "0" + new Integer(i).toString();
	} else {
	    return new Integer(i).toString();
	}
    }
}
