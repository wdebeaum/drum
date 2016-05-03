/*
 * @(#)TRIPSLog.java
 *
 * Dave Costello  , costello@cs.rochester.edu, Jun 1998
 * Time-stamp: <2000-11-17 14:11:56 costello>
 *
 * Writes text output to a log file.
 * design allows for many objects to write
 * to the same static File.
 */

package TRIPS.util;

import java.io.*;
import TRIPS.KQML.*;


public class TRIPSLog {

    //class variables
    protected static PrintWriter log; 
    protected static String      logpath;
    //default logname
    public static final String   DEFAULTLOG = "tripsclient.log";    

    //instance attributes
    private static TimeStamp timestamp;         //log time/date object

    //constructors
    public TRIPSLog(String filepath) throws IOException {
	logpath = filepath;
	createLog(logpath);
    }
    public TRIPSLog() throws IOException {
	logpath = DEFAULTLOG;
	createLog(logpath);
    }

    //public interface

    synchronized public void writeLog(String msg) {
	toFile(msg);
    }
    synchronized public void writeLog(KQMLPerformative msg) {
	toFile(msg.toString());
    }
    synchronized public void writeLogReceive(String fromWhom, String msg) {
	timestamp.updateTime();
        String tstamp = timestamp.timeStampString(); 
	// gf: 3/10/2000: New format
	//String rMsg = new String("<"+fromWhom+" "+tstamp+" "+msg);
	//toFile(rMsg);
	toFile("(RECEIVE\n" +
	       "  :time \"" + tstamp + "\"\n" +
	       "  :sender " + fromWhom + "\n" +
	       "  :content " + msg + ")");
    }
    synchronized public void writeLogSend(String toWhom, String msg) {
	timestamp.updateTime();
        String tstamp = timestamp.timeStampString(); 
	// gf: 3/10/2000: New format
	//String sMsg = new String(">"+toWhom+" "+tstamp+" "+msg);
	//toFile(sMsg);
	toFile("(SEND\n" +
	       "  :time \"" + tstamp + "\"\n" +
	       "  :receiver " + toWhom + "\n" +
	       "  :content " + msg + ")");
    }
    synchronized public void writeLogConnect(String fromWhom) {
	timestamp.updateTime();
        String tstamp = timestamp.timeStampString(); 
	toFile("(CONNECT\n" +
	       "  :time \"" + tstamp + "\"\n" +
	       "  :sender " + fromWhom + ")");
    }
    synchronized public void writeLogDisconnect(String fromWhom) {
	timestamp.updateTime();
        String tstamp = timestamp.timeStampString(); 
	toFile("(DISCONNECT\n" +
	       "  :time \"" + tstamp + "\"\n" +
	       "  :sender " + fromWhom + ")");
    }

    synchronized public void restartLog(String path)  throws IOException {
	close();
	logpath = path;
	createLog(logpath);
    }

    public void close() {
	closeLog();
    }

    //private static methods

    /**
     * static file access is private.
     * protects the file from static access outside of an instance object.
     * You must have a reference to an Log instance and use
     * its public interface to write to the file and thus
     * Log.writeLog is not possible so encapsulation is not broken
     */
    synchronized private static void toFile(String s) {
	log.println(s);
    }

    synchronized private static void createLog(String path) {
	try {
	    //autoflushed
	    log     = new PrintWriter(new FileWriter(path),true);
	    timestamp = new TimeStamp();
	    log.println("(START\n" +
			"  :time \"" + timestamp.timeStampString() + "\"\n" +
			"  :file \"" + path + "\")");
	    DEBUG("Log: started at "+logpath);
	}catch (IOException e ){
	    DEBUG(e.toString());
	}
    }

    private static void closeLog() {
	logpath = null;
	log.close();
	DEBUG("log closed");
    }

    private static void DEBUG(String s){
	//System.err.println(s);
    }

    //for testing
    public static void main(String[] argv){

	try{
	    TRIPSLog l = new TRIPSLog("test.log");
	}catch (Exception e){}

    }
}

