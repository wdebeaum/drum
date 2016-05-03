/*
 * TimeStamp.java
 *
 * Dave Costello  , costello@cs.rochester.edu, Jun 1998
 * Time-stamp: <2000-11-17 13:26:40 costello>
 *
 *
 */

package TRIPS.util;

import java.io.*;
import java.util.Date;
import java.text.DateFormat;

/**
 * When constructed holds a start time and contains methods
 * for returning time elapsed in hours, minutes, and seconds.
 * Encapsulates time functions for timestamping log files. 
 */
public class TimeStamp {

    //instance attributes
    private Date     start_timestamp;         //log start time/date
    private long     starttime_ms;            //connect time in msec
    private long     timenow_ms;              //curent time  in msec
    private long     elapsed_ms;              //msec elapsed since start
    private int      hrs_elapsed,mins_elapsed,secs_elapsed; //elapsed time
    private int      tenth_secs,hundredth_secs;

    //constructors
    TimeStamp(Date d) {
	start_timestamp = d;
	starttime_ms    = start_timestamp.getTime();
	timenow_ms      = starttime_ms;
	elapsed_ms      = 0;
	hrs_elapsed     = 0;
	mins_elapsed    = 0;
        secs_elapsed    = 0;
	tenth_secs      = 0;
	hundredth_secs  = 0;  
    }
    TimeStamp() {
	start_timestamp = new Date();
	starttime_ms    = start_timestamp.getTime();
	timenow_ms      = starttime_ms;
	elapsed_ms      = 0;
	hrs_elapsed     = 0;
	mins_elapsed    = 0;
        secs_elapsed    = 0;
	tenth_secs      = 0;
	hundredth_secs  = 0;  
    }

    //public instance methods

    public void updateTime() {
	timenow_ms = new Date().getTime();
	elapsed_ms = timenow_ms - starttime_ms;

	hrs_elapsed     = (int)((double)elapsed_ms / 3600000);
	mins_elapsed    = (int)(((double)elapsed_ms % 3600000)/60000);
	secs_elapsed    = (int)((double)elapsed_ms % 60000)/1000;
	//tenth_secs      = (int)((double)elapsed_ms % 1000)/100;
	hundredth_secs  = (int)((double)elapsed_ms % 1000)/10;  	
    }

    public int hoursElapsed() {
	return hrs_elapsed;
    }
    public int minsElapsed() {
	return mins_elapsed;
    }
    public int secsElapsed() {
	return secs_elapsed;
    }

    public String timeStartString() {
	return DateFormat.getDateTimeInstance().format(start_timestamp);
    }

    public String timeStampString(){
	String ts = new String(padNum(hrs_elapsed)+":"+
			       padNum(mins_elapsed)+":"+
			       padNum(secs_elapsed)+"."+
			       padNum(hundredth_secs));
	return ts;
    }

    //crude formatting adds a leading zero if single digit	
    private String padNum(int i){
	String stringInt;
	if (i < 10) {
	    stringInt = new String("0"+new Integer(i).toString());
	    return stringInt;
	}else {
	    stringInt = new Integer(i).toString();
	    return stringInt;
	}
    }

    
}
