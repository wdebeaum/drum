package TRIPS.OWL;

import java.util.*;

/**
 * this class can be used to time events in java
 * just use the start method to start the timer and the stop method to stop it
 */
public class Stopwatch {

    /**
     * the time recorded when start is called
     */
    protected Date startTime;
    public Date getStartTime() {return startTime;}

    /**
     * the time recorded when stop is called
     */
    protected Date stopTime;
    public Date getStopTime() {return stopTime;}

    /**
     * starts the watch by recording a new start time and clearing the stop time
     * returns the Date we get the start at
     */
    public Date start() {
	startTime = new Date();
	stopTime = null; // reset this
	return startTime;
    } // end start

    /**
     * stops the watch by recording the stop time.  Returns the Date when we stopped
     * note that this should be usable multiple times in a row without resetting the startTime
     */
    public Date stop() {
	stopTime = new Date();
	return stopTime;
    } // end stop

    /**
     * returns the amount of time between the start and stop time
     * throws an exception if start or stop time hasn't been defined
     */
    public Duration getDuration() {
	if (!hasDuration()) {
	    throw new RuntimeException("start or stop time hasn't been defined yet: " + this);
	}

	return new Duration(startTime, stopTime);
    } // end getDuration

    /**
     * returns true if this has a duration, i.e., if start time and stopTime are both defined
     */
    public boolean hasDuration() {
	return ((startTime != null) && (stopTime != null));
    } // end hasDuration

    /**
     * prints out start and stop time if they're defined
     */
    public String toString() {
	String retval = "";

	retval += "start time: " + startTime + " stop time: " + stopTime + " duration: ";
	if (hasDuration()) {
	    retval += getDuration();
	} else { // doesn't have a duration defined
	    retval += "undefined";
	}

	return retval;
    } // end toString

    /**
     * for unit testing
     */
    public static void main(String[] args) {
	Stopwatch s = new Stopwatch();
	s.start();
	s.stop();
	System.out.println(s);
	
	Stopwatch t = new Stopwatch();
	t.start();
	t.stop();
	s.stop();
	System.out.println(s);
	System.out.println(t);

    } // end main
    

} // end class Stopwatch