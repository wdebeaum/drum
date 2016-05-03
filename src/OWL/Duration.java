package TRIPS.OWL;

import java.util.*;
import java.io.*;
import java.text.SimpleDateFormat;

/**
 * encapsulates a duration of time in milliseconds measured with a long
 */
public class Duration implements Serializable {
    protected static final long serialVersionUID = 2;

    /**
     * used for pretty printing the duration
     */
    private static SimpleDateFormat dateFormat = new SimpleDateFormat("HH:mm:ss.SSS");
    {dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));} // fool format to do for time passed

    /**
     * the duration in milliseconds
     */
    protected long milliseconds;
    public long getMilliseconds() {return milliseconds;}

    /**
     * create a duration of 0
     */
    public Duration() {
	milliseconds = 0;
    }

    /**
     * create between two Dates
     */
    public Duration(Date time1, Date time2) {
	if (time1.after(time2)) { // wrong order
	    throw new RuntimeException("time1 is after time2");
	}
	milliseconds = time2.getTime() - time1.getTime();
    } // end constructor


    /**
     * creates a new duration which is the sum of this and the arg
     */
    public Duration plus(Duration dur2) {
	return new Duration(getMilliseconds() + dur2.getMilliseconds());
    }

    /**
     * divides the duration by the given double, and returns the result as a duration
     */
    public Duration dividedBy(double divisor) {
	return new Duration((long) (getMilliseconds() / divisor));
    }

    /**
     * create from a long
     */
    public Duration(long milliseconds) {
	this.milliseconds = milliseconds;
    } // end constructor

    /**
     * creates a new duration from the given amount of seconds
     */
    public static Duration fromSeconds(double seconds) {
	return new Duration(sec2ms(seconds));
    }

    /**
     * returns time as seconds (not rounded)
     */
    public double toSeconds() {
	long ms = getMilliseconds();
	return ms2sec(ms);
    } // end toSeconds

    /**
     * returns the time as minutes (not rounded)
     */
    public double toMinutes() {
	double secs = toSeconds();
	return sec2min(secs);
    }

    /**
     * returns the time as hours (not rounded)
     */
    public double toHours() {
	double secs = toSeconds();
	return sec2hour(secs);
    }

    /**
     * converts seconds into hours
     */
    public static double sec2hour(double seconds) {
	double minutes = sec2min(seconds);
	return minutes / 60;
    }

    /**
     * converts seconds into minutes
     */
    public static double sec2min(double seconds) {
	return seconds / 60;
    }

    /**
     * converts seconds as a double to ms as a long
     */
    public static long sec2ms(double seconds) {
	return (long) seconds * 1000;
    }

    /**
     * converts a ms a long to a seconds as double
     */
    public static double ms2sec(long ms) {
	return ((double) ms) / 1000.0;
    }

    /**
     * pretty prints to a string like HH:mm:ss.SSS (doesn't do days, months, etc. yet)
     */
    public String toString() {
	return dateFormat.format(new Date(milliseconds));
    }


} // end class Duration