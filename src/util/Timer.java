/*
 * Timer.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 20 Apr 2000
 * Time-stamp: <Thu Apr 20 12:08:45 EDT 2000 ferguson>
 */

package TRIPS.util;

/**
 * One-shot timer class.
 * To extend to a repeating timer, put a ``while (running)'' loop around
 * the code in run(), and add a ``stopSafely()'' method that sets ``running''
 * to false. Could do this by extending this class.
 */
public class Timer extends Thread {
    //
    // Fields
    //
    protected int interval;
    protected Timed target;
    //
    // Constructor
    //
    public Timer(int interval, Timed target) {
	this.interval = interval;
	this.target = target;
	setDaemon(true);
    }
    //
    // Thread method
    //
    public void run() {
	try {
	    sleep(interval);
	} catch(InterruptedException e) {
	}
	target.tick(this);
    }
}
