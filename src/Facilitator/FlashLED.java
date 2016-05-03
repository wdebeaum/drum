/*
 * FlashLED.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 20 Apr 2000
 * $Id: FlashLED.java,v 1.1.1.1 2005/01/14 19:48:08 ferguson Exp $
 */

package TRIPS.Facilitator;

import TRIPS.util.Timed;
import TRIPS.util.Timer;

/**
 * Implements a flashable LED that stays on as long as someone keeps
 * flashing it.
 */
public class FlashLED extends LED implements Timed {
    //
    // Constants
    //
    protected static final int FLASH_INTERVAL = 500;
    //
    // Fields
    //
    int count;
    //
    // Constructor
    //
    public FlashLED() {
	super();
    }
    //
    // Method
    //
    public synchronized void flash(int color) {
	//System.err.println("FlashLED.flash: count=" + count);
	// Increment count for this LED
	count += 1;
	// If we're the first to turn it on...
	if (count == 1) {
	    // ...Then actually change the image (else just leave it on)
	    setColor(color);
	    setState(true);
	}
	// In any event, check back later to (possibly) turn off
	new Timer(FLASH_INTERVAL, this).start();
	//System.err.println("FlashLED.flash: done");
    }
    //
    // Timed method
    //
    public synchronized void tick(Timer t) {
	//System.err.println("FlashLED.tick: count=" + count);
	// Decrement count for this LED
	count -= 1;
	// Sanity check
	if (count < 0) {
	    count = 0;
	}
	// If we're the last one to turn it off...
	if (count == 0) {
	    // ...Then actually chage the image (else leave it on)
	    setState(false);
	}
	//System.err.println("FlashLED.tick: done");
    }
}
