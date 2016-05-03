/*
 * @(#)NullDisplay.java
 *
 * David Costello, costello@cs.rochester.edu,  05 Oct 1999
 * $Id: NullDisplay.java,v 1.2 2006/05/10 16:35:35 nchambers Exp $
 *
 */

package TRIPS.Facilitator;

import TRIPS.KQML.KQMLPerformative;

/**
 * Dummy FacilitatorDisplay class used as a stub.  Contains empty methods
 * implementing the abstract methods of FaciltatorDisplay.java.
 * The purpose of this object is to provide a 'dead' interface as .
 * the display of the TRIPS Facilitator when no display is requested 
 * The advantage being that it eliminates the need to constantly check if a 
 * display object is null or not.  This can be error prone.  
 * Using NullDisplay provides a FaciltatorDisplay object that has Nops for
 * basic FaciltatorDisplay method calls.
 */
public class NullDisplay implements FacilitatorDisplay {
    //
    // Constructor
    //
    public NullDisplay() {
    }
    public NullDisplay(Facilitator facilitator) {
    }
    public NullDisplay(Facilitator facilitator, Boolean bool) {
    }
    //
    // Display methods
    //
    public void add(String name) {
    }
    public void remove(String name) {
    }
    public void changeName(String oldname, String newname) {
    }
    public void indicateMessageSent(String sender, KQMLPerformative msg) {
    }
    public void indicateMessageReceived(String receiver, KQMLPerformative msg) {
    }
    public void indicateStatus(String name, String status) {
    }
    public void hideWindow() {
    }
    public void showWindow() {
    }
}







