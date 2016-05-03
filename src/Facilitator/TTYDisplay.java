/*
 * @(#)TTYDisplay.java
 *
 * George ferguson, ferguson@cs.rochester.edu, 14 Dec 2000
 * $Id: TTYDisplay.java,v 1.2 2006/05/10 16:35:35 nchambers Exp $
 *
 */

package TRIPS.Facilitator;

import TRIPS.KQML.KQMLPerformative;

public class TTYDisplay implements FacilitatorDisplay {
    Boolean noTraffic = Boolean.FALSE;
    //
    // Constructor
    //
    public TTYDisplay() {
    }
    public TTYDisplay(Facilitator facilitator) {
    }
    public TTYDisplay(Facilitator facilitator, Boolean bool) {
	noTraffic = bool;
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
	if( !noTraffic.booleanValue() ) System.out.println("<" + sender + "\n" + msg.toString());
    }
    public void indicateMessageReceived(String receiver, KQMLPerformative msg) {
	if( !noTraffic.booleanValue() ) System.out.println(">" + receiver + "\n" + msg.toString());
    }
    public void indicateStatus(String name, String status) {
    }
    public void hideWindow() {
    }
    public void showWindow() {
    }
}







