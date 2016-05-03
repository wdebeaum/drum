/*
 * @(#)FacilitatorDisplay.java
 *
 * David Costello, costello@cs.rochester.edu,  8 Jul 1999
 * Time-stamp: <Thu Jan  8 15:14:43 EST 2004 ferguson>
 *
 */

package TRIPS.Facilitator;

import TRIPS.KQML.KQMLPerformative;

public interface FacilitatorDisplay {
    public void add(String name);
    public void remove(String name);
    public void changeName(String oldname, String newname);
    public void indicateMessageSent(String sender, KQMLPerformative msg);
    public void indicateMessageReceived(String receiver, KQMLPerformative msg);
    public void indicateStatus(String name, String status);
    public void showWindow();
    public void hideWindow();
}
