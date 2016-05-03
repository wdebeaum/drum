/*
 * @(#)ClientGroup.java
 *
 * David Costello, costello@cs.rochester.edu,  8 Jan 2000
 * $Id: ClientGroup.java,v 1.1.1.1 2005/01/14 19:48:08 ferguson Exp $
 *
 * gf: 11/9/2002: My quick hacks here using static vars/methods probably
 *                aren't right.Should do something with the registry instead.
 * 
 */

package TRIPS.Facilitator;

import java.util.Vector;
import java.util.Enumeration;
import java.util.NoSuchElementException;
import TRIPS.KQML.KQMLPerformative;

public class ClientGroup implements Sendable {
    //
    // Fields
    //
    protected String name;
    protected Vector members = new Vector();
    //
    // Constructors
    //
    ClientGroup(String name) {
	this(name, null);
    }
    ClientGroup(String name, ClientGroup parent) {
	this.name = name;
	if (parent != null) {
	    parent.add(this);
	}
    }
    //
    // Sendable methods
    //
    public String getName() {
	return name;
    }
    public void sendTo(KQMLPerformative msg, Sendable fromWhom) {
	if (members.isEmpty()){
	    Debug.warn("ClientGroup.sendTo: group is empty: " + name);
	} else {
	    Enumeration e = members.elements();
	    try {
		while (e.hasMoreElements()) {
		    Sendable toWhom = (Sendable)e.nextElement();
		    // Address message to this member of the group
		    // TODO msg.setParameter(":receiver", client.getName());
		    toWhom.sendTo(msg, fromWhom);
		}
	    } catch (NoSuchElementException ex) {
	    }
	}
    }
    //
    // Other methods for manipulating the group
    //
    public void add(Sendable client) {
	members.addElement(client);
    }
    public void remove(Sendable client) {
	members.removeElement(client);
    }
    public boolean contains(Sendable client) {
	return members.contains(client);
    }

    public void dump() {
	System.err.print(name + " = ");
	Enumeration e = members.elements();
	try {
	    while (e.hasMoreElements()) {
		System.err.print(e.nextElement() + " ");
	    }
	} catch (NoSuchElementException ex) {
	}
	System.err.println("");
    }
    //
    // Pretty-printing
    //
    public String toString() {
	return "ClientGroup[" + name + "]";
    }
}			   

