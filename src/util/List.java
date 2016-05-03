/*
 * List.java: Linked-list class
 *
 * George Ferguson, ferguson@cs.rochester.edu, 31 Mar 1999
 * Time-stamp: <Wed Sep 13 16:53:20 EDT 2000 ferguson>
 *
 * This is implemented as doubly-linked list to support efficient
 * removals. It would be easy to make a (slightly) more efficient
 * singly-linked version if all you really wanted was CONS, CAR, and CDR.
 *
 * Note that JDK1.2 LinkedList class doesn't support CDR operation...
 */

package TRIPS.util;

import java.util.Enumeration;
import java.util.NoSuchElementException;

public class List {
    //
    // Fields
    //
    private Link head = null;
    private Link tail = null;
    private int len = 0;		// cache for speed
    //
    // Constructor
    //
    public List() {
    }
    // Methods
    public int size() {
	return len;
    }
    public boolean isEmpty() {
	return (len == 0);
    }
    public synchronized void addElement(Object item) {
	Link p = new Link(item, null, tail);
	if (head == null) {
	    head = p;
	}
	if (tail != null) {
	    tail.next = p;
	}
	tail = p;
	len += 1;
    }
    public synchronized void pushElement(Object item) {
	Link p = new Link(item, head, null);
	if (head != null) {
	    head.prev = p;
	}
	if (tail == null) {
	    tail = p;
	}
	head = p;
	len += 1;
    }
    public synchronized Object popElement() throws NoSuchElementException {
	if (head == null) {
	    throw(new NoSuchElementException());
	}
	Object data = head.data;
	head = head.next;
	if (head != null) {
	    head.prev = null;
	}
	len -= 1;
	return data;
    }
    public synchronized boolean findElement(Object item) {
	for (Link p = head; p != null; p = p.next) {
	    if (p.data.equals(item)) {
		return true;
	    }
	}
	return false;
    }
    public synchronized void insertBefore(Object item, Object target){
	Link itemp = new Link(item,null,null);
	boolean found = false;
	for (Link ptr = head; ptr != null; ptr = ptr.next) {
	    if (ptr.data.equals(target)) {
		itemp.next = ptr;
		itemp.prev = ptr.prev;
		ptr.prev = itemp;
		itemp.prev.next = itemp;
		//ptr = itemp;
		found = true;
		len+=1;
	    }
	}
    }
    public synchronized void insertAfter(Object item, Object target){
	throw new Error("List.insertAfter not yet implemented!");
    }
    public synchronized boolean removeElement(Object item) {
	for (Link p = head; p != null; p = p.next) {
	    if (p.data.equals(item)) {
		if (p.prev == null) {
		    head = p.next;
		} else {
		    p.prev.next = p.next;
		}
		if (p.next == null) {
		    tail = p.prev;
		} else {
		    p.next.prev = p.prev;
		}
		len -= 1;
		return true;
	    }
	}
	return false;
    }
    public synchronized void removeAllElements() {
	head = tail = null;
	len = 0;
    }
    public Enumeration elements() {
	return new ListEnumeration(head);
    }
    public RevEnumeration elementsReverse(){
	return new ListEnumeration(tail);
    }


    //
    // Class Link
    //
    class Link {
	// Fields
	Object data;
	Link next;
	Link prev;
	// Constructor
	public Link(Object d, Link n, Link p) {
	    data = d;
	    next = n;
	    prev = p;
	}
    }
    //
    // Class ListEnumeration
    //
    class ListEnumeration implements Enumeration, RevEnumeration {
	//
	// Fields
	//
	Link pointer;
	//
	// Constructor
	//
	public ListEnumeration(Link l) {
	    pointer = l;
	}
	//
	// Enumeration methods
	//
	public boolean hasMoreElements() {
	    return (pointer != null);
	}
	public Object nextElement() throws NoSuchElementException {
	    if (pointer == null) {
		throw new NoSuchElementException();
	    }
	    Object data = pointer.data;
	    pointer = pointer.next;
	    return data;
	}
	// RevEnumeration methods
	public Object previousElement() throws NoSuchElementException {
	    if (pointer == null) {
		throw new NoSuchElementException();
	    }
	    Object data = pointer.data;
	    pointer = pointer.prev;
	    return data;
	}	
	public boolean hasPrevious(){
	    return (pointer != null);
	}
    }
}
