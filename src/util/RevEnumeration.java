/*
 * RevEnumeration.java: Interface for traversing a List backwards
 *
 * Dave Costello, costello@cs.rochester.edu, 12 May 2000
 * Time-stamp: <Wed Sep 13 16:54:51 EDT 2000 ferguson>
 */

package TRIPS.util;

import java.util.NoSuchElementException;

public interface RevEnumeration {
    public Object previousElement() throws NoSuchElementException;
    public boolean hasPrevious();
}



