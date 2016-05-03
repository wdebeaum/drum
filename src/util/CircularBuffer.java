/*
 * CircularBuffer.java
 *
 * George Ferguson, ferguson@cs.rochester.edu,  9 Jan 2004
 * Time-stamp: <Fri Jan  9 21:42:20 EST 2004 ferguson>
 */

package TRIPS.util;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Implements a fixed-size circular buffer of Objects.
 * The buffer will wrap around without siganlling an error.
 * These methods are not synchronized.
 * <p>
 * TODO: Resize buffer, preserving content where possible.
 *       Could also implement Collection interface or AbstractList or
 *       something.
 */
public class CircularBuffer {

    Object[] data;
    int size;
    int first = 0;
    int last = 0;

    public CircularBuffer(int size) {
	this.size = size;
	data = new Object[size];
    }

    public int getSize() {
	return size;
    }

    public boolean isEmpty() {
	return (first == last);
    }

    public boolean isFull() {
	return ((first == 0 && last == size-1) || (last == first-1));
    }

    public void add(Object x) {
	data[last] = x;
	last = (last + 1) % size;
	// Wrap around without error
	if (last == first) {
	    first = (first + 1) % size;
	}
    }

    public Object remove() throws NoSuchElementException {
	if (isEmpty()) {
	    throw new NoSuchElementException();
	}
	Object x = data[first];
	first = (first + 1) % size;
	return x;
    }

    public void clear() {
	first = last = 0;
	// Zero out data to avoid holding onto references
	for (int i=0; i < size; i++) {
	    data[i] = null;
	}
    }

    public Iterator iterator() {
	return new CircularBufferIterator(this);
    }
}

class CircularBufferIterator implements Iterator {
    CircularBuffer buf;
    int index;
    public CircularBufferIterator(CircularBuffer buf) {
	this.buf = buf;
	index = buf.first;
    }
    public boolean hasNext() {
	return (index != buf.last);
    }
    public Object next() throws NoSuchElementException {
	if (index == buf.last) {
	    throw new NoSuchElementException();
	}
	Object x = buf.data[index];
	index = (index + 1) % buf.size;
	return x;
    }
    public void remove() throws UnsupportedOperationException {
	throw new UnsupportedOperationException();
    }
}
