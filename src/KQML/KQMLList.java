/*
 * KQMLList.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 17 Feb 1998
 * Time-stamp: <Fri Jun  1 13:40:57 EDT 2007 ferguson>
 *
 * A KQMLList is basically a vector that prints itself using Lisp syntax.
 * Correction: I'm now using Java Collection classes, specifically a
 * LinkedList.
 * Update: Now using generics with the List/LinkedList.
 */

package TRIPS.KQML;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.NoSuchElementException;

/**
 * Class representing KQML lists.
 *
 * @see KQMLReader
 */
public class KQMLList extends KQMLObject implements List<KQMLObject> {

    protected List<KQMLObject> data;

    /**
     * Returns a new empty KQMLList.
     *
     * @see KQMLPerformative
     * @see KQMLReader
     */
    public KQMLList() {
	data = new LinkedList<KQMLObject>();
    }
    public KQMLList(KQMLObject a1) {
	this();
	data.add(a1);
    }
    public KQMLList(KQMLObject a1, KQMLObject a2) {
	this();
	data.add(a1);
	data.add(a2);
    }
    public KQMLList(KQMLObject a1, KQMLObject a2, KQMLObject a3) {
	this();
	data.add(a1);
	data.add(a2);
	data.add(a3);
    }
    public KQMLList(KQMLObject a1, KQMLObject a2, KQMLObject a3, KQMLObject a4) {
	this();
	data.add(a1);
	data.add(a2);
	data.add(a3);
	data.add(a4);
    }
    public KQMLList(KQMLObject a1, KQMLObject a2, KQMLObject a3, KQMLObject a4, KQMLObject a5) {
	this();
	data.add(a1);
	data.add(a2);
	data.add(a3);
	data.add(a4);
	data.add(a5);
    }

    /**
     * Adds an element to the end of a KQMLList (the String argument will
     * be converetd to a KQMLToken).
     *
     * @param  obj  String to add
     */
    public void add(String obj) {
	add(new KQMLToken(obj));
    }

    /**
     * Searches for the first occurence of the given string, which
     * is first converted to a KQMLToken then tested for using the
     * equals method.
     *
     * @return  index of element, or -1
     */
    public int indexOfString(String s) {
 	return indexOf(new KQMLToken(s));
    }

    /**
     * Adds an element to the front a KQMLList.
     *
     * @param  obj  KQMLObject to add
     */
    public void push(KQMLObject obj) {
	data.add(0, obj);
    }

    /**
     * @deprecated  - use {@link #add(int index, KQMLObject element)} instead
     * Inserts an element at the given index of an KQMLList.
     *
     * @param  obj  KQMLObject to add
     * @param  index  Index at which to insert
     */
    @Deprecated
    public void insertAt(KQMLObject obj, int index) {
	add(index,obj);
    }
    /**
     * @deprecated  - use {@link #remove(int index)} instead.
     * Removes the element at the given index of an KQMLList.
     *
     * @param  index  Index at which to delete
     */
    @Deprecated
    public void removeAt(int index) {
	remove(index);
    }

    /**
     * @deprecated  - use {@link #get(int index)} instead.
     * Returns the requested element of a KQMLList.
     *
     * @param  n  Index of KQMLObject
     * @return  KQMLObject at that index
     */
    @Deprecated
    public KQMLObject nth(int n) {
	return get(n);
    }
    
    /**
     * @deprecated  - use {@link #size()} instead.
     * Returns the length of a KQMLList.
     *
     * @return  Length of list
     */
    @Deprecated
    public int length() {
	return size();
    }
    /**
     * Returns the KQMLObject following the given keyword in the list.
     * Uses case-insensitive matching on the keyword.
     *
     * @param     keyword Name of parameter (including colon)
     * @return    Value of parameter (KQMLToken, KQMLString, KQMLQuotation,
     *            or KQMLList)
     * @see       java.lang.String
     * @see       KQMLString
     * @see       KQMLQuotation
     * @see       KQMLList
     */
    public KQMLObject getKeywordArg(String keyword) {
	ListIterator<KQMLObject> iterator = data.listIterator();
	while (iterator.hasNext()) {
	    String key = iterator.next().toString();
	    if (key.equalsIgnoreCase(keyword)) {
		if (iterator.hasNext()) {
		    return iterator.next();
		} else {
		    return null;
		}
	    }
	}
	return null;
    }

    /**
     * removes the keyword its arg from the list and returns the arg
     * returns null if keyword not found
     */
    public KQMLObject removeKeywordArg(String keyword) {
	KQMLObject retval = getKeywordArg(keyword);
	if (retval != null) { // it is in there
	    int pos = indexOfIgnoreCase(keyword);
	    if (pos == -1) {
		throw new RuntimeException("keyword: " + keyword + " found in " + this + 
					   " but position not found with indexOf -- probably an incongruence in handling of ignoreCase in KQMLList.indexOf and KQMLList.getKeywordArg");
	    }
	    remove(pos);
	    remove(pos); // this one removes the arg after it
	}
	return retval;
    }

    /**
     * Print this KQMLList to given Writer in KQML syntax.
     */
    public void write(Writer out) throws IOException {
	out.write("(");
	ListIterator<KQMLObject> iterator = data.listIterator();
	while (iterator.hasNext()) {
	    iterator.next().write(out);
	    if (iterator.hasNext()) {
		out.write(" ");
	    }
	}
	out.write(")");
    }
    /**
     * Returns a KQMLList as a String in KQML syntax.
     *
     * @return  String denoting KQMLList
     */
    public String toString() {
	StringWriter out = new StringWriter();
	try {
	    write(out);
	} catch (IOException ex) {
	}
	return out.toString();
    }
    /**
     * Reads a KQMLList from a string.
     *
     * @return    KQMLList described by string
     * @exception KQMLException If the input is not proper KQML.
     * @exception java.io.EOFException If end-of-string is reached.
     * @exception java.io.IOException For any other I/O error.
     */
    public static KQMLList fromString(String s) throws IOException {
	StringReader sreader = new StringReader(s);
	KQMLReader kreader = new KQMLReader(sreader);
	return kreader.readList();
    }

    /***************** METHODS IMPLEMENTED FROM LIST ******************/

    /**
     * Adds an element to the end of a KQMLList.
     *
     * @param  obj  KQMLObject to add
     */
    public boolean add(KQMLObject obj) {
	return data.add(obj);
    }

    public void add(int index, KQMLObject element) {
	data.add(index,element);
    }

    /**
     * Adds all elements to the end of this one.
     *
     * @param  collection to add
     */
    public boolean addAll(Collection<? extends KQMLObject> other) {
	return data.addAll(other);
    }

    public boolean addAll(int index, Collection<? extends KQMLObject> c) {
	return data.addAll(index,c);
    }

    public void clear() {
	data.clear();
    }

    public boolean contains(Object o) {
	return data.contains(o);
    }

    public boolean containsAll(Collection<?> c) {
	return data.containsAll(c);
    }

    /**
     * Returns true if the two lists are equal (in the sense of their
     * respective elements being equal). This is what Lisp calls EQUAL.
     *
     * @return  true if lists are ``equal''
     */
    public boolean equals(Object o) {
	return data.equals(o);
    }

    public KQMLObject get(int index) {
	return data.get(index);
    }

    public int hashCode() {
	return data.hashCode();
    }

    /**
     * Searches for the first occurence of the given argument, testing
     * for equality using the equals method. Note that this is a case sensitive match for tokens.
     * If x is a String, converts
     * to KQMLToken first (for backwards compatibility).
     * 
     *
     * @return  index of element, or -1
     */
    public int indexOf(Object x) {
	if (x instanceof KQMLObject)
	    return data.indexOf(x);
	else if (x instanceof String)
	    return indexOfString((String)x);
	else
	    throw new RuntimeException("unexpected parameter type: " + x.getClass());
    }

    /**
     * same as indexOf except that it's case insensitive
     * only works for Strings -- compatible with getKeywordArg()
     */
    public int indexOfIgnoreCase(String keyword) {
	for (int pos = 0; pos < data.size(); pos++) {
	    String item = data.get(pos).toString();
	    if (keyword.equalsIgnoreCase(item)) {
		return pos;
	    }
	}

	return -1;
    } // end indexOfIgnoreCase

    public boolean isEmpty() {
	return data.isEmpty();
    }

    public Iterator<KQMLObject> iterator() {
	return data.iterator();
    }

    public int lastIndexOf(Object o) {
	return data.lastIndexOf(o);
    }

    /**
     * Returns the a ListIterator for the KQMLList
     *
     * @return ListIterator for data
     */
    public ListIterator<KQMLObject> listIterator() {
	return data.listIterator();
    }

    public ListIterator<KQMLObject> listIterator(int index) {
	return data.listIterator(index);
    }

    public KQMLObject remove(int index) {
	return data.remove(index);
    }

    public boolean remove(Object o) {
	return data.remove(o);
    }

    public boolean removeAll(Collection<?> c) {
	return data.removeAll(c);
    }

    public boolean retainAll(Collection<?> c) {
	return data.retainAll(c);
    }

    public KQMLObject set(int index, KQMLObject obj) {
	return data.set(index,obj);
    }

    public int size() {
	return data.size();
    }
	
    /**
     * Returns a new KQMLList consisting of the elements from fromIndex
     * (inclusive) to toIndex (exclusive). This list is backed by the
     * original list.
     *
     * @return KQMLList denoted sublist
     */
    public KQMLList subList(int fromIndex, int toIndex) {
	KQMLList newlist = new KQMLList();
	newlist.data = data.subList(fromIndex, toIndex);
	return newlist;
    }

    public Object[] toArray() {
	return data.toArray();
    }

    public <T> T[] toArray(T[] a) {
	return data.toArray(a);
    }

}
