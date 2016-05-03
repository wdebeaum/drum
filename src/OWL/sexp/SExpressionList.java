package TRIPS.OWL.sexp;

import java.io.*;
import java.util.*;

import TRIPS.KQML.*;

/**
 * an list in an SExpression
 */
public class SExpressionList extends SExpression 
    implements Serializable, List<SExpression> {

    protected static final long serialVersionUID = 3;

    /**
     * the actual List that represents the list
     */
    protected List<SExpression> list;

    /**
     * creates an empty list
     */
    public SExpressionList() {
	list = new ArrayList<SExpression>();
    } // end constructor()

    /**
     * creates from a list of sexpression
     */
    public SExpressionList(List<SExpression> list) {
	this.list = list;
    } // end constructor(List)

    /**
     * converts a KQMLList
     */
    public SExpressionList(KQMLList kqml) {
	this();
	for (KQMLObject obj : kqml) {
	    add(SExpression.fromKQML(obj));
	}
    }

    /**
     * returns an iterator through the chilrden
     * doesn't go into the children of sublists
     */
    public Iterator<SExpression> iterator() {
	return list.iterator();
    } // end iterator

    /**
     * returns the nth element from the underlying list
     */
    public SExpression get(int n) {
	return list.get(n);
    }

    /**
     * returns the first element
     */
    public SExpression first() {
	return get(0);
    }

    /**
     * like the lisp "rest" operator -- returns a list from 1 to n
     * this is *not* the same internal structure like in lisp, just a new list that
     * points to the same elements
     */
    public SExpressionList rest() {
	SExpressionList retval = new SExpressionList();

	for (int i = 1; i < size(); i++) {
	    retval.add(get(i));
	} // end for each

	return retval;
    } // end rest

    /**
     * returns the item following keyword or null if keyword not found (or last item)
     */
    public SExpression getKeywordVal(String keyword) {
	int pos = indexOf(SExpressionSymbol.createKeyword(keyword));

	if ((pos != -1) && (pos != (size() -1)))
	    return get(pos+1);
	else
	    return null;
    } // end getKeywordVal

    /**
     * traverses the whole 'tree' of the list and deletes any item that is equal
     * to the SExpressionSymbol
     */
    public void deleteInTree(SExpressionSymbol symb) {
	Iterator<SExpression> iter = iterator();
	while (iter.hasNext()) {
	    SExpression exp = iter.next();
	    if (exp instanceof SExpressionList) {
		((SExpressionList)exp).deleteInTree(symb);
	    } else {
		if (((SExpressionSymbol)exp).equals(symb))
		    iter.remove();
	    }	   
	}
    } // end deleteInTree

    /**
     * returns a List<SExpressionList> from this (a list of lists)
     * (throws exception if all children not SExpressionList)
     */
    public List<SExpressionList> toListSExpressionList() {
	List<SExpressionList> retval = new ArrayList<SExpressionList>();

	for (SExpression expr : this) {
	    if (!(expr instanceof SExpressionList))
		throw new RuntimeException("element not an SExpressionList: " + expr);
	    retval.add((SExpressionList)expr);
	}

	return retval;
    } // end toListSExpressionList

    /**
     * prints out for lisp (also does some limited pretty printing)
     */
    @Override
    public String toString() {
	return SExpressionPrettyPrinter.makeString(this);
    }

    /**
     * converts to the shortest string possible (not pretty printed)
     * use only if you don't care if the human reads it
     */
    public String toCompactString() {
	String retval = "";
	
	retval += "(";

	Iterator<SExpression> iter = list.iterator();
	while (iter.hasNext()) {
	    SExpression sexp = iter.next();
	    retval += sexp;
	    if (iter.hasNext()) {
		retval += " ";
	    }
	} // end for each elem

	retval += ")";
	
	return retval;
    } // end toCompactString


    /**
     * implement for list interface
     */
    public List<SExpression> subList(int fromIndex, int toIndex) {
	return list.subList(fromIndex, toIndex);
    } // end subList

    public ListIterator<SExpression> listIterator() {
	return list.listIterator();
    }

    public ListIterator<SExpression> listIterator(int index) {
	return list.listIterator(index);
    }

    public int lastIndexOf(Object o) {
	return list.lastIndexOf(o);
    }

    public int indexOf(Object o) {
	return list.indexOf(o);
    }

    public SExpression remove(int index) {
	return list.remove(index);
    }

    public boolean remove(Object o) {
	return list.remove(o);
    }

    public boolean add(SExpression o) {
	return list.add(o);
    }

    public void add(int index, SExpression o) {
	list.add(index, o);
    }

    public SExpression set(int index, SExpression element) {
	return list.set(index, element);
    }

    public void clear() {
	list.clear();
    }

    public boolean retainAll(Collection<?> c) {
	return list.retainAll(c);
    }

    public boolean removeAll(Collection<?> c) {
	return list.removeAll(c);
    }

    public boolean addAll(Collection<? extends SExpression> c) {
	return list.addAll(c);
    }

    public boolean addAll(int index, Collection<? extends SExpression> c) {
	return list.addAll(index, c);
    }

    public boolean containsAll(Collection<?> c) {
	return list.containsAll(c);
    }

    public <T> T[] toArray(T[] a) {
	return list.toArray(a);
    }

    public Object[] toArray() {
	return list.toArray();
    }

    public boolean contains(Object o) {
	return list.contains(o);
    }

    public boolean isEmpty() {
	return list.isEmpty();
    }

    public int size() {
	return list.size();
    }

} // end class SExpressionList