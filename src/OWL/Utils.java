package TRIPS.OWL;

import java.util.*;

/**
 * various utility functions
 */
public class Utils {

    /**
     * returns a new list that has all the elemnts of list cast to type newClass
     * throws and exception if one of the casts is unsuccessful
     */
    public static <O, N extends O> List<N> castList(Iterable<O> list, Class<N> newClass) {
	return castList(list,newClass,false);
    }

    /**
     * returns a new list that has all the elements of list cast to type newClass
     * if filterMismatches is true, it leaves elements of list that cannot be cast
     * to newClass out, if it is false, it throws a ClassCastException
     */
    public static <O, N extends O> List<N> castList(Iterable<O> list, Class<N> newClass, 
						    boolean filterMismatches) {
	List<N> retval = new ArrayList<N>();

	for (O elem : list) {
	    if (newClass.isInstance(elem))
		retval.add((N)elem);
	    else if (!filterMismatches)
		throw new ClassCastException("instance " + elem + " cannot be cast to " + newClass);
	}

	return retval;
    } // end castList


}