package TRIPS.OWL.sexp;

import java.util.*;

/**
 * a pretty printer for SExpressions
 * main entry point is makeString
 */
public class SExpressionPrettyPrinter {

    /**
     * returns a string that is nicely formatted
     */
    public static String makeString(SExpression sexp) {
	return makeString(sexp,0);
    }

    /**
     * indents the string, with startColumn position of the first char
     * (assumes "cursor" already at that column)
     */
    protected static String makeString(SExpression sexp, int startColumn) {
	String retval = null;

	if (sexp.isList()) {
	    retval = stringFromList(sexp.asList(), startColumn);
	} else { // an atom - just return it's normal string
	    retval = sexp.toString();
	}

	return retval;
    } // end makeString

    /**
     * makes a SExpressionList into a string with startColumn position
     * (assumes "cursor" already at that position)
     */
    protected static String stringFromList(SExpressionList list, int startColumn) {
	String retval = "";

	//retval += computeIndent(indentLevel);

	if (list.isEmpty()) // special case for empty list
	    return retval + "()";

	if (list.size() == 1) // special case for 1 item list
	    return retval + "(" + makeString(list.first(),startColumn+1) + ")";

	// if we're here, the list has 2 or more items
	retval += "(";

	List<List<SExpression>> lines = divideIntoLines(list);

	// output each line
	//for (List<SExpression> line : lines) {
	Iterator<List<SExpression>> linesIter = lines.iterator();
	while (linesIter.hasNext()) {
	    List<SExpression> line = linesIter.next();
	    
	    int col = startColumn + 1; // where we're starting on this line
	    // output each item on the line
	    boolean first = true;
	    for (SExpression sexp : line) {
		if (first) {
		    first = false;
		} else {
		    retval += " ";
		    col++;
		}
		String thisStr = makeString(sexp,col);
		retval += thisStr;
		col += thisStr.length();
	    } // end for each sexp

	    if (linesIter.hasNext()) // keeps end parens together
		retval += newlineToColumn(startColumn + 1);
	    
	} // end for each line

	retval += ")";
	
	return retval;
    } // end stringFromList

    /**
     * this returns a newline followed by enough spaces to put the "cursor" to the
     * given column
     */
    protected static String newlineToColumn(int column) {
	String retval = "\n";

	for (int i = 0; i < column; i++)
	    retval += " ";

	return retval;
    } // end newlineToColumn


    /**
     * divides the contents of a list into a list of lines to output on
     * (wihtin each line is a list of sexp)
     */
    protected static List<List<SExpression>> divideIntoLines(SExpressionList list) {
	List<List<SExpression>> retval = new ArrayList<List<SExpression>>();

	if (list.isEmpty()) // empty for empty list
	    return retval;

	// try algorithm of breaking at lists
	// except where a keyword comes before the list, in which case we
	// break before that keyword and not at the list

	ListIterator<SExpression> iter = list.listIterator();

	while (iter.hasNext()) {
	    List<SExpression> thisLine = new ArrayList<SExpression>();
	    SExpression first = iter.next();
	    thisLine.add(first); // always put the first one on here

	    boolean justDidKeyword = first.isKeyword();

	    // compute the rest of the contents of this line
	    while (iter.hasNext() && 
		   (justDidKeyword || !nextIsList(iter))) {
		SExpression sexp = iter.next();
		if (sexp.isKeyword() && !nextIsKeyword(iter)) { // newline after keyword non-kw
		    iter.previous();
		    break;
		}

		thisLine.add(sexp);
		justDidKeyword = sexp.isKeyword();
	    } // end while still on this line

	    retval.add(thisLine);
	} // end while (for lines)

	return retval;
    } // end divideIntoLines

    /**
     * peeks at the next item on the iterator and checks if it's a list
     */
    protected static boolean nextIsList(ListIterator<SExpression> iter) {
	SExpression p = peek(iter);
	if (p == null)
	    return false;

	return p.isList();
    } // end nextIsList


    /**
     * peeks at the next item on the iterator and checks if it's a kewyord
     */
    protected static boolean nextIsKeyword(ListIterator<SExpression> iter) {
	SExpression p = peek(iter);
	if (p == null)
	    return false;

	return p.isKeyword();
    } // end nextIsList

    /**
     * returns the next item in the list but doesn't advance the iterator
     * if iter is at end, just returns null
     */
    protected static SExpression peek(ListIterator<SExpression> iter) {
	if (!iter.hasNext())
	    return null;

	SExpression retval = iter.next();
	iter.previous();
	
	return retval;
    } // end peek

//     /**
//      * returns a string with the contents of the list (not the parens) with
//      * no line breaks (just a space between each item
//      */
//     protected static String stringFromListNoLineBreaks(SExpressionList list) {
// 	String retval = "";

// 	Iterator<SExpression> iter = list.iterator();
// 	while (iter.hasNext()) {
// 	    SExpression sexp = iter.next();
// 	    retval += sexp;
// 	    if (iter.hasNext()) {
// 		retval += " ";
// 	    }
// 	} // end for each elem

// 	return retval;
//     } // end stringFromListNoLineBreaks

//     /**
//      * returns a string from the list with line breaks (only called on lists of 2 or more)
//      */
//     protected static String stringFromListLineBreaks(SExpressionList list, int indentLevel) {
// 	String retval = "";

// 	Iterator<SExpression> iter = list.iterator();

// 	SExpression firstItem = iter.next();

// 	// always put the first element on the same line as the open paren
// 	retval += makeString(firstItem,indentLevel+1);

// 	SExpression secondItem = iter.next();

// 	// decide if we put secondItem on the same line as the first
// 	if ((!firstItem.isList()) && (!secondItem.isList())) {
// 	    retval += " " + makeString(secondItem,indentLevel+1);
// 	} else { // one or the other is a list, new line
// 	    retval += "\n" + computeIndent(indentLevel) + " " + makeString(secondItem);
// 	}
	
// 	while (iter.hasNext()) {
// 	    retval += "\n" + computeIndent(indentLevel);
// 	    SExpression sexp = iter.next();

// 	    retval += makeString(sexp,indentLevel+1);

// 	    // if the last was a keyword, keep the next on this line
// 	    if (iter.hasNext() && (sexp instanceof SExpressionSymbol) && 
// 		(((SExpressionSymbol) sexp).isKeyword())) {
// 		SExpression next = iter.next();
// 		retval += " " + makeString(next,indentLevel+1);
// 	    } // end if keyword
	    
// 	} // end for each item

// 	return retval;
//     } // end stringFromListLineBreaks

//     /**
//      * analyzes the list and decides if we should do line breaks
//      */
//     protected static boolean shouldDoLineBreaks(SExpressionList list) {
// 	// for now, return yes if it has a list somewhere in it
// 	for (SExpression sexp : list) {
// 	    if (sexp.isList())
// 		return true;
// 	}

// 	return false;
//     } // end shouldDoLineBreaks

//     /**
//      * returns the string for the indent for the given level
//      */
//     protected static String computeIndent(int indentLevel) {
// 	String retval = "";
// 	for (int i = 0; i < indentLevel; i++)
// 	    retval += INDENT_STRING;
// 	return retval;
//     } // end computeIndent


} // end class SExpressionPrettyPrinter
