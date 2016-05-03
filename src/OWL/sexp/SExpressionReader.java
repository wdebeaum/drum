package TRIPS.OWL.sexp;

import java.io.*;
import java.util.*;

/**
 * an s-expression reader
 * takes a Reader and can read s-expressions from the stream
 * these can either be an SExpressionList or an SExpressionSymbol
 * (we don't yet do numbers, chars, or strings yet, or probably a number of other things)
 */
public class SExpressionReader {

    protected Reader input;

    protected int lineno = 1; // current line we're on
    protected int charno = 0; // current char in line
    protected String currentLine = ""; // the chars of the line we've read in so far
    protected String previousLine = ""; // the previous line we read in

    /**
     * creates a new instance with the reader (assumed to be open)
     */
    public SExpressionReader(Reader input) {
	this.input = input;
    }

    /**
     * creates a new instance from a file
     * if you use this, you'll need to make sure the file gets closed yourself
     */
    public SExpressionReader(String filename) {
	try {
	    input = new BufferedReader(new FileReader(filename));
	} catch (IOException e) {
	    throw new RuntimeException("problme opening file: " + filename,e);
	}
    } // end constructor(String)

    /**
     * closes the underlying reader
     */
    public void close() {
	try {
	    input.close();
	} catch (IOException e) {
	    throw new RuntimeException("problem closing stream I'm reading from",e);
	}
    } // end close

    /**
     * reads in a single SExpression from an open text stream
     * leaves the stream open
     * if there are no more sexpressions, returns null
     */
    public SExpression nextSExpression() {

	int c;

	while ((c = advanceToNextToken()) != -1) {
	    //System.out.println("read: " + (char) c);
	    if (isSymbolChar(c)) { // part of symbol
		return readSymbol();
	    } else if (isListStartChar(c)) { // start new list, recurse
		return readList();
	    } else { // if we see anything else, it shouldn't be there
		error("unexpected char read: '" + (char) c + "'");
	    }
	} // end while input
	    
	// if we've gotten here, we didn't find anything
	return null;	
    } // end nextSExpression

    /**
     * reads in an entire stream of SExpressions and returns the list of them
     * if nothing found, returns the empty list
     * closes the stream when it's done
     */
    public List<SExpression> readStream() {
	List<SExpression> retval = new ArrayList<SExpression>();

	SExpression sexp;
	while ((sexp = nextSExpression()) != null) {
	    retval.add(sexp);
	}

	close();

	return retval;
    } // end readStream

   /**
     * reads in an SExpressionList from the stream
     */
    public SExpressionList readList() {
	List<SExpression> retval = new ArrayList<SExpression>();

	int c;

	c = advanceToNextToken();

	if (c == -1) { // EOF
	    throw new RuntimeException("no start of list found before end of file");
	} else if (!isListStartChar(c)) {
	    error("expected an open paren at start of list, found instead: " + (char) c);
	}

	read(); // eat the open paren

	while ((c = advanceToNextToken()) != -1) {
	    //System.out.println("c=" + (char)c);
	    if (isListEndChar(c)) { // finished with this list
		read(); // eat the close paren
		return new SExpressionList(retval);
	    }

	    SExpression sexp = nextSExpression();
	    //System.out.println("next=" + sexp);
	    //System.out.println("peek=" + (char)peek());
	    if (sexp == null) { // EOF
		break; 
	    }

	    retval.add(sexp);
	} // end while next element

	// if we got here, the stream ended before we closed this list
	throw new RuntimeException("parse error - expected a close paren but didn't get one" + 
				   " before end of stream");
    } // end readList



    /**
     * reads in a single symbol from an open text stream
     * leaves the stream open
     */
    public SExpressionSymbol readSymbol() {
	StringBuilder retval = new StringBuilder();
	
	int c;
	
	while ((c = peek()) != -1) {
	    if (isSymbolChar(c)) { // part of identifier
		read();
		retval.append((char) c);
	    } else { // anything else and we're done
		break;
	    }		
	} // end while input
	    
	if (retval.length() == 0) { // we didn't parse anything
	    return null;
	}
	
	return SExpressionSymbol.parseSymbol(retval.toString());
    } // end readSymbol

    /**
     * advances the stream to the next expression (skips whitespace and comments)
     * when done, the stream will be at the start of the next expression, or it will
     * be EOF  -= returns the first char of the next expression (which the next read() will
     * return) or -1 if EOF has been reached
     */
    protected int advanceToNextToken() {
	int c;

	while ((c = peek()) != -1) {
	    if (isTokenStartChar(c)) { // we found one
		return c;
	    } else if (isCommentStartChar(c)) { // start of line end comment
		advancePastComment();
	    } else { // whitespace
		read();
	    }
	} // end while input
	    
	return -1; // if we get here, the stream is EOF
    } // end advanceToNextToken

    /**
     * reads in a comment between either ; \n or #| |#
     */
    protected void advancePastComment() {
	int c = read();
	if (c == ';') {
	    advanceToNextLine();
	} else if (c == '#') {
	    c = read();
	    if (c != '|')
		error("unexpected char read: '" + (char)c + "'");
	    int prev_c = 0;
	    while (true) {
		c = read();
		if (c < 0)
		    error("EOF inside hashpipe comment");
		if (prev_c == '|' && c == '#')
		    break;
		prev_c = c;
	    }
	} else {
	    error("hey, this isn't a comment at all!");
	}
    }

    /**
     * reads in everything until and including the end of line
     */
    protected void advanceToNextLine() {
	int c;
	while ((c = read()) != -1) {
	    if (c == '\n') {
		return;
	    }		    
	} // end while input

	return;
    } // end advanceToNextLine

    /**
     * returns true if the given char is the start of an expression (as opposed to whitespace
     * or a comment)
     */
    protected static boolean isTokenStartChar(int c) {
	return (isSymbolStartChar(c) || isListStartChar(c) || isListEndChar(c));
    } // end isTokenStartChar

    /**
     * returns true if this char could be the start of an symbol
     */
    protected static boolean isSymbolStartChar(int c) {
	return isSymbolChar(c);
    }

    /**
     * returns true if the given char could be the start of a list
     */
    protected static boolean isListStartChar(int c) {
	return (c == '(');
    }

    /**
     * returns true if the given char could be the end of a list
     */
    protected static boolean isListEndChar(int c) {
	return (c == ')');
    }

    /**
     * returns true if the given char could be whitespace
     */
    protected static boolean isWhitespaceChar(int c) {
	return Character.isWhitespace(c);
    }

    /**
     * returns true if the given char could be the start of a comment
     */
    protected static boolean isCommentStartChar(int c) {
	return (c == ';' || c == '#');
    }

   /**
    * returns true if the character is a char we allow as part of token
    */
    protected static boolean isSymbolChar(int c) {
	// for now, if it's not whitepace, and it's not open or close paren, it's part of an symbol
	return (!isWhitespaceChar(c)  && !isCommentStartChar(c) && 
		!isListStartChar(c) && !isListEndChar(c));
    } // end isSymbolChar


    /**
     * reads the next char
     */
    protected int read() {
	return read(false);
    }

    /**
     * reads the next char, if peek == true then doesn't record it in the error bookkeeping
     * (only peek() should call read(true), all else should just call read())
     */
    protected int read(boolean peek) {
	try {
	    int c = input.read();

	    // bookkeeping
	    if (!peek) {
		if (c == '\n') { // new line
		    lineno++;
		    charno = 0;
		    previousLine = currentLine;
		    currentLine = "";
		} else {
		    currentLine += (char) c;
		}
		charno++;
	    }
		
	    //System.out.print((char)c);
	    return c;
	} catch (IOException e) {
	    throw new RuntimeException(e);
	}
    } // end read

    /**
     * returns the next char in the stream wihout advancing the stream
     */
    protected int peek() {
	try {
	    input.mark(1);
	    int c = read(true);
	    input.reset();
	    return c;
	} catch (IOException e) {
	    throw new RuntimeException(e);
	}
    } // end peek

    /**
     * throws an exception and also reports the current line and char nos
     */
    protected void error(String msg) {
	throw new RuntimeException(" at line: " + lineno + ", char: " + charno + 
				   " of input file::\n" + 
				   "prev: " + previousLine + "\n" +
				   "curr: " + currentLine + "\n" +
				   msg);
    }

    /**
     * for unit testing
     */
    public static void main(String[] args) {
	if (args.length < 1) {
	    System.err.println("  usage: SExpressionReader <file>");
	    System.exit(1);
	}

	SExpressionReader r = new SExpressionReader(args[0]);
	SExpression sexp;
	while ((sexp = r.nextSExpression()) != null) {
	    System.out.println("-----------------------\n" + sexp);
	}

    } // end main


} // end class SExpression
