/*
 * KQMLReader.java:
 *
 * George Ferguson, ferguson@cs.rochester.edu, 19 Feb 1998
 * Time-stamp: <Wed Aug  4 18:52:37 EDT 2004 ferguson>
 *
 * KQMLReader: Turns an InputStream into a source of performatives by
 * parsing the input using a recursive-descent parser. The main method,
 * readPeformative, either returns a KQMLPerformative or throws a
 * KQMLException, which is a subclass of IOException (or throws
 * EOFException on EOF).
 * Also provides (with package scope) some functions that provide access
 * to the KQML parser for use by other classes' fromString() methods.
 */

package TRIPS.KQML;

import java.io.*;

/**
 * A class for reading KQML performatives from an InputStream.
 * For example:
 * <PRE>
 *    KQMLReader in = new KQMLReader(socket.getInputStream());
 * </PRE>
 *
 * @see KQMLPerformative
 */
public class KQMLReader {
    // Slots
    private PushbackReader reader;
    private StringBuffer inbuf;
    //
    // Constructor
    //
    /**
     * Creates a new stream from which to read KQML Performatives.
     *
     * @param	reader	Reader from which to read
     * @see	java.io.Reader
     */
    public KQMLReader(Reader reader) {
	this.reader = new PushbackReader(reader);
	inbuf = new StringBuffer();
    }
    /**
     * Creates a new KQMLReader from the given InputStream, by creating
     * an InputStreamReader from it.
     *
     * @param	stream	InputStream from which to read
     * @see	java.io.Reader
     * @see	java.io.InputStream
     */
    public KQMLReader(InputStream stream) {
	this(new InputStreamReader(stream));
    }
    //
    // Methods
    //
    public void close() throws IOException {
	reader.close();
    }
    private static void DEBUG(String s) {
    	System.err.println(s);
    }
    private char readChar() throws IOException {
	int inch = reader.read();
	if (inch == -1) {
	    throw new EOFException(inbuf.toString());
	} else {
	    char ch = (char)inch;
	    inbuf.append(ch);
	    return ch;
	}
    }	
    private void ungetChar(char ch) throws IOException {
	//DEBUG("pushing back char: " + ch);
	reader.unread(ch);
	inbuf.deleteCharAt(inbuf.length() - 1);
    }
    private char peekChar() throws IOException {
	char ch = readChar();
	ungetChar(ch);
	return ch;
    }
    private static boolean isSpecial(char ch) {
	// gf: 4 Aug 2004: Added pipe symbol to support Lisp case convention
	String specialChars = "<>=+-*/&^~_@$%:.!?|";
	return (specialChars.indexOf((int)ch) != -1);
    }
    private static boolean isTokenChar(char ch) {
	// old whitelist method, breaks on UTF-8 chars etc.
	//return (Character.isLetterOrDigit(ch) || isSpecial(ch));
	// new blacklist method, more like what KQML.pm and kqml.rb do
	String nonTokenChars = "'`\"#()";
	return ((!Character.isWhitespace(ch)) &&
	        nonTokenChars.indexOf((int)ch) == -1);
    }
    public KQMLObject readExpr() throws IOException {
	return readExpr(false);
    }
    private KQMLObject readExpr(boolean backquoted) throws IOException {
	//DEBUG("reading expression...");
	char ch = peekChar();
	switch (ch) {
	case '\'':
	case '`':
	    return readQuotation(backquoted);
	case '"':
	case '#':
	    return readString();
	case '(':
	    return readList(backquoted);
	case ',':
	    if (!backquoted) {
		// Gobble offending character (avoiding infinite loop)
		ch = readChar();
		// And throw
		throw new KQMLBadCommaException(inbuf.toString());
	    } else {
		return readQuotation(backquoted);
	    }
	default:
	    if (isTokenChar(ch)) {
		return readToken();
	    } else {
		// Gobble offending character
		ch = readChar();
		// And throw
		throw new KQMLBadCharacterException(inbuf.toString());
	    }
	}
    }
    KQMLToken readToken() throws IOException {
	//DEBUG("reading word");
	StringBuffer buf = new StringBuffer();
	boolean done = false;
	boolean inPipes = false;
	char ch;
	while (!done) {
	    ch = readChar();
	    if (ch == '|')
	      inPipes = !inPipes;
	    if (inPipes || isTokenChar(ch)) {
		buf.append(ch);
	    } else {
		ungetChar(ch);
		done = true;
	    }
	}
	//DEBUG("returning word: " + buf.toString());
	return new KQMLToken(buf.toString());
    }	
    KQMLQuotation readQuotation(boolean backquoted) throws IOException {
	char ch = readChar();
	//DEBUG("reading quotation of type " + ch + "...");
	switch (ch) {
	case '`':
	    return new KQMLQuotation(ch, readExpr(true));
	case '\'':
	case ',':
	    return new KQMLQuotation(ch, readExpr(backquoted));
	default:
	    return null;
	}
    }
    KQMLString readString() throws IOException {
	char ch = readChar();
	if (ch == '"') {
	    return readQuotedString();
	} else {
	    return readHashedString();
	}
    }
    private KQMLString readQuotedString() throws IOException {
	//DEBUG("reading quoted string");
	StringBuffer buf = new StringBuffer();
	char ch;
	while ((ch = readChar()) != '"') {
	    if (ch == '\\') {
		ch = readChar();
	    }
	    buf.append(ch);
	}
	//DEBUG("returning quoted string: " + buf.toString());
	return new KQMLString(buf.toString());
    }
    private KQMLString readHashedString() throws IOException {
	//DEBUG("reading hashed string");
	StringBuffer buf = new StringBuffer();
	int count = 0;
	char ch;
	while ((ch = readChar()) != '"') {
	    if (!Character.isDigit(ch)) {
		throw new KQMLBadHashException(inbuf.toString());
	    } else {
		count = count * 10 + Character.digit(ch, 10);
	    }
	}
	if (count == 0) {
	    //DEBUG("returning empty hased string");
	    return new KQMLString("");
	} else {
	    while (count-- > 0) {
		buf.append(readChar());
	    }
	}
	//DEBUG("returning hashed string: " + buf.toString());
	return new KQMLString(buf.toString());
    }
   
    /**
     * use for reading lists from a file, where they might be proceeded
     * by whitespace and returns null if hits EOF
     */
    public KQMLList readListForFile() throws IOException {
	KQMLList retval;
	
	try {
	skipWhitespace();
	retval = readList();
	} catch (EOFException e) {
	retval = null;
	}

	return retval;
    }

    public KQMLList readList() throws IOException {
	return readList(false);
    }
    private KQMLList readList(boolean backquoted) throws IOException {
	//DEBUG("reading list");
	KQMLList list = new KQMLList();
	char ch;
	if ((ch = readChar()) != '(') {
	    throw new KQMLBadOpenException(inbuf.toString());
	}

	/* changed nb 2010.03.15
	 * allow case where open paren is followed by whitepace before first char
	 * e.g., ( token1)
	 */
	skipWhitespace();

	while ((ch = peekChar()) != ')') {
	    //DEBUG("not at end of list yet");
	    list.add(readExpr(backquoted));
	    //DEBUG("readList() list="+list.toString());
	    if ((ch = peekChar()) != ')') {
		/* changed dc 2000.08.01
		 * more leniency if two lists are stuck together
		 * with no whitespace in between (list1)(list2)...
		 */
		//DEBUG("readList() peek != ) ch="+ch);
		if (ch == '('){ 
		    //okay new list starting
		    //DEBUG("DON'T TRY TO READ WHITESPACE");
		}else{
		    readWhitespace();
		}
	    }
	}
	if ((ch = readChar()) != ')') {
	    throw new KQMLBadCloseException(inbuf.toString());
	}
	//DEBUG("returning list: " + list);
	return list;
    }
    // This one requires at least one whitespace
    private void readWhitespace() throws IOException {
	//DEBUG("reading whitespace");
	boolean done = false;
	char ch = readChar();
	if (!Character.isWhitespace(ch)) {
	    throw new KQMLExpectedWhitespaceException(inbuf.toString());
	} else {
	    skipWhitespace();
	}
	//DEBUG("done reading whitespace");
    }	
    // This method allows no whitespace
    private void skipWhitespace() throws IOException {
	//DEBUG("skipping whitespace");
	boolean done = false;
	char ch;
	while (!done) {
	    ch = readChar();
	    if (!Character.isWhitespace(ch)) {
		ungetChar(ch);
		done = true;
	    }
	}
	//DEBUG("done skipping whitespace");
    }	
    /**
     * Reads a performative.
     *
     * @return    Next performative from input stream
     * @exception KQMLException If the input is not proper KQML.
     * @exception java.io.EOFException If EOF is reached.
     * @exception java.io.IOException For any other I/O error.
     */
    public KQMLPerformative readPerformative() throws IOException {
	// Reset input buffer (for error messages)
	inbuf.setLength(0);
	// Skip leading whitespace
	skipWhitespace();
	// Reset input buffer again
	inbuf.setLength(0);
	// Try to read an expression
	KQMLObject expr = readExpr();
	// Better be a list
	if (expr instanceof KQMLList) {
	    return new KQMLPerformative((KQMLList)expr);
	} else {
	    throw new KQMLExpectedListException(inbuf.toString());
	}
    }
    /**
     * For testing.
     *
     */
    public static void main(String a[]) {
	KQMLReader in;
	try {
	    in = new KQMLReader(System.in);
	    System.out.println("ready for KQML input...");
	    while (true) {
		try {
		    KQMLPerformative msg = in.readPerformative();
		    System.out.println("message was: " + msg);
		    receive(msg);
		} catch (KQMLException ex) {
		    System.err.println("KQML error: " + ex);
		}
	    }
	} catch (EOFException ex) {
	    System.out.println("EOF");
	} catch (IOException ex) {
	    System.err.println("IO error: " + ex);
	}
    }
    private static void receive(KQMLPerformative msg) {
	String verb = msg.getVerb();
	if (verb.equals("tell")) {
	    System.out.println("that was a TELL");
	    Object content = msg.getParameter(":content");
	    if (content == null) {
		System.out.println("it had no content");
	    } else if (!(content instanceof KQMLList)) {
		System.out.println("its content was " + content);
	    } else {
		KQMLList clist = (KQMLList)content;
		// Technically need to check that car is a String...
		String content0 = clist.nth(0).toString();
		System.out.println("its content started with " + content0);
	    }
	} else if (verb.equals("request")) {
	    System.out.println("that was a REQUEST");
	} else {
	    System.out.println("that was neither request nor tell");
	}
    }
}


