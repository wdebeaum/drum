package TRIPS.AKRL;

import TRIPS.KQML.*;
import java.util.*;
import java.io.*;

/**
 * a string value in AKRL
 */
public class AkrlString extends AkrlValue implements Serializable {
    protected static final long serialVersionUID = 2;

    /**
     * the value we're wrapping -- without the quotes or anything
     */
    protected String string;
    public String getString() {return string;}

    /**
     * create from a string
     */
    public AkrlString(String string) {
	this.string = string;
    }

    /**
     * create from a KQMLToken
     */
    public AkrlString(KQMLString string) {
	this(string.stringValue());
    }

    @Override
    public boolean equals(Object o) {
	if (!(o instanceof AkrlString))
	    return false;
	return getString().equals(((AkrlString)o).getString()); // strings are case sensitive
    }

    @Override
    public int hashCode() {
	return getString().hashCode();
    }

    /**
     * returns a quoted string (with appropriate escaping
     * if you just want the string, do getString()
     */
    @Override
    public String toString() {
	// piggyback on KQMLString functionality
	KQMLString kstring = new KQMLString(getString());
	//System.err.println("spitting: XXX" + getString() + "XXX::" + kstring + "::");
	return kstring.toString();
    }

}