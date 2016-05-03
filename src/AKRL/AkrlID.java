package TRIPS.AKRL;

import TRIPS.KQML.*;
import java.util.*;
import java.io.*;

/**
 * this encapsulates an ID for referring to an AKRL term, or possibly a type in the ontology
 */
public class AkrlID extends AkrlValue implements Serializable, Comparable<AkrlID> {
    protected static final long serialVersionUID = 2;

    /**
     * the actual ID
     */
    protected String id;
    public String getID() {return id;}
    public void setID(String id) {
	checkValidID(id);
	this.id = id.toLowerCase();
    }

    /**
     * create from a string
     */
    public AkrlID(String id) {
	checkValidID(id);
	setID(id);
    }

    /**
     * create from a KQMLToken
     */
    public AkrlID(KQMLToken id) {
	this(id.getName());
    }

    /**
     * checks that the given string is a valid ID and throws and exception if not
     */
    protected void checkValidID(String id) {
	if (id.equalsIgnoreCase("NIL"))
	    throw new AkrlNilTokenException("NIL is not a valid ID");
	// this one doesn't work, since we get 401_EAST_CHASE_STREET as an ID
// 	if (!id.matches("[a-zA-Z+\\-*].*")) // has to start with a letter or a *
// 	    throw new RuntimeException("not a valid ID: XX" + id + "XX");
    }

    @Override
    public boolean equals(Object o) {
	if (!(o instanceof AkrlID))
	    return false;
	AkrlID a = (AkrlID)o;
	boolean retval = getID().equalsIgnoreCase(a.getID());
	return retval;
    }

    @Override
    public int hashCode() {
	return getID().toUpperCase().hashCode();
    }

    /**
     * to implement Comparable
     */
    public int compareTo(AkrlID o) {
	return id.compareTo(o.id);
    } 


    @Override
    public String toString() {
	return getID();
    }

} // end class AkrlID