package TRIPS.AKRL;

//import us.ihmc.trips.pursuit.*;
import TRIPS.KQML.*;
import java.util.*;
import java.io.*;

/**
 * this encapsulates a single type in Akrl which is either of the form
 *  TYPE or (:* TYPE MINORTYPE)
 * also some (provisional?) special handling for SET-OF
 */
public class AkrlType extends AkrlValue implements Serializable {
    protected static final long serialVersionUID = 2;

    // sample KQML
//        (F V603113 (:* IN-RELATION BE) :CO-THEME V603689 :THEME V603056
//         :TENSE W::PRES)

    /**
     * a pointer to ROOT
     */
    public static final AkrlType ROOT_TYPE = new AkrlType("ROOT");

    /**
     * the first token in a complex type reference
     */
    public static final KQMLToken COMPLEX_TYPE_REF_FIRST_TOKEN = new KQMLToken(":*");

    /**
     * the main type
     */
    protected AkrlID majorType;
    public AkrlID getMajorType() {return majorType;}

    /**
     * the minorType modifier on the type (if any)
     */
    protected AkrlID minorType = null;
    public AkrlID getMinorType() {return minorType;}

    public AkrlType() {}

    public AkrlType(String majorType) {
	this.majorType = new AkrlID(majorType);
    }

    public AkrlType(String majorType, String minorType) {
	this(majorType);
	this.minorType = new AkrlID(minorType);
    }

    /**
     * creates from an AkrlListValue of the form (* major minor)
     */
    public AkrlType(List<AkrlValue> lst) {
	this(lst.get(1).toString(),lst.get(2).toString());
    }

    /**
     * creates from KQML
     */
    public AkrlType(KQMLObject kqml) {
	if (kqml instanceof KQMLList) { // complex type
	    KQMLList klist = (KQMLList) kqml;
	    KQMLToken startToken = (KQMLToken)klist.get(0);
	    String start = startToken.getName();

	    // different cases for types
	    if (isComplexTypeRef(klist)) {
		if (klist.size() > 3)
		    throw new RuntimeException("got list with more than 3 elements: " + klist);

		majorType = new AkrlID((KQMLToken)klist.get(1));		
		if (klist.size() == 3)
		    minorType = new AkrlID((KQMLToken)klist.get(2));
	    } else if (start.equalsIgnoreCase("<OR>")) { // don't know what this means yet
 		System.out.println("warning: <OR> in type: " + kqml);
		// so far, we've only seen it with 1 other element
		if (klist.size() > 2)
		    throw new RuntimeException("<OR> list with more than 1 other element: " 
					       + klist);
		majorType = 
		    new AkrlID((KQMLToken)klist.get(1)); // just ignore <OR> now and take major
	    } else if (start.equalsIgnoreCase("SET-OF")) { // a set
		if (klist.size() != 2)
		    throw new RuntimeException("unexpected SET-OF construction");
		majorType = new AkrlID(start);
		// not sure exactly how to handle this.  subclass AkrlType????
		// right now just make minor type the thing that follows
		minorType = new AkrlID((KQMLToken)klist.get(1));
	    } else { // something we're not expecting
		throw new RuntimeException("unexpected first element of complex type: " + kqml);
	    }
	} else if (kqml instanceof KQMLToken) {
	    majorType = new AkrlID((KQMLToken)kqml);
	    minorType = null;
		} else { // unexpected
	    throw new RuntimeException("unexpected KQML type: " + kqml.getClass() + ": " + kqml);
	}
    } // end constructor

    /**
     * returns true if this list is a reference to a complex type e.g., (:* major minor)
     */
    public static boolean isComplexTypeRef(KQMLList list) {
	KQMLObject first = list.get(0);
	if ((first instanceof KQMLToken) && first.equals(COMPLEX_TYPE_REF_FIRST_TOKEN))
	    return true;
	else
	    return false;
    } // end isComplexTypeRef

    /**
     * override to compare the minor and major types
     */
    @Override
    public boolean equals(Object obj) {
	if (!(obj instanceof AkrlType)) {
	    return false;
	}

	AkrlType aobj = (AkrlType) obj;

	if (majorType.equals(aobj.getMajorType())) {
	    if (!isComplexType() && !aobj.isComplexType()) {// just majors
		return true;
	    } else if (isComplexType() && aobj.isComplexType()) { // has minor
		if (getMinorType().equals(aobj.getMinorType())) {
		    return true;
		}
	    }
	} // end if major types are equal

	return false;
    } // end equals

    /**
     * override since we override equals
     */
    @Override
    public int hashCode() {
	return (getMajorType().hashCode() * 13) + getMinorType().hashCode();
    }

    /**
     * returns true if this is a complex type
     */
    public boolean isComplexType() {
	return (minorType != null);
    }

    public String toString() {
	if (isComplexType())
	    return "(:* " + majorType + " " + minorType + ")";
	else
	    return majorType.toString();
    } // end toString


} // end class AkrlType