package TRIPS.AKRL;

import TRIPS.KQML.*;
import java.util.*;
import java.io.*;

/**
 * a numerical value in AKRL.  Meant to cover any number than can show up in the KQML
 * which seems to include any Lisp number format.  So far, we cover doubles and
 * Lisp ratios (e.g., 1/2 has shown up from "half")
 */
public class AkrlNumber extends AkrlValue implements Serializable {
    protected static final long serialVersionUID = 2;

    /**
     * the value we're wrapping
     */
    protected Double number;
    public Double getNumber() {return number;}

    public AkrlNumber(Double number) {
	this.number = number;
    }

    /**
     * create from a KQMLToken
     */
    public AkrlNumber(KQMLToken number) {
	this.number = parseNumber(number);
	if (this.number == null)
	    throw new RuntimeException("not able to parse this as a number: " + number);
    }

    /**
     * parses the KQMLToken to a number (in lisp format, including "1/2") 
     * and returns the value as a double
     * if it is not in the form of a number, just returns null
     * note: this is far from complete! only handles java parsable numbers and
     * ratios (e.g., 1/2)
     */
    public static Double parseNumber(KQMLToken number) {
	try {
	Double retval;
	if (number.hasPackage())
	    return null;
	String numStr = number.toString();
	int slashPos = numStr.indexOf('/');
	if (slashPos == -1) { // just a regular number
	    retval = Double.parseDouble(numStr);
	} else { // a ratio
	    String numeratorStr = numStr.substring(0,slashPos);
	    String denomStr = numStr.substring(slashPos+1,numStr.length());
	    Double numerator = Double.parseDouble(numeratorStr);
	    Double denom = Double.parseDouble(denomStr);
	    retval = numerator / denom;
	}
	return retval;
	} catch (NumberFormatException e) {
	    return null; // we weren't able to parse it as a number
	}
    } // end parseNumber

    @Override
    public boolean equals(Object o) {
	if (!(o instanceof AkrlNumber))
	    return false;
	return getNumber() == ((AkrlNumber)o).getNumber();
    }

    @Override
    public int hashCode() {
	return getNumber().hashCode();
    }

    /**
     * returns a quoted string (with appropriate escaping
     * if you just want the string, do getString()
     */
    @Override
    public String toString() {
	return getNumber().toString();
    }

}