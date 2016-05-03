package TRIPS.AKRL;

import TRIPS.KQML.*;
import java.util.*;
import java.io.*;

/**
 * parent class for "simple" (non-list) slot filler values
 * (needed since AKRL does not nest lists)
 */
public abstract class AkrlValue {

    /**
     * creates a new instance of the appropriate subtype
     */
    public static AkrlValue createNew(KQMLObject kqml) {
	AkrlValue retval = null;

	if (kqml instanceof KQMLToken) {
	    Double val = AkrlNumber.parseNumber((KQMLToken)kqml);
	    if (val != null) 
		retval = new AkrlNumber(val);
	    else
		retval = new AkrlID((KQMLToken)kqml);
	} else if (kqml instanceof KQMLString) { // string
	    retval = new AkrlString((KQMLString)kqml);
	} else { // unexpected
	    throw new RuntimeException("unexpected type of kqmlObject: " + kqml.getClass() +
				       ": " + kqml);
	}

	return retval;
    } // end createNew

}