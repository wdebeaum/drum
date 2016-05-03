package TRIPS.AKRL;

//import us.ihmc.trips.pursuit.*;
import TRIPS.KQML.*;
import java.util.*;
import java.io.*;

/**
 * this represents the indicator of an AKRL term
 * (i.e., the first element in the list)
 */
public class AkrlIndicator implements Serializable {
    protected static final long serialVersionUID = 2;

//         SPEECHACT, THE, A, RELN, KIND,
// 	SA_SEQ,
// 	IMPRO, QUANTIFIER, WH_TERM, WH_QUANTITY, QUANTITY_TERM,
// 	PRO,
// 	INDEF_SET, THE_SET, PRO_SET,
// 	BARE, UNKNOWN;

    /**
     * the actual indicator
     */
    protected AkrlID indicator;
    public AkrlID getIndicator() {return indicator;}

    /**
     * create from a string (assumes the ONT:: is stripped off)
     */
    public AkrlIndicator(String str) {
	indicator = new AkrlID(str);
    }

    /**
     * create from a KQMLToken
     */
    public AkrlIndicator(KQMLToken token) {
	indicator = new AkrlID(token);
    }

    @Override
    public boolean equals(Object o) {
	if (!(o instanceof AkrlIndicator))
	    return false;
	AkrlIndicator a = (AkrlIndicator)o;
	boolean retval = getIndicator().equals(a.getIndicator());
	return retval;
    }

    @Override
    public int hashCode() {
	return getIndicator().hashCode();
    }

    @Override
    public String toString() {
	return getIndicator().toString();
    }

} // end class AkrlIndicator