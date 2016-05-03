package TRIPS.AKRL;

import java.util.*;
import java.io.*;

import TRIPS.KQML.*;

/**
 * represents the provenance of an AkrlTerm, as recorded in its structure
 * i.e., where in the source utterance it came from
 */
public class AkrlProvenance implements Serializable {
    protected static final long serialVersionUID = 2;

    public static final AkrlRole START_ROLE = new AkrlRole(":START"); // start in utt string
    public static final AkrlRole END_ROLE = new AkrlRole(":END"); // end in utt string

    /**
     * the start pos in the original string (0-based, gaps between chars in string)
     * will be null if not known
     */
    protected Integer startPos;
    public Integer getStartPos() {return startPos;}
    public void setStartPos(Integer pos) {startPos = pos;}

    /**
     * the end pos in the original string (0-based, gaps between chars in string)
     * will be null if not known
     */
    protected Integer endPos;
    public Integer getEndPos() {return endPos;}
    public void setEndPos(Integer pos) {endPos = pos;}

    /**
     * creates a new provenance from a KQMLList which represents an AKRLTerm
     * this also changes the KQMLList parameter by removing all the keyword-args
     * for provenance information -- to stop them from being considered regular keywords
     * always returns a new instance, even if no information about provenance is in the kqml
     */
    public static AkrlProvenance fromKQML(KQMLList kqml) {
	AkrlProvenance retval = new AkrlProvenance();

	retval.setStartPos(removeIntegerArg(START_ROLE.toString(),kqml));
	retval.setEndPos(removeIntegerArg(END_ROLE.toString(),kqml));

	return retval;
    } // end parseProvenance

    /**
     * removes and returns an arg that is expected to be an Integer (or null if none)
     */
    protected static Integer removeIntegerArg(String keyword, KQMLList kqml) {
	KQMLObject obj = kqml.removeKeywordArg(keyword);
	if (obj == null) // not found
	    return null;
	String objString = obj.toString(); // hack -- should use instanceof
	Integer retval = Integer.parseInt(objString);
	return retval;
    }

//     /**
//      * helper routine that removes a given keyword and its arg from a KQMLList
//      * and then returns the value -- changes the KQMLList that is passed in
//      */
//     protected static KQMLObject removeKeywordArg(String keyword, KQMLList kqml) {
// 	KQMLObject retval = kqml.getKeywordArg(keyword);
// 	if (retval != null) { // it is in there
// 	    int pos = kqml.indexOfIgnoreCase(keyword);
// 	    if (pos == -1) {
// 		throw new RuntimeException("keyword: " + keyword + " found in " + kqml + 
// 					   " but position not found with indexOf -- probably an incongruence in handling of ignoreCase in KQMLList.indexOf and KQMLList.getKeywordArg");
// 	    }
// 	    kqml.remove(pos);
// 	    kqml.remove(pos); // do twice to get the arg as well
// 	}

// 	return retval;
//     } // end removeKeywordArg

    @Override
    public String toString() {
	return START_ROLE.toString() + " " + getStartPos() + " " 
	    + END_ROLE.toString() + " " + getEndPos();
    }

} // end class AkrlProvenance