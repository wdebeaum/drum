package TRIPS.AKRL;

import java.util.*;
import java.io.*;

import TRIPS.KQML.*;
import TRIPS.util.StringUtils;
import TRIPS.util.Misc;

/**
 * this encapsulates a single term of AKRL, i.e., the definition of one object
 * e.g., (THE V123 :instance-of TYPE :slot1 VAL1 :slot2 VAL2)
 * it is comprised of an indicator "THE", an ID "V123" a type "TYPE"
 * and a list of slots and their values
 */
public class AkrlTerm implements Serializable, Comparable<AkrlTerm> {
    protected static final long serialVersionUID = 2;    

    // sample LF
//        (F V603113 (:* IN-RELATION BE) :CO-THEME V603689 :THEME V603056
//         :TENSE W::PRES)
    // sample AKRL
    //    (THE V603113 :instance-of BOB :author V123 :title V124)

    /**
     * in AKRL expressions, the name of the role that indicates the type
     */
    public static final String TYPE_ROLE_STRING = ":INSTANCE-OF";
    public static final AkrlRole NAME_OF_ROLE = new AkrlRole(":NAME-OF"); // coerced to strings

    /**
     * the indicator
     */
    protected AkrlIndicator indicator;
    public AkrlIndicator getIndicator() {return indicator;}
    public void setIndicator(AkrlIndicator indicator) {this.indicator = indicator;}

    /**
     * the ID of this term (e.g., V12345)
     */
    protected AkrlID id;
    public AkrlID getID() {return id;}
    public void setID(AkrlID id) {this.id = id;}

    /**
     * the object type
     */ 
    protected AkrlType type;
    public AkrlType getType() {return type;}
    public void setType(AkrlType type) {this.type = type;}

    /**
     * the provenance of the term -- where it came from in the original utterance
     */
    protected AkrlProvenance provenance;
    public AkrlProvenance getProvenance() {return provenance;}
    public void setProvenance(AkrlProvenance provenance) {this.provenance = provenance;}

    /**
     * the domain-info -- this slot is for packing in all domain-specific info
     * about the object, which may be represented in a non-standard way for LF
     * we just preserve it as it is
     */
    protected KQMLObject domainInfo;
    public KQMLObject getDomainInfo() {return domainInfo;}
    public void setDomainInfo(KQMLObject domainInfo) {this.domainInfo = domainInfo;}

    // former :TMA roles
    protected AkrlID tense;
    public AkrlID getTense() {return tense;}
    public void setTense(AkrlID tense) {this.tense = tense;}
    protected AkrlType modality;
    public AkrlType getModality() {return modality;}
    public void setModality(AkrlType modality) {this.modality = modality;}
    protected Boolean progressive;
    public Boolean getProgressive() {return progressive;}
    public void setProgressive(Boolean progressive) {this.progressive = progressive;}
    protected Boolean perfective;
    public Boolean getPerfective() {return perfective;}
    public void setPerfective(Boolean perfective) {this.perfective = perfective;}
    protected Boolean negated;
    public Boolean getNegated() {return negated;}
    public void setNegated(Boolean negated) {this.negated = negated;}
    protected Boolean passive;
    public Boolean getPassive() {return passive;}
    public void setPassive(Boolean passive) {this.passive = passive;}
    // other roles that end up in mention-info
    protected AkrlID proform;
    public AkrlID getProform() {return proform;}
    public void setProform(AkrlID proform) {this.proform = proform;}
    protected AkrlID operator;
    public AkrlID getOperator() {return operator;}
    public void setOperator(AkrlID operator) {this.operator = operator;}
    protected AkrlID quan;
    public AkrlID getQuan() {return quan;}
    public void setQuan(AkrlID quan) {this.quan = quan;}

    /**
     * the rest of the slots - mapped to their values
     */
    protected Map<AkrlRole,List<AkrlValue>> slots;
    public Map<AkrlRole,List<AkrlValue>> getSlots() {return slots;}
    public void setSlots(Map<AkrlRole,List<AkrlValue>> slots) {this.slots = slots;}

    /**
     * creates a new one from LF format
     */
    public static AkrlTerm fromLF(KQMLList kqml) {
	if (kqml.size() < 3)
	    throw new RuntimeException("need at least 3 elements for LF term: " + kqml);


	try {
	AkrlTerm retval = new AkrlTerm();
	retval.indicator = parseIndicator(kqml);
	retval.id = parseID(kqml);

	retval.type = parseType(retval.id,kqml.get(2));

	retval.provenance = parseProvenance(kqml);

	retval.domainInfo = parseDomainInfo(kqml);

	retval.parseAndSetTMA(kqml);

	retval.slots = parseSlots(retval.id,kqml.subList(3,kqml.size()));

	// check and see if we have :WNSENSE slot for the type
// 	AkrlValue sense = retval.getSlotValue(WNSENSE_ROLE);
// 	if (sense != null) {
// 	    retval.deleteSlot(WNSENSE_ROLE);
// 	    try {
// 	    retval.type = parseWNSenseType(sense);
// 	    } catch (Exception e) {
// 		System.err.println("error handling :WNSENSE as type. Using declared type instead.\n" + Misc.getStackTraceString(e));
// 	    }
// 	} // end if sense is there

	return retval;
	} catch (Exception e) {
	    throw new RuntimeException("problem processing term: " + kqml,e);
	}
    } // end fromLF

    /**
     * creates a new one from AKRL format
     */
    public static AkrlTerm fromAkrl(KQMLList kqml) {
	AkrlTerm retval = new AkrlTerm();
	retval.indicator = parseIndicator(kqml);
	retval.id = parseID(kqml);

	// get and remove the type info from the list
	int typeRolePos = kqml.indexOfString(TYPE_ROLE_STRING);
	if (typeRolePos == -1) // not found
	    throw new RuntimeException("no type declaration of " + TYPE_ROLE_STRING + " on " 
				       + kqml);
	if (typeRolePos == kqml.size() - 1) // the role is at the end of the list w/o a value
	    throw new RuntimeException("role " + TYPE_ROLE_STRING + 
				       " at end of list without a value " + kqml);
	KQMLObject typeExpr = kqml.remove(typeRolePos+1);
	kqml.remove(typeRolePos); // remove the :instance-of as well
	retval.type = parseType(retval.id,typeExpr);

	retval.provenance = parseProvenance(kqml);

	retval.domainInfo = parseDomainInfo(kqml);
	
	retval.parseAndSetTMA(kqml);

	retval.slots = parseSlots(retval.id,kqml.subList(2,kqml.size()));

	return retval;
    } // end fromAkrl


    /**
     * converts the type name into something OWL can handle
     * assumes input of |name%1:11:11::|
     * also replaces ' with - since OWL doesn't like it (for stuff like "al'qaeda")
     */
    public static String synsetToOwl(String synset) {
	String retval = synset.replaceAll("\\|","");
	retval = retval.replaceAll("::","");
 	retval = retval.replaceAll(":","--");
	retval = retval.replaceAll("%","*");
	retval = retval.replaceAll("'","-");
	retval = retval.replaceAll("\\^","-");
	return retval;
    }

    /**
     * changes a name back into a synset
     */
    public static String owlToSynset(String owl) {
	String retval = owl;
 	retval = owl.replaceAll("--",":");
	retval = retval.replaceAll("\\*","%");
	retval = "|" + retval + "::|";
	return retval;
    }

//     /**
//      * parses a :WNSENSE value as a type
//      */
//     protected static AkrlType parseWNSenseType(AkrlValue senseVal) {
// 	AkrlString senseStr = (AkrlString) senseVal;
// 	String owlStr = synsetToOwl(senseStr.getString());
// 	return new AkrlType(owlStr);
//     } // end parseWNSenseType

    /**
     * parses the type
     */
    protected static AkrlType parseType(AkrlID id, KQMLObject typeExpr) {
	try {
	return new AkrlType(typeExpr);
	} catch (AkrlNilTokenException e) {
	    System.out.println("warning: NIL detected in type on " + id + 
			       ". Setting type to ROOT. " + typeExpr);
	    return AkrlType.ROOT_TYPE;
	}
    } // end parseType

    /**
     * extracts the provenance information from the term and removes
     * that information from the original kqml so it doesn't get put
     * into the regular slot information
     */
    protected static AkrlProvenance parseProvenance(KQMLList kqml) {
	return AkrlProvenance.fromKQML(kqml);
    }

    /**
     * pulls out domain info and deletes that info from original kqml
     */
    protected static KQMLObject parseDomainInfo(KQMLList kqml) {
	KQMLObject obj = kqml.removeKeywordArg(":domain-info");
	return obj;
    }

    /**
     * parses a boolean slot value; returns true for +, false for -, and null
     * for null
     */
    protected static Boolean parseBoolean(KQMLObject kqml) {
	if (kqml == null)
	    return null;
	String str = kqml.toString();
	if (str.equals("+")) {
	    return true;
	} else if (str.equals("-")) {
	    return false;
	} else {
	    throw new RuntimeException("invalid boolean value in AKRL: " + str);
	}
    }

    /**
     * pulls out TMA slots (and a few others), deleting them from the original
     * kqml and adding them to this
     */
    protected void parseAndSetTMA(KQMLList kqml) {
	if (kqml.getKeywordArg(":tma") != null) { // :tma shouldn't come out of the parser anymore
	    kqml.removeKeywordArg(":tma");
	    System.err.println("Warning: :TMA found in AKRL: " + kqml);
	}
	if (kqml.getKeywordArg(":tense") instanceof KQMLToken)
	    tense = new AkrlID((KQMLToken)kqml.removeKeywordArg(":tense"));
	if (kqml.getKeywordArg(":modality") != null)
	    modality = new AkrlType(kqml.removeKeywordArg(":modality"));
	progressive = parseBoolean(kqml.removeKeywordArg(":progr"));
	perfective = parseBoolean(kqml.removeKeywordArg(":perf"));
	negated = parseBoolean(kqml.removeKeywordArg(":negation"));
	passive = parseBoolean(kqml.removeKeywordArg(":passive"));
	// these weren't ever in :TMA, but they're treated similarly
	if (kqml.getKeywordArg(":proform") instanceof KQMLToken)
	    proform = new AkrlID((KQMLToken)kqml.removeKeywordArg(":proform"));
	if (kqml.getKeywordArg(":operator") instanceof KQMLToken)
	    operator = new AkrlID((KQMLToken)kqml.removeKeywordArg(":operator"));
	if (kqml.getKeywordArg(":quan") instanceof KQMLToken)
	    quan = new AkrlID((KQMLToken)kqml.removeKeywordArg(":quan"));
    }

    /**
     * returns the additional slots (not including type) from the KQMLList
     * note that the list is just :keyword arg pairs (none other stuff)
     * if the list is empty, just returns an empty map
     * AkrlID id included only for error messages (so we know where it was)
     */
    protected static Map<AkrlRole,List<AkrlValue>> parseSlots(AkrlID id, List<KQMLObject> lst) {
	if (lst == null)
	    throw new RuntimeException("null list");
	if ((lst.size() % 2) != 0) // not even
	    throw new RuntimeException("not an even numbered list: " + lst);
	
	Map<AkrlRole,List<AkrlValue>> retval = new HashMap<AkrlRole,List<AkrlValue>>();
	Iterator<KQMLObject> iter = lst.iterator();
	while (iter.hasNext()) {
	    KQMLToken roleToken = (KQMLToken) iter.next();
	    KQMLObject valueExpr = iter.next();
	    try {
		AkrlRole role = new AkrlRole(roleToken);
		List<AkrlValue> list = retval.get(role);
		if (list == null)
		    list = new ArrayList<AkrlValue>();
		if (role.equals(NAME_OF_ROLE)) {
		    // hack :name-of values to strings
		    list.add(makeNameOfValue(valueExpr));
		} else if (valueExpr instanceof KQMLList) {
		    KQMLList valueList = (KQMLList)valueExpr;
		    if (valueList.size() == 3 &&
			valueList.get(0) instanceof KQMLToken &&
			valueList.get(0).toString().equals(":*")) {
			// (:* ONT::foo W::bar) type
			list.add(new AkrlType(valueList));
		    } else {
			// list of values
			for (KQMLObject obj : valueList) {
			    // in the case of NIL in the list, skip -- sending it to AkrlValue leads to AkrlNilTokenException
			    if(obj != null && obj.stringValue().equalsIgnoreCase("NIL"))
				continue;
			 
			    list.add(AkrlValue.createNew(obj));
			}
		    }
		} else {
		    // single value
		    list.add(AkrlValue.createNew(valueExpr));
		}

		retval.put(role, list);
	    } catch (AkrlNilTokenException e) {
		System.out.println("warning: NIL detected on role-value pair on " + id +
				   ". Ignoring. (" + roleToken + " " + valueExpr + ")");
	    } catch (Exception e) {
		System.out.println("warning: problem processing role-value pair: (" + roleToken
				   + ", " + valueExpr + ") on " + id + ". Ignoring.\n    Error: " + 
				   Misc.getStackTraceString(e));
	    }
	} // end for each role/value pair

	return retval;
    } // end parseSlots

    /**
     * converts the token of :name-of into a string (temp until :name-of gets changed)
     */
    protected static AkrlString makeNameOfValue(KQMLObject valExpr) {
	if (valExpr instanceof KQMLString) {
	    return new AkrlString((KQMLString)valExpr);
	} else if (valExpr instanceof KQMLToken) {
	    String valstr = ((KQMLToken)valExpr).getName();
	    String retstr = valstr.replaceAll("_"," ");
	    retstr = retstr.replaceAll("\\^","'");
	    AkrlString retval = new AkrlString(retstr);
	    return retval;
	} else if (valExpr instanceof KQMLList) { // list
	    List<String> strVals = new ArrayList<String>();
	    for (KQMLObject val : (KQMLList)valExpr) {
		strVals.add(makeNameOfValue(val).getString());
	    }
	    return new AkrlString(StringUtils.join(" ",strVals));
	} else {
	    throw new RuntimeException("unexpected obj type: " + valExpr.getClass() + 
				       " " + valExpr);
	}
    } // end makeNameOfValue

    /**
     * returns an indicator from KQML (LF or AKRL)
     */
    protected static AkrlIndicator parseIndicator(KQMLList kqml) {
	AkrlIndicator retval = new AkrlIndicator((KQMLToken)kqml.get(0));
	return retval;
    }

    /**
     * returns an ID from KQML (LF or AKRL)
     */
    protected static AkrlID parseID(KQMLList kqml) {
	return new AkrlID((KQMLToken)kqml.get(1));
    }

    /**
     * to implement Comparable
     */
    public int compareTo(AkrlTerm o) {
	return getID().compareTo(o.getID());
    } 

    /**
     * adds a value to this slot
     */
    public void addSlotValue(AkrlRole slot, AkrlValue value) {
	List<AkrlValue> curVals = getSlotValues(slot);
	if (curVals == null) { // nothing there yet
	    curVals = new ArrayList<AkrlValue>();
	    setSlotValues(slot,curVals);
	}
	curVals.add(value);
    } // end addSlotValue

    /**
     * returns the list of values on this slot
     * returns null if there are no values here
     */
    public List<AkrlValue> getSlotValues(AkrlRole slot) {
	return getSlots().get(slot);
    }

    /**
     * returns the *first* slot value for this (if multiple)
     * or null if none -- this is a nice shortcut if you know that
     * the role only has at most one value
     */
    public AkrlValue getSlotValue(AkrlRole slot) {
	List<AkrlValue> vals = getSlotValues(slot);
	if (vals == null)
	    return null;
	else
	    return vals.get(0);
    }

    /**
     * this removes the slot from the term as well as any values it might have had
     */
    public void deleteSlot(AkrlRole slot) {
	getSlots().remove(slot);
    }

    /**
     * sets the given list as the new value for that slot (erasing anything else that was
     * there before)
     */
    public void setSlotValues(AkrlRole slot, List<AkrlValue> values) {
	getSlots().put(slot,values);
    }

    @Override
    public String toString() {
	String retval = "";
	retval += "(" + indicator + " " + id + " " + TYPE_ROLE_STRING + " "+ type;

	for (Map.Entry<AkrlRole,List<AkrlValue>> entry : slots.entrySet()) {
	    List<AkrlValue> values = entry.getValue();
	    retval += " " + entry.getKey() + " ";
	    if (values.size() == 1) {
		retval += values.get(0) + " ";
	    } else {
		retval += "(";
		retval += StringUtils.join(" ",values);
		retval += ")";
	    }
	} // end for each slot
	
	retval += " " + getProvenance();

	retval += ")";
	 return retval;
    } // end toString

} // end class AkrlTerm
