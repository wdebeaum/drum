package TRIPS.OWL;

import java.util.*;

import edu.stanford.smi.protegex.owl.model.*;
import edu.stanford.smi.protegex.owl.*;

import TRIPS.AKRL.*;


/**
 * static class with methods for asserting the contents of Akrl into the KB
 * all methods assume the TripsOnotlogy has been created
 */
public class AkrlAsserter {
    /**
     * things that we need to treat specially
     */
    public static final AkrlIndicator KindIndicator = new AkrlIndicator("KIND");

    /**
     * roles that we should ignore (pretend they and their values aren't there)
     * when asserting to OWL
     */
    public static final List<AkrlRole> ROLES_TO_IGNORE = 
	Arrays.asList(new AkrlRole(":tma"));


    /**
     * keep track of the things that we had to create on our own (and we shouldn't have had to)
     */
    public static Set<AkrlRole> createdProperties = new HashSet<AkrlRole>();
    public static Set<AkrlID> createdTypes = new HashSet<AkrlID>();
    public static Set<AkrlID> createdIndividuals = new HashSet<AkrlID>();
    public static Set<AkrlID> createdGensyms = new HashSet<AkrlID>();
    public static Set<String> nameClashes = new HashSet<String>();
    public static Set<String> possibleNameClashes = new HashSet<String>();

    /**
     * asserts whatever is in the AKRL (must process all together since we have
     * a graph)
     */
    public static void assertAkrlBundle(AkrlBundle bundle) {
	assertAkrlBundle(bundle,null);
    }

    /**
     * asserts whatever is in the AKRL, including the ID for the utterance
     */
    public static void assertAkrlBundle(AkrlBundle bundle, String utteranceID) {
	// create all the "stubs"
	for (AkrlTerm term : bundle.getTerms()) {
	    createStubFromTerm(term,utteranceID);
	}

	// go through and assert the slots
	for (AkrlTerm term : bundle.getTerms()) {
	    assertSlots(term);
	}	
    } // end assertAkrlBundle

    /**
     * asserts the slots listed on this term
     */
    protected static void assertSlots(AkrlTerm term) {
	AkrlID id = term.getID();
	try {
	Map<OntProperty,List<Object>> slotsVals = computeSlotsAndValues(term);

	OntObject thisObj = OntObject.getNew(id.getID());
	if (thisObj instanceof OntType)
 	    assertSlotsOnKind((OntType)thisObj,slotsVals);
	else if (thisObj instanceof OntIndividual)
	    assertSlotsOnIndividual((OntIndividual)thisObj,slotsVals);
	else
	    throw new RuntimeException("unexpected object class " + thisObj.getClass() + 
				       " for term " + term);
	} catch (Exception e) {
	    throw new RuntimeException("problem asserting slots on term: " + term,e);
	}
    } // end assertSlots

    public static void assertSlotsOnKind(OntType kind, 
					    Map<OntProperty,List<Object>> slotsVals) {
	for(Map.Entry<OntProperty,List<Object>> entry : slotsVals.entrySet()) {
	    OntProperty prop = entry.getKey();
	    List<Object> vals = entry.getValue();

	    for(Object val : vals) {
		OWLRestriction restriction;
		if (val instanceof String) {
		    restriction = TripsOntology.ont.model.createOWLHasValue(prop.getResource(),val);
		} else if (val instanceof Number) {
		    OntIndividual newInd = createNumber((Number)val);
		    restriction = 
			TripsOntology.ont.model.createOWLHasValue(prop.getResource(),
								  newInd.getResource());
		} else if (val instanceof OntIndividual) {
		    restriction = 
			TripsOntology.ont.model.createOWLHasValue(prop.getResource(),
							    ((OntIndividual)val).getResource());
		} else if (val instanceof OntType) {
		    restriction = 
			TripsOntology.ont.model.createOWLAllValuesFrom(prop.getResource(),
							       ((OntType)val).getResource());
		} else if (val instanceof RDFResource) {
		    restriction =
			TripsOntology.ont.model.createOWLAllValuesFrom(prop.getResource(),
								       (RDFResource)val);
		} else {
		    throw new RuntimeException("unexpected type class " + val.getClass() + 
					       " on " + val);
		}
		kind.getResource().addSuperclass(restriction);
	    } // end for each val
	} // end for each entry
    } // end assertSlotsOnKind

    protected static void assertSlotsOnIndividual(OntIndividual ind, 
						  Map<OntProperty,List<Object>> slotsVals) {

	for(Map.Entry<OntProperty,List<Object>> entry : slotsVals.entrySet()) {
	    OntProperty prop = entry.getKey();
	    List<Object> vals = entry.getValue();

	    for(Object val : vals) {
		try {
		if (val instanceof String) {
		    ind.addPropertyValue(prop,(String)val);
		} else if (val instanceof Number) {
		    OntIndividual newInd = createNumber((Number)val);
		    ind.addPropertyValue(prop,newInd);
		} else if (val instanceof OntIndividual) {
		    ind.addPropertyValue(prop,(OntIndividual)val);
		} else if (val instanceof OntType) {
		    OntIndividual slotValue = ((OntType)val).createIndividual(gensym("A"));
		    ind.addPropertyValue(prop,slotValue);
		} else {
		    throw new RuntimeException("unexpected type class " + val.getClass() + 
					       " on " + val);
		}
		} catch (Exception e) {
		    throw new RuntimeException("problem asserting slot: (" + prop + "," +
					       val + ")",e);
		}
	    } // end for each val
	} // end for each entry
    } // end assertSlotsOnIndividual

    /**
     * returns a map of ontproperty and a list of values (String, OntType, or OntIndividual)
     */
    protected static Map<OntProperty,List<Object>> computeSlotsAndValues(AkrlTerm term) {
	Map<OntProperty,List<Object>> retval = new HashMap<OntProperty,List<Object>>();
	
	for(Map.Entry<AkrlRole,List<AkrlValue>> entry : term.getSlots().entrySet()) {
	    AkrlRole arole = entry.getKey();
	    
	    if (ROLES_TO_IGNORE.contains(arole)) // skip this role and its values
		continue;

	    List<AkrlValue> aval = entry.getValue();

	    try {
	    OntProperty prop = OntProperty.getNew(arole);
	    if (prop == null) {
		prop = OntProperty.createNew(arole);
		createdProperties.add(arole);
	    }

	    List<Object> vals = computeValues(aval);
	    if (!vals.isEmpty())
		retval.put(prop,vals);
	    } catch (Exception e) {
		System.out.println("warning: problem computing slot: (" + 
				   arole.toString().toUpperCase() + " "
				   + aval.toString().toUpperCase() + "). Ignoring. "
				   + "\n         Error message: " + e.getMessage());
	    }
	} // end for each
	return retval;
    } // end computeSlotsAndValues

    /**
     * returns a list of Object (String, OntType, or OntIndividual) which are the
     * resolved values for the stuff in aval
     */
    protected static List<Object> computeValues(List<AkrlValue> aval) {
	List<Object> retval = new ArrayList<Object>();

	for (AkrlValue simpleVal : aval) {
	    Object val = computeValue(simpleVal);
	    if (val != null)
		retval.add(val);
	}

	return retval;
    } // end computeValues

    /**
     * computes for a single value
     */
    protected static Object computeValue(AkrlValue aval) {
	try {
	if (aval instanceof AkrlString) {
	    return aval.toString();
	} else if (aval instanceof AkrlNumber) {
	    return ((AkrlNumber)aval).getNumber();
	} else if (aval instanceof AkrlType) {
	    return computeType((AkrlType)aval);
	} else if (aval instanceof AkrlID) {
	    AkrlID thisID = (AkrlID)aval;
	    OntObject obj = OntObject.getNew(thisID.getID());
	    if (obj == null) { // doesn't exist
		if (isGensymID(thisID)) {
		    createdGensyms.add(thisID);
		} else {
		    createdIndividuals.add(thisID);
		}
	    } // end if null
	    return obj;
	} else {
	    throw new RuntimeException("unexpected type: " + aval.getClass() + " on "
				       + aval);
	}

	} catch (OntNameClashException e) {
	    nameClashes.add(e.name);
	    return null;
	} catch (Exception e) {
	    throw new RuntimeException("problem on computing value for " + aval);
	}
    } // end computeValue

    /**
     * returns true if this looks like a GenSym ID from LF (e.g., v123...)
     */
    public static boolean isGensymID(AkrlID id) {
	return id.getID().matches("^v\\d+$");
    }


    /**
     * handles the case where a type has been used 'inline' in a slot value
     * we create an instance of the type and return it
     */
    protected static AkrlID handleInlineType(List<AkrlValue> lst) {
	AkrlType atype = new AkrlType(lst);
	OntType type = computeType(atype);
	String newName = gensym("N");
	AkrlID retval = new AkrlID(newName);
	createIndividual(type,retval);
	return retval;
    } // end handleInlineTYpe

    /**
     * returns an AkrlID to point to the obj
     */
    protected static AkrlID getAkrlID(OntObject obj) {
	return new AkrlID(obj.getName());
    }

    /**
     * returns a type for the akrl type (creating it if necessary)
     */
    public static OntType computeType(AkrlType atype) {
	return computeType(atype, false);
    } // end computeType

    public static OntType computeType(AkrlType atype, boolean ignoreMinorType) {
	OntType majorType =  OntType.getNew(atype.getMajorType().getID());
	if (majorType == null) { // type not there
// 	    throw new RuntimeException("type not found: " + atype.getMajorType());
// 	    System.out.println("warning: type " + atype.getMajorType().toString().toUpperCase() + 
// 			       " not found. Creating.");
	    majorType = OntType.createNew(atype.getMajorType().getID());
	    majorType.setParent(TripsOntology.ont.getRoot());
	    createdTypes.add(atype.getMajorType());
	}

	if (atype.isComplexType() && !ignoreMinorType) { // has a minor type
	    String minorTypeName = majorType.getName() + "." + atype.getMinorType().getID();
	    OntType minorType = OntType.getOrCreateNew(minorTypeName);
	    if (minorType.getParent() == null) {
		OntType parentType = majorType.getDefaultClass(); // should go off parent
		if (parentType == null)
		    parentType = majorType;
		minorType.setParent(parentType);
	    }

	    return minorType;
	} else {
	    return majorType;
	}
    } // end computeType

    /**
     * creates an indivdiaul for the ID
     */
    protected static OntIndividual createIndividual(OntType type, AkrlID id) {
	OntIndividual ind = OntIndividual.getNew(id.getID());
	if (ind != null) { // already something there
	    System.out.println("warning: redefinition of individual: " + id + 
			       ". Deleteing the original and replacing with the new.");
	    ind.getResource().delete();
	}
	ind = type.createIndividual(id.getID());
	return ind;
    } // end createIndividual

    /**
     * creates the individual from the term and makes it of given type
     * does not assert any of the slots
     * also asserts the utteranceID if not null
     */
//     @SuppressWarnings("fallthrough")
    protected static void createStubFromTerm(AkrlTerm term) {
	createStubFromTerm(term,null);
    }

    protected static void createStubFromTerm(AkrlTerm term, String utteranceID) {
	try {
	AkrlIndicator indicator = term.getIndicator();
	AkrlID id = term.getID();
	AkrlProvenance provenance = term.getProvenance();
	AkrlType atype = term.getType();
	OntType type = computeType(atype);
	OntIndividual mentionInfo = createMentionInfo(term, utteranceID);
        OntProperty hasMentionInfo = OntProperty.getNew("_has-mention-info");

	if (indicator.equals(KindIndicator)) {
	    OntType kind = createKindStub(id, type);
	    OWLRestriction restriction = TripsOntology.ont.model.createOWLHasValue(hasMentionInfo.getResource(), mentionInfo.getResource());
	    kind.getResource().addSuperclass(restriction);
	} else { // default case
	    OntIndividual ind = createIndividual(type, id);
	    ind.addPropertyValue(hasMentionInfo, mentionInfo);
	}
	} catch (Exception e) {
	    throw new RuntimeException("problem asserting: " + term,e);
	}
    } // end createStubFromTerm

    /**
     * this pulls out (destructively) the mention info from the AkrlTerm
     * and asserts it in a newly created instnace of MentionInfo, which it returns
     */
    protected static OntIndividual createMentionInfo(AkrlTerm term, String utteranceID) {
	OntIndividual retval = 
	    OntType.getNew("ont-mention-info").createIndividual(gensym("ont-mention-info"));

	OntIndividual indicator = createIndicator(term.getIndicator());
	retval.addPropertyValue(OntProperty.getNew("_has-indicator"),indicator);
	// assert the indicator
// 	retval.addPropertyValue(OntProperty.getNew("_has-indicator"),
// 				OntIndividual.getNew(term.getIndicator().getIndicator().getID()));
// NB - need to create new indicator as instance of ont-indicator -- need to separate out a new
// OntIndicator class, unfortunately

	OntIndividual provenance = createProvenance(term.getProvenance());
	retval.addPropertyValue(OntProperty.getNew("_has-provenance"),provenance);

	if (term.getDomainInfo() != null)
	    retval.addPropertyValue(OntProperty.getNew("_has-domain-info"), term.getDomainInfo().toString().replaceAll("\"","\\\""));

	if (utteranceID != null)
	    retval.addPropertyValue(OntProperty.getNew("_from-utterance-id"), utteranceID);

	// former :TMA roles
	if (term.getTense() != null) {
	    OntIndividual tense = createTense(term.getTense());
	    retval.addPropertyValue(OntProperty.getNew("_has-tense"), tense);
	}
	if (term.getModality() != null) {
	    OntType modalityType = computeType(term.getModality());
	    OntIndividual modality = modalityType.createIndividual(gensym("N"));
	    retval.addPropertyValue(OntProperty.getNew("_has-modality"), modality);
	}
	if (term.getProgressive() != null)
	    retval.addPropertyValue(OntProperty.getNew("_is-progressive"), term.getProgressive());
	if (term.getPerfective() != null)
	    retval.addPropertyValue(OntProperty.getNew("_is-perfective"), term.getPerfective());
	if (term.getNegated() != null)
	    retval.addPropertyValue(OntProperty.getNew("_is-negated"), term.getNegated());
	if (term.getPassive() != null)
	    retval.addPropertyValue(OntProperty.getNew("_is-passive"), term.getPassive());

        // other roles with special values
	if (term.getProform() != null)
	    addIDValuedProperty(retval, "proform", term.getProform());
	if (term.getOperator() != null)
	    addIDValuedProperty(retval, "operator", term.getOperator());
	if (term.getQuan() != null)
	    addIDValuedProperty(retval, "quan", term.getQuan());

	return retval;
    }

    protected static OntIndividual createIndicator(AkrlIndicator ind) {
	String name = "ont-" + ind.toString();
	OntIndividual retval = OntIndividual.getNew(name);
	if (retval == null) { // doesn't exist yet
	    System.out.println("warning: indicator not found in ontology, creating: " + name);
	    OntType indicatorClass = OntType.getNew("ont-indicator");
	    retval = indicatorClass.createIndividual(name);	    
	}

	return retval;
    }

    /** Get or create a box for the given Number, so we can put it in an object property. */
    protected static OntIndividual createNumber(Number number) {
	String name = "ont-number-" + number.toString().replace(".","_");
	OntIndividual retval = OntIndividual.getNew(name);
	if (retval == null) {
	    OntType type = OntType.getOrCreateNew("ont-data");
	    retval = OntIndividual.createNew(type, name);
	    retval.addPropertyValue(OntProperty.getNew("_has-value"), number);
	}
	return retval;
    }

    protected static OntIndividual createTense(AkrlID id) {
	String str = id.toString().toLowerCase();
	if (str.equals("past")) {
	    return OntIndividual.getNew("ont-past");
	} else if (str.equals("pres")) {
	    return OntIndividual.getNew("ont-present");
	} else if (str.equals("fut")) {
	    return OntIndividual.getNew("ont-future");
	} else {
	    System.out.println("warning: unknown tense ID, creating: ont-" + str);
	    return OntIndividual.getOrCreateNew(OntType.getNew("ont-tense"), "ont-" + str);
	}
    }

    /**
     * Add a property to subject of the form _has-predicate whose value is an
     * instance of the class ont-predicate named ont-object
     */
    protected static void addIDValuedProperty(OntIndividual subject, String predicate, AkrlID object) {
	subject.addPropertyValue(
	    OntProperty.getNew("_has-" + predicate),
	    OntIndividual.getOrCreateNew(
		OntType.getNew("ont-" + predicate),
		"ont-" + object.toString().toLowerCase()
	    )
	);
    }

    /**
     * creates a new provenance individual
     */
    protected static OntIndividual createProvenance(AkrlProvenance provenance) {

	OntIndividual retval = 
	    OntType.getNew("ont-provenance").createIndividual(gensym("ont-provenance"));

	if (provenance.getStartPos() != null)
	    retval.addPropertyValue(OntProperty.getNew("_has-start"),
				    provenance.getStartPos().toString());
	if (provenance.getEndPos() != null)
	    retval.addPropertyValue(OntProperty.getNew("_has-end"),
				    provenance.getEndPos().toString());

	return retval;
    }

    /**
     * creates a stub for KIND (just the name, and superclass)
     */
    public static OntType createKindStub(AkrlID id, OntType superType) {
	OntType type = OntType.getNew(id.getID());
	if (type != null) { // already something there
	    System.out.println("warning: redefinition of defined type: " + id + 
			       ". Deleteing the original and replacing with the new.");
	    type.getResource().delete();
	}
	type = OntType.createNew(id.getID());
	type.setParent(superType);
	return type;
    } // end createKindStub

    /**
     * global counter for gensym
     */
    private static long gensymCounter = 0;

    /**
     * creates a new (hopefully unique) string prefixed by the prefix
     * works by incrmenting a global counter
     */
    public static String gensym(String prefix) {
	gensymCounter++;
	return prefix + gensymCounter;
    }

    /**
     * creates a class that's equivalent to a union class
     */
    public static OntType createUnionEquivalent(String id, List<AkrlType> values) {
	OntType equivalentType = OntType.createNew(id);
	OWLNamedClass namedEquivalent = equivalentType.getResource();
	ArrayList<OWLNamedClass> unions = new ArrayList<OWLNamedClass>();
	
	for(AkrlType value : values) {
	    OntType type = computeType(value, true);
	    System.out.println("Create type for union: " + type.toString());
	    unions.add(type.getResource());
	}

	equivalentType.setParent(TripsOntology.ont.getRoot());
	OWLUnionClass unionClass = TripsOntology.ont.model.createOWLUnionClass(unions);
	namedEquivalent.addEquivalentClass(unionClass);

	return equivalentType;
    }

    /**
     * creates a class that's equivalent to an intersection class
     */
    public static OntType createIntersectionEquivalent(String id, List<AkrlType> values) {
	OntType equivalentType = OntType.createNew(id);
	OWLNamedClass namedEquivalent = equivalentType.getResource();
	ArrayList<OWLNamedClass> intersections = new ArrayList<OWLNamedClass>();
	
	for(AkrlType value : values) {
	    OntType type = computeType(value, true);
	    System.out.println("Create type for intersection: " + type.toString());
	    intersections.add(type.getResource());
	}

	//equivalentType.setParent(TripsOntology.ont.getRoot());
	OWLIntersectionClass intersectionClass = TripsOntology.ont.model.createOWLIntersectionClass(intersections);
	namedEquivalent.addEquivalentClass(intersectionClass);

	return equivalentType;
    }
 } // end class
