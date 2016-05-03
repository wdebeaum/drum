package TRIPS.OWL.lispont;

import java.util.*;
import java.io.*;
import edu.stanford.smi.protegex.owl.model.*;
import edu.stanford.smi.protegex.owl.*;

import TRIPS.util.Misc;
import TRIPS.OWL.sexp.*;
import TRIPS.OWL.*;

/**
 * this class is used to export a LispOntology to a TripsOntology
 * use one of the static from... methods
 */
public class TripsOntologyExporter {

    /**
     * global variable to keep track of the ontolgoy we're creating
     */
    TripsOntology tripsOnt;
    LispOntology lispOnt;

    /**
     * globals to keep track of temp classes/properties we use in processing
     */
    Map<OntFeature,List<OntNamedFeatureListType>> featureLists = 
	new HashMap<OntFeature,List<OntNamedFeatureListType>>();

    /**
     * this class only instantiated internally -- use static methods from outside
     */
    private TripsOntologyExporter(LispOntology lispOnt) {
	//tripsOnt = TripsOntology.createNew();
 	tripsOnt = TripsOntology.fromCoreFile("coreOntology.owl");
	this.lispOnt = lispOnt;

    } // end constructor

    /**
     * creates and returns new ontology from a lisp ontology file
     */
    public static TripsOntology fromLispOntologyFile(String filename) {
	LispOntology lispOnt = LispOntology.fromFile(filename);

	return fromLispOntology(lispOnt);
    } // end fromLispOntologyFile

    /**
     * converts from a lisp ontology
     */
    public static TripsOntology fromLispOntology(LispOntology lispOnt) {
	TripsOntologyExporter importer = new TripsOntologyExporter(lispOnt);
	importer.doImport();

	return importer.tripsOnt;
    } // end fromLispOntology

    /**
     * performs the import
     */
    protected void doImport() {
	OntObject.inBootstrapMode = true;

	// hack to cover datatype properties until we get the information represented in LispOnt
// 	TripsOntology.ont.model.createOWLDatatypeProperty("_name-of");

	// first pass
	System.out.println("\nprocessing basic taxonomy");
	processBasicTaxonomy();

	// classify
	tripsOnt.classifyTaxonomy();
	checkForNothings();
	    
	// another pass: get default stuff
	System.out.println("\nprocessing defaults");
 	processSemDefaults();

	// classify
	tripsOnt.classifyTaxonomy();
	checkForNothings();

	// the arguments
	System.out.println("\nprocessing arguments");
 	processAllArguments();
	processFeatureArguments();

	OntObject.inBootstrapMode = false;
    } // end doImport

    /**
     * checks to see if any equivalent classes were computed for NOTHING
     * and prints them out as a warning
     */
    @SuppressWarnings("unchecked")
    protected void checkForNothings() {
	OWLNamedClass nothing = tripsOnt.model.getOWLNothing();
	Collection<RDFResource> equivs = nothing.getInferredEquivalentClasses();
	if (!equivs.isEmpty())
	    warn("the following classes were inferred to be impossible: " + 
		 OntObject.getNewList(equivs));
    } // end checkForNothings

    /**
     * goes through and applies all feature arguments to any matching type
     */
    protected void processFeatureArguments() {
	for (DefineFeatureArgument def : lispOnt.defineFeatureArgumentDecls) {
	    try {

	    OntNamedFeatureValue val = def.featureValueRef.getReferent();
	    List<OWLRestriction> args = new ArrayList<OWLRestriction>();
	    for (TypeArgument typeArg : def.arguments) {
		args.add(typeArg.createRestrictionClass());
	    }
	    
	    List<OntNamedFeatureValue> matchingVals = val.getTrueChildren(true);
	    matchingVals.add(0,val); // include top val in search as well

	    // go through each of the matching types and get the Types that declare
	    // them -- note, we shoudl probably be doing this with reasoning...
	    Set<OntType> types = new HashSet<OntType>();
	    for (OntNamedFeatureValue matchingVal : matchingVals) {
		types.addAll(matchingVal.getTypeChildren());
	    }

	    for (OntType type : types) {
		for (OWLRestriction arg : args) {
		    type.getResource().addSuperclass(arg);
		}
	    } // end for each type

	    } catch (Exception e) {
		throw new RuntimeException("problem processing feature arg: " + def,e);
	    }
	} // end for each def
    } // end processFeatureArguments

    /**
     * goes throug hte defaults on :sem and puts them in the owl ontology
     * by pushing down the defaults until they can be asserted for real
     * and making 'dummy' default children for each non-terminal
     */
    protected void processSemDefaults() {
	//createDefaultClasses(tripsOnt.getRoot());

	// go top-down for each feature and figure out where to assert the defaults
	List<OntFeature> features = tripsOnt.getFeatures();
	for (OntFeature feature : features) {
	    try {
	    processSemDefault(tripsOnt.getRoot(),feature,null);
	    } catch (Exception e) {
		throw new RuntimeException("problem processing default for " + feature,e);
	    }
	} // end for each feature

    } // end processSemDefaults

    /**
     * processes the single feature value in the subtree and returns true
     * if the subtree is all 'consistent' (subsumedBy) with the given defaultVal
     * if defaultVal == null - means that no default value had been specified higher
     * up in the tree.  if that's the case, always returns false
     */
    protected boolean processSemDefault(OntType root, OntFeature feature, 
					OntFeatureValue defaultVal) {
	try {
	// what for this feature is asserted on this node
	OntFeatureValue thisRequiredVal = root.getFeatureValueParent(feature);

	// what for this feature is asserted on this node as :default
	OntFeatureValue thisDefaultVal = root.getDefaultFeatureValue(feature);

	// fill in thisDefaultValue from featureListType if nothing set above or on this
	if ((thisRequiredVal == null) && (thisDefaultVal == null) && (defaultVal == null)) {
	    OntFeatureListType flType = root.getFeatureListTypeParent(); // must be explicit on this
	    if ((flType != null) // has declared flType
		&& (flType instanceof OntNamedFeatureListType)) { // ignore unions
		thisDefaultVal = ((OntNamedFeatureListType)flType).getDefaultFeatureValue(feature);
	    }
	} // end if use default on featureListType

        // check for errors in ontology

	// does it have a declared :required and :default of the same type?
	// if so, ignore the default
	if ((thisRequiredVal != null) && (thisDefaultVal != null)) {
	    warn(root.toString().toUpperCase() + 
		 " has both :required and :default sem decls for the same feature type: :required " 
		 + thisRequiredVal + ", :default " + thisDefaultVal + ". Ignoring default decl.");
	    thisDefaultVal = null;
	}

	// cases where a :default is declared
	if (thisDefaultVal != null) {

	    // what for this feature is asserted somewhere above (as required)
	    OntFeatureValue inheritedRequiredVal = root.getFeatureValue(feature); 
	    
	    // does it have a default that isn't subsumed by a higher required? (i.e., parent
	    // has required of this type -- default is allowed, but only if subsumed
	    // if so, ignore the default
	    if ((inheritedRequiredVal != null) && 
		(!thisDefaultVal.isSubsumedBy(inheritedRequiredVal))) {
		warn(root.toString().toUpperCase() + " declares default sem " + thisDefaultVal + 
				   ", but this isn't subsumed by a higher declared required " +
				   inheritedRequiredVal + ". Ignoring default decl.");
		thisDefaultVal = null;
	    }
	} // end if has :default declared

	// now process stuff
	OntFeatureValue newDefaultVal = null; // what we're passing to our children as default
	boolean consistentWithParent = true;
	if (thisRequiredVal != null) { // has required
	    // already an assertion done here, so we don't need to assert, and should be
	    // consistent below this as well (we still process below to look for errors)
	    if (defaultVal != null) {
		consistentWithParent = thisRequiredVal.isSubsumedBy(defaultVal);
	    }
	    else
		consistentWithParent = false;
	    if (thisRequiredVal != null)
		newDefaultVal = (OntFeatureValue)thisRequiredVal.copy();
	} else if (thisDefaultVal != null) { // has default
	    if (defaultVal != null) {
		consistentWithParent = thisDefaultVal.isSubsumedBy(defaultVal);
	    }
	    else
		consistentWithParent = false;
	    if (thisDefaultVal != null)
		newDefaultVal =  (OntFeatureValue)thisDefaultVal.copy(); 
	} else { // no decls here, use same one
	    if (defaultVal != null)
		newDefaultVal =  (OntFeatureValue)defaultVal.copy();
	}

	boolean childrenConsistent = true;
	// process for each child
	for (OntType child : root.getNonDefaultChildren()) {
	    boolean childConsistent = processSemDefault(child,feature,newDefaultVal);
	    if (!childConsistent) // if one is not consistent, this is not consistent
		childrenConsistent = false;
	} // end for each child

	// to be consistent withparent, need to have all children consistent with us
	consistentWithParent = consistentWithParent && childrenConsistent;

	// now process the defaultVal on root
	if ((newDefaultVal != null) && !newDefaultVal.equals(thisRequiredVal)) {
	    if (childrenConsistent) { // subtree is consistent with this, we assert as a fact
// 		try {
		    root.setFeatureValue(newDefaultVal);
// 		} catch (InvalidFeatureValueException e) {
// 		    warn(e.getMessage());
// 		}
	    } else { // !childrenConsistent, assert on the default node
		root.getOrCreateDefaultClass().setFeatureValue(newDefaultVal);
	    }
	}

	return consistentWithParent;
	} catch (Exception e) {
	    throw new RuntimeException("problem on '" + root + "' " + feature +
				       " defaultVal: " + defaultVal,e);
	}
    } // end processSemDefault

    /**
     * creates default classes for everything below root
     */
    protected void createDefaultClasses(OntType root) {
	// create for this one
	root.createDefaultClass();

	// recursively call on all children
	List<OntType> children = root.getNonDefaultChildren();
	for (OntType child : children) {
	    createDefaultClasses(child);
	}
    } // end createDefaultClasses(OWLNamedClass)

    /**
     * this goes through and processes the arguments for everything
     */
    protected void processAllArguments() {
	for (DefineType def : lispOnt.defineTypeDecls) {
	    OntType oclass = def.typeRef.getReferent();
	    // handle :arguments
	    processArguments(oclass, def);
	} // end for each

    } // end processAllArguments
    
   /**
     * first pass which reads in initial data from the deftype stuff
     * puts together the class and feature hierarchy and annotations
     */
    protected void processBasicTaxonomy() {
	// go through and process each list of defs

	// features
	for (DefineFeature def : lispOnt.defineFeatureDecls) {

	    processDefineFeature(def);
	} // end for each

	// features-list-types
	for (DefineFeatureListType def : lispOnt.defineFeatureListTypeDecls) {

	    processDefineFeatureListType(def);
	} // end for each

	// make the feature-list-types disjoint
	assertMutualDisjointnessFLT(tripsOnt.getFeatureListTypes());

	// assert parent links between the features and the types they're in
	processFeatureLists();

	// create the feature rules (they are applied in OntType.setFeatureValue)
	for (DefineFeatureRule def : lispOnt.defineFeatureRuleDecls) {
	    processDefineFeatureRule(def);
	}

	tripsOnt.classifyTaxonomy();

	// define-types
	for (DefineType def : lispOnt.defineTypeDecls) {
	    processDefineType(def);
	} // end for each

    } // end basicTaxonomy

    /**
     * creates, but does not apply, the feature rule
     * (they are applied in OntType.setFeaturevlaue)
     * and adss it to tripsOnt.featureRules
     */
    protected void processDefineFeatureRule(DefineFeatureRule def) {
	OntNamedFeatureValue value = def.featureValueRef.getReferent();
	
	// only pull out the :requireds.  shouldn't have :defaults, and I think
	// we can safely ignore the featureListType
	List<OntFeatureValueRef> requireds = def.implies.requiredSems;
	if (requireds.isEmpty()) {
	    warn("no required feature values listed on rule. Ignoring. " + def);
	    return;
	}

	List<OntNamedFeatureValue> consequent = new ArrayList<OntNamedFeatureValue>();
	for (OntFeatureValueRef required : requireds) {
	    OntNamedFeatureValue c = (OntNamedFeatureValue)required.getReferent();
	    if (c != null)  // had an ANY on there, ignore.
		consequent.add(c);
	}

	if (tripsOnt.featureRules.put(value, consequent) != null) // had a previous value
	    throw new RuntimeException("rule for " + value + " already had an entry.");
    } // end processFeatureRules

    /**
     * asserts links between features and the feature lists types they are part of
     */
    protected void processFeatureLists() {

	Set<Map.Entry<OntFeature,List<OntNamedFeatureListType>>> entries = 
	    featureLists.entrySet();

	for (Map.Entry<OntFeature,List<OntNamedFeatureListType>> entry : entries) {
	    OntFeature feature = entry.getKey();
	    List<OntNamedFeatureListType> types = entry.getValue();

	    if (types.isEmpty()) { // this feature is part of no types...
		warn("feature: " + feature + " is not part of any feature list type.");
		continue; // skip the rest
	    }

	    feature.setFeatureListTypes(types);
	} // end for each feature

    } // end processFeatureLists

    /**
     * process a single featureListTYpe
     */
    protected void processDefineFeatureListType(DefineFeatureListType def) {
	// create an individual for this type
	OntNamedFeatureListType thisListType = def.ref.createReferent();

	// record the feature list contents
	for (OntFeatureRef featureRef : def.features) {
	    OntFeature feature = featureRef.getReferent();
	    featureLists.get(feature).add(thisListType);
	} // end for each child

	// annotate the info on defaults
	for (OntFeatureValueRef defaultValRef : def.defaults) {
	    try {

	    OntFeatureValue featureClass = defaultValRef.getReferent();
	    if (featureClass != null) // wasn't an ANY def
		addAnnotation(thisListType,tripsOnt.res.hasDefaultSem,featureClass);

	    } catch (Exception e) {
		throw new RuntimeException("problem in default sem: " + defaultValRef, e);
// 		warn("error in processing default sem: " + defaultValRef + " on " + thisListType +
// 		     ", ignoring. " +
// 		     Utils.getExceptionChain(e));
	    }
	    
	} // end for each default

    } // end processDefineFeatureListTYpe

    /**
     * processes a single defType and puts the updates the model
     */
    protected void processDefineType(DefineType def) {

	// create the class (if hasn't already been created)
	OntType oclass = def.typeRef.createReferent();
	
	if (def.parent != null) { //  has explicit parent
	    OntType parent = def.parent.getReferent();
	    oclass.setParent(parent);
	} else { // parent is Thing
	    oclass.getResource().addSuperclass(tripsOnt.model.getOWLThingClass());
	}

	FeatureVector semList = def.sem;

	if (semList != null) { // we have a semlist
	    // handle the feature-list-type
	    if (semList.featureListType != null) {
		oclass.setFeatureListType(semList.featureListType.getReferent());
	    } // end if has featureListType

	    // handle :sem
	    processSemanticFeatures(oclass, def);
	} // end if :sem

    } // end processDefineType


    /**
     * processes the arguments on a define-type
     */
    protected void processArguments(OntType thisClass, DefineType def) {
	try {

        // required
        for (TypeArgument arg : def.arguments) {
	    try {

 	    // create the property, since needed by getrestrictionclass
	    arg.propertyRef.getOrCreateReferent();

	    // get the property restriction (if any)
	    OWLRestriction restriction = arg.createRestrictionClass();

	    // add as superclass
	    if (restriction != null) {
// 		System.err.println("\n--------------------------\n\nasserting restriction on "
// 				   + thisClass + "::: " + restriction.getOnProperty() +
// 				   ":::" + restriction.getFillerText() + "\n\n");
		thisClass.getResource().addSuperclass(restriction);
	    }

	    } catch (Exception e) {
		throw new RuntimeException("problem in arg " + arg, e);
// 		warn("problem on argument in " + thisClass + ". Ignoring argument: " +
// 		     arg + ".  Problem: " + Utils.getExceptionChain(e) + "\n");
	    }
	} // end for each feature

	} catch (Exception e) {
	    throw new RuntimeException("problem for class: " + thisClass.toString().toUpperCase(),
				       e);
	}

    } // end processArguments

    /**
     * processes the semantic features :sem part of a define-type
     */
    protected void processSemanticFeatures(OntType thisClass, DefineType def) {
	try {

        // required
        for (OntFeatureValueRef ref : def.sem.requiredSems) {
	    try {
		
	    OntFeatureValue featureClass = ref.getReferent();
	    if (featureClass != null) { // if null, then it is an "any" class, which is meaningless
		thisClass.setFeatureValue(featureClass);
	    }	    

	    } catch (Exception e) {
		//throw new RuntimeException("problem in required sem: " + ref, e);
		warn("error in processing " + thisClass.toString().toUpperCase() 
		     + " required sem: " + ref + 
		     ", ignoring.\n     " + 
		     Misc.getExceptionChain(e));
	    }
	} // end for each feature

	// default
	for (OntFeatureValueRef ref : def.sem.defaultSems) {
	    try {

	    OntFeatureValue featureClass = ref.getReferent();
	    addAnnotation(thisClass,tripsOnt.res.hasDefaultSem,featureClass);

	    } catch (Exception e) {
		//throw new RuntimeException("problem in default sem: " + ref, e);
		warn("error in processing default sem: " + ref + ", ignoring.\n     " +
		     Misc.getExceptionChain(e));
	    }
	} // end for each default sem

	} catch (Exception e) {
	    throw new RuntimeException("problem for class: " + 
				       thisClass
				       + " and def: " + def,e);
	}

    } // end processSemanticFeatures

    /**
     * adds an annotation on the object
     */
    protected void addAnnotation(OntObject subj, OWLObjectProperty prop, OntObject val) {
	subj.getResource().addPropertyValue(prop,val.getResource());
    } // end addAnnoation

    /**
     * processes a single defFeature and puts the updates in the model
     */
    protected void processDefineFeature(DefineFeature def) {

	OntFeature feature = def.featureRef.createReferent();

	featureLists.put(feature,new ArrayList<OntNamedFeatureListType>()); // used later

	if (def.values != null) {

	    List<OntNamedFeatureValue> levelValues = new ArrayList<OntNamedFeatureValue>();
	    Iterator<SExpression> iter = def.values.iterator();
	    while (iter.hasNext()) {
		SExpressionList subtree = (SExpressionList)iter.next();
		String bareFeatureName = def.featureRef.getBareName();
		OntNamedFeatureValue subtreeRoot = 
		    createNewFeatureValue(bareFeatureName,(SExpressionSymbol)subtree.get(0));
		levelValues.add(subtreeRoot);
		feature.addChild(subtreeRoot);
		processFeatureTree(bareFeatureName,subtreeRoot,subtree);
	    }

	    // assert all features at level as disjoint
	    assertMutualDisjointnessFV(levelValues);
	} // end if has values
	
    } // end processDefineFeature

    /**
     * takes an sexpression that is a tree structure of features and
     * a prefix name (basically a concatinated list of the nodes to the root)
     * and adds the definitions to the model
     */
    protected void processFeatureTree(String bareFeatureName, OntNamedFeatureValue parent, 
				      SExpressionList tree) {
	try {

	if (tree.size() > 1) { // has a subtree below it
	    List<OntNamedFeatureValue> levelValues = new ArrayList<OntNamedFeatureValue>();
	    for (int pos = 1; pos < tree.size(); pos++) {
		SExpressionList subtree = (SExpressionList) tree.get(pos);
		OntNamedFeatureValue subtreeRoot = 
		    createNewFeatureValue(bareFeatureName,(SExpressionSymbol)subtree.get(0));
		levelValues.add(subtreeRoot);
		parent.addChild(subtreeRoot);
		processFeatureTree(bareFeatureName,subtreeRoot,subtree);
	    } // end for each child

	    // assert all features at level as disjoint
	    assertMutualDisjointnessFV(levelValues);
	} // end if subtree

	} catch (Exception e) {
	    throw new RuntimeException("exception on processing for parent: " 
				       + parent + 
				       " tree:\n" + tree,e);
	}
    } // end processFeatureTree

    /**
     * asserts all feature values in the list as disjoint from each other
     */
    protected void assertMutualDisjointnessFV(List<OntNamedFeatureValue> vals) {
// 	for (OntNamedFeatureValue thisVal : vals) {

// 	    for (OntNamedFeatureValue disjointVal : vals) {
// 		if (thisVal == disjointVal) // skip self
// 		    continue;
// 		thisVal.getResource().addDisjointClass(disjointVal.getResource());
// 	    } // end for each disjointVal

// 	} // end for each val
    } // end assertMutualDisjointness

    /**
     * asserts all feature values in the list as disjoint from each other
     */
    protected void assertMutualDisjointnessFLT(List<OntNamedFeatureListType> vals) {
// 	for (OntNamedFeatureListType thisVal : vals) {

// 	    for (OntNamedFeatureListType disjointVal : vals) {
// 		if (thisVal == disjointVal) // skip self
// 		    continue;
// 		thisVal.getResource().addDisjointClass(disjointVal.getResource());
// 	    } // end for each disjointVal

// 	} // end for each val
    } // end assertMutualDisjointness

    /**
     * creates and returns a new feature value from the parent name and a symbol
     * shared factored code
     */
    protected OntNamedFeatureValue createNewFeatureValue(String featureBareName, 
							 SExpressionSymbol thisNameSexp) {
	String thisBareName = LispOntology.getSymbolName(thisNameSexp);
	String thisName = TripsOntology.FEATURE_VALUE_PREFIX + featureBareName 
	    + TripsOntology.FEATURE_INFIX + thisBareName;
	return OntNamedFeatureValue.createNew(thisName);
    } // end createNewFeatureValue

    /**
     * prints out a warning to stderr (with a \n at the end)
     */
    protected static void warn(String mesg) {
	System.err.println("Warning: " + mesg);
    }

} // end class TripsOntologyExporter