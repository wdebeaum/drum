package TRIPS.OWL;

import java.util.*;
import java.io.*;
import java.net.*;

import edu.stanford.smi.protegex.owl.model.util.ImportHelper;
import edu.stanford.smi.protegex.owl.model.*;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.*;
import edu.stanford.smi.protegex.owl.inference.reasoner.ProtegeReasoner;
import edu.stanford.smi.protegex.owl.inference.protegeowl.ReasonerManager;
import edu.stanford.smi.protegex.owl.inference.pellet.ProtegePelletJenaReasoner;
import com.hp.hpl.jena.util.FileUtils;

import TRIPS.AKRL.*;

/**
 * this encapsulates the Trips Ontology (ONT) packed in a JENA OntModel
 * we assume there is only ever 1 TripsOntology in the runtime (hence the static routines)
 */
public class TripsOntology {

    /**
     * name of this ontology
     */
    public static final String ONTOLOGY_NAME = "http://trips.ihmc.us/ont#";

    /**
     * name of the root class (the root of the trips hierarchy)
     */
    public static final String ROOT_STR = "root";
    public static final String REFERENTIAL_SEM_STR = "referential-sem";


    /**
     * the prefix for all 'default' dummy classes
     */
    public static final String DEFAULT_CLASS_PREFIX = "notarealonttype-default-";

    /**
     * the prefix that gets appended to all features in OWL (to distinguist true features)
     */
    public static final String PROPERTY_PREFIX = "_";

    /**
     * the prefix that gets appended to all features in OWL (to distinguist true features)
     */
    public static final String FEATURE_PREFIX = "F_";

    /**
     * the prefix that gets appended to all features in OWL (to distinguist true features)
     */
    public static final String FEATURE_VALUE_PREFIX = "FV_";

    /**
     * the prefix that gets appended to all features in OWL (to distinguist true features)
     */
    public static final String FEATURE_LIST_TYPE_PREFIX = "FLT_";

    /**
     * the string we use to concatinate feature names for subfeatures
     */
    public static final String FEATURE_INFIX = "_";

    /**
     * the ontology instance (we only ever have 1)
     */
    public static TripsOntology ont = null;

    /**
     * the ontology model
     */
    public static OWLModel model;

    /**
     * the reasoner we're using
     */
    protected ProtegeReasoner reasoner;

    /**
     * holds shortcuts to some of the static resources we access
     */
    public StaticResources res;

    /**
     * represents the feature-rules as a mapping from feature values to a list of other
     * feature values implied -- may at some point want to look into putting these
     * in the actual ontology as SWRL rules or something
     */
    public Map<OntNamedFeatureValue,List<OntNamedFeatureValue>> featureRules =
	new HashMap<OntNamedFeatureValue,List<OntNamedFeatureValue>>();

    /**
     * strips the prefix from a string (throws an exception if not there
     */
    public static String stripPrefix(String prefix, String str) {
	if (!str.startsWith(prefix))
	    throw new RuntimeException(str + " does not start with prefix " + 
				       prefix + ".");
	return str.substring(prefix.length());
    } // end stripPrefix

    private TripsOntology() {
	ont = this;
	res = new StaticResources();

    } // end constructor()

    public static TripsOntology getTripsOntology() {
	if (ont == null)
	    new TripsOntology();
	return ont;
    }

    /**
     * creates a new blank ontology
     */
    public static TripsOntology createNew() {
 	try {
	TripsOntology retval = getTripsOntology();

	model = ProtegeOWL.createJenaOWLModel();
	model.getNamespaceManager().setDefaultNamespace(ONTOLOGY_NAME);
  
	retval.res.create();
	retval.createReasoner();

	return retval;
	} catch (Exception e) {
	    throw new RuntimeException("error creating model",e);
	}	
    } // end createNew

    /**
     * gets an OWL model
     */
    public static OWLModel getOWLModel() {
	return model;
    }

    /**
     * reads in the OWL core file (for TRIPS) in order to build the rest of the ontology
     * on top of that (get rid of this when go to pure non-lisp ontology approach
     */
    public static TripsOntology fromCoreFile(String filename) {
 	try {
	TripsOntology retval = getTripsOntology();

	retval.model = ProtegeOWL.createJenaOWLModelFromInputStream(new FileInputStream(filename));
// 	model = ProtegeOWL.createJenaOWLModel();
// 	model.getNamespaceManager().setDefaultNamespace(ONTOLOGY_NAME);
  
	retval.res.create();
	retval.createReasoner();

	return retval;
	} catch (Exception e) {
	    throw new RuntimeException("error creating model from core ontology",e);
	}	
    } // end fromCoreFile

    /**
     * reads in an Owl file as a trips ontology (and returns the ontology)
     */
    public static TripsOntology fromFile(String filename) {
	TripsOntology retval = getTripsOntology();

	try {
	retval.model = ProtegeOWL.createJenaOWLModelFromInputStream(new FileInputStream(filename));

// 	NamespaceManager nsManager = retval.model.getNamespaceManager();
// 	nsManager.setDefaultNamespace(ONTOLOGY_NAME);
// 	System.out.println("default ns: " + nsManager.getDefaultNamespace());

	// imports everything we are used to working with (e.g., sqwrl)
	importStandardOntologies(retval.model);

	// check and see if we were successful in loading (will always have Root class)
// 	OWLNamedClass checkRoot = model.getOWLNamedClass(ROOT_STR);
// 	if (checkRoot == null) {
// 	    retval.writeOWL("error.owl");
// 	    throw new RuntimeException("No TRIPS '" + ROOT_STR + "' class found in ontology. Somehow the ontology didn't load right.  What *was* there has been saved to error.owl for inspection");
// 	}

	retval.createReasoner();
	retval.res.load();


	} catch (Exception e) {
	    throw new RuntimeException("error loading file: " + filename,e);
	}
	return retval;
    } // end fromFile

    /** 
     * Uses an existing OWLModel, assumed to already have the TRIPS ontology
     * loaded into it, to make a TripsOntology.
     */
    public static TripsOntology fromOWLModel(OWLModel model) {
	TripsOntology retval = getTripsOntology();

	try {
	retval.model = model;

	// imports everything we are used to working with (e.g., sqwrl)
	importStandardOntologies(retval.model);

	// check and see if we were successful in loading (will always have Root class)
	OWLNamedClass checkRoot = model.getOWLNamedClass(ROOT_STR);
	if (checkRoot == null) {
	    retval.writeOWL("error.owl");
	    throw new RuntimeException("No TRIPS '" + ROOT_STR + "' class found in ontology. Somehow the ontology didn't load right.  What *was* there has been saved to error.owl for inspection");
	}

	ReasonerManager rm = ReasonerManager.getInstance();
	if (rm.hasAttachedReasoner(model)) {
	    retval.reasoner = rm.getProtegeReasoner(model);
	} else {
	    retval.createReasoner();
	}
	retval.res.load();

	} catch (Exception e) {
	    throw new RuntimeException("error using existing OWLModel: ",e);
	}
	return retval;
    } // end fromOWLModel

    /**
     * imports all the ontologies we need standard (e.g., sqwrl)
     */
    protected static void importStandardOntologies(OWLModel m) {
// 	//create the ImportHelper
// 	ImportHelper importHelper = new ImportHelper(m);

// 	NamespaceManager nm = m.getNamespaceManager();

// 	//add the imports (multiple imports can be added here)
// 	nm.setPrefix(URI.create("http://swrl.stanford.edu/ontologies/built-ins/3.3/temporal.owl#"),"temporal");
// 	importHelper.addImport(URI.create("http://swrl.stanford.edu/ontologies/built-ins/3.3/temporal.owl#"));

// 	nm.setPrefix(URI.create("http://swrl.stanford.edu/ontologies/built-ins/3.4/rdfb.owl#"),"rdfb");
// 	importHelper.addImport(URI.create("http://swrl.stanford.edu/ontologies/built-ins/3.4/rdfb.owl#"));

// 	nm.setPrefix(URI.create("http://swrl.stanford.edu/ontologies/3.3/swrla.owl#"),"swrla");
// 	importHelper.addImport(URI.create("http://swrl.stanford.edu/ontologies/3.3/swrla.owl#"));

// 	nm.setPrefix(URI.create("http://swrl.stanford.edu/ontologies/built-ins/3.4/swrlxml.owl#"),"swrlxml");
// 	importHelper.addImport(URI.create("http://swrl.stanford.edu/ontologies/built-ins/3.4/swrlxml.owl#"));

// 	nm.setPrefix(URI.create("http://swrl.stanford.edu/ontologies/built-ins/3.3/swrlx.owl#"),"swrlx");
// 	importHelper.addImport(URI.create("http://swrl.stanford.edu/ontologies/built-ins/3.3/swrlx.owl#"));

// 	nm.setPrefix(URI.create("http://swrl.stanford.edu/ontologies/built-ins/3.4/swrlm.owl#"),"swrlm");
// 	importHelper.addImport(URI.create("http://swrl.stanford.edu/ontologies/built-ins/3.4/swrlm.owl#"));

// 	nm.setPrefix(URI.create("http://swrl.stanford.edu/ontologies/built-ins/3.3/abox.owl#"),"abox");
// 	importHelper.addImport(URI.create("http://swrl.stanford.edu/ontologies/built-ins/3.3/abox.owl#"));

// 	nm.setPrefix(URI.create("http://sqwrl.stanford.edu/ontologies/built-ins/3.4/sqwrl.owl#"),"sqwrl");
// 	importHelper.addImport(URI.create("http://sqwrl.stanford.edu/ontologies/built-ins/3.4/sqwrl.owl#"));

// 	nm.setPrefix(URI.create("http://swrl.stanford.edu/ontologies/built-ins/3.3/tbox.owl#"),"tbox");
// 	importHelper.addImport(URI.create("http://swrl.stanford.edu/ontologies/built-ins/3.3/tbox.owl#"));

// 	try {
// 	    //do the actual import
// 	    importHelper.importOntologies(false);
// 	} catch (Exception e) {
// 	    throw new RuntimeException("problem importing ontologies",e);
// 	}
	

	// check to see if sqwrl loaded properly
// 	RDFResource r = model.getRDFResource("sqwrl:select");
// 	RDFResource r = model.getRDFResource("http://sqwrl.stanford.edu/ontologies/built-ins/3.4/sqwrl.owl#select");
// 	if (r == null)
// 	    throw new RuntimeException("got null for sqwrl:select");	
    } // end importStandardOntologies

    /**
     * instantiates the reasoner we'll be using
     */
    public void createReasoner() {	
	if (model == null)
	    throw new RuntimeException("attempt to create reasoner before model");

	// Get the reasoner manager instance
	ReasonerManager reasonerManager = ReasonerManager.getInstance();	
	//Get an instance of the Protege Pellet reasoner
	ProtegeReasoner reasoner = 
	    reasonerManager.createProtegeReasoner(model, ProtegePelletJenaReasoner.class);
	 this.reasoner = reasoner;
    } // end createReasoner

    /**
     * classifies the taxonomy
     */
    public void classifyTaxonomy() {
	try {
	System.out.println("classifying taxonomy");
	Stopwatch watch = new Stopwatch();
	watch.start();
	reasoner.classifyTaxonomy();
	watch.stop();
	System.out.println("done classifying.  Finished in " + watch.getDuration());

	} catch (Exception e) {
	    throw new RuntimeException("error during classification",e);
	}
    } // end classifyTaxonomy

    /**
     * returns a list of all (user-defined named) classes in the model
     */
    @SuppressWarnings("unchecked")
    public List<OntObject> getAllClasses() {
	Collection<OWLNamedClass> classes = model.getUserDefinedOWLNamedClasses();
	List<OntObject> retval = OntObject.getNewList(classes);
	return retval;
    } // end getAllClasses

    /**
     * returns a collection of the top-level features (the immediate children of feature)
     */    
    @SuppressWarnings("unchecked")
    public List<OntFeature> getFeatures() {
	Collection<OWLNamedClass> children = res.featureMarker.getNamedSubclasses();
	List<OntFeature> retval = Utils.castList(OntObject.getNewList(children),
						 OntFeature.class);
	return retval;
    } // end geFeatures

    /**
     * returns the root class (the root of the trips ontology)
     */
    public OntType getRoot() {
	return OntType.getNew(ROOT_STR);
    } // end getRoot

    /**
     * returns the referentialsem class
     */
    public OntType getReferentialSem() {
	return OntType.getNew(REFERENTIAL_SEM_STR);
    } // end getRoot


    /**
     * returns the OWLNamedClass for the root of the Trips ontology
     */
    public OWLNamedClass getRootClass() {
	return (OWLNamedClass)model.getRDFResource(ROOT_STR);
    }

    /**
     * returns a list of all the (named) feature list types
     */
    @SuppressWarnings("unchecked")
    public List<OntNamedFeatureListType> getFeatureListTypes() {
	Collection<OWLNamedClass> children = res.featureListTypeMarker.getNamedSubclasses();
	List<OntNamedFeatureListType> retval = Utils.castList(OntObject.getNewList(children),
							      OntNamedFeatureListType.class);
	return retval;
    } // end getFeatureListTypes

    /**
     * returns a list of all the non-feature classes
     */
    public List<OntType> getAllTypes() {
	List<OntObject> classes = getAllClasses();

	List<OntType> retval = new ArrayList<OntType>();

	for (OntObject cls : classes) {
	    if (cls instanceof OntType)
		retval.add((OntType)cls);
	} // end for each class

	return retval;
    } // end getAllTypes

    /**
     * this writes the model out as an OWL file
     */
    public void writeOWL(String filename) {

	// save the model to file
	try {

	    Collection errors = new ArrayList();	    
// 	    ((JenaOWLModel) model).save(new File(filename).toURI(), 
// 				       FileUtils.langXMLAbbrev, 
// 				       errors);
// 	    System.out.println("File saved with " + errors.size() + " errors.");
// 	    System.err.println("ontology name: xx" + ONTOLOGY_NAME + "XX");
	    ((JenaOWLModel)model).save(new File(filename),
				       ((JenaOWLModel)model).getOntModel(),
				       FileUtils.langXMLAbbrev,
				       ONTOLOGY_NAME,ONTOLOGY_NAME);

	} catch (Exception e) {
	    throw new RuntimeException("error writing out to file: " + filename,e);
	}

    } // end writeOWL

    

    /**
     * for now, just prints out all the names of classes
     */
    @Override
    public String toString() {
	return model.getUserDefinedOWLNamedClasses().toString() + "\n" +
	    model.getUserDefinedOWLProperties().toString();
    } // end toString

    /**
     * encapsulates some of the common "static" resources we need to access
     */
    public class StaticResources {
	// the constants
	protected static final String FEATURE_STR = "*feature*";
	protected static final String FEATURE_LIST_TYPE_STR = "*feature-list-type*";
	protected static final String HAS_DEFAULT_SEM_STR = "hasDefaultSem";

// 	protected static final String INDICATOR_PREFIX = "ind_";
// 	protected static final String INDICATOR_STR = "*indicator*";
// 	protected static final String HAS_INDICATOR_STR = "hasIndicator";

// 	protected static final String FROM_UTTERANCE_ID_STR = "fromUtteranceID";
// 	protected static final String HAS_START_POS_STR = "hasStartPos";
// 	protected static final String HAS_END_POS_STR = "hasEndPos";

 	public OWLObjectProperty hasDefaultSem;	
	public OWLNamedClass featureMarker;
	public OWLNamedClass featureListTypeMarker;

// 	public OWLNamedClass indicator;
// 	public OWLObjectProperty hasIndicator;
// 	public Map<AkrlIndicator,OWLIndividual> indicators =
// 	    new EnumMap<AkrlIndicator,OWLIndividual>(AkrlIndicator.class);

// 	public OWLDatatypeProperty fromUtteranceID;
// 	public OWLDatatypeProperty hasStartPos;
// 	public OWLDatatypeProperty hasEndPos;

	/**
	 * creates and adds all the resources to the ontology
	 */
	public void create() {
	    featureMarker = OwlUtils.createOWLNamedClass(FEATURE_STR);
	    featureListTypeMarker = OwlUtils.createOWLNamedClass(FEATURE_LIST_TYPE_STR);
	    hasDefaultSem = 
		model.createAnnotationOWLObjectProperty(HAS_DEFAULT_SEM_STR);

// 	    indicator = OwlUtils.createOWLNamedClass(INDICATOR_STR);
// 	    hasIndicator = model.createOWLObjectProperty(HAS_INDICATOR_STR);
// 	    for (AkrlIndicator aind : AkrlIndicator.values()) {
// 		OWLIndividual ind = indicator.createOWLIndividual(createIndicatorName(aind));
// 		if (ind == null)
// 		    throw new RuntimeException("got null in attempt to create indicator: " + aind);

// 		indicators.put(aind,ind);
// 	    }

// 	    fromUtteranceID = model.createOWLDatatypeProperty(FROM_UTTERANCE_ID_STR);
// 	    hasStartPos = model.createOWLDatatypeProperty(HAS_START_POS_STR);
// 	    hasEndPos = model.createOWLDatatypeProperty(HAS_END_POS_STR);

	} // end createResources

	/**
	 * loads the resources from the current model
	 */
	public void load() {
	    featureMarker = model.getOWLNamedClass(FEATURE_STR);
// 	    if (featureMarker == null) // check that stuff we expect is there
// 		throw new RuntimeException("The *feature* class not found after loading.  Probably means that the ontology did not load correctly");
	    featureListTypeMarker = model.getOWLNamedClass(FEATURE_LIST_TYPE_STR);
	    hasDefaultSem = model.getOWLObjectProperty(HAS_DEFAULT_SEM_STR);

// 	    indicator = model.getOWLNamedClass(INDICATOR_STR);
// 	    hasIndicator = model.getOWLObjectProperty(HAS_INDICATOR_STR);
// 	    for (AkrlIndicator aind : AkrlIndicator.values()) {
// 		OWLIndividual ind = model.getOWLIndividual(createIndicatorName(aind));
// 		if (ind == null)
// 		    throw new RuntimeException("got null in attempt to load indicator " + aind +
// 					       ". Probably means that the indicator wasn't in " +
// 					       "the ontology when we were loading it.");
// 		indicators.put(aind,ind);
// 	    }

// 	    fromUtteranceID = model.getOWLDatatypeProperty(FROM_UTTERANCE_ID_STR);
// 	    hasStartPos = model.getOWLDatatypeProperty(HAS_START_POS_STR);
// 	    hasEndPos = model.getOWLDatatypeProperty(HAS_END_POS_STR);

	} // end load

// 	/**
// 	 * creates the OWL name (with prefix) for an indicator (use to avoid name clashes)
// 	 */
// 	protected String createIndicatorName(AkrlIndicator ind) {
// 	    return INDICATOR_PREFIX + ind.toString();
// 	}

    } // end class StaticResources

} // end class TripsOntology
