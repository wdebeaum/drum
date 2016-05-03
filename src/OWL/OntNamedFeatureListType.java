package TRIPS.OWL;

import java.util.*;
import java.io.*;

import edu.stanford.smi.protegex.owl.*;
import edu.stanford.smi.protegex.owl.model.*;

//import TRIPS.OWL.util.*;

/**
 * a feature-list-type
 */
public class OntNamedFeatureListType extends OntFeatureListType {

    /**
     * the resource we're wrapping
     */
    protected OWLNamedClass resource;
    @Override
    public OWLNamedClass getResource() {return resource;}

    /**
     * wraps the resource
     */
    public OntNamedFeatureListType(OWLNamedClass featureListType) {	
	resource = featureListType;
    }

    /**
     * throws an exception if name isn't a valid one
     */
    public static void assertFeatureListTypeName(String name) {
	if (!isFeatureListTypeName(name))
	    throw new RuntimeException("not a valid feature list type name: " + name);
    }

    /**
     * returns a new from an existing flt
     */
    public static OntNamedFeatureListType getNew(String name) {
	assertFeatureListTypeName(name);

	OWLNamedClass cls = TripsOntology.ont.model.getOWLNamedClass(name);
	if (cls == null) {
	    return null;
	} else if (!isNamedFeatureListType(cls)) {
	    throw new RuntimeException(cls + " is not a named feature list type.");
	} else {
	    return new OntNamedFeatureListType(cls);
	}	
    } // end getNew

    public static OntNamedFeatureListType createNew(String name) {
	assertFeatureListTypeName(name);

	OWLNamedClass cls = OwlUtils.createOWLNamedClass(name);
	cls.addSuperclass(TripsOntology.ont.res.featureListTypeMarker);
	OntNamedFeatureListType retval = new OntNamedFeatureListType(cls);
	return retval;
    } // end createnew

    /**
     * returns true if the class represents a featureListType
     */
    public static boolean isNamedFeatureListType(OWLNamedClass cls) {
	return isFeatureListTypeName(OwlUtils.getLocalName(cls));
// 	return cls.isSubclassOf(TripsOntology.ont.res.featureListTypeMarker);
    }

    /**
     * returns true if this is a feature name (starts with the prefix
     */
    public static boolean isFeatureListTypeName(String name) {
	return name.startsWith(TripsOntology.FEATURE_LIST_TYPE_PREFIX);
    }


    /**
     * returns the list of features associated with this list
     * or empty list if none
     */
    public List<OntFeature> getFeatures() {
	// go through each feature in the system and see if it has this as a parent or in a
	// parent union
	List<OntFeature> retval = TripsOntology.ont.getFeatures();
	Iterator<OntFeature> iter = retval.iterator();
	while (iter.hasNext()) {
	    OntFeature feature = iter.next();
	    if (!feature.getFeatureListTypes().contains(this))
		iter.remove();
	} // end while

	return retval;
    } // end getFeatures

    /**
     * returns the list of features values that were declared :default on this type
     */
    @SuppressWarnings("unchecked")
    public List<OntFeatureValue> getDefaultFeatureValues() {
	Collection<OWLNamedClass> vals = 
	    getResource().getPropertyValues(TripsOntology.ont.res.hasDefaultSem);
	return Utils.castList(OntObject.getNewList(vals),OntFeatureValue.class);
    } // end getDefaultFeatureValues

    /**
     * returns the feature value for feature that was declared :default on this class
     * or null if none declared
     */
    public OntFeatureValue getDefaultFeatureValue(OntFeature feature) {
	List<OntFeatureValue> defaults = getDefaultFeatureValues();
	Iterator<OntFeatureValue> iter = defaults.iterator();
	while (iter.hasNext()) {
	    OntFeatureValue def = iter.next();
	    if (!def.isValueForFeature(feature))
		iter.remove();
	}
	if (defaults.isEmpty()) {
	    return null;
	} else if (defaults.size() == 1) {
	    return defaults.get(0);
	} else { // more than 1 match
	    throw new RuntimeException("more than 1 default value declared on " + this + 
				       " for feature " + feature + ": " + defaults);
	}
    } // end getDefaultFeatureValue


    @Override
    public List<OntNamedFeatureListType> getTypeList() {
	List<OntNamedFeatureListType> retval = new ArrayList<OntNamedFeatureListType>();
	retval.add(this);
	return retval;
    }

} // end class OntNamedFeatureListType