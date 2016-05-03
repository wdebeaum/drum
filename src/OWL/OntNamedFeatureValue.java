package TRIPS.OWL;

import java.util.*;
import java.io.*;
import edu.stanford.smi.protegex.owl.*;
import edu.stanford.smi.protegex.owl.model.*;

//import TRIPS.OWL.util.*;

/**
 * encapsulates an OWLNamedClass that is a named feature value
 */
public class OntNamedFeatureValue extends OntFeatureValue {

    /**
     * the resource we're wrapping
     */
    protected OWLNamedClass resource;
    @Override
    public OWLNamedClass getResource() {return resource;}

    /**
     * wraps the given feature
     */
    public OntNamedFeatureValue(OWLNamedClass feature) {
	resource = feature;
    }

    /**
     * throws an exception if name isn't a valid one
     */
    public static void assertFeatureValueName(String name) {
	if (!isFeatureValueName(name))
	    throw new RuntimeException("not a valid feature value name: " + name);
    }

    /**
     * gets a new one
     */
    public static OntNamedFeatureValue getNew(String name) {
	assertFeatureValueName(name);

	OWLNamedClass cls = TripsOntology.ont.model.getOWLNamedClass(name);
	if (cls == null) {
	    return null;
	} else if (!isNamedFeatureValue(cls)) {
	    throw new RuntimeException(cls + " is not a named feature value.");
	} else {
	    return new OntNamedFeatureValue(cls);
	}	
    } // end getNew

    /**
     * creates a new one
     */
    public static OntNamedFeatureValue createNew(String name) {
	assertFeatureValueName(name);

	OWLNamedClass cls = OwlUtils.createOWLNamedClass(name);
	return new OntNamedFeatureValue(cls);
    }

    /**
     * returns true if the given owl class is actually representing a named feature value
     */
    public static boolean isNamedFeatureValue(OWLNamedClass cls) {
	return isFeatureValueName(OwlUtils.getLocalName(cls));
    } // end isNamedFeatureValue

    /**
     * returns true if this is a feature name (starts with the prefix
     */
    public static boolean isFeatureValueName(String name) {
	return name.startsWith(TripsOntology.FEATURE_VALUE_PREFIX);
    }

    /**
     * adds this as a child
     */
    public void addChild(OntNamedFeatureValue val) {
	val.getResource().addSuperclass(getResource());
    }

    /**
     * returns all the immediate parents
     */
    @SuppressWarnings("unchecked")
    public List<OntObject> getAllParents() {
	return getNewList(getResource().getSuperclasses(false));
    }

    /**
     * returns the single parent of this featue -- each feature has at most one parent
     * returns null if no feature parent (i.e., at the top of the feature value hirerachy
     */
    public OntNamedFeatureValue getTrueParent() {
	List<OntObject> parents = getAllParents();

	// feature hierarchy is single inheritence, but we impose some multiple inheritence
	// on it to handle feature rules... so, we mimic single inheritence
	// here by only returning the parent that is the true parent

	OntFeature feature = getFeature();
	Iterator<OntObject> iter = parents.iterator();
	while (iter.hasNext()) {
	    OntObject thisParent = iter.next();
	    if (!(thisParent instanceof OntNamedFeatureValue) || // non-feature parent
		(!((OntNamedFeatureValue)thisParent).isValueForFeature(feature))) // not true parent
		iter.remove();
	}

	if (parents.isEmpty()) {
	    return null; // no true feature parent
	} else if (parents.size() > 1) {
	    throw new RuntimeException("size of parents not 1 for " + this + ":: " + parents);
	} else { // we have one
	    return (OntNamedFeatureValue)parents.get(0);
	}
    } // end getTrueParent

    /**
     * returns the feature for this value
     * for now, just uses the naming conventions
     * in the future, we may want to do this will annotations
     */
    @Override
    public OntFeature getFeature() {
	List<String> path = splitName();
	OntFeature retval = OntFeature.getNew(TripsOntology.FEATURE_PREFIX + path.get(0));
	if (retval == null)
	    throw new RuntimeException("got feature of null: " + this + ", path was: " + path);
	return retval;
    } // end getFeature

    /**
     * returns a list of 2 strings which have the feature name (bare) and the value name (bare)
     */
    public List<String> splitName() {
	return splitName(getName());
    }

    public static List<String> splitName(String name) {
	List<String> retval = new ArrayList<String>();

	String bareName = TripsOntology.stripPrefix(TripsOntology.FEATURE_VALUE_PREFIX,name);
	String[] split = bareName.split(TripsOntology.FEATURE_INFIX);
	retval = Arrays.asList(split);

	if (retval.size() != 2) // should get this and root
	    throw new RuntimeException("got path not equals to 2 for " + name + " ::: " + retval);
	
	return retval;
    } // end splitName

    /**
     * returns all named children from this
     */
    @SuppressWarnings("unchecked")
    public List<OntObject> getChildren(boolean transitive) {
	return OntObject.getNewList(getResource().getNamedSubclasses(transitive));
    } // end getChildren

    /**
     * returns the direct children of the feature which are features and
     * belong in this featurevalue hierarchy (i.e., have the same feature)
     * (e.g., and not
     * classes that inherit from a feature but aren't features
     */
    public List<OntNamedFeatureValue> getTrueChildren(boolean transitive) {
	List<OntNamedFeatureValue> retval = new ArrayList<OntNamedFeatureValue>();

	OntFeature feature = getFeature();

	for (OntObject child : getChildren(transitive)) {
	    if ((child instanceof OntNamedFeatureValue) &&  // skip non-feature children
		((OntNamedFeatureValue)child).isValueForFeature(feature)) { // same tree 
		retval.add((OntNamedFeatureValue)child);
	    } // end for each
	} // end for each

	return retval;
    } // end getTrueChildren

    public List<OntNamedFeatureValue> getTrueChildren() {
	return getTrueChildren(false);
    }

    /**
     * returns a list of all direct children of this FV that are OntType
     */
    public List<OntType> getTypeChildren(boolean transitive) {
	List<OntType> retval = new ArrayList<OntType>();

	for (OntObject child : getChildren(transitive)) {
	    if (child instanceof OntType)
		retval.add((OntType)child);
	} // end for each

	return retval;
    } // end getTypeChildren

    public List<OntType> getTypeChildren() {
	return getTypeChildren(false);
    }

} // end class OntNamedFeatureValue