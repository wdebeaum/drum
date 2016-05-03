package TRIPS.OWL;

import java.util.*;
import java.io.*;
import edu.stanford.smi.protegex.owl.*;
import edu.stanford.smi.protegex.owl.model.*;

/**
 * this represents a feature, which can take on a featurevalue
 * in the OWl version, a feature is actually the top of a tree
 * of OntNamedFeatureValue
 */
public class OntFeature extends OntObject {

    /**
     * the resource we're wrapping
     */
    protected OWLNamedClass resource;
    @Override
    public OWLNamedClass getResource() {return resource;}

    public OntFeature(OWLNamedClass resource) {
	this.resource = resource;
    }

    /**
     * throws an exception if name isn't a valid one
     */
    public static void assertFeatureName(String name) {
	if (!isFeatureName(name))
	    throw new RuntimeException("not a valid feature name: " + name);
    }

    /**
     * creates a new one and adds it as a child of *feature*
     */
    public static OntFeature createNew(String name) {
	assertFeatureName(name);
	assertNameNotInUse(name);

	OWLNamedClass fclass = OwlUtils.createOWLNamedClass(name);
	fclass.addSuperclass(TripsOntology.ont.res.featureMarker);
	return new OntFeature(fclass);
    }

    /**
     * returns a new object from a previously existing feature
     */
    public static OntFeature getNew(String name) {
	assertFeatureName(name);

	OWLNamedClass cls = TripsOntology.ont.model.getOWLNamedClass(name);
	if (cls == null) {
	    return null;
	} else if (!isFeature(cls)) {
	    throw new RuntimeException(cls + " is not a feature.");
	} else {
	    return new OntFeature(cls);
	}	
    } // end getNew

    /**
     * returns true if the class represents a feature
     */
    public static boolean isFeature(OWLNamedClass cls) {
	return isFeatureName(OwlUtils.getLocalName(cls));
// 	return cls.isSubclassOf(TripsOntology.ont.res.featureMarker);
    }

    /**
     * returns true if this is a feature name (starts with the prefix
     */
    public static boolean isFeatureName(String name) {
	return name.startsWith(TripsOntology.FEATURE_PREFIX);
    }

    /**
     * this returns the immediate feature value children of this feature
     * if transitive = true, then returns all feature values in the subtree
     * does not include feature values in a subclass relation that are not
     * valid feature values for this feature
     */
    @SuppressWarnings("unchecked")
    public List<OntNamedFeatureValue> getTrueChildren(boolean transitive) {
	List<OntNamedFeatureValue> retval = new ArrayList<OntNamedFeatureValue>();

	Collection<OWLNamedClass> subclasses = getResource().getNamedSubclasses(transitive); 
	for (OntObject child : getNewList(subclasses)) {
	    if (child instanceof OntNamedFeatureValue) {
//  &&  // skip non-feature children
// 		((OntNamedFeatureValue)child).isValueForFeature(this)) { // same tree 
		retval.add((OntNamedFeatureValue)child);
	    } // end for each
	} // end for each

	return retval;	
    } // end getTrueChildren

    /**
     * calls getTrueChildren(false)
     */
    public List<OntNamedFeatureValue> getTrueChildren() {
	return getTrueChildren(false);
    }

    /**
     * adds this as a child
     */
    public void addChild(OntNamedFeatureValue val) {
	val.getResource().addSuperclass(getResource());
    }

    /**
     * returns true if this feature is a member of the featureListTYpe
     */
    public boolean isValidForFeatureListType(OntFeatureListType type) {
	List<OntNamedFeatureListType> thisTypeList = getFeatureListTypes();
	List<OntNamedFeatureListType> paramTypeList = type.getTypeList();
	paramTypeList.retainAll(thisTypeList); // set intersection
	return !paramTypeList.isEmpty(); // if set intersection not empty, it's valid
    } // end isValidForFeatureListType

    /**
     * returns the OntNamedFeatureListTypes that this feature is part of
     */
    @SuppressWarnings("unchecked")
    public List<OntNamedFeatureListType> getFeatureListTypes() {
	List<OntNamedFeatureListType> retval = new ArrayList<OntNamedFeatureListType>();

	Collection<OWLNamedClass> parents = getResource().getSuperclasses(false);

	// should only have 2 parents: *feature* and the OntFeatureListType (union or named)
	parents.remove(TripsOntology.ont.res.featureMarker);

	if (parents.isEmpty()) {
	    throw new RuntimeException("feature not part of any feature list: " + this);
	} else if (parents.size() > 1) {
	    throw new RuntimeException("feature " + this + " has unexpected parent(s). " + 
				       "expected only *feature* and FLT, but got: " + parents);
	} else { // 1 parent
	    OntObject parent = OntObject.getNew(parents.iterator().next());
	    if (parent instanceof OntNamedFeatureListType) { // 1 parent
		retval.add((OntNamedFeatureListType)parent);
	    } else if (parent instanceof OntUnionFeatureListType) {
		retval.addAll(((OntUnionFeatureListType)parent).getNamedOperands());
	    } else { 
		throw new RuntimeException("parent type not FTL: " + this + " :: " + parent);
	    }
	} // end else 1 parent

	return retval;
    } // end getFeatureListTypes

    /**
     * sets the types that this feature is a part of
     * note that this has to be set atomically (all at one) and not added, since
     * it is asserted as a union class (if there's more than one) -- could do add, but jsut
     * easier this way
     */
    public void setFeatureListTypes(List<OntNamedFeatureListType> typeList) {
	if (typeList.size() == 1) { // singleton list, just do direct named class
	    getResource().addSuperclass(typeList.get(0).getResource());
	} else if (typeList.size() > 1) { 
	    getResource().addSuperclass(OntUnionFeatureListType.
					createFromTypes(typeList).getResource());
	} else {
	    throw new RuntimeException("typeList is empty! " + this);
	}
    } // end setFeatureListTypes

} // end class OntFeature