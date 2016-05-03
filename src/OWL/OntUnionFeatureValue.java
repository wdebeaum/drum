package TRIPS.OWL;

import java.util.*;
import java.io.*;
import edu.stanford.smi.protegex.owl.*;
import edu.stanford.smi.protegex.owl.model.*;

/**
 * encapsulates a union of features
 */
public class OntUnionFeatureValue extends OntFeatureValue {

    /**
     * the resource we're wrapping
     */
    protected OWLUnionClass resource;
    @Override
    public OWLUnionClass getResource() {return resource;}

    /**
     * wraps the feature
     */
    public OntUnionFeatureValue(OWLUnionClass union) {
	if (union.getNamedOperands().isEmpty())
	    throw new RuntimeException("attempt to create a union from an empty unionclass :" +
				       union);
	resource = union;
// 	System.err.println("creating new union: " + this);
    }

    /**
     * creates from a bunch of classes
     */
    public static OntUnionFeatureValue createFromValues(List<OntNamedFeatureValue> values) {
	List<OWLNamedClass> classes = new ArrayList<OWLNamedClass>();

	for (OntNamedFeatureValue value : values) {
	    classes.add(value.getResource());
	}

	return createFromClasses(classes);
    } // end create

    public static OntUnionFeatureValue createFromClasses(List<OWLNamedClass> classes) {
	return new OntUnionFeatureValue(TripsOntology.ont.model.createOWLUnionClass(classes));
    }

    /**
     * creates a copy of this (by recreating the underlying class -- needed since
     * these are dependent on a host named class, and if that geos away, or it's detached
     * from that, this suddenlty turns into an empty list
     */
    @Override
    public OntUnionFeatureValue copy() {
	return createFromValues(getNamedOperands());
    } // end copy

    /**
     * returns true if this is a union feature value
     */
    public static boolean isUnionFeatureValue(OWLUnionClass cls) {
	Collection<RDFSNamedClass> ops = cls.getNamedOperands();
	for (RDFSNamedClass op : ops) { // all ops have to be feature values
	    if (!OntNamedFeatureValue.isNamedFeatureValue((OWLNamedClass)op)) 
		return false;
	}
	return true;
    } // end isUnionFeatureValue

    /**
     * returns the individual classes in the union
     */
    @SuppressWarnings("unchecked")
    public List<OntNamedFeatureValue> getNamedOperands() {
	Collection<RDFSNamedClass> ops = getResource().getNamedOperands();
	List<OntNamedFeatureValue> retval = new ArrayList<OntNamedFeatureValue>();
	for (RDFSNamedClass op : ops) {
	    retval.add(new OntNamedFeatureValue((OWLNamedClass)op));
	}
	return retval;
    } // end getNamedOperands

    /**
     * make equal if it has the same elements (in any order)
     */
    @Override
    public boolean equals(Object o) { 
	// same type
	if (!(o instanceof OntUnionFeatureValue))
	    return false;

	OntUnionFeatureValue f = (OntUnionFeatureValue) o;
	
	List<OntNamedFeatureValue> thisElements = getNamedOperands();
	List<OntNamedFeatureValue> fElements = f.getNamedOperands();

	// same length
	if (thisElements.size() != fElements.size())
	    return false;

	// same elements
	for (OntNamedFeatureValue elem : thisElements) {
	    if (!fElements.contains(elem))
		return false;
	}

	return true;
    } // end equals

    /**
     * returns the feature for this value
     */
    @Override
    public OntFeature getFeature() {
	List<OntNamedFeatureValue> elements = getNamedOperands();
	
	// the types in a union should always be the same feature, although
	// we check just to make sure
	Set<OntFeature> features = new HashSet<OntFeature>();

	for (OntNamedFeatureValue element : elements) {
	    features.add(element.getFeature());
	}

	if (features.size() != 1)
	    throw new RuntimeException("empty or non-singleton set of features: " + this + 
				       " ::: " + features);

	return features.iterator().next(); // only element
    } // end getFeature

    @Override
    public String toString() {
	return getNamedOperands().toString();
    }

} // end class OntUnionFeatureValue