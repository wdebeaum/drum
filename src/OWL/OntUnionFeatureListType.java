package TRIPS.OWL;

import java.util.*;
import java.io.*;
import edu.stanford.smi.protegex.owl.*;
import edu.stanford.smi.protegex.owl.model.*;

/**
 * encapsulates a union of features list types
 */
public class OntUnionFeatureListType extends OntFeatureListType {

    /**
     * the resource we're wrapping
     */
    protected OWLUnionClass resource;
    @Override
    public OWLUnionClass getResource() {return resource;}

    /**
     * wraps the feature
     */
    public OntUnionFeatureListType(OWLUnionClass union) {
	resource = union;
    }

    /**
     * creates from a bunch of classes
     */
    public static OntUnionFeatureListType createFromTypes(List<OntNamedFeatureListType> types) {
	List<OWLNamedClass> classes = new ArrayList<OWLNamedClass>();

	for (OntNamedFeatureListType type : types) {
	    classes.add(type.getResource());
	}

	return createFromClasses(classes);
    } // end create

    public static OntUnionFeatureListType createFromClasses(List<OWLNamedClass> classes) {
	return new OntUnionFeatureListType(TripsOntology.ont.model.createOWLUnionClass(classes));
    }

    /**
     * returns true if this is a union feature value
     */
    public static boolean isUnionFeatureListType(OWLUnionClass cls) {
	Collection<RDFSNamedClass> ops = cls.getNamedOperands();
	for (RDFSNamedClass op : ops) { // all ops have to be feature values
	    if (!OntNamedFeatureListType.isNamedFeatureListType((OWLNamedClass)op)) 
		return false;
	}
	return true;
    } // end isUnionFeatureValue


    /**
     * returns the individual classes in the union
     */
    @SuppressWarnings("unchecked")
    public List<OntNamedFeatureListType> getNamedOperands() {
	Collection<RDFSNamedClass> ops = getResource().getNamedOperands();
	List<OntNamedFeatureListType> retval = new ArrayList<OntNamedFeatureListType>();
	for (RDFSNamedClass op : ops) {
	    retval.add(new OntNamedFeatureListType((OWLNamedClass)op));
	}
	return retval;
    } // end getNamedOperands

    @Override
    public OntUnionFeatureListType copy() {
	return createFromTypes(getNamedOperands());
    } // end copy

    /**
     * make equal if it has the same elements (in any order)
     */
    @Override
    public boolean equals(Object o) { 
	// same type
	if (!(o instanceof OntUnionFeatureListType))
	    return false;

	OntUnionFeatureListType f = (OntUnionFeatureListType) o;
	
	List<OntNamedFeatureListType> thisElements = getNamedOperands();
	List<OntNamedFeatureListType> fElements = f.getNamedOperands();

	// same length
	if (thisElements.size() != fElements.size())
	    return false;

	// same elements
	for (OntNamedFeatureListType elem : thisElements) {
	    if (!fElements.contains(elem))
		return false;
	}

	return true;
    } // end equals

    @Override
    public String toString() {
	return getNamedOperands().toString();
    }

    @Override
    public List<OntNamedFeatureListType> getTypeList() {
	return getNamedOperands();
    }


} // end class OntUnionFeatureListType