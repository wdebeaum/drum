package TRIPS.OWL;

import java.util.*;
import java.io.*;
import edu.stanford.smi.protegex.owl.*;
import edu.stanford.smi.protegex.owl.model.*;

/**
 * the abstract parent class of all types of ontolgoical objects
 * all of these wrap an RDFResource and encapsulate routines from a Trips ontology
 * standpoint - these should consistute a complete partition, i.e., any RDFResource
 * should fit into exactly one of these types
 * 
 * the hierarchy:  (* means abstract)
 * *OntObject 
 *      OntIndividual (any OWLIndividual)
 *      OntProperty   (any OWLProperty (both data or Ont)
 *     *OntClass
 *        *OntAbstractType
 *           OntType       (core types in the ontology
 *           OntKind       (complex types that come from KIND constructions, form v123...)
 *        *OntAbstractFeature
 *            *OntFeatureListType (feature lists types from the TRIPS ontology)
 *               OntNamedFeatureListType (feature list types with names)
 *               OntUnionFeatureListTYpe (union of feature list types)
 *             OntFeature (feature names from the ontology)
 *            *OntFeatureValue (values of features)
 *               OntNamedFeatureValue (named values)
 *               OntUnionFeatureValue (unions of values)
 *         OntOtherClass      (any OWLClass not covered by the above types)
 */
public abstract class OntObject {

    /**
     * tricky stuff happens when we're computing the original ontology
     * where we want to be more lax on errors while we bootstrap
     * that code will set this to true and then to false again when it's done
     */
    static public boolean inBootstrapMode = false;

    /**
     * the resource that is being wrapped
     * subclasses will probably specialize this to e.g., OWLNamedClass
     */
    abstract public RDFResource getResource(); // {return resource;}

    /**
     * this has to be used instead of class.getLocalName since there are some bugs
     * in it that cause +  and * not to appear
     */
    public String getName() {
	return OwlUtils.getLocalName(getResource());
    }

    /**
     * this provides a shallow copy --- the union subclasses override to be a deep copy
     * since those classes are dependent on another named class
     */
    public OntObject copy() {
	return getNew(getResource());
    }

    /**
     * returns true if the given resource is a 'special' class (e.g., one that isn't really part of
     * the original ontology
     * thses should always begin with *
     */
    public static boolean isSpecial(RDFResource res) {
	String name = OwlUtils.getLocalName(res);
	return name.startsWith("*");
    } // end isSpecial

    /**
     * returns true if this is a 'special' class (e.g., one that isn't really part of
     * the original ontology
     * thses should always begin with *
     */
    public boolean isSpecial() {
	return isSpecial(getResource());
    } // end isSpecial


    public static List<OntObject> getNewList(Collection<? extends RDFResource> lst) {
	return getNewList(lst,false);
    }

    /**
     * runs getNew over a collection and returns a list
     * if ignoreNonOntObjects is true, we just ignore them (and leave them out of the list)
     */
    public static List<OntObject> getNewList(Collection<? extends RDFResource> lst,
					     boolean ignoreNonOntObjects) {
	List<OntObject> retval = new ArrayList<OntObject>();
	for (RDFResource res : lst) {
	    OntObject thisVal = getNew(res,ignoreNonOntObjects);
	    if (thisVal != null)
		retval.add(thisVal);
	}
	return retval;
    } // end getNewList

    public static OntObject getNew(RDFResource res) {
	return getNew(res,false);
    }

    /**
     * makes a new ontResource from the resource to be wrapped (but doesn't create the resource in
     * the DB)
     * automatically detects which subtype it should be
     * if it can't figure out, throws an exception unless ignoreNonOntObjects is true
     * in which case, it returns null for that one
     */
    public static OntObject getNew(RDFResource res, boolean ignoreNonOntObjects) {
	if (res == null)
	    throw new RuntimeException("getNew called on a null RDFObject");

	OntObject retval = null;

	// figure out which class this belongs to
	if (res instanceof OWLProperty) {
	    retval = new OntProperty((OWLProperty)res);
	} else if (res instanceof OWLIndividual) { 
	    OWLIndividual ind = (OWLIndividual) res;
	    retval = new OntIndividual(ind);
	} else if (res instanceof OWLClass) {
	    OWLClass cls = (OWLClass) res;

	    if (cls instanceof OWLNamedClass) { // type or namedfeature{typelist}, feature
		OWLNamedClass namedCls = (OWLNamedClass) cls;
		if (OntNamedFeatureValue.isNamedFeatureValue(namedCls)) {
		    retval = new OntNamedFeatureValue(namedCls);
		} else if (OntFeature.isFeature(namedCls)) {
		    retval = new OntFeature(namedCls);
		} else if (OntNamedFeatureListType.isNamedFeatureListType(namedCls)) {
		    retval = new OntNamedFeatureListType(namedCls);
		} else if ((namedCls.equals(TripsOntology.ont.model.getOWLThingClass())) ||
			   (namedCls.equals(TripsOntology.ont.model.getOWLNothing()))) {
		    if (ignoreNonOntObjects)
			return null;
		    else
			throw new NotAnOntObjectException("got Thing or Nothing as a type: " + 
							  res.getBrowserText());
// 		} else if (!inBootstrapMode &&
// 			   !OwlUtils.isSubsumedBy(namedCls,TripsOntology.ont.getRootClass())) {
// 		    if (ignoreNonOntObjects)
// 			return null;
// 		    else
// 			throw new NotAnOntObjectException("attempt to make class above 'root' an object: " +
// 							  res.getBrowserText());
		} else {
		    retval = new OntType(namedCls);
		}
	    } else if (cls instanceof OWLUnionClass) {
		OWLUnionClass unionCls = (OWLUnionClass) cls;
		if (OntUnionFeatureValue.isUnionFeatureValue(unionCls)) {
		    retval = new OntUnionFeatureValue(unionCls);
		} else if (OntUnionFeatureListType.isUnionFeatureListType(unionCls)) {
		    retval = new OntUnionFeatureListType(unionCls);
		} else {
		    if (ignoreNonOntObjects)
			return null;
		    else 
			throw new NotAnOntObjectException("unexpected union class. Not feature or featurelisttype " + res.getBrowserText());
		}
	    } else {
		if (ignoreNonOntObjects)
		    return null;
		else 
		    throw new NotAnOntObjectException("unexcected class type: " + res.getClass() +
						      " on " + res.getBrowserText());
	    }
	} else { // none of the types matched
	    if (ignoreNonOntObjects)
		return null;
	    else 
		throw new NotAnOntObjectException("unexpected resource: " + res.getBrowserText() + " type: " + res.getClass());
	}

	if (retval == null) { // somehow we missed a case
	    throw new RuntimeException("got to end with null retval.. means a bug in the code");
	}

	return retval;
    } // end getNew

    /**
     * retrieves the resource for the given name and then creates the Ont version appropriate
     * to its type.  Returns null if not found
     */
    public static OntObject getNew(String name) {
	RDFResource res = TripsOntology.ont.model.getRDFResource(name);
	if (res == null)
	    return null;
	return getNew(res);
    } // end getNew(String)

    /**
     * returns the resource and verifies that it is the right type or throws an exception if not
     * this is only called by the subclasses of OntObject (don't see any reason for it
     * to be public)
     */
    protected static <T extends OntObject> T getNew(String name, Class<T> retClass) {
	OntObject obj = getNew(name);
	if (obj == null)
	    return null;

	if (!retClass.isInstance(obj))
	    throw new OntNameClashException(name,obj,
					    "Cannot get " + name.toUpperCase() + 
					    " as class " + retClass.getSimpleName() +
					    ".  It is type " +
					    obj.getClass().getSimpleName());
	return retClass.cast(obj);
    } // end getNew

    /**
     * throws an exception if the given string already exists as some sort of resource
     * in the ontology
     */
    public static void assertNameNotInUse(String name) {
	RDFResource res = TripsOntology.ont.model.getRDFResource(name);
	if (res != null)
	    throw new OntNameClashException(name,res,"name " + name + 
					    " already exists in the ontology as a " 
					    + res.getClass().getSimpleName());
    } // end assertNameNotInUse

    /**
     * returns true if this is a subclass of cls
     */
    public boolean isSubsumedBy(OntObject subsumer) {
	// weed out cases where it shouldn't apply
	if ((this instanceof OntProperty) || (this instanceof OntIndividual))
	    throw new RuntimeException("class: " + this + " is not an applicable class.");

	if ((subsumer instanceof OntProperty) || (subsumer instanceof OntIndividual))
	    throw new RuntimeException("param: " + subsumer + " is not an applicable class.");

	return OwlUtils.isSubsumedBy((OWLClass)getResource(),(OWLClass)subsumer.getResource());
    } // end isSubsumedBy

    /**
     * two resources are equal iff the wrapped resources are equal
     */
    @Override
    public boolean equals(Object o) {
	// have to be OntObjects
	if (!(o instanceof OntObject))
	    return false;
	OntObject r = (OntObject) o;

	return getResource().equals(r.getResource());
    } // end equals
    
    /**
     * override with equals
     */
    @Override
    public int hashCode() {
	return getResource().hashCode();
    }

    /**
     * prints out just the name
     */
    @Override
    public String toString() {
	return OwlUtils.getLocalName(getResource());
    }

} // end class OntObject