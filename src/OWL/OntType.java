package TRIPS.OWL;

import java.util.*;
import java.io.*;
import edu.stanford.smi.protegex.owl.*;
import edu.stanford.smi.protegex.owl.model.*;

/**
 * encapsulates a trips type (class)
 */
public class OntType extends OntObject {

    /**
     * the resource we're wrapping
     */
    protected OWLNamedClass resource;
    @Override
    public OWLNamedClass getResource() {return resource;}

    /**
     * wraps the given type
     */
    public OntType(OWLNamedClass type) {
	resource = type;
    }

    /**
     * searches for the name in the KB and if found instantiates a new OntType
     * otherwise returns null
     */
    public static OntType getNew(String name) {
	String nname = name.toLowerCase();
	return OntObject.getNew(nname,OntType.class);
    } // end getNew

    /**
     * creates a new one
     */
    public static OntType createNew(String name) {
	String nname = name.toLowerCase();
	assertNameNotInUse(nname);
	OWLNamedClass cls = OwlUtils.createOWLNamedClass(nname);
	return new OntType(cls);
    }

    public static OntType getOrCreateNew(String name) {
	OntType retval = getNew(name);
	if (retval == null)
	    retval = createNew(name);
	return retval;
    }

    /**
     * returns the list of all the direct parents that are feature values
     */
    public List<OntFeatureValue> getFeatureValueParents() {
	List<OntObject> parents = getAllParents();
	List<OntFeatureValue> retval = new ArrayList<OntFeatureValue>();
	for (OntObject parent : parents) {
	    if (parent instanceof OntFeatureValue) {
		retval.add((OntFeatureValue)parent);
	    }
	}

	return retval;
    } // end getFeatureValueParents
      

    /**
     * returns the direct parent of the class that is a type (types form a single inheritence hier)
     */
    public OntType getParent() {
	List<OntType> typeParents = new ArrayList<OntType>();
	List<OntObject> parents = getAllParents();

	for (OntObject parent : parents) {
	    if (parent instanceof OntType) {
		typeParents.add((OntType)parent);
	    }
	}

	if (typeParents.isEmpty())
	    return null;
	else if (typeParents.size() > 1)
	    throw new RuntimeException("Found more than one type parent on " + this
				       + ": " + typeParents);
	else
	    return typeParents.get(0);
    } // end getParent

    /**
     * returns the direct parents of the class that are OntTYpes
     */
    public List<OntType> getTypeParents() {
	List<OntType> typeParents = new ArrayList<OntType>();
	List<OntObject> parents = getAllParents();

	for (OntObject parent : parents) {
	    if (parent instanceof OntType) {
		typeParents.add((OntType)parent);
	    }
	}

	return typeParents;
    } // end getTypeParents

    /**
     * returns the direct parents of this class that are OntTypes but not also equivalentclasses
     */
    public List<OntType> getNonEquivalentTypeParents() {
	List<OntType> parents = getTypeParents();
	List<OntType> retval = new ArrayList<OntType>();
	
	for (OntType parent : parents) {
	    if (!parent.hasChild(this))
		retval.add(parent);
	}

	return retval;
    }

    /**
     * sets the single parent of this type
     * for now, throws an exception if it already has a parent
     * should make it replace the link that was there
     */
    public void setParent(OntType parent) {
	if (parent == null)
	    throw new RuntimeException("attempt to set paraent to null on " + this);
// 	OntType currentParent = getParent();
// 	if ((currentParent != null) && !currentParent.equals(parent))
// 	    throw new RuntimeException("attempt to set a parent to " + parent + 
// 				       " when there already is one: " +
// 				       getParent() + 
// 				       ". This capability hasn't been implemented yet.");
	
	getResource().addSuperclass(parent.getResource());
    } // end setParent

    /**
     * adds a parent to the list
     */
    public void addParent(OntType parent) {
	getResource().addSuperclass(parent.getResource());
    }

    /**
     * sets the feature list type of this type
     */
    public void setFeatureListType(OntFeatureListType flType) {
	getResource().addSuperclass(flType.getResource());
    } // end setFeatureListType

    /**
     * sets a feature value on this
     * we only allow one feature value to be set per feature (because that's how it's 
     * supposed to be), we also only allow a feature value to be set *once* and then
     * after that, it can only be tightened down to something subsumed by the previous
     * value -- the reason for this is that doing a general set for this (which would
     * allow a value to be changed to something vastly different) would require a lot
     * of work and back processing.  It requires us to back up and potentially handle
     * :defaults differently, and asserted feature-rules, etc.  Right now we simplify
     * and assume we're only setting this when we're initially reading in the ontology
     * from the lisp file
     *
     * so, the cases are:
     * (1) previous val is null -- we just set it any everything's fine
     * (2) previous val subsumes new val -- we set the new val (and delete the previous)
     * (3) new val subsumes previous val -- we ignore it and leave the previous val there
     * (4) otherwise -- (new val is incompatible with previous) -- this causes an exception
     */
    public void setFeatureValue(OntFeatureValue val) {
	OntFeature feature = val.getFeature();
	// check that it's a valid featureLIstTYpe
	if (!feature.isValidForFeatureListType(getFeatureListType()))
	    throw new InvalidFeatureValueException("attempt on '" + this 
						   + "' to set a feature value: " + val
				       + " but " + feature + " is not a valid feature for " +
				       getFeatureListType());

	OntFeatureValue previousVal = getFeatureValue(feature);

	if (previousVal == null) { // nothing set yet
	    actuallySetFeatureValue(val);
	} else if (val.isSubsumedBy(previousVal)) { // we're tightening down, ok
	    actuallySetFeatureValue(val);
	    getResource().removeSuperclass(previousVal.getResource()); // remove the old one
	} else if (previousVal.isSubsumedBy(val)) { // new more general, just silently ignore
	    // do nothing
	} else { // previous and new values incompatible -- error
	    throw new InvalidFeatureValueException("attempt on '" + this 
						   + "' to set a feature value: " + val + 
				       " but the feature had already been set as " + previousVal
				       + " which is incompatible.");
	}

    } // end setFeatureValue

    /**
     * actually used to set the feature value, use setFeaturevalue from outside
     */
    private void actuallySetFeatureValue(OntFeatureValue val) {
	getResource().addSuperclass(val.getResource());

	// apply any featureRules implied by this val
	List<OntNamedFeatureValue> implies = TripsOntology.ont.featureRules.get(val);
	if (implies != null) { // a rule matches
// 	    System.err.println("applying rule on " + this + ": " + val + "==>" + implies);
	    for (OntNamedFeatureValue impliedVal : implies)
		setFeatureValue(impliedVal);
	} // end if a rule matches
    } // end actuallySetFeatureValue

    /**
     * returns equivalent classes to this
     */
    @SuppressWarnings("unchecked")
    public List<OntObject> getEquivalentClasses() {
// 	Collection<RDFSClass> equivs = getResource().getEquivalentClasses();
	Set<OWLClass> equivs = OwlUtils.getEquivalentClassesWithInferred(this.getResource());
	List<OntObject> retval = new ArrayList<OntObject>();
	for (OWLClass equiv : equivs) {
	    try {
		retval.add(OntObject.getNew(equiv));
	    } catch (NotAnOntObjectException e) {
		continue; // skip
	    }
	}
	return retval;
    }

    /**
     * same as getEquivalentClasses, but restricts to OntTypes
     */
    public List<OntType> getEquivalentTypes() {
	List<OntType> retval = new ArrayList<OntType>();
	List<OntObject> equivs = getEquivalentClasses();

	for (OntObject equiv : equivs) {
	    if (equiv instanceof OntType) {
		retval.add((OntType)equiv);
	    }
	}

	return retval;
    }

    /**
     * returns all the immediate parents
     * discards any that are not TripsOntology constructs (like OWLSomeValuesFrom)
     */
    @SuppressWarnings("unchecked")
    public List<OntObject> getAllParents() {
// 	return getNewList(getResource().getSuperclasses(false));
	Collection<RDFSClass> supers = getResource().getPureSuperclasses();
	List<OntObject> retval = new ArrayList<OntObject>();
	for (RDFSClass sup : supers) {
	    try {
		retval.add(OntObject.getNew(sup));
	    } catch (NotAnOntObjectException e) {
		continue; // skip
	    }
	}
	return retval;
    } // end getAllParents

    /**
     * returns the immediate parent which is a feature value for feature
     * declared on this type -- or null if none
     * does not work on inherited values, use @getFeatureValue instead
     */
    public OntFeatureValue getFeatureValueParent(OntFeature feature) {
	List<OntFeatureValue> parents = getFeatureValueParents();

	// go through and filter out all the features not of type feature
	Iterator<OntFeatureValue> iter = parents.iterator();
	while (iter.hasNext()) {
	     OntFeatureValue thisVal = iter.next();
	     if (!thisVal.isValueForFeature(feature)) {
		 iter.remove();
	     }
	} // end while

	if (parents.isEmpty()) {
	    return null;
	} else if (parents.size() > 1) { // shouldn't happen
	    throw new RuntimeException(this + " has more than 1 feature value parents of type " 
				       + feature + ": " + parents);
	} else { // has 1 result
	    return parents.get(0);
	}
    } // end getFeatureValueParent	

    /**
     * returns the value set for this feature, or null if none set
     * this works even if the value is indirectly inherited
     * ?? (can we use subsumption reasoning to do this more efficiently?)
     */
    public OntFeatureValue getFeatureValue(OntFeature feature) {
	if (this.equals(TripsOntology.ont.getRoot())) // we're at the top, no features found
	    return null;

	OntFeatureValue thisVal = getFeatureValueParent(feature);
	if (thisVal != null) { // value directly on this class
	    return thisVal;
	} else { // nothing direct, so recurse on parent
	    return getParent().getFeatureValue(feature);
	}
    } // end getFeatureValue

    /**
     * returns the immediate parent is feature list type
     * declared on this type -- or null if none
     * does not work on inherited values, use @getFeatureListType instead
     */
    public OntFeatureListType getFeatureListTypeParent() {
	List<OntObject> parents = getAllParents();

	// go through and filter out all the features not of type featureListTYpe
	Iterator<OntObject> iter = parents.iterator();
	while (iter.hasNext()) {
	     OntObject thisVal = iter.next();
	     if (!(thisVal instanceof OntFeatureListType))
		 iter.remove();
	} // end while

	if (parents.isEmpty())
	    return null;
	else if (parents.size() > 1)
	    throw new RuntimeException("more than 1 feature list type parent found for " + this + 
				       ".  should only be 1: " + parents);
	else
	    return (OntFeatureListType)parents.get(0);
    } // end getFeatureListTypeParent	

    /**
     * returns the value set for this feature, or null if none set
     * this works even if the value is indirectly inherited
     * ?? (can we use subsumption reasoning to do this more efficiently?)
     */
    public OntFeatureListType getFeatureListType() {
	if (this.equals(TripsOntology.ont.getRoot())) // we're at the top, no features found
	    return null;

	OntFeatureListType thisVal = getFeatureListTypeParent();
	if (thisVal != null) { // value directly on this class
	    return thisVal;
	} else { // nothing direct, so recurse on parent
	    return getParent().getFeatureListType();
	}
    } // end getFeatureListType

    /**
     * returns the list of features values that were declared :default on this class
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

    /**
     * returns true if this is a 'default' class
     */
    public boolean isDefaultClass() {
	String name = getName();
	return name.startsWith(TripsOntology.DEFAULT_CLASS_PREFIX);
    } // end isDefaultClass

    /**
     * creates the defualt class for this class
     */
    public OntType createDefaultClass() {
	String name = getDefaultClassName();	
	OntType retval = createNew(name);
	retval.setParent(this); // put as a subclass to this
	return retval;
    } // end createDefaultClass


    /**
     * returns the 'default' class for this class, if exists, or null otherwise
     */
    public OntType getDefaultClass() {
	String name = getDefaultClassName();
	OntType retval = getNew(name);
	return retval;
    }

    /**
     * gets the default class or creates it if it doesn't yet exist
     */
    public OntType getOrCreateDefaultClass() {
	OntType retval = getDefaultClass();
	if (retval == null)
	    retval = createDefaultClass();
	return retval;
    } // end getOrCreateDefaultClass

    /**
     * returns the default class name for the given class
     */
    public static String getDefaultClassName(String name) {
	return TripsOntology.DEFAULT_CLASS_PREFIX + name;
    }

    public String getDefaultClassName() {
	return getDefaultClassName(getName());
    }

    /**
     * returns true if the parameter is a child of this
     */
    public boolean hasChild(OntType child) {
	List<OntType> children = getChildren();
	return children.contains(child);
    }

    /**
     * returns all direct children of this class that aren't equivalent,
     * including its default class. Uses stored inferences.
     */
    @SuppressWarnings("unchecked")
    public List<OntType> getChildren() {
	Set<OWLClass> subs = OwlUtils.getPureSubclassesWithInferred(getResource());
	List<OntType> retval = Utils.castList(getNewList(subs),OntType.class,true);
//  	System.out.println(this + " kids: " + retval);
	return retval;
    }

    /**
     * returns all direct children of this class, including equivalent classes
     * and its default. Does not use stored inferences.
     */
    @SuppressWarnings("unchecked")
    public List<OntType> getAllDirectChildren() {
	Set<OWLClass> subs = OwlUtils.getDirectSubclasses(getResource());
	List<OntType> retval = Utils.castList(getNewList(subs),OntType.class,true);
	return retval;
    }

    /**
     * returns the direct children of this class that aren't equivalent or its
     * default. Uses stored inferences.
     */
    public List<OntType> getNonDefaultChildren() {
	List<OntType> children = getChildren();

	Iterator<OntType> iter = children.iterator();
	while (iter.hasNext()) {
	    OntType child = iter.next();
	    if (child.isDefaultClass())
		iter.remove();
	}
	return children;
    } // end getNonDefaultChildren

    /**
     * creates and returns a new individual of this type with the given name
     * note that, because of :defaults processing, we acdtually
     * always create it as an individual of the defaultClass of this class,
     * if it exists
     */
    public OntIndividual createIndividual(String name) {
	assertNameNotInUse(name);
	OntType spawningClass = getDefaultClass();
	if (spawningClass == null) // no default class
	    spawningClass = this;

	OWLIndividual ind = spawningClass.getResource().createOWLIndividual(name);
	// apply hasValue restrictions (necessary because SQWRL doesn't import
	// them)
	for (Object restr : spawningClass.getResource().getRestrictions()) {
	  if (restr instanceof OWLHasValue) {
	    OWLHasValue hasValue = (OWLHasValue)restr;
	    Object value = hasValue.getHasValue();
	    RDFProperty property = hasValue.getOnProperty();
	    if (property instanceof OWLProperty) {
	      ind.addPropertyValue(property, value);
	    } else {
	      System.err.println("warning: hasValue restriction on non-OWLProperty '" + property.getName() + "'; ignoring");
	    }
	  }
	}

	return new OntIndividual(ind);
    } // end createIndividual

} // end class OntType
