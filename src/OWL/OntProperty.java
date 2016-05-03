package TRIPS.OWL;

import java.util.*;
import java.io.*;
import edu.stanford.smi.protegex.owl.*;
import edu.stanford.smi.protegex.owl.model.*;

import TRIPS.AKRL.*;

/**
 * encapsulates a OWLProperty (this encapsulates both object and data properties)
 */
public class OntProperty extends OntObject {

    OWLProperty resource;
    @Override
    public OWLProperty getResource() {return resource;}

    public OntProperty(OWLProperty prop) {
	resource = prop;
    }

    /**
     * searches for the name in the KB and if found instantiates a new OntType
     * otherwise returns null
     */
    public static OntProperty getNew(String name) {
	String realName = makePropertyName(name);
	return OntObject.getNew(realName,OntProperty.class);
    } // end getNew

    public static OntProperty getNew(AkrlRole role) {
	return getNew(role.getBareName());
    }

    /**
     * creates a new OWLObjectProperty
     */
    public static OntProperty createNew(String name) {
	return createNew(name,false);
    }

    /**
     * creates a new property -- either an object property or data
     */
    public static OntProperty createNew(String name, boolean datatypeProperty) {
	String realName = makePropertyName(name);
	assertNameNotInUse(realName);

	OWLProperty prop;
	if (datatypeProperty)
	    prop = TripsOntology.ont.model.createOWLDatatypeProperty(realName);
	else
	    prop = TripsOntology.ont.model.createOWLObjectProperty(realName);
	return new OntProperty(prop);
    }

    public static OntProperty createNew(AkrlRole role) {
	return createNew(role.getBareName());
    }

    /**
     * returns a new wrapper or creates if not in KB
     */
    public static OntProperty getOrCreateNew(String name) {
	OntProperty prop = getNew(name);
	if (prop == null)
	    prop = createNew(name);
	return prop;
    }

    public static OntProperty getOrCreateNew(AkrlRole role) {
	return getOrCreateNew(role.getBareName());
    }

    /**
     * returns a string that is a propety name (prefixed by _
     * if it already has that prefix, returns itself
     */
    protected static String makePropertyName(String name) {
	String propName = name.toLowerCase();
	if (!name.startsWith(TripsOntology.PROPERTY_PREFIX))
	    propName = TripsOntology.PROPERTY_PREFIX + name;
	return propName;
    }

    /**
     * returns true if this is a datatype property, false if an object property
     */
    public boolean isObjectProperty() {
	return getResource().isObjectProperty();
    }

    /**
     * adds a superproperty
     */
    public void addSuperproperty(OntProperty prop) {
	if (prop == null)
	    throw new RuntimeException("argument is null");
	getResource().addSuperproperty(prop.getResource());
    }

} // end class OntProperty