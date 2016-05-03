package TRIPS.OWL;

import java.math.BigDecimal;
import java.util.*;
import java.io.*;
import edu.stanford.smi.protegex.owl.*;
import edu.stanford.smi.protegex.owl.model.*;

/**
 * encapsulates a OWLIndividual
 */
public class OntIndividual extends OntObject {

    OWLIndividual resource;
    @Override
    public OWLIndividual getResource() {return resource;}

    public OntIndividual(OWLIndividual prop) {
	resource = prop;
    }

    /**
     * returns a new wrapper for the individual in the KB
     */
    public static OntIndividual getNew(String name) {
	return OntObject.getNew(name,OntIndividual.class);
    } // end getNew

    /**
     * creates a new individual of the given name and type
     * note that, apparently, in the Protege-owl api,
     * an individual can only be created as a given type
     * from the get-go.  You can't change its type afterwards
     */
    public static OntIndividual createNew(OntType type, String name) {
	return type.createIndividual(name);
    }

    /**
     * gets an existing one if matches, or creates new
     * for now, does not check if it's already exsisting if it is of type type
     * probably should
     */
    public static OntIndividual getOrCreateNew(OntType type, String name) {
	OntIndividual retval = getNew(name);
	if (retval == null)
	    retval = createNew(type,name);
	return retval;
    }

    /**
     * adds the given indivdiual on the property
     */
    public void addPropertyValue(OntProperty prop, OntIndividual ind) {
	if (!prop.isObjectProperty())
	    throw new RuntimeException("attempt to assert an individual as a value of a datatype "
				       + "property: " + prop + " " + ind);
	addPropertyValue(prop.getResource(),ind.getResource());
    } // end addPropertyValue

    public void addPropertyValue(OWLProperty prop, Object val) {
	try {
	getResource().addPropertyValue(prop,val);
	} catch (Exception e) {
	    throw new RuntimeException("error in addiing property value: (" + this + ", " +
				       OwlUtils.getLocalName(prop) + ", " 
				       + val + ")",e);
	}
    }
	

    /**
     * adds the given String as a slot value (needs to be a data property)
     */
    public void addPropertyValue(OntProperty prop, String stringValue) {
	if (prop.isObjectProperty())
	    throw new RuntimeException("attempt to assert a datatype as a value of an object "
				       + "property: " + prop + " " + stringValue);
	getResource().addPropertyValue(prop.getResource(),stringValue);
    } // end addPropertyValue

    /**
     * adds the given Number as a slot value (needs to be a data property)
     */
    public void addPropertyValue(OntProperty prop, Number numValue) {
	if (numValue instanceof Float || numValue instanceof Integer) {
	    // do nothing (Protege likes these types)
        } else if (numValue instanceof Double || numValue instanceof BigDecimal) {
	    // convert to Float
	    numValue = Float.valueOf(numValue.floatValue());
	} else {
	    // convert to Integer
	    numValue = Integer.valueOf(numValue.intValue());
	}
	if (prop.isObjectProperty())
	    throw new RuntimeException("attempt to assert a datatype as a value of an object "
				       + "property: " + prop + " " + numValue);
	getResource().addPropertyValue(prop.getResource(),numValue);
    } // end addPropertyValue

    /**
     * adds the given Boolean as a slot value (needs to be a data property)
     */
    public void addPropertyValue(OntProperty prop, Boolean booleanValue) {
	if (prop.isObjectProperty())
	    throw new RuntimeException("attempt to assert a datatype as a value of an object "
				       + "property: " + prop + " " + booleanValue);
	getResource().addPropertyValue(prop.getResource(),booleanValue);
    } // end addPropertyValue

} // end class OntIndividual
