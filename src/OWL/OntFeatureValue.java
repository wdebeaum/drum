package TRIPS.OWL;

import java.util.*;
import java.io.*;
import edu.stanford.smi.protegex.owl.*;
import edu.stanford.smi.protegex.owl.model.*;

/**
 * encapsulates a feature value
 */
public abstract class OntFeatureValue extends OntObject {

    /**
     * returns true if this is a value for the given feature
     * (even in union, this is the getFeature is guarnteed to be unique, so this works)
     */
    public boolean isValueForFeature(OntFeature feature) {
	return feature.equals(getFeature());
    }

    abstract public OWLClass getResource(); // {return resource;}

    /**
     * returns the top-level feature type for this feature
     */
    public abstract OntFeature getFeature();

} // end class OntFeatureValue