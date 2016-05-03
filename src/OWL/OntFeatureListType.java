package TRIPS.OWL;

import java.util.*;
import java.io.*;
import edu.stanford.smi.protegex.owl.*;
import edu.stanford.smi.protegex.owl.model.*;

/**
 * a feature-list-type (or union of them
 */
public abstract class OntFeatureListType extends OntObject {

    abstract public OWLClass getResource();

    /**
     * returns a list of all the named elements of this
     * if it's already naned, a singleton set
     */
    abstract public List<OntNamedFeatureListType> getTypeList();

} // end class OntFeatureListType