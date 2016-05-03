package TRIPS.OWL.lispont;

import java.util.*;
import edu.stanford.smi.protegex.owl.model.*;
import edu.stanford.smi.protegex.owl.*;

import TRIPS.OWL.sexp.*;
import TRIPS.OWL.*;

/**
 * this is the parent class to all types of feature value references
 */
public abstract class OntFeatureListTypeRef extends OntObjectRef {

    /**
     * resolves the reference as an OWL class
     */
    @Override
    abstract public OntFeatureListType getReferent();

    @Override
    abstract public OntFeatureListType createReferent();

    /**
     * creates a union or named ref from the given sexp
     */
    public static OntFeatureListTypeRef create(SExpression sexp) {

	OntFeatureListTypeRef retval = null;

	// process by different kinds of references
	if (sexp.isList()) { // union
	    return new OntUnionFeatureListTypeRef((SExpressionList)sexp);
	} else { // named
	    return new OntNamedFeatureListTypeRef(sexp);
	}
    } // end create

//     /**
//      * creates a featureRefbare or featurerefSub or union from the given class
//      */
//     public static OntFeatureListTypeRef createFeature(OntFeatureListType feature) {

// 	if (feature instanceof OntUnionFeature) { // union
// 	    return UnionOntFeatureListTypeRef.fromUnionFeature((OntUnionFeature)feature);
// 	} else if (feature.isFeatureType()) { // top-level
// 	    return new NamedOntFeatureListTypeRefBare((OntNamedFeature)feature);
// 	} else { // not top-level feature
// 	    return new NamedOntFeatureListTypeRefSub((OntNamedFeature)feature);
// 	}

//     } // end createFeature



} // end class OntFeatureListTypeRef