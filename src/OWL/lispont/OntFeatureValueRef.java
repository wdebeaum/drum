package TRIPS.OWL.lispont;

import java.util.*;
import edu.stanford.smi.protegex.owl.model.*;
import edu.stanford.smi.protegex.owl.*;

import TRIPS.OWL.sexp.*;
import TRIPS.OWL.*;

/**
 * this is the parent class to all types of feature value references
 */
public abstract class OntFeatureValueRef extends OntObjectRef {

    /**
     * resolves the reference as an OWL class
     */
    @Override
    abstract public OntFeatureValue getReferent();

    @Override
    abstract public OntFeatureValue createReferent();

    public static List<OntFeatureValueRef> createList(SExpressionList sexpList) {
	List<OntFeatureValueRef> retval = new ArrayList<OntFeatureValueRef>();
	for (SExpression sexp : sexpList) {
	    retval.add(create((SExpressionList)sexp));
	} // end for
	return retval;
    } // end createList

    /**
     * creates a union or named ref from the given sexp
     */
    public static OntFeatureValueRef create(SExpressionList sexp) {
	if (sexp.size() < 2)
	    throw new RuntimeException("sexp not at least size 2: " + sexp);

	OntFeatureValueRef retval = null;

	// process by different kinds of references
	if (sexp.get(1).isList()) { // union
	    return new OntUnionFeatureValueRef(sexp);
	} else { // named
	    return new OntNamedFeatureValueRef(sexp);
	}
    } // end create

//     /**
//      * creates a featureRefbare or featurerefSub or union from the given class
//      */
//     public static OntFeatureValueRef createFeature(OntFeatureValue feature) {

// 	if (feature instanceof OntUnionFeature) { // union
// 	    return UnionOntFeatureValueRef.fromUnionFeature((OntUnionFeature)feature);
// 	} else if (feature.isFeatureType()) { // top-level
// 	    return new NamedOntFeatureValueRefBare((OntNamedFeature)feature);
// 	} else { // not top-level feature
// 	    return new NamedOntFeatureValueRefSub((OntNamedFeature)feature);
// 	}

//     } // end createFeature



} // end class OntFeatureValueRef