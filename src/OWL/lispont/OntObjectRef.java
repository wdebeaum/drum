package TRIPS.OWL.lispont;

import java.util.*;

import edu.stanford.smi.protegex.owl.model.*;
import edu.stanford.smi.protegex.owl.*;

import TRIPS.OWL.sexp.*;
import TRIPS.OWL.*;

/**
 * this encapsulates a reference in a LispOntology file an OntObject
 */
public abstract class OntObjectRef implements Comparable<OntObjectRef> {

//     /**
//      * creates a list of OntObjectRefs from an SExpressionList
//      */
//     public static List<OntObjectRef> createList(SExpressionList sexpList) {
// 	List<OntObjectRef> retval = new ArrayList<OntObjectRef>();
// 	for (SExpression sexp : sexpList) {
// 	    retval.add(create(sexp));
// 	} // end for
// 	return retval;
//     } // end createList

//     /**
//      * creates a new OntObjectRef (one of the children classes) from the Sexpression
//      */
//     public static OntObjectRef create(SExpression sexp) {
// 	OntObjectRef retval = null;

// 	// process by different kinds of references
// 	if (sexp.isList()) { // a list
// 	    SExpressionList sexpList = sexp.asList();
	    
// 	    if (sexpList.size() == 2) { // a super - sub / or super - union sub
// 		SExpression superEx = sexpList.get(0);
// 		SExpression subEx = sexpList.get(1);

// 		if (subEx instanceof SExpressionSymbol) { // (super sub)
// 		    retval = new NamedFeatureRefSub(superEx,subEx);
// 		} else if (isUnionRef(subEx)) { // (super (union sub))
// 		    retval = UnionFeatureRef.createFromSubList(superEx,subEx);
// 		} else { // unknown
// 		    throw new RuntimeException("unexpected syntax on featureref: " + sexp);
// 		}
// 	    } else if (isUnionRef(sexpList)) { // a union
// 		 // get rid of 1st 2 items and process that list
// 		List<OntObjectRef> components = createList(sexpList.rest().rest());
// 		retval = UnionFeatureRef.createFromBareList(sexpList);
// 	    } else { // unexpected
// 		throw new RuntimeException("unknown type of feature ref: " + sexp);
// 	    }
// 	} else { // bare
// 	    SExpressionSymbol atom = sexp.asSymbol();
// 	    String symbolName = LispOntology.getSymbolName(atom);
// 	    if (atom.isInPackage(LispOntology.FEATURE_PACKAGE)) { // it's a feature (F::)
// 		retval = new NamedFeatureRefBare(symbolName);
// 	    } else if (atom.isInPackage(LispOntology.ONT_PACKAGE)) { // it's an ONT::
// 		retval = new TypeRef(symbolName);
// 	    } else { // unknown package
// 		throw new RuntimeException("unknown package: " + atom + ", package: "
// 					   + atom.getPackage());
// 	    }
// 	} // end else (bare)

// 	return retval;
//     } // end create

//     /**
//      * returns true if the first element of the list is "?"
//      * in the def-type stuff, this signifies a "one of the following" list
//      */
//     protected static boolean isUnionRef(SExpression lst) {
// 	SExpression first = ((SExpressionList)lst).first();
// 	if ((first instanceof SExpressionSymbol) && ((first.toString().equals("?"))))
// 	    return true;
// 	else
// 	    return false;		
//     } // end isUnionRef

    /**
     * resolves the references as an OWLClass in the loaded model
     */
    public abstract OntObject getReferent();

    /**
     * resolves the references by creating and returning
     */
    public abstract OntObject createReferent();

    /**
     * converts this to an sexpression for lisp output
     */
    public abstract SExpression toSExpression();

    @Override
    public int compareTo(OntObjectRef o) {
	return toSExpression().toString().compareTo(o.toSExpression().toString());
    } // end compareTo


} // end class OntObjectRef