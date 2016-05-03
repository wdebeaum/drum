package TRIPS.OWL.lispont;

import java.util.*;

import edu.stanford.smi.protegex.owl.model.*;

import TRIPS.OWL.sexp.*;
import TRIPS.OWL.*;

/**
 * this represents a feature vector in the lisp ontology (shows up in :sem and in type args)
 */
public class FeatureVector {

    /**
     * type of feature list -- null if a variable name is used (a question mark (?))
     */
    OntFeatureListTypeRef featureListType;

    /**
     * required semantic features
     */
    public List<OntFeatureValueRef> requiredSems = new ArrayList<OntFeatureValueRef>();

    /**
     * default semantic features
     */
    public List<OntFeatureValueRef> defaultSems  = new ArrayList<OntFeatureValueRef>();

    /**
     * parses the information into the sem list and saves it in the member variables
     */
    public static FeatureVector create(SExpressionList sem) {
	// there are two types of declarations: in one, we have explicit
	// :required and/or :default declared and the features in sublists, e.g.,
	//   :sem (F::situation (:required (F::trajectory +))(:default (F::aspect F::dynamic)(F::time-span F::extended)))
	//
	// in the other, everything is implicitly :required and we have a top level list, e.g.,
	//  :sem (F::Situation (F::Intentional -) (F::information F::mental-construct) (F::container -))
	try {

	if (sem.isEmpty()) {
	    return null;
	}

	FeatureVector retval = new FeatureVector();
	
	// featureListType
	SExpression fltDecl = sem.get(0);
	if (!isFLTVariable(fltDecl)) { // FLT isn't declared as a variable (?)
	    retval.featureListType = OntFeatureListTypeRef.create(fltDecl);
	}

	// go through and process each item as default or required (or implicit required
	for (SExpression itemExpr : sem.rest()) {
	    SExpressionList item = (SExpressionList) itemExpr;
	    
	    String marker = item.first().toString();
	    if (marker.equalsIgnoreCase(":required")) {
		retval.requiredSems.addAll(OntFeatureValueRef.createList(item.rest()));
	    } else if (marker.equalsIgnoreCase(":default")) {
		retval.defaultSems.addAll(OntFeatureValueRef.createList(item.rest()));
	    } else { // implicit required
		retval.requiredSems.add(OntFeatureValueRef.create(item));
	    }
	} // end for each item

	return retval;

	} catch (Exception e) {
	    throw new RuntimeException("problem processing :sem list " + sem,e);
	}
    } // end create

    /**
     * returns true if this is a "variable" declaration for a featureListTYpe
     * (stars with a question mark (?))
     */
    protected static boolean isFLTVariable(SExpression sexp) {
	return (sexp.isSymbol() && sexp.asSymbol().toString().startsWith("?"));
    }

    public SExpressionList toSExpression() {
	SExpressionList retval = new SExpressionList();

	if (featureListType != null) {
	    retval.add(featureListType.toSExpression());
	} else { // it's a var
	    retval.add(SExpressionSymbol.createUnqualified("?"));
	}
	
	if (!requiredSems.isEmpty()) {
	    SExpressionList reqList = new SExpressionList();
	    reqList.add(SExpressionSymbol.createKeyword("required"));
	    for (OntFeatureValueRef ref : requiredSems)
		reqList.add(ref.toSExpression());
 
	    retval.add(reqList);
	}

	if (!defaultSems.isEmpty()) {
	    SExpressionList reqList = new SExpressionList();
	    reqList.add(SExpressionSymbol.createKeyword("default"));
	    for (OntFeatureValueRef ref : requiredSems)
		reqList.add(ref.toSExpression());
 
	    retval.add(reqList);
	}

	return retval;
    } // end toSExpresion

    @Override
    public String toString() {
	return SExpressionPrettyPrinter.makeString(toSExpression());
    }

} // end class FeatureVector