package TRIPS.OWL.lispont;

import java.util.*;

import TRIPS.OWL.sexp.*;


/**
 * a java rep of the lisp define-feature structures used in the ontology
 */
public class DefineFeature implements Comparable<DefineFeature> {

    /**
     * the reference to this featue
     */
    OntFeatureRef featureRef;

    /**
     * the subfeature hierarchy (as a tree)
     */
    public SExpressionList values;

    public DefineFeature() {
	// do nothing
    }

    /**
     * create from an SExpressionList
     */
    public DefineFeature(SExpressionList lispExpr) {

	// make sure this is a define-type
	if (!lispExpr.get(0).equals(LispOntology.DEFINE_FEATURE_SYMBOL)) {
// 	if (!lispExpr.get(0).toString().equalsIgnoreCase(LispOntology.DEFINE_FEATURE_SYMBOL)) {
	    throw new RuntimeException("not a define feature.  Expected first symbol to be " + 
				       LispOntology.DEFINE_FEATURE_SYMBOL + " but instead it is " + 
				       lispExpr.get(0) + " full def:\n" + lispExpr);
	}

	featureRef = new OntFeatureRef(lispExpr.get(1));

	values = (SExpressionList) lispExpr.getKeywordVal("values");

    } // end constructor

    /**
     * output for a lisp file
     */
    public SExpressionList toSExpression() {
	SExpressionList retval = new SExpressionList();

	// start token
	retval.add(LispOntology.DEFINE_FEATURE_SYMBOL);

	// name
	retval.add(featureRef.toSExpression());

	// :values
	retval.add(SExpressionSymbol.createKeyword("values"));
	retval.add(values);

	return retval;
    } // end toSExpression

    /**
     * sorts on name (for Comparable)
     */
    @Override
    public int compareTo(DefineFeature o) {
	return featureRef.getBareName().compareTo(o.featureRef.getBareName());
    } // end compareTo

    @Override
    public String toString() {
	return SExpressionPrettyPrinter.makeString(toSExpression());
    } // end toString

} // end class DefineFeature