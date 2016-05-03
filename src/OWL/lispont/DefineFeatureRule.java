package TRIPS.OWL.lispont;

import java.util.*;

import TRIPS.OWL.sexp.*;

/**
 * a java rep of the lisp define-feature-rule structures used in the ontology
 */
public class DefineFeatureRule implements Comparable<DefineFeatureRule> {

    /**
     * the name
     */
    public String name;

    /**
     * the feature value that implies the rest
     */
    public OntNamedFeatureValueRef featureValueRef;

    /**
     * the :implies part
     */
    public FeatureVector implies;

    /**
     * create from an SExpressionList
     */
    public DefineFeatureRule(SExpressionList lispExpr) {

	// make sure this is a define-type
	if (!lispExpr.get(0).equals(LispOntology.DEFINE_FEATURE_RULE_SYMBOL)) {
	    throw new RuntimeException("not a define feature rule.  " + 
				       "Expected first symbol to be " + 
				       LispOntology.DEFINE_FEATURE_RULE_SYMBOL + 
				       " but instead it is " + 
				       lispExpr.get(0) + " full def:\n" + lispExpr);
	}

	name = lispExpr.get(1).toString();

	SExpressionList featureList = (SExpressionList) lispExpr.getKeywordVal("feature");
	if (featureList == null)
	    throw new RuntimeException("no :feature found on " + lispExpr);
	featureValueRef = new OntNamedFeatureValueRef(featureList);

	SExpressionList impliesList = (SExpressionList) lispExpr.getKeywordVal("implies");
	if (impliesList == null)
	    throw new RuntimeException("no :implies found on " + lispExpr);
	implies = FeatureVector.create(impliesList);

	if (!implies.defaultSems.isEmpty()) // shouldn't have any defaults
	    TripsOntologyExporter.warn("feature-rule " + name + " has :default on the :implies. "
				       + "Ignoring. " + impliesList);

    } // end constructor

    /**
     * output a string for output to a file like the original lisp
     */
    public SExpressionList toSExpression() {
	SExpressionList retval = new SExpressionList();

	retval.add(LispOntology.DEFINE_FEATURE_RULE_SYMBOL);
	retval.add(SExpressionSymbol.createUnqualified(name));
	retval.add(SExpressionSymbol.createKeyword("feature"));
	retval.add(featureValueRef.toSExpression());
	retval.add(SExpressionSymbol.createKeyword("implies"));
	retval.add(implies.toSExpression());

	return retval;
    } // end toSExpression
    
    @Override
    public int compareTo(DefineFeatureRule o) {
	return name.compareTo(o.name);
    } // end compareTo

    @Override
    public String toString() {
	return SExpressionPrettyPrinter.makeString(toSExpression());
    } // end toString

} // end class DefineFeatureRule