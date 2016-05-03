package TRIPS.OWL.lispont;

import java.util.*;

import TRIPS.OWL.sexp.*;

/**
 * a java rep of the lisp define-feature-list-type structures used in the ontology
 */
public class DefineFeatureListType implements Comparable<DefineFeatureListType> {

    /**
     * the referenece to this feature
     */
    public OntNamedFeatureListTypeRef ref;

    /**
     * the features that are part of this
     */
    public List<OntFeatureRef> features;

    /**
     * the defaults
     */
    public List<OntFeatureValueRef> defaults;

    /**
     * create from an SExpressionList
     */
    public DefineFeatureListType(SExpressionList lispExpr) {

	// make sure this is a define-type
	if (!lispExpr.get(0).equals(LispOntology.DEFINE_FEATURE_LIST_TYPE_SYMBOL)) {
	    throw new RuntimeException("not a define feature list type.  " + 
				       "Expected first symbol to be " + 
				       LispOntology.DEFINE_FEATURE_LIST_TYPE_SYMBOL + " but instead it is " + 
				       lispExpr.get(0) + " full def:\n" + lispExpr);
	}

	ref = new OntNamedFeatureListTypeRef(lispExpr.get(1));

	features = new ArrayList<OntFeatureRef>();

	SExpressionList featureList = (SExpressionList) lispExpr.getKeywordVal("features");
	if (featureList == null)
	    throw new RuntimeException("no :features found on " + lispExpr);
	for (SExpression feature : featureList) {
	    features.add(new OntFeatureRef((SExpressionSymbol)feature));
	}

	defaults = new ArrayList<OntFeatureValueRef>();
	SExpressionList defaultList = (SExpressionList) lispExpr.getKeywordVal("defaults");
	if (defaultList != null) { // has defaults
	    for (SExpression defaultVal : defaultList) {
		defaults.add(OntFeatureValueRef.create((SExpressionList)defaultVal));
	    }
	} // end if has defaults
    } // end constructor

    /**
     * output a string for output to a file like the original lisp
     */
    public SExpressionList toSExpression() {
	SExpressionList retval = new SExpressionList();

	retval.add(LispOntology.DEFINE_FEATURE_LIST_TYPE_SYMBOL);
	retval.add(ref.toSExpression());
	retval.add(SExpressionSymbol.createKeyword("features"));
	SExpressionList featureList = new SExpressionList();
	retval.add(featureList);

	// add each of the features to the list
	Collections.sort(features);
	for (OntFeatureRef feature : features) {
	    featureList.add(feature.toSExpression());
	} // end for each feature

	return retval;
    } // end toSExpression
    
    @Override
    public int compareTo(DefineFeatureListType o) {
	return ref.getBareName().compareTo(o.ref.getBareName());
    } // end compareTo

    @Override
    public String toString() {	
	String retval = "";

	retval += ref;

	retval += "\n:features: " + features;

	return retval;
    } // end toString

} // end class DefineFeatureListType