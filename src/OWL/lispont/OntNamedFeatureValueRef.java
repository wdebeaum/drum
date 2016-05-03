package TRIPS.OWL.lispont;

import java.util.*;
import edu.stanford.smi.protegex.owl.model.*;
import edu.stanford.smi.protegex.owl.*;

import TRIPS.OWL.sexp.*;
import TRIPS.OWL.*;

/**
 * this encapsulates a reference to a named feature
 */
public class OntNamedFeatureValueRef extends OntFeatureValueRef {

    /**
     * LF ont uses this prefix to mark features that basically mean any of the decendents
     * e.g., (f::scale f::any-scale)
     * of this -- we have to detect this by string matches
     */
    protected static final String ANY_PREFIX = "any-";

    /**
     * the super is always a reference to a top-level feature
     */
    public OntFeatureRef featureRef;

    /**
     * the sub is a name of some feature that is a decendent of superFeature -- not a direct ref
     */
    public String valueName;

    /**
     * creates a new one from (feature value)
     */
    public OntNamedFeatureValueRef(SExpressionList sexp) {
	this(new OntFeatureRef(LispOntology.getSymbolName(sexp.get(0))),
	     (SExpressionSymbol)sexp.get(1));
    } // end constructor

    /**
     * creates from a feature (the parent) and an sexp of the bare name
     * used in processing the (feature (? x v1 v2)) syntax
     */
    public OntNamedFeatureValueRef(OntFeatureRef featureRef, SExpressionSymbol valueName) {
	this.featureRef = featureRef;
	this.valueName = LispOntology.getSymbolName(valueName);
    }

    /**
     * resolves the reference
     * finds the feature class from the form (root - subtype)
     * note that subtype may not be an immediate subtype of root, so we have to search
     * the hierarchy under root for a match
     *
     * if it's an 'any' def, we return null for now
     */
    @Override
    public OntNamedFeatureValue getReferent() {
	try {
	if (isAnyDef())
	    return null;

	OntFeature feature =  featureRef.getReferent();
	if (feature == null)
	    throw new RuntimeException("feature not found: " + feature);

	OntNamedFeatureValue retval = null;

	for (OntNamedFeatureValue thisVal : feature.getTrueChildren(true)) {
	    List<String> path = thisVal.splitName();
	    if (path.get(1).equalsIgnoreCase(valueName)) {
		retval = thisVal;
		break;
	    }
	} // end for each subtype

// HACK disable this check for now since it chokes on the new :name-only t features
// 	if (retval == null)
// 	    throw new RuntimeException("unable to find a subtype " + valueName +
//				       " for " + feature);

	return retval;

	} catch (Exception e) {
	    throw new RuntimeException("problem getting feature value from " + this, e);
	}
    } // end getReferent

    @Override
    public OntNamedFeatureValue createReferent() {
	throw new UnsupportedOperationException("not possible to create classes from this");
    } // end createReferent

    /**
     * this returns true if this is of the form (f::x f::any-X)
     */
    protected boolean isAnyDef() {
	boolean retval = false;

	String featureName = featureRef.getBareName();

	if (valueName.equalsIgnoreCase(ANY_PREFIX + featureName))
	    retval = true;

	return retval;
    } // end isAnyDef

    /**
     * converts this to an sexpression for lisp output
     */
    @Override
    public SExpressionList toSExpression() {
	SExpressionList retval = new SExpressionList();
	retval.add(featureRef.toSExpression());
	SExpressionSymbol val;
	if (valueName.equals("+") || valueName.equals("-")) // these don't use the f:: package
	    val = SExpressionSymbol.createUnqualified(valueName);
	else
	    val = SExpressionSymbol.createInPackage(LispOntology.FEATURE_PACKAGE,
						    valueName,false);
	retval.add(val);
	return retval;
    } // end toSExpression

    /**
     * prints out in original form (super sub)
     */
    @Override
    public String toString() {
	return toSExpression().toString();
    } // end toString

} // end class OntNamedFeatureValueRef
