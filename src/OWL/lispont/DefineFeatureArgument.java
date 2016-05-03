package TRIPS.OWL.lispont;

import java.util.*;

import TRIPS.OWL.sexp.*;

/**
 * a java rep of the lisp define-feature-argument structures used in the ontology
 */
public class DefineFeatureArgument {

    /**
     * the name
     */
    // public String name; // no names

    /**
     * the feature value that implies the rest
     */
    public OntNamedFeatureValueRef featureValueRef;

    /**
     * the declared roles (content of :arguments)
     */
    public List<TypeArgument> arguments = new ArrayList<TypeArgument>();

    /**
     * create from an SExpressionList
     */
    public DefineFeatureArgument(SExpressionList lispExpr) {

	// make sure this is a define-type
	if (!lispExpr.get(0).equals(LispOntology.DEFINE_FEATURE_ARGUMENTS_SYMBOL)) {
	    throw new RuntimeException("not a define feature arguments.  " + 
				       "Expected first symbol to be " + 
				       LispOntology.DEFINE_FEATURE_ARGUMENTS_SYMBOL + 
				       " but instead it is " + 
				       lispExpr.get(0) + " full def:\n" + lispExpr);
	}

	SExpressionList featureList = (SExpressionList) lispExpr.getKeywordVal("feature");
	if (featureList == null)
	    throw new RuntimeException("no :feature found on " + lispExpr);
	featureValueRef = new OntNamedFeatureValueRef(featureList);

	SExpressionList argumentsList = (SExpressionList) lispExpr.getKeywordVal("arguments");
	if ((argumentsList == null) || (argumentsList.isEmpty()))
	    throw new RuntimeException("no or blank :arguments found on " + lispExpr);
	for (SExpression arg : argumentsList) {	    
	    // there is now tag on these, but assume it's :ESSENTIAL for now
	    arg.asList().add(0,SExpressionSymbol.createKeyword("ESSENTIAL"));
	    arguments.add(new TypeArgument(arg.asList()));
	}
    } // end constructor

    /**
     * output a string for output to a file like the original lisp
     */
    public SExpressionList toSExpression() {
	SExpressionList retval = new SExpressionList();

	retval.add(LispOntology.DEFINE_FEATURE_ARGUMENTS_SYMBOL);
	retval.add(SExpressionSymbol.createKeyword("feature"));
	retval.add(featureValueRef.toSExpression());
	retval.add(SExpressionSymbol.createKeyword("arguments"));
	SExpressionList argList = new SExpressionList();
	retval.add(argList);
	for (TypeArgument argument : arguments)
	    argList.add(argument.toSExpression());


	return retval;
    } // end toSExpression

    @Override
    public String toString() {
	return SExpressionPrettyPrinter.makeString(toSExpression());
    } // end toString

} // end class DefineFeatureArgument