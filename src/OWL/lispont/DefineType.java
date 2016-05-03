package TRIPS.OWL.lispont;

import java.util.*;

import TRIPS.util.Misc;
import TRIPS.OWL.sexp.*;

/**
 * a java rep of the lisp define-type structures used in the ontology
 */
public class DefineType implements Comparable<DefineType> {

    /**
     * the refernece of this one
     */
    public OntTypeRef typeRef;

    /**
     * the declared parent of the type
     */
    public OntTypeRef parent;

    /**
     * the declared roles (content of :arguments)
     */
    public List<TypeArgument> arguments = new ArrayList<TypeArgument>();

    /**
     * the semantics (:sem)
     */
    public FeatureVector sem;

    /**
     * just creates a blank one
     */
    public DefineType() {
	// do nothing
    }

    /**
     * create from an SExpressionList
     */
    public DefineType(SExpressionList lispExpr) {

	try {
	// make sure this is a define-type
       if (!lispExpr.get(0).equals(LispOntology.DEFINE_TYPE_SYMBOL)) {
	    throw new RuntimeException("not a define type.  Expected first symbol to be " + 
				       LispOntology.DEFINE_TYPE_SYMBOL + " but instead it is " + 
				       lispExpr.get(0) + " full def:\n" + lispExpr);
	}

	typeRef = new OntTypeRef(lispExpr.get(1));

	SExpression parentExpr = lispExpr.getKeywordVal("parent");
	if (parentExpr != null) { // has a parent
	    parent = new OntTypeRef(parentExpr);
	} // end if has parent

	SExpressionList semList = (SExpressionList) lispExpr.getKeywordVal("sem");
	if (semList != null)
	    sem = FeatureVector.create(semList);

	SExpressionList argList = (SExpressionList) lispExpr.getKeywordVal("arguments");
	if (argList != null)
	    parseArgList(argList);

	} catch (Exception e) {
	    throw new RuntimeException("problem creating DefineType: " + lispExpr,e);
	}
    } // end constructor

    /**
     * parses the :arguments list and puts into arguments
     */
    protected void parseArgList(SExpressionList argList) {
	for (SExpression argDecl : argList) {
	    try {

	    TypeArgument arg = new TypeArgument(argDecl.asList());
	    if (arg != null)
		arguments.add(arg);

	    } catch (Exception e) {
		//throw new RuntimeException("problem processing argument " + argDecl,e);
		System.err.println("Problem processing argument on " + 
				   typeRef.toString().toUpperCase()  + 
				   ". Ignoring argument. " + argDecl + "\n   " +
				   Misc.getExceptionChain(e) + "\n");
	    }
	} // end for each arg
    } // end parseArgList

    /**
     * converts to a SExpression like in the file
     */
    public SExpressionList toSExpression() {
	SExpressionList retval = new SExpressionList();

	retval.add(LispOntology.DEFINE_TYPE_SYMBOL);
	
	// name
	retval.add(typeRef.toSExpression());

	// parent
	if (parent != null) { // root doesn't have a parent
	    retval.add(SExpressionSymbol.createKeyword("parent"));
	    retval.add(parent.toSExpression());
	}

	// sem
	if (sem != null) {
	    retval.add(SExpressionSymbol.createKeyword("sem"));
	    retval.add(sem.toSExpression());
	} // end if has sem

	if (arguments != null) {
	    retval.add(SExpressionSymbol.createKeyword("arguments"));
	    SExpressionList argList = new SExpressionList();
	    retval.add(argList);
	    for (TypeArgument argument : arguments)
		argList.add(argument.toSExpression());
	}

	return retval;
    } // end toSExpression

    @Override // Comparable
    public int compareTo(DefineType o) {
	return typeRef.getName().compareTo(o.typeRef.getName());
    } // end compareTo
	
    @Override
    public String toString() {
	return SExpressionPrettyPrinter.makeString(toSExpression());
    } // end toString

} // end class DefineType