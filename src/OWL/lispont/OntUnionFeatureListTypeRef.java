package TRIPS.OWL.lispont;

import java.util.*;
import edu.stanford.smi.protegex.owl.model.*;
import edu.stanford.smi.protegex.owl.*;

import TRIPS.OWL.sexp.*;
import TRIPS.OWL.*;

/**
 * this encapsulates a reference in the def file to a feature list type of the form
 * (? x ...) --- e.g., (? rst F::phys-obj F::abstr-obj F::situation F::proposition)
 */
public class OntUnionFeatureListTypeRef extends OntFeatureListTypeRef {

    public List<OntNamedFeatureListTypeRef> featureListTypes =
	new ArrayList<OntNamedFeatureListTypeRef>();

    protected OntUnionFeatureListTypeRef() {
	featureListTypes = new ArrayList<OntNamedFeatureListTypeRef>();
    }

    /**
     * creates this from a bare list of the form (? var name1 name2 ...)
     */
    public OntUnionFeatureListTypeRef(SExpressionList lst) {
	SExpressionSymbol first = (SExpressionSymbol)lst.get(0);
	if (!first.getName().equals("?"))
	    throw new RuntimeException("not a union list (doesn't start with ?): " + lst);

	SExpressionList names = getNames(lst);
	for (SExpression name : names) {
	    featureListTypes.add(new OntNamedFeatureListTypeRef((SExpressionSymbol)name));
	}
    } // end createFromBareList

    /**
     * this takes a list of the form (? var name1 name2 ...) and returns just the names,
     * properly formatted for tripsontology
     */
    protected static SExpressionList getNames(SExpressionList lst) {
	return lst.rest().rest(); // skip first 2
    } // end getNames

    /**
     * resolves the references --
     * just calls create (impossible to 'search' for anonymous class in
     * protege since they're linked to a named class always)
     */
    @Override
    public OntUnionFeatureListType getReferent() {
	return createReferent();
    }

    /**
     * retrieves the class or creates it
     */
    @Override
    public OntUnionFeatureListType createReferent() {
	List<OntNamedFeatureListType> types = new ArrayList<OntNamedFeatureListType>();

	for (OntNamedFeatureListTypeRef feature : featureListTypes) {
	    types.add(feature.getReferent());
	}

	return OntUnionFeatureListType.createFromTypes(types);
    } // end createReferent

    /**
     * converts this to an sexpression for lisp output
     */
    @Override
    public SExpressionList toSExpression() {
	SExpressionList retval = new SExpressionList();
	retval.add(SExpressionSymbol.createUnqualified("?"));
	retval.add(SExpressionSymbol.createUnqualified("x"));

	Collections.sort(featureListTypes);
	for (OntNamedFeatureListTypeRef ref : featureListTypes) {
	    retval.add(ref.toSExpression());
	}

	return retval;
    } // end toSExpression

    /**
     * prints out like it was in lisp (kind of)
     */
    @Override
    public String toString() {
	String retval = SExpressionPrettyPrinter.makeString(toSExpression());

	return retval;
    } // end toString

} // end class OntUnionFeatureListTypeRef