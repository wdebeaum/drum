package TRIPS.OWL.lispont;

import java.util.*;
import edu.stanford.smi.protegex.owl.model.*;
import edu.stanford.smi.protegex.owl.*;

import TRIPS.OWL.sexp.*;
import TRIPS.OWL.*;

/**
 * this encapsulates a reference in the def file to a feature of the form
 * or (super (? x ...))
 */
public class OntUnionFeatureValueRef extends OntFeatureValueRef {

    public List<OntNamedFeatureValueRef> featureVals;

    protected OntUnionFeatureValueRef() {
	featureVals = new ArrayList<OntNamedFeatureValueRef>();
    }

//     public OntUnionFeatureValueRef(List<OntNamedFeatureValueRef> fRefs) {
// 	features = fRefs;
//     } // end constructor

//     public static OntUnionFeatureValueRef fromUnionFeatureValue(OntUnionFeatureValue union) {
// 	return fromClasses(union.getNamedOperands());
//     }

//     public static OntUnionFeatureValueRef fromClasses(List<OntNamedFeatureValue> owlFeatures) {
// 	OntUnionFeatureValueRef retval = new OntUnionFeatureValueRef();
// 	for (OntNamedFeature feature : owlFeatures) {
// 	    retval.features.add(new OntNamedFeatureValueRef(feature));
// 	}
// 	return retval;
//     } // end constructor

    /**
     * creates this from (feature (? x value value ...))
     */
    public OntUnionFeatureValueRef(SExpressionList lst) {
	OntFeatureRef featureRef = new OntFeatureRef(LispOntology.getSymbolName(lst.get(0)));

	SExpressionList subList = (SExpressionList)lst.get(1);
	SExpressionList names = getNames(subList);

	List<OntNamedFeatureValueRef> fs = new ArrayList<OntNamedFeatureValueRef>();

	for (SExpression name : names) {
	    fs.add(new OntNamedFeatureValueRef(featureRef,(SExpressionSymbol)name));
	}

	featureVals = fs;
    } // end createFromSubList

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
    public OntUnionFeatureValue getReferent() {
	return createReferent();
    }

    /**
     * retrieves the class or creates it
     */
    @Override
    public OntUnionFeatureValue createReferent() {
	List<OntNamedFeatureValue> values = new ArrayList<OntNamedFeatureValue>();

	for (OntNamedFeatureValueRef featureVal : featureVals) {
	    values.add(featureVal.getReferent());
	}

	return OntUnionFeatureValue.createFromValues(values);
    } // end createReferent

    /**
     * converts this to an sexpression for lisp output
     */
    @Override
    public SExpressionList toSExpression() {
	SExpressionList retval = new SExpressionList();
	retval.add(SExpressionSymbol.createUnqualified("?"));
	retval.add(SExpressionSymbol.createUnqualified("x"));

	Collections.sort(featureVals);
	for (OntNamedFeatureValueRef ref : featureVals) {
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

} // end class OntUnionFeatureValueRef