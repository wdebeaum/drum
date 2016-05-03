package TRIPS.OWL.lispont;

import java.util.*;

import edu.stanford.smi.protegex.owl.model.*;

import TRIPS.OWL.sexp.*;
import TRIPS.OWL.*;

/**
 * this represents an item in the :arguments under define-type
 */
public class TypeArgument {

    /**
     * what argument type it is (essential, required, optional)
     */
    String type;

    /**
     * the property it's on
     */
    OntPropertyRef propertyRef;

    /**
     * the restriction part
     */
    FeatureVector restriction;

    /**
     * the :imlements (string until we figure it out)
     */
    String implementsDecl;

    /**
     * creates from a Sexp from the lisp ont of form (:TYPE ONT::PROPERTYNAME [(FLTYPE [r1 ...])])
     */
    public TypeArgument(SExpressionList sexp) {
	if ((sexp.size() < 2) || (sexp.size() > 4)) // check size assumptions
	    throw new RuntimeException("arg def doesn't have between 2 and 4 elements: " + sexp);

	type = sexp.get(0).toString();
	propertyRef = new OntPropertyRef(sexp.get(1));

	if (sexp.size() >= 3) { // we have a restriction decl	    
	    restriction = FeatureVector.create(sexp.get(2).asList());
	} // if has non-empty restrictiondecl

	// get :implements if it's there
	if (sexp.size() == 4) {
	    SExpressionList impList = sexp.get(3).asList();
	    if (impList.size() != 2)
		throw new RuntimeException("unexpected size (not 2) of implements list: " + impList);
	    implementsDecl = impList.get(1).asSymbol().getName();
	} // end if implements
	
    } // end constructor
    
    /**
     * assumes that the property has already been created
     * using the restrictions, it creates and returns an OWLRestriction
     * returns null if restrictions is empty
     */
    public OWLRestriction createRestrictionClass() {
	// get the prop
	OntProperty property = propertyRef.getReferent();
	if (property == null) 
	    throw new RuntimeException("property not found; " + propertyRef);

	List<OWLClass> includedFeatures = new ArrayList<OWLClass>();

	// check if we have restrictions, if not, put from root
	if ((restriction == null) || (restriction.featureListType == null)) {
	    includedFeatures.add(TripsOntology.ont.getRoot().getResource());
	} else { // has restrictions
	    // for now, only do the :required -- ignoring the :defaults
	    OntFeatureListType flType = restriction.featureListType.getReferent();
	    if (flType == null) {
		TripsOntologyExporter.warn("no referent found for feature-list-type " 
					   + restriction.featureListType +
					   " on restriction " + this + ". Ignoring.");
		return null;
	    }
	    includedFeatures.add(flType.getResource());
	    for (OntFeatureValueRef req : restriction.requiredSems) {
		OntFeatureValue required = req.getReferent();
		if (required != null)
		    includedFeatures.add(required.getResource());
	    } // end for each restriction
	}

	OWLClass fillerClass;
	if (includedFeatures.isEmpty()) { // no listed restrictions, just the name of the type
	    return null;
	} else if (includedFeatures.size() == 1) { // just a single filler
	    fillerClass = includedFeatures.get(0);
	} else { // more than one filler, we need a union
	    fillerClass = TripsOntology.ont.model.createOWLIntersectionClass(includedFeatures);
	}

	OWLRestriction retval = null;

	if (type.equalsIgnoreCase(":essential") || type.equalsIgnoreCase(":required")) {
	    retval = 
		TripsOntology.ont.model.createOWLSomeValuesFrom(property.getResource(),fillerClass);
	} else if (type.equalsIgnoreCase(":optional")) {
	    retval = 
		TripsOntology.ont.model.createOWLAllValuesFrom(property.getResource(),fillerClass);
	} else { // unknown type
	    throw new RuntimeException("unexpected type: " + type);
	}

	return retval;
    } // end createRestrictionClass

    public SExpressionList toSExpression() {
	SExpressionList retval = new SExpressionList();

	retval.add(SExpressionSymbol.createKeyword(type.substring(1)));
	retval.add(propertyRef.toSExpression());
	retval.add(restriction.toSExpression());

	return retval;
    } // end toSExpression

    @Override
    public String toString() {
	return toSExpression().toString();
    }

} // end class TypeArgument