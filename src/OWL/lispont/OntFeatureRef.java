package TRIPS.OWL.lispont;

import java.util.*;
import edu.stanford.smi.protegex.owl.model.*;
import edu.stanford.smi.protegex.owl.*;

import TRIPS.OWL.sexp.*;
import TRIPS.OWL.*;

/**
 * this class refers to a OntFeature
 */
public class OntFeatureRef extends OntObjectRef {

    /**
     * the name of the type (without the feature prefix)
     */
    protected String bareName;
    public String getBareName() {
	return bareName;
    }

    /**
     * creates a new classref from a name
     */
    public OntFeatureRef(String bareName) {
	if (bareName == null)
	    throw new RuntimeException("attempt to create using null name");
	if (bareName.equals(""))
	    throw new RuntimeException("attempt to create using empty string name");
	this.bareName = bareName;
    }

    /**
     * creates a reference to the given type
     */
    public OntFeatureRef(OntFeature res) {
	this(TripsOntology.stripPrefix(TripsOntology.FEATURE_PREFIX,res.getName()));
    }

    /**
     * creates a new ref from an SExpressionSymbol
     */
    public OntFeatureRef(SExpression name) {
	if (!(name instanceof SExpressionSymbol))
	    throw new RuntimeException("bare class ref arg is not a sybmol: " + name);

	SExpressionSymbol sname = (SExpressionSymbol)name;
	if (!sname.isInPackage(LispOntology.FEATURE_PACKAGE))
	    throw new RuntimeException("attempt to create a featureref with a non-feature package symbol: " + sname);

	this.bareName = LispOntology.getSymbolName(sname);
    } // end constructor

    /**
     * returns the name of the feature as it appears in owl (with the prefix)
     */
    public String getOwlName() {
	return TripsOntology.FEATURE_PREFIX + getBareName();
    }

    /**
     * resolves the reference as an OWL class
     */
    @Override
    public OntFeature getReferent() {
	return OntFeature.getNew(getOwlName());
    } // end getOntClass

    @Override
    public OntFeature createReferent() {
	return OntFeature.createNew(getOwlName());
    } // end createReferent

    /**
     * outputs string for lisp (with package name) (e.g., ONT:move)
     */
    @Override
    public SExpressionSymbol toSExpression() {
	return SExpressionSymbol.createInPackage(LispOntology.FEATURE_PACKAGE,getBareName());
    } // end toSExpression

    /**
     * prints the owl name
     */
    @Override
    public String toString() {
	return getOwlName();
    }

} // end class OntFeatureRef