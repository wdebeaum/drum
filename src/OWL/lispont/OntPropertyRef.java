package TRIPS.OWL.lispont;

import java.util.*;
import edu.stanford.smi.protegex.owl.model.*;
import edu.stanford.smi.protegex.owl.*;

import TRIPS.OWL.sexp.*;
import TRIPS.OWL.*;

/**
 * this is a referent to an OntProperty
 */
public class OntPropertyRef extends OntObjectRef {

    /**
     * the name of the property (without the property prefix)
     */
    protected String bareName;
    public String getBareName() {
	return bareName;
    }

    /**
     * returns the name of the feature as it appears in owl (with the prefix)
     */
    public String getOwlName() {
	return TripsOntology.PROPERTY_PREFIX + getBareName();
    }


    /**
     * creates a new classref from a name
     */
    public OntPropertyRef(String bareName) {
	if (bareName == null)
	    throw new RuntimeException("attempt to create using null name");
	if (bareName.equals(""))
	    throw new RuntimeException("attempt to create using empty string name");
	this.bareName = bareName;
    }

//     /**
//      * creates a reference to the given type
//      */
//     public OntPropertyRef(OntProperty res) {
// 	this(res.getBareName());
//     }

    /**
     * creates a new ref from an SExpressionSymbol
     */
    public OntPropertyRef(SExpression name) {
	if (!(name instanceof SExpressionSymbol))
	    throw new RuntimeException("bare ref arg is not a sybmol: " + name);

	SExpressionSymbol sname = (SExpressionSymbol)name;
	if (!sname.isInPackage(LispOntology.ONT_PACKAGE) && 
	    !sname.toString().equalsIgnoreCase("content")) // special case for this prop...
	    throw new RuntimeException("attempt to create a property without an ONT package symbol: " + sname);
	this.bareName = LispOntology.getSymbolName(name);
    } // end constructor

    /**
     * resolves the reference as an OWL class
     */
    @Override
    public OntProperty getReferent() {
	return OntProperty.getNew(getOwlName());
    } // end getOntClass

    @Override
    public OntProperty createReferent() {
	return OntProperty.createNew(getOwlName());
    } // end createReferent

    /**
     * gets it, or if it doesn't exist, creates it
     */
    public OntProperty getOrCreateReferent() {
	OntProperty retval = getReferent();
	if (retval == null)
	    retval = createReferent();
	return retval;
    } // end getOrCreateReferent

    /**
     * outputs string for lisp (with package name) (e.g., ONT:move)
     */
    @Override
    public SExpressionSymbol toSExpression() {
	return SExpressionSymbol.createInPackage(LispOntology.ONT_PACKAGE,getBareName());
    } // end toSExpression

    /**
     * just prints out the bareName
     */
    @Override
    public String toString() {
	return getOwlName();
    }

} // end class OntPropertyRef