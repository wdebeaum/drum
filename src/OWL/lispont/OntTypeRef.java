package TRIPS.OWL.lispont;

import java.util.*;
import edu.stanford.smi.protegex.owl.model.*;
import edu.stanford.smi.protegex.owl.*;

import TRIPS.OWL.sexp.*;
import TRIPS.OWL.*;

/**
 * this encapsulates a reference to a type
 */
public class OntTypeRef extends OntObjectRef {

    /**
     * the name of the type
     */
    protected String name;
    public String getName() {return name;}

    /**
     * creates a new classref from a name
     */
    public OntTypeRef(String name) {
	if (name == null)
	    throw new RuntimeException("attempt to create using null name");
	if (name.equals(""))
	    throw new RuntimeException("attempt to create using empty string name");
	this.name = name;
    }

    /**
     * creates a reference to the given type
     */
    public OntTypeRef(OntType res) {
	this(res.getName());
    }

    /**
     * creates a new ref from an SExpressionSymbol
     */
    public OntTypeRef(SExpression name) {
	if (!(name instanceof SExpressionSymbol)) {
	    throw new RuntimeException("bare class ref arg is not a sybmol: " + name);
	}

	SExpressionSymbol sname = (SExpressionSymbol)name;
	if (!sname.isInPackage(LispOntology.ONT_PACKAGE))
	    // exceptions for + and -
	    if (!(sname.toString().equals("+")) && !(sname.toString().equals("-")))
		throw new RuntimeException("attempt to create a type without an ONT package symbol: " + sname);
	this.name = LispOntology.getSymbolName(name);
    } // end constructor

    /**
     * resolves the reference as an OWL class
     */
    @Override
    public OntType getReferent() {
	return OntType.getNew(getName());
    } // end getOntClass

    @Override
    public OntType createReferent() {
	return OntType.createNew(getName());
    } // end createReferent

    /**
     * outputs string for lisp (with package name) (e.g., ONT:move)
     */
    @Override
    public SExpressionSymbol toSExpression() {
	return SExpressionSymbol.createInPackage(LispOntology.ONT_PACKAGE,getName());
    } // end toSExpression

    /**
     * just prints out the bareName
     */
    @Override
    public String toString() {
	return getName();
    }

} // end class OntTypeRef