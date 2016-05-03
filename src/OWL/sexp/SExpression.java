package TRIPS.OWL.sexp;

import java.io.*;
import java.util.*;

import TRIPS.KQML.*;

/**
 * an s-expression
 * these can either be an SExpressionList or an SExpressionAtom
 * still don't have some subclasses of Atom (e.g., string, number, char, etc.)
 * also don't handle things like quoting, backquotes, commas, comma-at, etc.
 */
public abstract class SExpression {

    /**
     * shorthand for (this instanceof SExpressionList) -- looks nicer in code
     */
    public boolean isList() {
	return (this instanceof SExpressionList);
    }

    /**
     * casts this to a SExpressionList (if it isn't throws an exception)
     */
    public SExpressionList asList() {
	if (!isList()) {
	    throw new RuntimeException("SExpression " + this + " is not a list");
	}
	return (SExpressionList) this;
    }

    public boolean isAtom() {return (this instanceof SExpressionAtom);}
    public SExpressionAtom asAtom() {return (SExpressionAtom) this;}

    public boolean isSymbol() {return (this instanceof SExpressionSymbol);}
    public SExpressionSymbol asSymbol() {return (SExpressionSymbol) this;}

    /**
     * returns true if this is a symbol *and* isKeyword() is true
     */
    public boolean isKeyword() {
	if (!isSymbol())
	    return false;
	return asSymbol().isKeyword();
    } // end isKeyword

    /**
     * converts the kqml object into an SExpression
     */
    public static SExpression fromKQML(KQMLObject kqml) {
	if (kqml instanceof KQMLList) {
	    return new SExpressionList((KQMLList)kqml);
	} else if (kqml instanceof KQMLPerformative) {
	    return new SExpressionList(((KQMLPerformative)kqml).toList());
	} else if (kqml instanceof KQMLToken) {
	    return SExpressionSymbol.fromKQML((KQMLToken)kqml);
	} else {
	    throw new RuntimeException("unexpected class of kqml: " + kqml.getClass() 
				       + " :: " + kqml);
	}
    } // end fromKQML

} // end class SExpression
