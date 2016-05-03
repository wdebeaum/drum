package TRIPS.OWL.lispont;

import java.util.*;
import java.io.*;

import TRIPS.OWL.sexp.*;
import TRIPS.OWL.*;

/**
 * this class contains an internal rep of the lisp define-* type ontology declaration
 * used as a middlepoint in importing and exporting to that format
 */
public class LispOntology {

    /**
     * the symbol used to declare a define-* item
     */
    public static final String DEFINE_TYPE_SYMBOL_STR = "define-type";
    public static final SExpressionSymbol DEFINE_TYPE_SYMBOL =
	SExpressionSymbol.createUnqualified(DEFINE_TYPE_SYMBOL_STR);
    public static final String DEFINE_FEATURE_SYMBOL_STR = "define-feature";
    public static final SExpressionSymbol DEFINE_FEATURE_SYMBOL =
	SExpressionSymbol.createUnqualified(DEFINE_FEATURE_SYMBOL_STR);
    public static final String DEFINE_FEATURE_RULE_SYMBOL_STR = "define-feature-rule";
    public static final SExpressionSymbol DEFINE_FEATURE_RULE_SYMBOL =
	SExpressionSymbol.createUnqualified(DEFINE_FEATURE_RULE_SYMBOL_STR);
    public static final String DEFINE_FEATURE_ARGUMENTS_SYMBOL_STR = "define-feature-arguments";
    public static final SExpressionSymbol DEFINE_FEATURE_ARGUMENTS_SYMBOL =
	SExpressionSymbol.createUnqualified(DEFINE_FEATURE_ARGUMENTS_SYMBOL_STR);
    public static final String DEFINE_FEATURE_LIST_TYPE_SYMBOL_STR = "define-feature-list-type";
    public static final SExpressionSymbol DEFINE_FEATURE_LIST_TYPE_SYMBOL =
	SExpressionSymbol.createUnqualified(DEFINE_FEATURE_LIST_TYPE_SYMBOL_STR);

    /**
     * the name of the lisp package that features are declared in
     */
    public static final String FEATURE_PACKAGE = "f";

    /**
     * the name of the lisp package the ontology types are declared in
     */
    public static final String ONT_PACKAGE = "ont";

    /**
     * list of the various types of defs
     */
    public List<DefineFeature> defineFeatureDecls;
    public List<DefineFeatureRule> defineFeatureRuleDecls;
    public List<DefineFeatureArgument> defineFeatureArgumentDecls;
    public List<DefineFeatureListType> defineFeatureListTypeDecls;
    public List<DefineType> defineTypeDecls;

    /**
     * start off with empty lists
     */
    public LispOntology() {
	defineFeatureDecls  = new ArrayList<DefineFeature>();
	defineFeatureRuleDecls = new ArrayList<DefineFeatureRule>();
	defineFeatureArgumentDecls = new ArrayList<DefineFeatureArgument>();
	defineFeatureListTypeDecls = new ArrayList<DefineFeatureListType>();
	defineTypeDecls = new ArrayList<DefineType>();
    } // end constructor

    /**
     * reads in from a file and returns a LispOntology
     */
    public static LispOntology fromFile(String filename) {
	// pull out all the defTypes from the lisp file
	SExpressionReader sreader = new SExpressionReader(filename);
	List<SExpression> defs = sreader.readStream();
	sreader.close();

	return fromListSExpression(defs);
    } // end fromFile

    /**
     * reads in from a list of sexpressions
     */
    public static LispOntology fromListSExpression(List<SExpression> defs) {
	LispOntology retval = new LispOntology();

	for (SExpression defE : defs) {
	  
	    if (!(defE instanceof SExpressionList))
		throw new RuntimeException("definition not a list: " + defE);

	    SExpressionList def = (SExpressionList) defE;

	    SExpression first = def.first();
	    if (!(first instanceof SExpressionSymbol))
		throw new RuntimeException("start of def not a symbol: " + first);

	    SExpressionSymbol definitionType = (SExpressionSymbol) first;

	    if (definitionType.equals(DEFINE_TYPE_SYMBOL)) {
		retval.defineTypeDecls.add(new DefineType(def));
	    } else if (definitionType.equals(DEFINE_FEATURE_SYMBOL)) {
		retval.defineFeatureDecls.add(new DefineFeature(def));
	    } else if (definitionType.equals(DEFINE_FEATURE_RULE_SYMBOL)) {
		retval.defineFeatureRuleDecls.add(new DefineFeatureRule(def));
	    } else if (definitionType.equals(DEFINE_FEATURE_ARGUMENTS_SYMBOL)) {
		retval.defineFeatureArgumentDecls.add(new DefineFeatureArgument(def));
	    } else if (definitionType.equals(DEFINE_FEATURE_LIST_TYPE_SYMBOL)) {
		retval.defineFeatureListTypeDecls.add(new DefineFeatureListType(def));
	    } else if (definitionType.toString().equalsIgnoreCase("in-package")) {
		// skip in-package decls
	    } else { // no match
		throw new RuntimeException("not a define item definition, unknown head symbol: " + def);
	    }

	} // end for each def

	return retval;	
    } // end fromListSExpression

    /**
     * casts this to an atom and strips off the package and returns just the name in all lower case
     * this should be where all things are processed to maintain consistency
     * throws exception if not given an atom
     */
    public static String getSymbolName(SExpression sexp) {
	try {

	    SExpressionSymbol atom = (SExpressionSymbol) sexp;
	    String retval = atom.getName().toLowerCase();
	    //retval = retval.replaceAll("\\+",".plus.");

	    return retval;
	} catch (Exception e) {
	    throw new RuntimeException("problem getting symbol name: " + sexp, e);
	}

    } // end getSymbolName

    /**
     * writes this lisp ontology out to a file
     */
    public void toFile(String filename) {

	// open the file
	try {
	PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(filename)));

	out.println("\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;");
	out.println(";;; FEATURES");
	out.println(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;");
	for (DefineFeature def : defineFeatureDecls) {
	    out.println(def.toSExpression() + "\n");
	}

	out.println("\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;");
	out.println(";;; FEATURE LIST TYPES");
	out.println(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;");
	for (DefineFeatureListType def : defineFeatureListTypeDecls) {
	    out.println(def.toSExpression() + "\n");
	}

	out.println("\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;");
	out.println(";;; TYPES");
	out.println(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;");
	for (DefineType def : defineTypeDecls) {
	    out.println(def.toSExpression() + "\n");
	}

	out.close();

	} catch (Exception e) {
	    throw new RuntimeException("error writing to file; " + filename,e);
	}

    } // end toFile

} // end class LispOntology