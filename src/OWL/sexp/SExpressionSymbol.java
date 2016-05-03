package TRIPS.OWL.sexp;

import java.io.*;
import java.util.*;

import TRIPS.KQML.*;

/**
 * a symbol in an SExpression
 */
public class SExpressionSymbol extends SExpressionAtom implements Serializable {
    protected static final long serialVersionUID = 44;

    /**
     * the name of the package (null if no declared package, empty string if keyword package)
     */
    protected String lispPackage;
    public String getPackage() {return lispPackage;}

    /**
     * the name of the symbol itself (without the package or :)
     */
    protected String name;
    public String getName() {return name;}

    /**
     * the type of package reference made
     */
    protected PackageReferenceType packageReferenceType;

    /**
     * creates a new symbol from the given info, use static create methods from outside
     */
    private SExpressionSymbol(String lispPackage, String name, PackageReferenceType packRefType) {
	if (name == null)
	    throw new RuntimeException("attempt to create symbol with null name");
	if (name.equals(""))
	    throw new RuntimeException("attempt to create symbol with empty string name");
	this.lispPackage = lispPackage;
	this.name = name;
	this.packageReferenceType = packRefType;
    } // end constructor

    /**
     * creates a new unqualified symbol
     */
    public static SExpressionSymbol createUnqualified(String name) {
	return new SExpressionSymbol(null, name, PackageReferenceType.UNQUALIFIED);
    } // end createUnqualified

    /**
     * creates a new symbol in the keyword package
     */
    public static SExpressionSymbol createKeyword(String name) {
	return new SExpressionSymbol("", name, PackageReferenceType.KEYWORD);
    }

    /**
     * creates a new symbol in the given package, either internal or external
     */
    public static SExpressionSymbol createInPackage(String lispPackage, String name, 
						   boolean externalReference) {
	PackageReferenceType refType;
	if (externalReference)
	    refType = PackageReferenceType.EXTERNAL;
	else
	    refType = PackageReferenceType.INTERNAL;
	return new SExpressionSymbol(lispPackage, name, refType);
    } // end createInPackage

    /**
     * creates a new symbol in the given package with INTERNAL reference type
     */
    public static SExpressionSymbol createInPackage(String lispPackage, String name) {
	return createInPackage(lispPackage, name, false);
    } // end createInPackage

    /**
     * creates from KQMLToken
     */
    public static SExpressionSymbol fromKQML(KQMLToken kqml) {
	String lispPackage = kqml.getPackage();
	String name = kqml.getName();
	PackageReferenceType refType;
	if (lispPackage == KQMLToken.KEYWORD_PACKAGE_NAME) {
	    refType = PackageReferenceType.KEYWORD;
	    lispPackage = "";
	} else if (lispPackage == null) {
	    refType = PackageReferenceType.UNQUALIFIED;
	} else {
	    refType = PackageReferenceType.INTERNAL;
	}
	return new SExpressionSymbol(lispPackage,name,refType);
    }

    /**
     * creates a new symbol by parsing the string
     * throws an exception if recognized as not a valid symbol decl
     * (right now doesn't do a good job of recognizing if valid)
     */
    public static SExpressionSymbol parseSymbol(String symbolStr) {
	SExpressionSymbol retval = null;

	// split up package and name
	int dividorStartPos = symbolStr.indexOf(':');
	int dividorEndPos = dividorStartPos;
	if (dividorStartPos == -1) { // no package
	    retval = createUnqualified(symbolStr);
	} else { // there is a package
	    boolean isExternal = true;
	    if (symbolStr.charAt(dividorStartPos+1) == ':') { // has double colon
		isExternal = false;
		dividorEndPos = dividorStartPos+1;
	    }

	    String name = symbolStr.substring(dividorEndPos+1);	    

	    if (dividorStartPos == 0) { // colon is first -- keyword
		retval = createKeyword(name);
	    } else { // normal package
		String lispPackage = symbolStr.substring(0,dividorStartPos);
		retval = createInPackage(lispPackage, name, isExternal);
	    }

	} // end else (there is a package

	//System.err.println("new atom: " + symbolStr + " --> (" + lispPackage + "," + name + ")");

	return retval;
    } // end parseSymbol

    /**
     * override to make equal if same content
     * note that we (somewhat arbitrarily) say 2 symbols are not equal
     * if they have different packageReferenceType  (even if all that is different
     * is that one symbol is external and the other intnerlal in refernece
     */
    public boolean equals(Object o) {
	boolean retval = true;

	// right type
	if (!(o instanceof SExpressionSymbol))
	    retval = false;
	else {
	    SExpressionSymbol s = (SExpressionSymbol) o;

	    // matching names
	    if (!(name.equalsIgnoreCase(s.name)))
		retval = false;
	    else if (packageReferenceType != s.packageReferenceType) // matching reference types
		retval = false;
	    else if ((lispPackage == null) && (s.lispPackage == null)) // null packages
		retval = true;
	    else  // matching packages
		retval = (lispPackage != null) && lispPackage.equalsIgnoreCase(s.lispPackage);
	} // end else right type

	return retval;
    } // end equals

    /**
     * override with equals
     */
    public int hashCode() {
	return (lispPackage.hashCode() * 13) + (packageReferenceType.hashCode() * 7)  
	    +  name.hashCode();
    }

    /**
     * returns true if this has a package (even if keyword)
     */
    public boolean hasPackage() {
	return (packageReferenceType != PackageReferenceType.UNQUALIFIED);
    }

    /**
     * returns true if the package matches the string given
     */
    public boolean isInPackage(String pack) {
	if (hasPackage() && lispPackage.equalsIgnoreCase(pack))
	    return true;
	else
	    return false;
    } // end isInPackage

    /**
     * returns true if this is in the keyword package (.e.g, :keyword)
     */
    public boolean isKeyword() {
	return (packageReferenceType == PackageReferenceType.KEYWORD);
    }

    /**
     * returns a string as it would be displayed in lisp
     */
    public String toString() {
	String retval = null;

	switch(packageReferenceType) {
	case UNQUALIFIED: retval = getName(); break;
	case KEYWORD: retval = ":" + getName(); break;
	case INTERNAL: retval = lispPackage + "::" + getName(); break;
	case EXTERNAL: retval = lispPackage + ":" + getName(); break;
	} // end switch

	return retval;
    } // end toString

    /**
     * the possible values for what kind of package reference this symbol makes
     */
    public enum PackageReferenceType {
	INTERNAL, // ::
        EXTERNAL, // :
        KEYWORD, // in the keyword package (started with a :)
        UNQUALIFIED; // means no package was declared for this
    } // end enum PackageReferneceType

} // end class SExpressionSymbol