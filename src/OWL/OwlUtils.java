package TRIPS.OWL;

import java.util.*;
import java.util.regex.*;
import java.io.*;
import edu.stanford.smi.protegex.owl.*;
import edu.stanford.smi.protegex.owl.model.*;

//import TRIPS.OWL.util.*;

/**
 * Some utilities for owl.
 * Most of these are replacements for methods in the API that didn't quite do
 * the right thing.
 *
 * There are a bunch of methods relating to subclassing and equivalence. Here
 * is a guide to how they are named:
 <ul>
 <li>sub/superclasses may include the class itself and equivalent classes, "pure" sub/superclasses don't
 <li>sub/superclasses include indirect relationships (e.g. subclass of a subclass), "direct" sub/superclasses don't
 <li>getFoo gets only asserted Foo, getInferredFoo gets only inferred Foo (in the mind of whoever wrote this code, probably not in reality), and getFooWithInferred gets both
 <li>usually both named and anonymous classes are returned, but getNamed* returns only named classes
 </ul>
 */
public class OwlUtils {

    /**
     * the OWL/Jena version of this doesn't deal well with non-alpha chars like * and +
     */
    public static String getLocalName(RDFResource r) {
	if (r == null)
	    return "NULL";
	String name = r.getName();
	// just take whatever is after the first #
	int pos = name.indexOf("#");
	String retval = name.substring(pos+1);
	return retval;
    } // end getLocalName

    public static String getLocalName(String s) {
	if (s == null)
	    return "NULL";
	// just take whatever is after the first #
	int pos = s.indexOf("#");
	String retval = s.substring(pos+1);
	return retval;
    } // end getLocalName

    public static String collectionToString(Iterable coll) {
	String retval = "[";
	for (RDFResource r : (Iterable<RDFResource>)coll) {
	    retval += r.getBrowserText() + ", ";
	}
	
	retval += "]";
	return retval;
    }

    /**
     * Return a version of name that can be used as the name of a class.
     * Characters that don't belong in names are replaced with a "U" followed
     * by at least four hexadecimal digits encoding the Unicode code point of
     * the character.
     */
    public static String encodeClassName(String name) {
      Pattern p = Pattern.compile("[^\\w*-\\.]");
      Matcher m = p.matcher(name);
      StringBuffer sb = new StringBuffer();
      while (m.find()) {
	int codePoint = name.codePointAt(m.start());
	String replacement = String.format("U%04X", codePoint);
	m.appendReplacement(sb, replacement);
      }
      m.appendTail(sb);
      return sb.toString();
    }

    /**
     * central point for named class creation (for some reason, protege always asserts
     * Thing as a direct parent, which makes the hierarchy impossible to read
     * this one removes it
     */
    public static OWLNamedClass createOWLNamedClass(String name) {
	// HACK to avoid naming classes with weird characters and causing
	// crashes when running SQWRL queries
	name = encodeClassName(name);
	OWLNamedClass retval = TripsOntology.ont.model.createOWLNamedClass(name);
	retval.removeSuperclass(TripsOntology.ont.model.getOWLThingClass());
	return retval;
    } // end createOWLNamedClass

    /**
     * NB: there seem to be problems ProtegeReasoner.isSubsumedBy.  If union classes
     * are in either argument, it always returns false.
     *
     * this method checks those cases specially so it produces the expected behavior
     */
    public static boolean isSubsumedBy(OWLClass subsumee, OWLClass subsumer) {
	try {

	if (!(subsumee instanceof OWLUnionClass) && 
	    !(subsumer instanceof OWLUnionClass)) // simple case
	    return (TripsOntology.ont.reasoner.isSubsumedBy(subsumee, subsumer));
		
	// if we're here, one or both classes are unions
	// we convert each side to a list of named classes and then
	// isSubsumedBy is true iff each class in thisList is subsumed by at least
	// one class in subsumerList
	List<OWLNamedClass> subsumeeList;
	if (subsumee instanceof OWLUnionClass)
	    subsumeeList = Utils.castList(((OWLUnionClass)subsumee).getNamedOperands(),
					  OWLNamedClass.class);
	else
	    subsumeeList = Collections.singletonList((OWLNamedClass)subsumee);

	List<OWLNamedClass> subsumerList;
	if (subsumer instanceof OWLUnionClass)
	    subsumerList = Utils.castList(((OWLUnionClass)subsumer).getNamedOperands(),
					  OWLNamedClass.class);
	else
	    subsumerList = Collections.singletonList((OWLNamedClass)subsumer);

	boolean retval = true;
	for (OWLNamedClass subsumeeClass : subsumeeList) {
	    if (!isSubsumedByList(subsumeeClass,subsumerList)) {
		retval = false;
		break;
	    }
	} // end for each subsumee class

	return retval;
	} catch (Exception e) {
	    throw new RuntimeException("problem in subsumption test of " + subsumee + " and " 
				       + subsumer);
	}
    } // end isSubsumedBy

    /**
     * returns true if the subsumee is subsumed by at least one of the items in the subsumerList
     */
    protected static boolean isSubsumedByList(OWLNamedClass subsumee, 
					      List<OWLNamedClass> subsumerList) {
	boolean retval = false;
	for (OWLNamedClass subsumer : subsumerList) {
	    try {
	    if (TripsOntology.ont.reasoner.isSubsumedBy(subsumee,subsumer)) {
		retval = true;
		break;
	    }
	    } catch (Exception e) {
		throw new RuntimeException("problem testing subsumption: " + subsumee + " & " +
					   subsumer);
	    }
	}
	return retval;
    } // end isSubsumedByList



    /**
     * for a given class, this returns a Set of both the asserted and the inferred equivalent
     * classes
     */
    @SuppressWarnings("unchecked")
    public static Set<OWLClass> getEquivalentClassesWithInferred(OWLNamedClass type) {
	Set<OWLClass> retval = new HashSet<OWLClass>();
	// FIXME isn't this redundant? the inferred equivalent classes should include the asserted ones, right? (and BTW, the "asserted" ones include not only explicit occurrences of owl:equivalentClass, but any case of mutual subclassing)
	retval.addAll((Collection<OWLClass>)type.getEquivalentClasses());
	retval.addAll((Collection<OWLClass>)type.getInferredEquivalentClasses());
	return retval;
    }

    /**
     * for a given class, this returns the "pure" (or better "proper") superclasses
     * (without the equivalent classes) that have been asserted or inferred
     */
    @SuppressWarnings("unchecked")
    public static Set<OWLClass> getPureSuperclassesWithInferred(OWLNamedClass type) {
	Set<OWLClass> equivs = getEquivalentClassesWithInferred(type);
	Set<OWLClass> retval = getSuperclassesWithInferred(type);
	retval.removeAll(equivs);  // only get proper superclasses
	return retval;
    }

    /**
     * this returns all the superclasses (asserted or inferred) for the given class
     */
    @SuppressWarnings("unchecked")
    public static Set<OWLClass> getSuperclassesWithInferred(OWLNamedClass type) {
	Set<OWLClass> retval = new HashSet<OWLClass>();
	retval.addAll((Collection<OWLClass>)type.getPureSuperclasses());
	retval.addAll((Collection<OWLClass>)type.getInferredSuperclasses());
	return retval;
    }

    /**
     * returns all superclasses, including those that are inferred
     */
    @SuppressWarnings("unchecked")
    public static Set<OWLNamedClass> getNamedSuperclassesWithInferred(OWLNamedClass type,
								      boolean transitive) {
	Set<OWLNamedClass> retval = new HashSet<OWLNamedClass>();
	retval.addAll((Collection<OWLNamedClass>)type.getNamedSuperclasses(transitive));
	Collection<OWLClass> inferredClasses = type.getInferredSuperclasses();
	for (OWLClass inferred : inferredClasses) {
	    retval.addAll((Collection<OWLNamedClass>)inferred.getNamedSuperclasses(transitive));
	}

	return retval;
    }

    /**
     * returns all the subclasses (asserted or inferred) for the given class.
     */
    @SuppressWarnings("unchecked")
    public static Set<OWLClass> getSubclassesWithInferred(OWLNamedClass type) {
	Set<OWLClass> retval = new HashSet<OWLClass>();
	// getInferredSubclasses returns asserted subclasses too, so this is
	// unnecessary (not sure why this one was commented out and not the
	// others, though...)
	//retval.addAll((Collection<OWLClass>)type.getSubclasses());
	//System.out.println(getLocalName(type) + " subclasses: " + collectionToString(retval));
	retval.addAll((Collection<OWLClass>)type.getInferredSubclasses());
	//System.out.println("\tinferred: " + collectionToString(type.getInferredSubclasses()));
	return retval;
    }


    /**
     * returns the 'pure' or proper subclasses of the namedclass.
     */
    @SuppressWarnings("unchecked")
    public static Set<OWLClass> getPureSubclassesWithInferred(OWLNamedClass type) {
	Set<OWLClass> equivs = getEquivalentClassesWithInferred(type);
	Set<OWLClass> retval = getSubclassesWithInferred(type);
	//System.out.println(type + " equivs: " + equivs);
 	//System.out.println("\tsubs: " + retval);
	retval.removeAll(equivs);  // only get proper superclasses
	retval.remove(type); // sometimes self gets in there
 	//System.out.println("\tret: " + retval);
	return retval;
    }
	
    /**
     * returns all named direct subclasses of the namedclass. Does not use
     * stored inferences.
     */
    @SuppressWarnings("unchecked")
    public static Set<OWLClass> getDirectSubclasses(OWLNamedClass type) {
	Set<OWLClass> retval = new HashSet<OWLClass>();
	retval.addAll((Collection<OWLClass>)type.getNamedSubclasses(false));
	return retval;
    }
} // end class OwlUtils
