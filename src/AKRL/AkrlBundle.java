package TRIPS.AKRL;

import TRIPS.KQML.*;
import java.util.*;
import java.io.*;

/**
 * this encapsulates an AKRL expression of the form
 * (verb :content P1 :context ((term1...)(term2)...))
 * which bundles together a context with one of them being the focused content;
 * or an LF (Paragraph ...) list
 *
 * note that, I assume that a bundle contains (or should contain) terms for
 * all IDs used in it
 */
public class AkrlBundle implements Serializable {
    protected static final long serialVersionUID = 2;

    public static final String DEFAULT_VERB = "LogicalForm";

    /**
     * the "verb" or the first item -- we are agnostic to what it is here
     * outside, it may mean different things to the modules passing the AKRL around
     * note, this is only used when the bundle comes from an AKRL formatted list
     * in LF, this is just null
     */
    protected String verb = DEFAULT_VERB;
    public String getVerb() {return verb;}
    public void setVerb(String v) {verb = v;}

    /**
     * the content -- the ID of the part of the context that we're pointing at with this mesg
     * only if from an AKRL-formatted list.  null if from LF
     */
    protected AkrlID contentID = null;
    public AkrlID getContentID() {return contentID;}

    /**
     * the context, a list of terms, which includes the content
     */
    protected Map<AkrlID,AkrlTerm> context = new HashMap<AkrlID,AkrlTerm>();
    public Map<AkrlID,AkrlTerm> getContext() {return context;}

    /**
     * creates a new one from an AKRL-formatted KQMLList
     */
    public static AkrlBundle fromAkrl(KQMLList akrl) {
// 	if ((akrl.size() == 1) && (akrl.get(0).equals(AkrlID.NIL_TOKEN)))
// 	    return null;
	AkrlBundle retval = new AkrlBundle();
	retval.verb = akrl.get(0).toString();
	retval.contentID = new AkrlID((KQMLToken)akrl.getKeywordArg(":content"));
	KQMLList contextExpr = (KQMLList)akrl.getKeywordArg(":context");
	for (KQMLObject termExpr : contextExpr) {
	    try {
	    AkrlTerm term = AkrlTerm.fromAkrl((KQMLList)termExpr);
	    retval.addTerm(term);
	    } catch (Exception e) {
		throw new RuntimeException("problem processing term: " + termExpr,e);
	    }
	} // for each term
	return retval;
    } // end fromAkrl

    /**
     * from a list of LF paragraphs
     */
    public static AkrlBundle fromLFParagraphList(KQMLList lfParagraphList) {
	if (lfParagraphList == null)
	    throw new RuntimeException("passed a null paragraphList");
	if (lfParagraphList.isEmpty())
	    throw new RuntimeException("passed an empty paragraphList");

	AkrlBundle retval = new AkrlBundle();

	for (KQMLObject paragraph : lfParagraphList) {
	    try {
	    AkrlBundle thisBundle = fromLFParagraph((KQMLList)paragraph);
	    retval.addTerms(thisBundle);
	    } catch (Exception e) {
		throw new RuntimeException("problem processing paragraph: " + paragraph,e);
	    }
	}

	return retval;
    } // end fromLFParagraphList

    /**
     * creates a new one from an LF-formatted (paragraph ...)
     */
    public static AkrlBundle fromLFParagraph(KQMLList lfParagraph) {
	if (lfParagraph == null)
	    throw new RuntimeException("passed a null paragraph");
	AkrlBundle retval = new AkrlBundle();

	KQMLList terms = (KQMLList) lfParagraph.getKeywordArg(":terms");
	if (terms == null)
	    throw new RuntimeException("no :terms in this paragraph: " + lfParagraph);
	if (terms.isEmpty())
	    throw new RuntimeException("empty :terms in this paragraph: " + lfParagraph);

	for (KQMLObject sentence : terms) {
	    AkrlBundle thisBundle = fromLFSentence((KQMLList)sentence);
	    retval.addTerms(thisBundle);
	}

	return retval;
    } // end fromLFParagraph

    /**
     * creates from an LF-formatted SENTENCE
     */
    public static AkrlBundle fromLFSentence(KQMLList lfSentence) {
	if (lfSentence == null)
	    throw new RuntimeException("passed a null sentence");
	try {
	KQMLList terms = (KQMLList) lfSentence.getKeywordArg(":terms");
	return fromLFTermList(terms);
	} catch (Exception e) {
	    throw new RuntimeException("problem processing sentence: " + lfSentence,e);
	}
    } // end fromLFSentence

    /**
     * creates from an LF-formatted list of terms
     */
    public static AkrlBundle fromLFTermList(KQMLList terms) {
	if (terms == null)
	    throw new RuntimeException("terms is null");
	if (terms.isEmpty())
	    throw new RuntimeException("terms is empty");

	AkrlBundle retval = new AkrlBundle();

	for (KQMLObject term : terms) {
	    AkrlTerm thisTerm = null;
	    try {
	    thisTerm = AkrlTerm.fromLF((KQMLList)term);
	    } catch (Exception e) {
		e.printStackTrace();
	    }
	    if (thisTerm != null) // if exception on term, skip it
		retval.addTerm(thisTerm);
	}

	return retval;
    } // end fromLFTermList

    /**
     * adds all the terms from the AkrlBundle to this one
     */
    public void addTerms(AkrlBundle otherBundle) {
	for (AkrlTerm newTerm : otherBundle.getContext().values()) {
	    addTerm(newTerm);
	}
    } // end addTerms

    /**
     * adds a term to the bundle
     */
    public void addTerm(AkrlTerm term) {
	AkrlID id = term.getID();
	if (context.get(id) != null)
	    System.out.println("warning: attempt to add term when already defined in this bundle.  Ignoring the newest term:\n  old term:"
			       + context.get(id) + "\n  new term: " + term);
	else
	    context.put(id,term);
    } // end addTerm

    /**
     * returns the AkrlTerm for the given ID, or null if not there
     */
    public AkrlTerm getTerm(AkrlID id) {
	return context.get(id);
    }

    /**
     * returns all the terms
     */
    public List<AkrlTerm> getTerms() {
	return new ArrayList<AkrlTerm>(getContext().values());
    }

    /**
     * returns the AkrlTerm for the contentID
     */
    public AkrlTerm getContent() {
	return getTerm(getContentID());
    }

    public String toAkrlString() {
	String retval = "";
	retval += "(" + verb + " :content " + contentID + " :context ";
	retval += "(";
	List<AkrlTerm> terms = new ArrayList<AkrlTerm>(context.values());
	Collections.sort(terms); // sort output by ID
	for (AkrlTerm term : terms) 
	    retval += "\n    " + term;
	retval += ")";
	retval += ")";
	return retval;
    }
 
    @Override
    public String toString() {
	return toAkrlString();
    }

    /**
     * for testing
     */
    public static void main(String[] args) {
	try {
	System.out.println("hi");
	KQMLList l1 = KQMLList.fromString("(REGISTER-TASK :CONTENT P361820 :CONTEXT ((ONT::A P361820 :INSTANCE-OF ONT::TASK-DESCRIPTION 			:DESCRIPTION ONT::P359570 :TASK-ID \"PALMBLAYLOCK3472934094\" 			:CREATED-BY P361821 :KNOWLEDGE-PRECONDITIONS NIL) 		(ONT::THE P361821 :INSTANCE-OF ONT::PERSON :EQUALS ONT::USER) 		(ONT::RELN ONT::P359570 :INSTANCE-OF ONT::|Buy| :|object| ONT::V334967 			   :FORCE ONT::TRUE) 		(ONT::A ONT::V334967 :INSTANCE-OF ONT::BOOK :|the-name| (\"BOOYYY\")))))");
	AkrlBundle b1 = AkrlBundle.fromAkrl(l1);
	System.out.println("\n\n" + b1);

	KQMLList l2 = KQMLList.fromString("((PARAGRAPH :TERMS ((SENTENCE :UTTNUM 0 :TERMS ((BARE V2939673 (:* SUBSTANCE W)))) (SENTENCE :UTTNUM 1 :TERMS ((BARE V2940702 (:* FOOD D)) (SPEECHACT V3093439 SA_TELL :CONTENT V2940805) (F V2940805 (:* IN-RELATION LOOK-LIKE) :CO-THEME V2941854 :THEME V2940781 :MODS (V3092784) :TENSE W::PRES) (THE V2940781 (:* GEOGRAPHIC-REGION CHILDERS) :NAME-OF (CHILDERS)) (F V3092784 TIME-DURATION-REL :VAL V3092388 :OF V2940805) (A V3092600 FREQUENCY) (VALUE V3092388 FREQUENCY :REPEATS FREQUENCY) (THE V3090897 TIME-LOC :DOW V3089636) (THE V3089636 TIME-LOC :DOW V3086716) (THE V3086716 TIME-LOC :DOW V3083799) (THE V3083799 TIME-LOC :DOW V3080885) (THE V3080885 TIME-LOC :DOW V3077974) (THE V3077974 TIME-LOC :DOW V2940790) (PRO V2940790 (:* REFERENTIAL-SEM IT) :COREF V2940702) (THE V2941854 (:* GEOGRAPHIC-REGION PLAZA) :NAME-OF (PLAZA)))))))");
	AkrlBundle b2 = AkrlBundle.fromLFParagraphList(l2);
	System.out.println("\n\n" + b2);

	} catch (Exception e) {
	    throw new RuntimeException("top-level error",e);
	}
    } // end main

} // end AkrlBundle