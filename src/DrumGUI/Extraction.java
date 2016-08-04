/*
 * Extraction.java
 *
 * $Id: Extraction.java,v 1.50 2016/08/03 21:33:11 lgalescu Exp $
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>, 18 Feb 2010
 */

/* History
 * 20141028 lgalescu Created, based on CernlGUI's Extraction handler.
 */

/* Description
 * <extraction-msg>
 *  ::= (EXTRACTION-RESULT :value (<ex>) :context <cl> :uttnum <utt> ...)
 * <ex>
 *  ::= (<ex-type> <ex-var> <ont-type> [:name <var>] 
 *       :<role> <var> ...
 *       :polarity <p-val>
 *       :start <frame> :end <frame> ...)
 * <cl>
 *  ::= (<ct> ...)
 * <ct>
 *  ::= (<q> <var> (:* <ont-type> <word>) :<attr> <attr-val> ...)
 * <ex-type> ::= <symbol>
 * <ont-type> ::= ONT::<symbol> | (:* ONT::<symbol> <word>)
 * <*var> ::= ONT::<symbol>
 * <word> ::= W::<symbol>
 * <utt> := <number>
 * <frame> ::= <number>
 * <role> ::= <symbol>
 */

package TRIPS.DrumGUI;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.ListIterator;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLToken;

// generic extraction class
// may be subclassed to get more specific info
/**
 * Class for representing and handling extractions. This is the root of the
 * extraction class hierarchy. It provides members and methods common to all
 * subclasses; however, some methods provide limited functionality and ought to
 * be overridden. Generally, this class should only be instantiated for
 * extractions for which there isn't a more specific subclass in the hierarchy.
 * 
 * @author lgalescu
 *
 */
public class Extraction {
    /** The extraction value */
    protected KQMLList value;
    /** A simplified form of the extraction value (for the KB) */
    protected KQMLList shortValue;
    /** A possibly expanded form of {@link #shortValue} (for visualization) */
    protected KQMLList expandedValue;
    /** The extraction type */
    protected String exType;
    /** The extraction ID */
    protected String id;
    /** The ontology type for the extraction */
    protected String ontType;
    /** DRUM terms */
    protected ArrayList<KQMLList> drumTerms;
    /**
     * Coreference type for referential expressions
     * 
     * @deprecated Not used any more, but we may resurrect it.
     */
    @Deprecated
    protected String refType;
    /** Coreference var for referential expressions */
    protected String refVar;
    /** The utterance number for the extraction */
    protected int uttnum;
    /** Extraction start frame (character index) */
    protected int start;
    /** Extraction end frame (character index) */
    protected int end;
    /** Context for interpreting the extraction */
    protected KQMLList context;

    // TODO: remove -- figure out how to get offsets properly in all situations
    // and pass along the text!
    protected DrumKB ekb;

    // TODO: there's gotta be a better way to provide debugging info...
    private boolean debuggingEnabled = false;

    public Extraction(DrumKB ekb, KQMLList value, KQMLList context,
            int uttnum) {
        this.ekb = ekb;
        this.value = value;
        this.context = context;
        this.uttnum = uttnum;

        if (value != null) {
            exType = removePackage(pullTermHead(value));
        }

        id = pullTermVar(value);
        ontType = pullTermOntType(value);

        start = getKeywordArgInt(":START", value);
        end = getKeywordArgInt(":END", value);

        // set drumTerms
        fixDrumTermsFormat();
        pullDrumTerms();

        packRules();

        pullCorefInfo();

        shortValue = makeShortValue(value);
        expandedValue = shortValue;
    }

    //

    /**
     * Pack rules.
     */
    protected void packRules() {
        String rules = null;
        String delimiter = ",";
        while (true) {
            KQMLObject rule = value.removeKeywordArg(":RULE");
            if (rule == null) {
                break;
            }
            if (rules == null) {
                rules = rule.stringValue();
            } else {
                rules = rules + delimiter + rule.stringValue();
            }
        }
        value.add(":RULE");
        value.add(rules);
    }

    // // LF TERMS

    /**
     * Pull term head from LF term.
     */
    protected static String pullTermHead(KQMLList term) {
        return term.get(0).toString();
    }

    /**
     * Pull term variable from LF term.
     */
    protected static String pullTermVar(KQMLList term) {
        return term.get(1).toString();
    }

    /**
     * Pull term ID from LF term. Note: the term ID does not include the ONT::
     * qualifier present in the LF term.
     */
    private static String pullTermID(KQMLList term) {
        return removePackage(pullTermVar(term), false);
    }

    /**
     * Pulls ontology type and word (if available) from LF term. The term format
     * should be: {@code (?t V (:* T W) ...)}. The result, in this case, is
     * {@code (T W)}. For incomplete terms, with the format {@code (?t V T ...)}
     * the result will consist of only the type: {@code (T)}.
     */
    protected static KQMLList pullCompleteOntInfo(KQMLList term) {
        KQMLObject ontVal = term.get(2);
        KQMLList result = new KQMLList();
        if (ontVal instanceof KQMLList) {
            // this may have one or two elements
            int size = ((KQMLList) ontVal).size();
            result.addAll(((KQMLList) ontVal).subList(1, size));
        } else { // should not happen for true LF terms!
            Debug.warn("Incomplete ONT info in term: " + term);
            result.add(ontVal);
        }
        return result;
    }

    /**
     * Pull the ontology type from LF term. LF term format: {@code (?t V T ...)}
     * or {@code (?t V (:* T W) ...)} Result: T
     * 
     * @see #ontType
     */
    protected static String pullTermOntType(KQMLList term) {
        String type;
        if (term == null) {
            return "";
        }
        KQMLObject typeObj = term.get(2);
        if (typeObj instanceof KQMLList) {
            type = ((KQMLList) typeObj).get(1).toString();
        } else { // it must be a KQMLToken
            type = typeObj.toString();
        }
        return type;
    }

    /**
     * Pull "normalized" word from LF term. LF term format: (?t V (:* T W) ...)
     * Result: W
     */
    protected static String pullTermOntWord(KQMLList term) {
        if (term == null) {
            return "";
        }
        KQMLObject typeObj = term.get(2);
        Debug.debug("pullTermOntWord:: " + typeObj);
        if (typeObj instanceof KQMLList) {
            return ((KQMLList) typeObj).get(2).toString();
        }
        return "";
    }

    // EKB

    /**
     * Utility method that converts an utterance frame to a document offset.
     * 
     * @return offset in the document
     */
    public int getOffset(int frame) {
        return ekb.getOffset(uttnum, frame);
    }

    /**
     * Utility method for getting the current paragraph ID.
     * 
     */
    protected String getParagraphID() {
        return ekb.getPID(uttnum);
    }

    /**
     * Utility method for obtaining a text span in the current sentence.
     * 
     * @return the text span between two frames
     */
    protected String getTextSpan(int start, int end) {
        return ekb.getTextSpan(uttnum, start, end);
    }

    // // TERM ATTRIBUTES

    // TODO: make this throw exceptions when things don't go well
    protected static String getKeywordArgString(String attribute, KQMLList list) {
        if (list == null) {
            return "";
        }
        KQMLObject o = list.getKeywordArg(attribute);
        return (o == null) ? null : o.toString();
    }

    // TODO: make this throw exceptions when things don't go well
    protected static boolean getKeywordArgBool(String attribute, KQMLList list) {
        String value = getKeywordArgString(attribute, list);
        if ((value.equalsIgnoreCase("t")) || (value.equalsIgnoreCase("true"))) {
            return true;
        }
        return false;
    }

    protected static int getKeywordArgInt(String attribute, KQMLList list)
            throws NumberFormatException {
        int result = 0;
        if (list == null) {
            return 0;
        }
        String value = getKeywordArgString(attribute, list);
        result = Integer.parseInt(value);
        return result;
    }

    // // SPECIAL TERMS

    /**
     * Fix an inconsistency in Parser output.
     * <p>
     * WARNING: This is a temporary fix; it should go away eventually!
     */
    private void fixDrumTermsFormat() {
        KQMLObject drum = value.getKeywordArg(":DRUM");
        if (drum == null) {
            return;
        }
        if (drum instanceof KQMLToken) { // when missing, it may show as ":DRUM -"
            return;
        }
        KQMLList drumTermsList = findTermByHead("ONT::DRUM", (KQMLList) drum);
        if (drumTermsList != null) {
            Debug.warn("Fixing DRUM terms format for " + drumTermsList);
            drumTermsList.set(0, new KQMLToken(":DRUM"));
            for (KQMLObject term : drumTermsList) {
                if (!(term instanceof KQMLList)) {
                    continue;
                }
                KQMLList termAsList = (KQMLList) term;
                termAsList.set(0, new KQMLToken(removePackage(pullTermHead(termAsList))));
            }
        }
    }

    /**
     * Pull DRUM terms from {@link #value}.
     * 
     * @return
     */
    protected void pullDrumTerms() {
        KQMLObject drum = value.getKeywordArg(":DRUM");
        drumTerms = new ArrayList<KQMLList>();
        if (drum == null) {
            return;
        }
        if (drum instanceof KQMLToken) { // when missing, it may show as ":DRUM -"
            return;
        }
        KQMLList drumTermsList = findTermByHead(":DRUM", (KQMLList) drum);

        if (drumTermsList == null) {
            Debug.warn("DRUM terms doesn't include any: " + drum);
            return;
        }

        for (KQMLObject item : drumTermsList) {
            if (!(item instanceof KQMLList)) {
                continue;
            }
            KQMLList term = (KQMLList) item;
            String termHead = pullTermHead(term);
            if (termHead.equalsIgnoreCase("TERM")) {
                term.removeKeywordArg(":MAPPINGS");
                drumTerms.add(term);
            } else {
                drumTerms.add(term);
            }
        }
        Debug.warn("DRUM terms found: " + drumTerms);
    }

    /**
     * Returns list of all DRUM resource IDs, as a single {@link String}, using {@code |} as separator.
     * 
     * @return
     */
    protected String getDBTermIds() {
        HashSet<String> ids = new HashSet<String>();
        for (KQMLList term : drumTerms) {
            // only TERM terms have IDs
            if (!pullTermHead(term).equalsIgnoreCase("TERM")) {
                continue;
            }
            KQMLObject dbID = term.getKeywordArg(":ID");
            if (dbID != null) {
                ids.add(normalizeDBID(dbID.toString()));
            }
            KQMLObject dbXrefs = term.getKeywordArg(":DBXREFS");
            if (dbXrefs != null) {
                if (dbXrefs instanceof KQMLList) {
                    for (KQMLObject dbXref : (KQMLList) dbXrefs) {
                        ids.add(normalizeDBID(dbXref.toString()));
                    }
                } else {
                    Debug.warn("Unexpected :DBXREFS value: " + dbXrefs);
                }
            }
        }
        return join("|", ids);
    }

    /**
     * Normalizes a DRUM resource ID. In TRIPS, these IDs are lisp symbols in resource-specific packages. The normalized
     * form uses the lisp package as resource identifier, followed by a single colon as separator, followed by the
     * accession number (ID). We also allow for IDs not internalized by TRIPS; these have the format DB_ID. Any other
     * string will be returned unchanged, and a warning will be printed to STDERR.
     * 
     * @param dbid
     *            format: {@code DB::X12345} or {@code DB::|12345|} or {@code DB_X12345}
     * @return normalized id in the format: {@code DB:X12345} or {@code DB:12345}
     */
    protected static String normalizeDBID(String dbid) {
        String[] parsed = dbid.split(":+");
        if (parsed.length == 2) {
            return parsed[0] + ":" + parsed[1].replaceAll("\\|", "");
        }
        parsed = dbid.split("_");
        if (parsed.length == 2) {
            return parsed[0] + ":" + parsed[1];
        }
        Debug.warn("Unrecognized dbid format: " + dbid);
        return dbid;
    }

    /**
     * Join a list of strings, using the specified delimiter.
     * <p>
     * Note: Java 1.8 String class has a join function doing precisely this.
     */
    protected static String join(String delimiter, Iterable<String> elements) {
        String result = null;
        for (String elem : elements) {
            if (result == null) {
                result = elem;
            } else {
                result += (delimiter + elem);
            }
        }
        return result;
    }

    /**
     * Pulls coreference information from context.
     */
    private void pullCorefInfo() {
        KQMLList lfTerm = findTermByVar(id, context);
        if (lfTerm == null) {
            return;
        }
        refVar = getKeywordArgString(":COREF", lfTerm);
    }

    /**
     * Check that a term denotes a referring expression.
     * 
     * @param term
     * @return
     * 
     * @deprecated We don't use this anymore, but we may resurrect it eventually.
     */
    @Deprecated
    private boolean isRefExpr(KQMLList term) {
        String lfHead = pullTermHead(term);
        return (lfHead.equals("ONT::PRO") || lfHead.equals("ONT::PRO-SET"));
    }

    // // PACKAGES

    /**
     * Returns the symbol from a {@code package::symbol} token. If {@code withSpaces=true}, multi-word symbols are
     * returned as a space-separated word sequence.
     */
    protected static String removePackage(String w, boolean withSpaces) {
        if (w != null) {
            w = w.replaceAll("^[A-Za-z]+::", "");
            if (withSpaces) {
                w = w.replaceAll("[_-]", " ");
            }
        }
        return w;
    }

    protected static String removePackage(String w) {
        return removePackage(w, true);
    }

    protected static boolean isOntVar(String v) {
        return v.matches("(?i)\\AONT::[VX][0-9]+\\z");
    }

    protected static boolean isOntBool(String v) {
        return v.matches("(?i)\\AONT::(TRUE|FALSE)\\z");
    }

    /**
     * Normalizes a TRIPS ontology concept to a name.
     * 
     * @param name
     * @return normalized name
     */
    protected static String normalizeOnt(String name) {
        return removePackage(name, false)
                .replaceAll("-PUNC-SLASH-", "/")
                .replaceAll("-PUNC-MINUS-", "-")
                .replaceAll("-PUNC-EN-DASH-", "-")
                .replaceAll("-PUNC-PERIOD-", ".")
                .replaceAll("-PUNC-PERIOD", ".")
                .replaceAll("-PUNC-COMMA-", ".")
                .replaceAll("-START-PAREN-", "(")
                .replaceAll("START-PAREN-", "(")
                .replaceAll("-END-PAREN-", ")")
                .replaceAll("-END-PAREN", ")")
                .replaceAll(" ", "_");
    }

    // // CONTEXT

    /**
     * Returns the first term in the list, whose head is {@code head}, or
     * {@code null} if no such term exists.
     */
    protected KQMLList findTermByHead(String head, KQMLList terms) {
        if (terms == null)
            return null;
        ListIterator<KQMLObject> iterator = terms.listIterator();
        while (iterator.hasNext()) {
            KQMLObject next = iterator.next();
            if (!(next instanceof KQMLList)) {
                continue;
            }
            KQMLList term = (KQMLList) next;
            String itemHead = pullTermHead(term);
            if (head.equalsIgnoreCase(itemHead)) {
                return term;
            }
        }
        return null;
    }

    /**
     * Returns all the terms in the list, whose head is {@code head}, or
     * {@code null} if no such term exists.
     */
    protected KQMLList findAllTermsByHead(String head, KQMLList terms) {
        if (terms == null)
            return null;
        KQMLList result = new KQMLList();
        ListIterator<KQMLObject> iterator = terms.listIterator();
        while (iterator.hasNext()) {
            KQMLObject next = iterator.next();
            if (!(next instanceof KQMLList)) {
                continue;
            }
            KQMLList term = (KQMLList) next;
            String itemHead = pullTermHead(term);
            if (head.equalsIgnoreCase(itemHead)) {
                result.add(term);
            }
        }
        if (result.isEmpty()) {
            return null;
        }
        return result;
    }

    /**
     * Returns the first term in the list, whose type is {@code type}, or
     * {@code null} if no such term exists.
     */
    protected KQMLList findTermByType(String type, KQMLList terms) {
        if (terms == null)
            return null;
        ListIterator<KQMLObject> iterator = terms.listIterator();
        while (iterator.hasNext()) {
            KQMLList term = (KQMLList) iterator.next();
            String itemType = pullTermOntType(term);
            if (type.equalsIgnoreCase(itemType)) {
                return term;
            }
        }
        return null;
    }

    /**
     * Returns the first term in the list, whose variable is {@code var}, or
     * {@code null} if no such term exists.
     */
    protected KQMLList findTermByVar(String var, KQMLList terms) {
        if (terms == null)
            return null;
        ListIterator<KQMLObject> iterator = terms.listIterator();
        while (iterator.hasNext()) {
            KQMLList term = (KQMLList) iterator.next();
            String termVar = pullTermVar(term);
            if (termVar.equalsIgnoreCase(var)) {
                return term;
            }
        }
        return null;
    }

    // // VARIABLES

    /**
     * Makes new variable based on a given one, by adding an extension. The
     * separator is set to ".".
     * 
     * @param var
     *            base variable
     * @param ext
     *            extension
     * @return var.ext
     */
    protected String expandVar(String var, Object ext) {
        return var + "." + ext.toString();
    }

    /**
     * Returns the base variable for a possibly expanded one. If the variable is
     * not expanded, it returns it unaltered. If the variable is expanded, of
     * the form "x.y", it will return "x".
     * 
     * @param var
     *            a variable, possibly an expanded one
     * @return base variable
     */
    protected String getBaseVar(String var) {
        int sep = var.indexOf(".");
        if (sep < 0) {
            return var;
        }
        return var.substring(0, sep);
    }

    // // GETTERS

    public String getExType() {
        return exType;
    }

    protected String getID() {
        return id;
    }

    public String getOntType() {
        return ontType;
    }

    public KQMLList getValue() {
        return value;
    }

    public int getUttnum() {
        return uttnum;
    }

    public int getStart() {
        return start;
    }

    public int getEnd() {
        return end;
    }

    public KQMLList getContext() {
        return context;
    }

    // // EXTRACTION-EXTRACTION OPERATIONS

    /**
     * Compares the specified extraction with this one for equality. Two
     * {@link Extraction} objects are equal if their {@link #exType},
     * {@link #id} and {@link #ontType} members are equal.
     */
    public boolean equals(Extraction e) {
        return exType.equals(e.getExType()) && id.equals(e.getID())
                && ontType.equals(e.getOntType());
    }

    /**
     * Combines with another extraction. Should be overridden by subclasses.
     */
    protected void combineWith(Extraction other) {
        if (!this.equals(other)) {
            Debug.error("Cannot update: extractions not equal!");
            return;
        }
        // nop
        Debug.warn("Extraction update not implemented for " + exType);
    }

    // // EKB

    /**
     * Searches the EKB for EVENT or TERM extraction with a given id. If there are both an EVENT and a TERM extraction
     * sharing the same id, the EVENT extraction is returned.
     * 
     * @param id
     * @return an extraction with the given id
     */
    Extraction ekbFindExtraction(String id) {
        ArrayList<Extraction> ekbAssertions = ekb.lookupByID(id);
        Extraction termEx = null;
        for (Extraction ex : ekbAssertions) {
            if ((ex instanceof TermExtraction) && (termEx == null)) {
                termEx = ex;
            } else if ((ex instanceof EventExtraction)) {
                termEx = ex;
            }
            // other types are ignored
        }
        return termEx;
    }

    // // OTHER

    /**
     * Makes a simplified variant of an LF term (typically one representing an
     * extraction value).
     * 
     * @see #shortValue
     */
    protected KQMLList makeShortValue(KQMLList term) {
        try {
            // clone the original list (ugly implementation, but should work)
            KQMLList shortTerm = KQMLList.fromString(term.toString());
            String termVar = pullTermVar(term);
            // shortTerm.removeAt(1); // delete variable
            shortTerm.removeKeywordArg(":START");
            shortTerm.removeKeywordArg(":END");
            shortTerm.removeKeywordArg(":RULE");
            // :TYPE occurs in some extractions; it is just for tracking
            shortTerm.removeKeywordArg(":TYPE");
            return shortTerm;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Finds term references in this extraction. Should be overridden in
     * subclasses.
     */
    protected HashSet<String> findAllVars() {
        HashSet<String> vars = new HashSet<String>();
        vars.add(pullTermVar(value));
        // FIXME: add other vars, eg from :COREF
        return vars;
    }

    /**
     * Removes tags from text.
     */
    protected String removeTags(String s) {
        return s.replaceAll("<[^>]+>", "").trim();
    }

    /**
     * Performs generic XML formatting for LF terms.
     * 
     * @param xmlobj
     *            XML element type
     * @param variable
     *            LF term variable
     * @param context
     *            Context where the LF term should be found
     * @return String representing the XML formatted term
     * 
     * @deprecated We haven't used this in a long time; probably should be removed.
     */
    @Deprecated
    protected String lfTerm_toXML(String xmlobj, String variable,
            KQMLList context) {
        KQMLList lfTerm = findTermByVar(variable, context);
        String ontType = pullTermOntType(lfTerm);
        String word = pullTermOntWord(lfTerm);
        String start = lfTerm.getKeywordArg(":START").toString();
        String end = lfTerm.getKeywordArg(":END").toString();
        return "<" + xmlobj + " start=" + start + " end=" + end + " ont="
                + ontType + ">" + word + "</" + xmlobj + ">";
    }

    /**
     * Returns the string value of {@link #expandedValue}. Used for visualization.
     */
    public String toString() {
        return expandedValue.toString();
    }

    /**
     * Creates a lisp form from {@link #shortValue} and returns it as a string.
     */
    protected String getLispForm() {
        return escapeXML(shortValue.toString());
    }

    // XML

    /**
     * Returns a string representing this extraction in XML format. Should be overridden in subclasses.
     */
    public String toXML() {
        Debug.warn("No XML formatter for " + exType);
        return null;
    }

    /**
     * Returns a {@code drum-terms} XML element containing grounding information.
     * 
     * @return
     */
    protected String createDrumTermsXML() {
        String result = "";
        if (drumTerms.isEmpty())
            return result;
        for (KQMLList term : drumTerms) {
            if (pullTermHead(term).equalsIgnoreCase("TERM")) {
                result += makeDrumTermXML(term);
            }
        }
        if (result.isEmpty())
            return result;
        return "<drum-terms>" + result + "</drum-terms>";
    }

    /**
     * Returns a {@code drum-term} XML element containing grounding information.
     * <p>
     * Attributes: {@code dbid}, {@code name}, {@code match-score}, {@code matched-name} <br>
     * Sub-elements: {@code ont-types}, {@code xrefs}, {@code species}
     * <p>
     * Limitations: we only get the first matched name.
     * 
     * @return
     */
    protected String makeDrumTermXML(KQMLList drumTerm) {
        if (drumTerm == null) {
            return "";
        }
        // TODO: find out if other information might be useful
        KQMLObject dbID = drumTerm.getKeywordArg(":ID");
        // score may be missing
        KQMLObject matchScore = drumTerm.getKeywordArg(":SCORE");
        // name may be missing
        KQMLObject nameObj = drumTerm.getKeywordArg(":NAME");
        String name = (nameObj == null) ? null : nameObj.stringValue();
        // dbxrefs may be missing
        KQMLObject xRefs = drumTerm.getKeywordArg(":DBXREFS");
        // species may be missing
        KQMLObject species = drumTerm.getKeywordArg(":SPECIES");
        // ont-types must be present!
        KQMLObject ontTypes = drumTerm.getKeywordArg(":ONT-TYPES");
        // matches may be missing
        KQMLObject matches = drumTerm.getKeywordArg(":MATCHES");
        String matchedName = null;
        if (matches != null) {
            KQMLObject firstMatch = ((KQMLList) matches).get(0);
            matchedName = ((KQMLList) firstMatch).getKeywordArg(":MATCHED").stringValue();
        }
        return "<drum-term " +
                // attributes
                (dbID == null ? "" : ("dbid=\"" + normalizeDBID(dbID.toString()) + "\" ")) +
                (matchScore == null ? "" : ("match-score=\"" + matchScore + "\" ")) +
                (name == null ? "" : ("name=\"" + escapeXML(name) + "\" ")) +
                (matchedName == null ? "" : ("matched-name=\"" + escapeXML(matchedName) + "\" ")) + ">"
                // sub-elements
                + makeDrumTermOntXML((KQMLList) ontTypes)
                + makeDrumTermXrefsXML((KQMLList) xRefs)
                + (species == null ? "" : ("<species>" + escapeXML(species.stringValue()) + "</species>")) +
                "</drum-term>";
    }

    /**
     * Returns an {@code xrefs} element containing a set of {@code xref} sub-elements, each of them denoting a
     * cross-reference into a resource.
     * 
     * @param xRefs
     * @return
     */
    protected String makeDrumTermOntXML(KQMLList ontTypes) {
        String result = "";
        if (ontTypes == null)
            return result;
        for (KQMLObject type : (KQMLList) ontTypes) {
            result += "<type>" + type.toString() + "</type>";
        }
        return "<types>" + result + "</types>";
    }

    /**
     * Returns an {@code xrefs} element containing a set of {@code xref} sub-elements, each of them denoting a
     * cross-reference into a resource.
     * 
     * @param xRefs
     * @return
     */
    protected String makeDrumTermXrefsXML(KQMLList xRefs) {
        String result = "";
        if (xRefs == null)
            return result;
        for (KQMLObject xRef : (KQMLList) xRefs) {
            result += "<xref dbid=\"" + normalizeDBID(xRef.toString()) + "\"/>";
        }
        return "<xrefs>" + result + "</xrefs>";
    }


    /**
     * Utility function for replacing special characters in a string so it can
     * be written to XML.
     * 
     * @param s
     * @return
     */
    protected String escapeXML(String s) {
        String result = s.replaceAll("&", "&amp;");
        result = result.replaceAll("\"", "&quot;");
        result = result.replaceAll("\'", "&apos;");
        result = result.replaceAll("<", "&lt;");
        result = result.replaceAll(">", "&gt;");
        return result;
    }

}
