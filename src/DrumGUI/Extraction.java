/*
 * Extraction.java
 *
 * $Id: Extraction.java,v 1.57 2018/10/26 01:33:43 lgalescu Exp $
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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Properties;

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
        ontType = pullOntType(value);

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
     * Pulls ontology type from LF term. 
     * 
     * Note: the format of the onto-type varies. Normally, it is {@code (:* T W)}, but for some terms it may be {@code (:* T)} or even {@code T}.
     * 
     */
    protected static KQMLObject pullFullOntType(KQMLList term) {
        return term.get(2);
     }

    /**
     * Pull the ontology type from LF term. LF term format: {@code (?t V T ...)}
     * or {@code (?t V (:* T W) ...)} Result: T
     * 
     * @see #ontType
     */
    protected static String pullOntType(KQMLList term) {
        if (term == null)  return "";
        return ontType(term.get(2));
    }

    /**
     * Pull "normalized" word from LF term. LF term format: (?t V (:* T W) ...)
     * Result: W
     */
    protected static String pullOntWord(KQMLList term) {
        if (term == null) return "";
        return ontWord(term.get(2));
    }
    
    /**
     * Get the ont-type T from an ont-type of the form {@code (:* T W)}, {@code (:* T)} or a simple ont-type {@code T}.
     * 
     * @param type
     * @return
     */
    protected static String ontType(KQMLObject type) {
        if (type instanceof KQMLList) {
            KQMLList typeList = (KQMLList) type;
            if (typeList.size() < 3) {
                Debug.error("Incomplete ont-type: " + type);
            }
            if (typeList.size() < 2) {
                return "";
            }
            return typeList.get(1).toString();
        } else if (type instanceof KQMLToken) { 
            return type.toString();
        }
        // should not happen
        {
            StringWriter sw = new StringWriter();
            new Throwable("").printStackTrace(new PrintWriter(sw));
            Debug.error("Unexpected ont-type: " + type + "\n" + sw.toString());
        }
        return "";
    }

    /**
     * Get the word W from a specialized ont-type of the form (:* T W)
     * @param type
     * @return
     */
    protected static String ontWord(KQMLObject type) {
        if (type instanceof KQMLList) {
            KQMLList typeList = (KQMLList) type;
            if (typeList.size() != 3) {
                Debug.error("Incomplete ont-type: " + type);
                return "";
            }
            return typeList.get(2).toString();
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
                .replaceAll("-PUNC-COMMA-", ",")
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
            String itemType = pullOntType(term);
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
     * Searches the EKB for EVENT or TERM extraction with a given id. 
     * 
     * @param id
     * @return an extraction with the given id
     */
    Extraction ekbFindExtraction(String id) {
        ArrayList<Extraction> ekbAssertions = ekb.lookupByID(id); // raw extractions
        for (Extraction ex : ekbAssertions) {
            if (ex instanceof TermExtraction) {
                Debug.warn("... a TERM");
                return ex;
            } else if (ex instanceof EventExtraction) {
                Debug.warn("... an EVENT");
               return ex;
            }
        }
        Debug.warn("No match in EKB.");
       return null;
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
     * Returns the string value of {@link #expandedValue}. Used for visualization.
     */
    public String toString() {
        return expandedValue.toString();
    }

    /**
     * Creates a lisp form from {@link #shortValue} and returns it as a string.
     */
    protected String getLispForm() {
        return xml_escape(shortValue.toString());
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
     * XML attributes common to all extractions.
     * 
     * @return list of attributes
     */
    protected List<String> xml_commonAttributes() {
        List<String> attrs = new ArrayList<String>();
        attrs.add(xml_attribute("id", removePackage(id, false)));
        attrs.add(xml_attribute("start", String.valueOf(getOffset(start))));
        attrs.add(xml_attribute("end", String.valueOf(getOffset(end))));
        attrs.add(xml_attribute("paragraph", getParagraphID()));
        attrs.add(xml_attribute("uttnum", String.valueOf(uttnum)));
        attrs.add(xml_attribute("lisp", getLispForm()));
        attrs.add(xml_attribute("rule", value.getKeywordArg(":RULE").toString()));
        return attrs;
    }

    /**
     * XML contents (sub-elements) common to all extractions.
     * 
     * @return list of XML elements
     */
    protected List<String> xml_commonContents() {
        List<String> conts = new ArrayList<String>();
        conts.add(xml_element("type", "", ontType));
        conts.add(xml_element("text",
                xml_attribute("normalization", xml_escape(normalizeOnt(pullOntWord(value)))),
                xml_escape(removeTags(getTextSpan(start, end)))));
        return conts;
    }

    /**
     * Performs generic XML formatting for LF terms.
     * 
     * @param tag
     *            XML element type
     * @param var
     *            LF term variable
     * @return String representing the XML formatted term
     * 
     * @see #xml_lfTerm(String, String, List)
     */
    protected String xml_lfTerm(String tag, String var) {
        return xml_lfTerm(tag, var, null);
    }

    /**
     * Performs generic XML formatting for LF terms. Additional attributes can be specified.
     * 
     * @param tag
     *            XML element type
     * @param var
     *            LF term variable
     * @param attributes
     *            extra attributes
     * @return String representing the XML formatted term
     */
    protected String xml_lfTerm(String tag, String var, List<String> attributes) {
        KQMLList lfTerm = findTermByVar(var, context);
        KQMLObject termType = pullFullOntType(lfTerm);
        int start = getKeywordArgInt(":START", lfTerm);
        int end = getKeywordArgInt(":END", lfTerm);

        List<String> attrs = new ArrayList<String>();
        if (attributes != null)
            attrs.addAll(attributes);
        attrs.add(xml_attribute("start", Integer.toString(getOffset(start))));
        attrs.add(xml_attribute("end", Integer.toString(getOffset(end))));

        List<String> conts = new ArrayList<String>();
        conts.add(xml_element("type", "", ontType(termType)));
        conts.add(xml_element("text",
                xml_attribute("normalization", xml_escape(normalizeOnt(ontWord(termType)))),
                xml_escape(removeTags(getTextSpan(start, end)))));

        return xml_element(tag, attrs, conts);
    }

    /**
     * Returns a {@code drum-terms} XML element containing grounding information.
     * 
     */
    protected String xml_drumTerms() {
        List<String> conts = new ArrayList<String>();
        for (KQMLList term : drumTerms) {
            if (pullTermHead(term).equalsIgnoreCase("TERM")) {
                conts.add(xml_drumTerm(term));
            }
        }
        return xml_element("drum-terms", null, conts);
    }

    /**
     * Returns a {@code drum-term} XML element containing grounding information.
     * <p>
     * Attributes: {@code dbid}, {@code name}, {@code match-score}, {@code matched-name} <br>
     * Sub-elements: {@code ont-types}, {@code xrefs}, {@code species}
     * <p>
     * Limitations: we only get the first matched name.
     * 
     */
    protected String xml_drumTerm(KQMLList drumTerm) {
        if (drumTerm == null)
            return "";

        List<String> attrs = new ArrayList<String>();
        // TODO: find out if other information might be useful
        KQMLObject dbID = drumTerm.getKeywordArg(":ID");
        if (dbID != null)
            attrs.add(xml_attribute("dbid", normalizeDBID(dbID.toString())));
        // score may be missing
        KQMLObject matchScore = drumTerm.getKeywordArg(":SCORE");
        if (matchScore != null)
            attrs.add(xml_attribute("match-score", matchScore.toString()));
        // name may be missing
        KQMLObject nameObj = drumTerm.getKeywordArg(":NAME");
        if (nameObj != null)
            attrs.add(xml_attribute("name", xml_escape(nameObj.stringValue())));
        // matches may be missing
        KQMLObject matches = drumTerm.getKeywordArg(":MATCHES");
        String matchedName = null;
        if (matches != null) {
            KQMLObject firstMatch = ((KQMLList) matches).get(0);
            matchedName = ((KQMLList) firstMatch).getKeywordArg(":MATCHED").stringValue();
            attrs.add(xml_attribute("matched-name", xml_escape(matchedName)));
        }

        List<String> conts = new ArrayList<String>();
        // ont-types must be present!
        conts.add(xml_drumTermOntTypes((KQMLList) drumTerm.getKeywordArg(":ONT-TYPES")));
        // dbxrefs may be missing
        conts.add(xml_drumTermXrefs((KQMLList) drumTerm.getKeywordArg(":DBXREFS")));
        // species may be missing
        KQMLObject species = drumTerm.getKeywordArg(":SPECIES");
        if (species != null)
            conts.add(xml_element("species", "", xml_escape(species.stringValue())));

        return xml_element("drum-term", attrs, conts);
    }

    /**
     * Returns a {@code types} element containing a set of {@code type} sub-elements, each of them denoting a
     * an ONT type for the DRUM term.
     * 
     * @param ontTypes
     */
    protected String xml_drumTermOntTypes(KQMLList ontTypes) {
        if (ontTypes == null)
            return "";
        List<String> conts = new ArrayList<String>();
        for (KQMLObject type : (KQMLList) ontTypes) {
            conts.add(xml_element("type", null, type.toString()));
        }
        return xml_element("types", null, conts);
    }

    /**
     * Returns an {@code xrefs} element containing a set of {@code xref} sub-elements, each of them denoting a
     * cross-reference to a resource.
     * 
     * @param xRefs
     */
    protected String xml_drumTermXrefs(KQMLList xRefs) {
        if (xRefs == null)
            return "";
        List<String> conts = new ArrayList<String>();
        for (KQMLObject xRef : (KQMLList) xRefs) {
            conts.add(xml_element("xref", xml_attribute("dbid", normalizeDBID(xRef.toString())), null));
        }
        return xml_element("xrefs", null, conts);
    }

    /**
     * Utility function for making an XML attribute.
     * 
     * Note: If {@code value} is {@code null}, the function will return an empty string.
     * 
     * @see #xml_attribute(String, String, boolean)
     */
    protected String xml_attribute(String attribute, String value) {
        return xml_attribute(attribute, value, false);
    }

    /**
     * Utility function for making an XML attribute.
     * <p>
     * Notes:
     * <p>
     * <li>
     * If {@code value} is an empty string, the function will return an empty string.
     * <li>
     * If {@code value} is {@code null}, the function will return an empty string if {@code allowNulls} is
     * {@code false} and an attribute with the value {@code "null"} if {@code allowNulls} is
     * {@code true}.
     * 
     * @param attribute
     *            -- name of the attribute
     * @param value
     *            -- value of attribute
     * @param allowNulls
     *            -- whether a null value is allowed (as "null") or not
     * @return
     */
    protected String xml_attribute(String attribute, String value, boolean allowNulls) {
        if (attribute == null) {
            Debug.error("Null attribute!");
            return "";
        }
        return (!allowNulls && (value == null)) ? ""
                : attribute + "=\"" + value + "\"";
    }

    /**
     * Utility function for making an XML element with optional attributes and optional content.
     * 
     * @see #xml_element(String, String, String)
     */
    protected String xml_element(String tag, List<String> attributes, List<String> content) {
        return xml_element(tag, join(attributes, " "), join(content, ""));
    }

    /**
     * Utility function for making an XML element with optional attributes and optional sub-elements.
     * If the element is empty (no attributes and no content), it will return an empty string.
     */
    protected String xml_element(String tag, String attributes, String content) {
        if (attributes == null) {
            attributes = "";
        }
        if (content == null) {
            content = "";
        }
        if ((attributes.length() == 0) && (content.length() == 0)) {
            return "";
        }
        return "<" + tag
                + ((attributes.length() > 0) ? (" " + attributes) : "")
                + ((content.length() > 0) ? (">" + content + "</" + tag) : "/")
                + ">";
    }

    /**
     * Utility function for creating a simple XML element with no content and just an "id" attribute
     * whose value is the id of another XLM element.
     * 
     * @param tag
     * @param var
     * @return
     * 
     * @see #xml_elementWithID(String, String, List)
     */
    protected String xml_elementWithID(String tag, String var) {
        return xml_elementWithID(tag, var, null);
    }

    /**
     * Utility function for creating a simple XML element with no content, an "id" attribute
     * whose value is the id of another XLM element, and, optionally, other attributes.
     * 
     * @param tag
     * @param var
     * @param attributes
     * @return
     */
    protected String xml_elementWithID(String tag, String var, List<String> attributes) {
        List<String> attrs = new ArrayList<String>();
        attrs.add(xml_attribute("id", removePackage(var)));
        if (attributes != null)
            attrs.addAll(attributes);
        return xml_element(tag, attrs, null);
    }

    /**
     * Utility function for replacing special characters in a string so it can
     * be written to XML.
     * 
     * @param s
     * @return
     */
    protected String xml_escape(String s) {
        String result = s.replaceAll("&", "&amp;");
        result = result.replaceAll("\"", "&quot;");
        result = result.replaceAll("\'", "&apos;");
        result = result.replaceAll("<", "&lt;");
        result = result.replaceAll(">", "&gt;");
        return result;
    }

    /**
     * Utility function for joining strings with a delimiter
     */
    protected final String join(List<String> list, String delim) {
        StringBuilder sb = new StringBuilder();
        String loopDelim = "";
        if (list != null) {
            for (String s : list) {
                if (s.length() == 0) {
                    continue;
                }
                sb.append(loopDelim);
                sb.append(s);
                loopDelim = delim;
            }
        }
        return sb.toString();
    }
}
