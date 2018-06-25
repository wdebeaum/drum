/*
 * TermExtraction.java
 *
 * $Id: TermExtraction.java,v 1.48 2018/06/24 22:41:55 lgalescu Exp $
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>, 8 Jan 2015
 */

package TRIPS.DrumGUI;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLToken;

/**
 * Class is for representing and handling TERM extractions.
 * 
 * @author lgalescu
 *
 */
public class TermExtraction extends Extraction {

    /**
     * Term attributes
     * 
     * @author lgalescu
     */
    protected enum Attribute {
        // :NAME lexeme --> name of term
        NAME(":NAME"),
        // :SPEC --> quantifier 
        SPEC(":SPEC"),
        // :PRO context-term-id --> ID for event describing modifier
        PRO(":PRO"),
        // :BASE id --> an assoc-with for compositional terms, eg, "the Erk gene"
        BASE(":BASE"),
        // :M-SEQUENCE context-term-id --> sequence of term IDs
        MSEQ(":M-SEQUENCE"),
        // :LOGICALOP-SEQUENCE context-term-id --> sequence of term IDs
        LSEQ(":LOGICALOP-SEQUENCE"),
        // :OP ontType --> operator joining SEQ
        OP(":OPERATOR"),
        // :EXCEPT --> works in conjunction w/ SEQ ; ** FIXME: currently not provided! **
        SEQ_EXC(":EXCEPT"),
        // :CELL-LINE context-term-id --> cell line
        CELL_LINE(":CELL-LINE"),
        // :ACTIVE bool --> activation
        ACTIVE(":ACTIVE"),
        // :SITE context-term-id: ID for site (residue, domain) on a protein
        SITE(":SITE"),
        // CWMS: the following are LF attributes, or substitutes thereof
        SIZE(":SIZE"),
        SCALE(":SCALE"),
        ASSOC_POSS(":ASSOC-POSS"),
        MONTH(":MONTH"), // time
        YEAR(":YEAR") // time
        ;
        private String attrName;

        private Attribute(String name) {
            attrName = name;
        }

        public String toString() {
            return attrName;
        }

        public static boolean isAttribute(String item) {
            for (Attribute f : Attribute.values()) {
                if (f.attrName.equalsIgnoreCase(item))
                    return true;
            }
            return false;
        }
    };

    /**
     * Term poly-attributes (each attribute may appear multiple times)
     * 
     * @author lgalescu
     */
    protected enum PolyAttribute {
        // :DEGREE hyper-/hypo-/under-/over-
        DEGREE(":DEGREE"),
        // :FREQUENCY re-
        FREQUENCY(":FREQUENCY"),
        // :MODA auto-/trans-/homo-/hetero-/uni-/mono-/di-/tri-/poly-/multi-
        MODA(":MODA"),
        // :MODN de-/un-/non-/dis-
        MODN(":MODN"),
        // :INEVENT id: ID for event in which this term participates in some role
        INEVENT(":INEVENT"),
        // :LOC id: ID for cellular location term
        CELL_LOC(":LOC"),
        // :MUTATION id: ID for mutation term
        MUTATION(":MUTATION"),
        // THE FOLLOWING ARE LF attributes, or substitutes thereof, used in CWMS
        QUAL(":QUAL"),
        ASSOC(":ASSOC"),
        LOCATION(":LOCATION")
       ;
        private String attrName;

        private PolyAttribute(String name) {
            attrName = name;
        }

        public String toString() {
            return attrName;
        }

        public static boolean isPolyAttribute(String item) {
            for (PolyAttribute f : PolyAttribute.values()) {
                if (f.attrName.equalsIgnoreCase(item))
                    return true;
            }
            return false;
        }
    };

    /** List of attributes */
    private LinkedHashMap<Attribute, KQMLObject> attributes;
    /** List of poly-attributes */
    private LinkedHashMap<PolyAttribute, ArrayList<KQMLObject>> polyAttributes;

    /**
     * Constructor.
     * 
     * @param ekb
     * @param value
     * @param context
     * @param uttnum
     */
    public TermExtraction(DrumKB ekb, KQMLList value, KQMLList context, int uttnum) {
        super(ekb, value, context, uttnum);

        // pull attributes
        pullAttributes();
        pullPolyAttributes();

        packRules();

        // make expanded value
        makeExpandedValue();
    }

    //// TERM OPERATIONS

    /**
     * Combines with another extraction.
     */
    protected void combineWith(Extraction other) {
        if (!this.equals(other)) {
            Debug.error("Cannot update: extractions not equal: this=" + shortValue + " other=" + other.shortValue);
            return;
        }
        TermExtraction e = (TermExtraction) other;
        Debug.debug("Extraction before update: " + shortValue);
        Debug.debug("Update from: " + e.shortValue);

        // check attributes; issue error if any are different
        LinkedHashMap<Attribute, KQMLObject> eAttributes = e.getAttributes();
        for (Attribute attr : eAttributes.keySet()) {
            KQMLObject newVal = eAttributes.get(attr);
            if (newVal == null)
                continue;
            KQMLObject oldVal = attributes.get(attr);
            if (oldVal == null) {
                attributes.put(attr, newVal);
                shortValue.add(attr.toString());
                shortValue.add(newVal);
                Debug.debug("Updated: " + attr);
            } else if (!oldVal.equals(newVal)) { // TODO: this may be too strong!
                Debug.error("Conflicting values for " + attr + ":\n\told: " + oldVal + "\n\tnew: " + newVal
                        + "\n\t=> Will keep original.");
            } // else: they're equal, so no change

        }

        // check poly-attributes
        LinkedHashMap<PolyAttribute, ArrayList<KQMLObject>> ePolyAttributes = e.getPolyAttributes();
        for (PolyAttribute attr : ePolyAttributes.keySet()) {
            ArrayList<KQMLObject> paVals2 = ePolyAttributes.get(attr);
            if (paVals2 == null)
                continue;
            ArrayList<KQMLObject> paVals1 = polyAttributes.get(attr);
            if (paVals1 == null) {
                paVals1 = new ArrayList<KQMLObject>();
                polyAttributes.put(attr, paVals1);
            }
            for (KQMLObject aVal2 : paVals2) {
                if (!paVals1.contains(aVal2)) {
                    paVals1.add(aVal2);
                    shortValue.add(attr.toString());
                    shortValue.add(aVal2);
                    Debug.debug("Updated poly: " + attr + " of " + id + " to: " + polyAttributes.get(attr));
                }
            }
        }

        // update context
        KQMLList eCtxt = e.getContext();
        for (KQMLObject o: eCtxt) {
            if (context.indexOf(o) < 0) {
                context.add(o);
            }
        }
        // update boundaries
        int eStart = e.getStart();
        if (eStart < start) {
            start = eStart;
            Debug.debug("Updated: start  to: " + start);
        }
        int eEnd = e.getEnd();
        if (eEnd > end) {
            end = eEnd;
            Debug.debug("Updated: end  to: " + end);
        }
        // update rule
        KQMLObject eRule = e.value.getKeywordArg(":RULE");
        if (eRule != null) {
            value.add(":RULE");
            value.add(eRule.toString());
            packRules();
        }
        
        Debug.debug("New value: " + value);

        // update expandedValue
        makeExpandedValue();

        // done
        Debug.debug("Extraction after update: " + shortValue);
    }
    

    // // GETTERS

    /**
     * Gets the list of attributes.
     */
    public LinkedHashMap<Attribute, KQMLObject> getAttributes() {
        return attributes;
    }

    /**
     * Get value of a given attribute.
     * 
     * @param a
     * @return attribute value
     */
    public KQMLObject getAttribute(Attribute a) {
        return attributes.get(a);
    }

    /**
     * Gets the list of poly-attributes.
     */
    public LinkedHashMap<PolyAttribute, ArrayList<KQMLObject>> getPolyAttributes() {
        return polyAttributes;
    }

    /**
     * Get value of a given poly-attribute.
     * 
     * @param a
     * @return list of attribute values
     */
    public ArrayList<KQMLObject> getPolyAttribute(PolyAttribute a) {
        return polyAttributes.get(a);
    }

    // // PULLERS

    /**
     * Pulls attributes from {@code value}.
     */
    private void pullAttributes() {
        attributes = new LinkedHashMap<Attribute, KQMLObject>();
        for (Attribute attr : Attribute.values()) {
            String aName = attr.toString();
            KQMLObject aValue = shortValue.getKeywordArg(aName);
            // note: we ignore dash values, which we interpret as "undefined"
            if ((aValue != null) && !aValue.toString().equals("-")) {
                attributes.put(attr, aValue);
            }
            // fix for :LOGICALOP-SEQUENCE with :EXCEPT -- currently not included in the extraction
            if (attr.equals(Attribute.LSEQ) && (shortValue.getKeywordArg(":EXCEPT") == null)) {
                KQMLList term = findTermByVar(id, context);
                KQMLObject exceptVar = term.getKeywordArg(":EXCEPT");
                if (exceptVar != null) {
                    attributes.put(Attribute.SEQ_EXC, exceptVar);
                    Debug.debug("Added exception: " + exceptVar);
                }
            }
        }
    }

    /**
     * Pulls poly-attributes from {@code value}.
     */
    private void pullPolyAttributes() {
        polyAttributes = new LinkedHashMap<PolyAttribute, ArrayList<KQMLObject>>();
        try {
            ListIterator<KQMLObject> iterator;
            for (PolyAttribute attr : PolyAttribute.values()) {
                String attrName = attr.toString();
                iterator = shortValue.listIterator();
                ArrayList<KQMLObject> attrValues = new ArrayList<KQMLObject>();
                while (iterator.hasNext()) {
                    String key = iterator.next().toString();
                    if (key.equalsIgnoreCase(attr.toString())) {
                        // look ahead
                        int nextIndex = iterator.nextIndex();
                        KQMLObject value = shortValue.get(nextIndex);
                        Debug.debug("Found poly-attribute: " + key + " " + value);
                        if ((value instanceof KQMLToken) && ((KQMLToken) value).equalsIgnoreCase("-")) {
                            Debug.warn("Removed " + key + " " + value);
                            iterator.remove();
                            iterator.next();
                            iterator.remove();
                            continue;
                        }
                        if (value instanceof KQMLList) {
                            KQMLList valueList = (KQMLList) value;
                            if (((KQMLToken) valueList.get(0)).equalsIgnoreCase(":*")) {
                                Debug.warn("Removed [:*] from " + valueList);
                                valueList.remove(0);
                            }
                        }
                        attrValues.add(value);
                        iterator.next();
                    }
                }
                if (!attrValues.isEmpty()) {
                    polyAttributes.put(attr, attrValues);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    //// XML FORMATTING
    
    /** 
     * Returns an XML representation of the term. Typically, this will be an {@code <term>} XML element.
     */
    public String toXML() {
        if (attributes.get(Attribute.LSEQ) != null) { // logical sequence
            return createSequenceTermXML();
        }
        if (attributes.get(Attribute.MSEQ) != null) { // complex sequence
            return createComplexTermXML();
        }
        if (ontType.equalsIgnoreCase("ONT::MUTATION")) { // info from drumTerms
            return createMutationTermXML();
        }
        /*
         * TODO: remove; we are now handling these terms with the generic createTermXML()
         * if (ontType.equalsIgnoreCase("ONT::PROTEIN-FAMILY")) { // info from :DRUM
         * return createProtfamTermXML();
         * }
         */
        return createTermXML();
    }

    /**
     * Returns a {@code <term>} XML element representing a primitive term.
     */
    private String createTermXML() {
        Debug.debug("pTERM(" + value + ")");
        
        List<String> attrs = new ArrayList<String>();
        attrs.add(makeXMLAttribute("id", removePackage(pullTermVar(value), false)));
        attrs.add(makeXMLAttribute("dbid", getDBTermIds()));
        attrs.add(makeXMLAttribute("start", String.valueOf(getOffset(start))));
        attrs.add(makeXMLAttribute("end", String.valueOf(getOffset(end))));
        attrs.add(makeXMLAttribute("paragraph", getParagraphID()));
        attrs.add(makeXMLAttribute("uttnum", String.valueOf(uttnum)));
        attrs.add(makeXMLAttribute("lisp", getLispForm()));
        attrs.add(makeXMLAttribute("rule", value.getKeywordArg(":RULE").toString()));

        List<String> conts = new ArrayList<String>();
        conts.add(makeXMLElement("spec", "", attributes.get(Attribute.SPEC).toString()));
        conts.add(makeXMLElement("type", "", ontType));
        conts.add(createNameXML());
        conts.add(createNameXML());
        conts.add(createModsXML());
        conts.add(createFeaturesXML());
        conts.add(createCorefXML());
        conts.add(createBaseXML());
        conts.add(createDrumTermsXML());
        // CWMS
        conts.add(createAssocsXML());
        conts.add(createAssocPossXML());
        conts.add(createQualsXML());
        conts.add(createSizeXML());
        conts.add(createScaleXML());
        conts.add(createTimeXML());
        //
        conts.add(makeXMLElement("text", "", escapeXML(removeTags(getTextSpan(start, end)))));
        return makeXMLElement(exType, attrs, conts);
    }

    /**
     * Time expressions
     * @return
     */
    private String createTimeXML() {
        List<String> attrs = new ArrayList<String>();
        List<String> conts = new ArrayList<String>();
        //KQMLObject day = attributes.get(Attribute.DAY);
        KQMLObject month = attributes.get(Attribute.MONTH);
        if (month != null) {
            String mtext = "";
            if (month instanceof KQMLList) {
                mtext = normalizeOnt(((KQMLList) month).get(2).toString());
            }
            conts.add(makeXMLElement("month", null, mtext));
        }
        KQMLObject year = attributes.get(Attribute.YEAR);
        if (year != null) {
            conts.add(makeXMLElement("year", null, year.toString()));
        }
        if (conts.isEmpty()) {
            return "";
        }
        attrs.add(makeXMLAttribute("type","DATE"));
        return makeXMLElement("time", attrs, conts);
    }

    /**
     * Returns a {@code <term>} XML element representing a sequence term.
     */
    private String createSequenceTermXML() {
        Debug.debug("sTERM(" + value + ")");
        String var = pullTermVar(value);
        String id = removePackage(var, false);
        String ontText = normalizeOnt(pullTermOntWord(value));
        String parID = getParagraphID();

        String text = removeTags(getTextSpan(start, end));

        String ruleID = value.getKeywordArg(":RULE").toString();

        KQMLObject sequence = attributes.get(Attribute.LSEQ);
        // fix for :LOGICALOP-SEQUENCE -- sometimes we don't get a list
        if (sequence instanceof KQMLToken) {
            KQMLList new_seq = new KQMLList();
            new_seq.add(sequence);
            sequence = new_seq;
        }
        KQMLObject exception = attributes.get(Attribute.SEQ_EXC);
        KQMLObject operator = attributes.get(Attribute.OP);
        String aggregate = createAggregateXML(operator.toString(), (KQMLList) sequence, (KQMLToken) exception);
        
        // FIXME: should aggregates have :DRUM info???
        String dbID = getDBTermIds();

        return "<" + exType + " " +
                "id=\"" + id + "\" " +
                (dbID == null ? "" : ("dbid=\"" + dbID + "\" ")) +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\" " +
                "paragraph=\"" + parID + "\" " +
                "uttnum=\"" + uttnum + "\" " +
                "rule=\"" + ruleID + "\">"
                + "<type>" + ontType + "</type>"
                + aggregate
                + createModsXML()
                + createFeaturesXML()
                + createNameXML()
                + createCorefXML()
                + "<text normalization=\"" + escapeXML(ontText) + "\">" + escapeXML(text) + "</text>" +
                "</" + exType + ">";
    }

    /**
     * Returns an {@code <aggregate>} XML element representing an aggregation, via a logical operation, of terms.
     */
    private String createAggregateXML(String operator, KQMLList varSeq, KQMLToken varExc) {
        String result = "";
        String op = removePackage(operator, false);
        for (KQMLObject var: varSeq) {
            result += "<member id=\"" + removePackage(var.toString(), false) + "\" />";
        }
        if (varExc != null) {
            result += "<except id=\"" + removePackage(varExc.toString(), false) + "\" />";
            op = "BUT-NOT";
        }
        return "<aggregate operator=\"" + op + "\">" + result + "</aggregate>";
    }

    /**
     * Returns a {@code <term>} XML element representing a sequence term.
     */
    private String createComplexTermXML() {
        Debug.debug("cTERM(" + value + ")");
        String var = pullTermVar(value);
        String id = removePackage(var, false);
        String ontText = normalizeOnt(pullTermOntWord(value));
        String parID = getParagraphID();

        String text = removeTags(getTextSpan(start, end));

        String ruleID = value.getKeywordArg(":RULE").toString();

        KQMLObject sequence = attributes.get(Attribute.MSEQ);
        String subterms = createComponentsXML((KQMLList) sequence);

        String dbID = getDBTermIds();

        return "<" + exType + " " +
                "id=\"" + id + "\" " +
                (dbID == null ? "" : ("dbid=\"" + dbID + "\" ")) +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\" " +
                "paragraph=\"" + parID + "\" " +
                "uttnum=\"" + uttnum + "\" " +
                "rule=\"" + ruleID + "\">"
                + "<type>" + ontType + "</type>"
                + subterms
                + createModsXML()
                + createFeaturesXML()
                + createNameXML()
                + createCorefXML()
                + createDrumTermsXML()
                + "<text normalization=\"" + escapeXML(ontText) + "\">" + escapeXML(text) + "</text>" +
                "</" + exType + ">";
    }

    /**
     * Returns a {@code members} element containing members of a protein family.
     * 
     * @param term
     * @return
     */
    private String makeDrumTermProtFamXML(KQMLList term) {
        String members = "";
        String memberType = getKeywordArgString(":MEMBER-TYPE", term);

        // member DBIDs
        KQMLObject memberIDs = term.getKeywordArg(":MEMBERS");
        if (memberIDs != null) {
            members += "<members type=\"" + memberType + "\" >";
            for (KQMLObject memberID : (KQMLList) memberIDs) {
                members += "<member dbid=\"" + normalizeDBID(memberID.stringValue()) + "\"/>";
            }
            members += "</members>";
        }
        return members;
    }

    /**
     * Returns a {@code <components>} XML element containing references (term
     * IDs) to components of a complex term.
     */
    private String createComponentsXML(KQMLList varSeq) {
        String result = "";
        for (KQMLObject var : varSeq) {
            result += "<component id=\"" + removePackage(var.toString(), false) + "\" />";
        }
        return "<components>" + result + "</components>";
    }

    /**
     * Returns a {@code <term>} XML element representing a sequence term.
     */
    private String createMutationTermXML() {
        Debug.debug("mTERM(" + value + ")");
        String var = pullTermVar(value);
        String id = removePackage(var, false);
        String ontText = normalizeOnt(pullTermOntWord(value));
        String parID = getParagraphID();

        String text = removeTags(getTextSpan(start, end));

        String ruleID = value.getKeywordArg(":RULE").toString();

        String mutation = "";
        if (drumTerms.isEmpty()) {
            mutation = "<mutation>" + "ONT::TRUE" + "</mutation>";
        } else {
            for (KQMLList drumTerm : drumTerms) {
                if (!pullTermHead(drumTerm).equalsIgnoreCase("MUTATION")) {
                    continue;
                }
                mutation += "<mutation>" + parseMutationToXML(drumTerm) + "</mutation>";
            }
        }

        return "<"+ exType+ " "+ 
                "id=\"" + id + "\" " +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\" " +
                "paragraph=\"" + parID + "\" " +
                "uttnum=\"" + uttnum + "\" " +
                "rule=\"" + ruleID + "\">"
                + "<type>" + ontType + "</type>"
                + mutation
                + "<text normalization=\"" + escapeXML(ontText) + "\">" + escapeXML(text) + "</text>" +
                "</" + exType + ">";
    }

    /**
     * Returns a {@code drum-term} XML element containing grounding information.
     * <p>
     * Attributes: {@code dbid}, {@code name}, {@code match-score}, {@code matched-name} <br>
     * Sub-elements: {@code ont-types}, {@code xrefs}, {@code species}, {@code members}
     * <p>
     * Limitations: we only get the first matched name.
     * 
     * @return
     */
    protected String makeDrumTermXML(KQMLList drumTerm) {
        if (drumTerm == null)
            return "";
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
                ((dbID == null) ? "" : ("dbid=\"" + normalizeDBID(dbID.toString()) + "\" ")) +
                ((matchScore == null) ? "" : ("match-score=\"" + matchScore.toString() + "\" ")) +
                ((name == null) ? "" : ("name=\"" + escapeXML(name) + "\" ")) +
                ((matchedName == null) ? "" : ("matched-name=\"" + escapeXML(matchedName) + "\" ")) + ">"
                // sub-elements
                + makeDrumTermOntXML((KQMLList) ontTypes)
                + makeDrumTermProtFamXML(drumTerm)
                + makeDrumTermXrefsXML((KQMLList) xRefs)
                + (species == null ? "" : ("<species>" + escapeXML(species.stringValue()) + "</species>")) +
                "</drum-term>";
    }

    /**
     * Returns a {@code <term>} XML element representing a protein family term.
     * 
     * @deprecated Use {@link #createTermXML()}.
     */
    @Deprecated
    private String createProtfamTermXML() {
        Debug.debug("pfTERM(" + value + ")");
        String var = pullTermVar(value);
        String id = removePackage(var, false);
        String ontText = normalizeOnt(pullTermOntWord(value));
        String parID = getParagraphID();

        String text = removeTags(getTextSpan(start, end));

        String ruleID = value.getKeywordArg(":RULE").toString();

        // dbids
        String dbID = getDBTermIds();

        // get members from :DRUM
        String members = "";
        for (KQMLList drumTerm : drumTerms) {
            // we only look for the first PROTEIN-FAMILY term; there shouldn't be more than one!
            // N.B.: i made up PROTEIN-FAMILY; currently they use TERM as the head
            if (pullTermHead(drumTerm).equalsIgnoreCase("PROTEIN-FAMILY")) {
                members = makeDrumTermProtFamXML(drumTerm);
                break;
            }
        }

        return "<" + exType + " " +
                "id=\"" + id + "\" " +
                (dbID == null ? "" : ("dbid=\"" + dbID + "\" ")) +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\" " +
                "paragraph=\"" + parID + "\" " +
                "uttnum=\"" + uttnum + "\" " +
                "rule=\"" + ruleID + "\">"
                + "<type>" + ontType + "</type>"
                + members
                + createModsXML()
                + createFeaturesXML()
                + createDrumTermsXML()
                + createNameXML()
                + createCorefXML()
                + "<text normalization=\"" + escapeXML(ontText) + "\">" + escapeXML(text) + "</text>" +
                "</" + exType + ">";
    }

    /**
     * Returns a {@code <name>} XML element representing the name of the term,
     * or the empty string if no such information exists.
     */
    private String createNameXML() {
        KQMLObject nameObj = value.getKeywordArg(":NAME");
        String name = null;
        if (nameObj == null) {
            //nop
        } else if (nameObj instanceof KQMLToken) {
            name = normalizeOnt(nameObj.toString());
        } else if (nameObj instanceof KQMLList) {
            for (KQMLObject w: (KQMLList) nameObj) {
                if (name == null) 
                    name = "";
                else
                    name += " ";
                name += normalizeOnt(w.toString());
            }
        } else {
            // shouldn't happen
            Debug.warn("ILL-FORMED INPUT: " + nameObj);
        }

       return (name == null) ? "" 
                : makeXMLElement("name", "", escapeXML(name));
    }
    
    /**
     * Returns a {@code <coref>} XML element representing the term referred to by this term, or
     * the empty string if no such information exists.
     */
    private String createCorefXML() {
        return (refVar == null) ? "" 
                : makeXMLElement("coref", makeXMLAttribute("id", removePackage(refVar, false)), null);
    }

    /**
     * Returns a {@code <base>} XML element representing the term referred to by this term, or
     * the empty string if no such information exists.
     */
    private String createBaseXML() {
        KQMLObject baseObj = attributes.get(Attribute.BASE);
        return (baseObj == null) ? "" 
                : makeXMLElement("coref", makeXMLAttribute("id", removePackage(baseObj.stringValue(), false)), null);
    }

    /**
     * Returns a {@code <mods>} XML element representing the term modifiers, or
     * the empty string if no such information exists.
     */
    private String createModsXML() {
        List<String> mods = new ArrayList<String>();
        mods.add(createModsXML(PolyAttribute.DEGREE, "degree"));
        mods.add(createModsXML(PolyAttribute.FREQUENCY, "frequency"));
        mods.add(createModsXML(PolyAttribute.MODA, "mod"));
        mods.add(createModsXML(PolyAttribute.MODN, "mod"));
        return makeXMLElement("mods", null, mods);
    }
    
    /**
     * Creates a list of XML elements of the form {@code <X><type>T</type><value>V</value></X>}, where {@code X} is the
     * value of {@code modType}. An element of this form is created for each instance of a poly-attribute of type
     * {@code mod}.
     * 
     * @param mod
     * @param modType
     * @return
     */
    private String createModsXML(PolyAttribute mod, String modType) {
        ArrayList<KQMLObject> mods = polyAttributes.get(mod);
        if ((mods == null) || mods.isEmpty()) {
            return "";
        }
        String result = "";
        for (KQMLObject modValue : mods) {
            if (modValue instanceof KQMLList) {
                KQMLList modPair = (KQMLList) modValue;
                result += makeXMLElement(modType, null, 
                        makeXMLElement("type", null, modPair.get(0).toString()) +
                        makeXMLElement("value", null, removePackage(modPair.get(1).toString(), false)));
            } else if (isOntVar(modValue.toString())) {
                result += makeXMLfromTerm(modType, modValue.toString());
            } else { // should not happen!
                Debug.error("unexpected " + mod + " value: " + modValue);
                result += makeXMLElement(modType, null, removePackage(modValue.toString(), false));
            }
        }

        return result;
    }

    /**
     * Qualifiers
     */
    private String createQualsXML() {
        return makeXMLElement("qualifiers", null, createQualsXML(PolyAttribute.QUAL, "qual"));
    }
    
    /**
     * Qualifier
     * 
     * @param qual
     * @param tag
     * @return
     */
    private String createQualsXML(PolyAttribute qual, String tag) {
        ArrayList<KQMLObject> quals = polyAttributes.get(qual);
        if ((quals == null) || quals.isEmpty()) {
            return "";
        }
        String result = "";
        for (KQMLObject valObj : quals) {
            if (valObj instanceof KQMLList) {
                KQMLList valList = (KQMLList) valObj;
                result += makeXMLElement(tag, makeXMLAttribute("type", valList.get(0).toString()), null);
            } else if (isOntVar(valObj.toString())) {
                String var = valObj.toString();
                String id = removePackage(var);
                Extraction ekbTerm = ekbFindExtraction(var);
                if (ekbTerm != null) { // value points to another extraction
                    String attr = makeXMLAttribute("id", id);
                    result += makeXMLElement(tag, attr, null);
                } else { // we need to define the item here
                    result += makeXMLfromTerm(tag, var);
                }
            } else { // should not happen!
                Debug.error("unexpected " + qual + " value: " + valObj);
                result += makeXMLElement(tag, null, removePackage(valObj.toString(), false));
            }
        }
        return result;
    }

    /**
     * Assoc-with
     * 
     * @return
     */
    private String createAssocsXML() {
        ArrayList<KQMLObject> assocs = polyAttributes.get(PolyAttribute.ASSOC);
        if ((assocs == null) || assocs.isEmpty()) {
            return "";
        }
        String result = "";
        for (KQMLObject valObj : assocs) {
            if (valObj instanceof KQMLList) {
                KQMLList valList = (KQMLList) valObj;
                result += makeXMLElement("assoc-with", makeXMLAttribute("type", valList.get(0).toString()), null);
            } else if (isOntVar(valObj.toString())) {
                String var = valObj.toString();
                String id = removePackage(var);
                Extraction ekbTerm = ekbFindExtraction(var);
                if (ekbTerm != null) { // value points to another extraction
                    String attr = makeXMLAttribute("id", id);
                    result += makeXMLElement("assoc-with", attr, null);
                } else { // we need to define the item here
                    result += makeXMLfromTerm("assoc-with", var);
                }
            } else { // should not happen!
                Debug.error("unexpected :ASSOC value: " + valObj);
                result += makeXMLElement("assoc-with", null, removePackage(valObj.toString(), false));
            }
        }
        return result;
    }

    /**
     * Assoc-poss -- this is an abstract relation of possession between two entities. Reasoners may resolve it as a
     * possessed-by relation (e.g., "John's car"), or as an attribute-of relation (e.g., "the color of the car").
     * 
     * @return
     */
    private String createAssocPossXML() {
        KQMLObject assoc = attributes.get(Attribute.ASSOC_POSS);
        if (assoc == null) {
            return "";
        }
        String result = "";
        if (isOntVar(assoc.toString())) {
            String var = assoc.toString();
            String id = removePackage(var);
            Extraction ekbTerm = ekbFindExtraction(var);
            if (ekbTerm != null) { // value points to another extraction
                String attr = makeXMLAttribute("id", id);
                result += makeXMLElement("poss-by", attr, null);
            } else { // we need to define the item here
                result += makeXMLfromTerm("poss-by", var);
            }
        } else { // should not happen!
            Debug.error("unexpected :ASSOC-POSS value: " + assoc);
            result += makeXMLElement("poss-by", null, removePackage(assoc.toString(), false));
        }
        return result;
    }



    /**
     * Returns a {@code <features>} XML element representing the term features,
     * or the empty string if no such information exists.
     */
    private String createFeaturesXML() {
        String features = "";
        features += createIneventFeaturesXML();
        features += createActivityFeatureXML();
        features += createLocationFeatureXML(); // cellular location
        features += createMutationFeatureXML(); // mutations (for proteins, etc.)
        features += createDomSiteFeatureXML(); // domain
        features += createResSiteFeatureXML(); // residue
        features += createCelllineXML(); // cell-line
        return makeXMLElement("features", null, features);
    }
    
    private String createActivityFeatureXML() {
        KQMLObject isActive = attributes.get(Attribute.ACTIVE);
        if (isActive == null)
            return "";
        return makeXMLElement("active", null, removePackage(isActive.toString(), false));
    }

    private String createLocationFeatureXML() {
        List<KQMLObject> locations = new ArrayList<KQMLObject>();
        ArrayList<KQMLObject> cellLocs = polyAttributes.get(PolyAttribute.CELL_LOC);
        if (cellLocs != null) {
            locations.addAll(cellLocs); // DRUM
        }
        ArrayList<KQMLObject> locs = polyAttributes.get(PolyAttribute.LOCATION);
        if (locs != null) {
            locations.addAll(locs); // CWMS
        }

        // TODO: figure out what situations w/ multiple locations look like
        String result = "";
        for (KQMLObject location : locations) {
            if (!isOntVar(location.toString())) {
                Debug.warn(":LOCATION value: expected var, got " + location);
            } else {
                String var = location.toString();
                Extraction ekbTerm = ekbFindExtraction(var);
                if (ekbTerm != null) {
                    result += "<location id=\"" + removePackage(var, false) + "\" />";
                } else {
                    Debug.warn(":LOCATION value: no extraction found for " + var);
                    // TODO get info from term
                    result += makeXMLfromTerm("assoc-with", var);

                }
            }
        }
        return result;
    }
    
    /**
     * Creates explicit site (eg, domain) property for the term.
     * 
     * @return
     */
    private String createDomSiteFeatureXML() {
        KQMLObject site = attributes.get(Attribute.SITE);
        if (site == null) {
            return "";
        }
        if (!isOntVar(site.toString())) {
            Debug.warn(":SITE value: expected var, got " + site);
            return "";
        }

        return "<site id=\"" + removePackage(site.toString(), false) + "\" />";
    }

    private String createMutationFeatureXML() {
        ArrayList<KQMLObject> mutations = polyAttributes.get(PolyAttribute.MUTATION);
        if ((mutations == null) || mutations.isEmpty()) {
            return "";
        }
        String result = "";
        Debug.debug("mutations: " + mutations);
        for (KQMLObject mutTermVar : mutations) {
            // can be either ONT::TRUE or a term var
            String mutTermVal = mutTermVar.toString();
            if (isOntBool(mutTermVal)) {
                Debug.warn(":MUTATION value is bool only: " + mutTermVal);
                result +=  "<mutation>" + removePackage(mutTermVal, false)+ "</mutation>";
            } else if (!isOntVar(mutTermVal)) {
                Debug.warn(":MUTATION value: expected var, got " + mutTermVal);
                result += "<mutation>" + removePackage(mutTermVal, false)+ "</mutation>";
            } else {
                result += "<mutation id=\"" + removePackage(mutTermVal, false) + "\" />";
            }
        }

        return result;
    }

    private String parseMutationToXML(KQMLList term) {
        Debug.debug("mutation: " + term);
        KQMLObject mutType = term.getKeywordArg(":TYPE");
        String result = "<type>" + mutType.toString() + "</type>";
        if (mutType.toString().equalsIgnoreCase("DELETION")) {
            result += parseMutationDelToXML(term);
        }
        if (mutType.toString().equalsIgnoreCase("SUBSTITUTION")) {
            result += parseMutationSubToXML(term);
        }
        if (mutType.toString().equalsIgnoreCase("INSERTION")) {
            result += parseMutationInsToXML(term);
        }
        return result;
    }

    private String parseMutationDelToXML(KQMLList term) {
        Debug.debug("deletion: " + term);
        String result = "";
        KQMLObject posFrom = term.getKeywordArg(":LOWER");
        if (posFrom != null) {
            String aaFrom = parseAASiteToXML((KQMLList) posFrom);
            result += "<pos-from>" + aaFrom + "</pos-from>";
        }
        KQMLObject posTo = term.getKeywordArg(":UPPER");
        if (posTo != null) {
            String aaTo = parseAASiteToXML((KQMLList) posTo);
            result += "<pos-to>" + aaTo + "</pos-to>";
        }
        KQMLObject pos = term.getKeywordArg(":AA-INDEX");
        if (pos != null) {
            result += "<pos>" + pos.toString() + "</pos>";
        }
        KQMLObject posAA = term.getKeywordArg(":OLD");
        if (posAA != null) {
            String aa = parseAAToXML((KQMLList) posAA);
            result += "<aa-from>" + aa + "</aa-from>";
        }
        return result;
    }

    private String parseMutationSubToXML(KQMLList term) {
        Debug.debug("substitution: " + term);
        String result = "";

        // obligatory
        result += makeXMLElement("pos", "", term.getKeywordArg(":AA-INDEX").toString());
        // optional
        KQMLObject from = term.getKeywordArg(":OLD");
        if (from != null) {
            result += makeXMLElement("aa-from", "", parseAAToXML((KQMLList) from));
        }
        // obligatory
        result += makeXMLElement("aa-to", "", parseAAToXML((KQMLList) term.getKeywordArg(":NEW")));

        return result;
    }

    private String parseMutationInsToXML(KQMLList term) {
        Debug.debug("insertion: " + term);
        String result = "";
        result += makeXMLElement("pos-from", "", parseAASiteToXML((KQMLList) term.getKeywordArg(":LOWER")));
        result += makeXMLElement("pos-to", "", parseAASiteToXML((KQMLList) term.getKeywordArg(":UPPER")));
        result += makeXMLElement("insert", "", parseAAListXML((KQMLList) term.getKeywordArg(":NEW")));
        return result;
    }

    private String parseAAListXML(KQMLList term) {
        Debug.debug("aa-list: " + term);
        String result = "";
        for (KQMLObject aaTerm : term) {
            result += parseAAToXML((KQMLList) aaTerm);
        }
        return result;
    }

    private String parseAAToXML(KQMLList term) {
        Debug.debug("aa: " + term);
        List<String> conts = new ArrayList<String>();
        conts.add(makeXMLElement("name", "", term.getKeywordArg(":NAME").stringValue()));
        conts.add(makeXMLElement("code", "", term.getKeywordArg(":LETTER").stringValue()));
        return makeXMLElement("aa", null, conts);

    }

    private String parseAASiteToXML(KQMLList term) {
        Debug.debug("aa-site: " + term);
        List<String> conts = new ArrayList<String>();
        conts.add(makeXMLElement("name", "", term.getKeywordArg(":NAME").stringValue()));
        conts.add(makeXMLElement("code", "", term.getKeywordArg(":LETTER").stringValue()));
        conts.add(makeXMLElement("pos", "", term.getKeywordArg(":INDEX").toString()));
        return makeXMLElement("site", null, conts);
    }

    /**
     * Creates residue site term property.
     * 
     * @return
     */
    private String createResSiteFeatureXML() {
        if (drumTerms.isEmpty()) {
            return "";
        }
        for (KQMLList drumTerm : drumTerms) {
            // we only look for the first AA-SITE term; there shouldn't be more than one!
            if (pullTermHead(drumTerm).equalsIgnoreCase("AA-SITE")) {
                return parseAASiteToXML(drumTerm);
            }
        }
        return "";
    }

    /**
     * Returns a list of {@code inevent} "features".
     * TODO: re-think whether these are really "features" or something else!
     */
    private String createIneventFeaturesXML() {
        ArrayList<KQMLObject> inEvents = polyAttributes.get(PolyAttribute.INEVENT);
        Debug.warn("poly :INEVENT of " + id + " =  " + inEvents);
        if ((inEvents == null) || inEvents.isEmpty()) {
            return "";
        }
        String result = "";
        for (KQMLObject inEvVar : inEvents) {
            if (!isOntVar(inEvVar.toString())) {
                Debug.warn(":INEVENT value: expected var, got " + inEvVar);
            } else {
                KQMLList inEvTerm = findTermByVar(inEvVar.toString(), context);
                // TODO: get more info?
                result += "<inevent id=\"" + removePackage(inEvVar.toString(), false) + "\" />";
            }
        }

        return result;
    }

    /**
     * Returns a {@code <cell-line>} XML element representing cell-line
     * information attached to the term, or the empty string if no such
     * information exists.
     */
    private String createCelllineXML() {
        KQMLObject clVarObj = attributes.get(Attribute.CELL_LINE);
        if (clVarObj == null) {
            // we must not have :cell-line info!
            return "";
        }
        String clVar = clVarObj.toString();
        String id = removePackage(clVar);
        KQMLList clTerm = findTermByVar(clVar, context);
        KQMLList ontInfo = pullCompleteOntInfo(clTerm);
        String ontText = (ontInfo.size() > 1) ? normalizeOnt(ontInfo.get(1).toString()) : "";
        int start = getKeywordArgInt(":START", clTerm);
        int end = getKeywordArgInt(":END", clTerm);
        String text = removeTags(getTextSpan(start, end));
        Debug.debug("createCelllineXML: ready");

        return "<cell-line " +
                "id=\"" + id + "\" " +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\"" + ">"
                + "<type>" + ontInfo.get(0) + "</type>"
                + "<text normalization=\"" + escapeXML(ontText) + "\">" + escapeXML(text) + "</text>" +
                "</cell-line>";
    }
    
    /**
     * Size feature
     */
    private String createSizeXML() {
        List<String> attrs = new ArrayList<String>();
        List<String> content = new ArrayList<String>();
        KQMLObject valObj = attributes.get(Attribute.SIZE);
        if (valObj == null) {
            return "";
        }
        // value could be an ont type or a var
        String valStr = valObj.toString();
        KQMLList term = findTermByVar(valStr, context);
        if (term != null) { // got a var
            String attr = makeXMLAttribute("id", removePackage(valStr));
            attrs.add(attr);
        } else { // must be an ont type
            content.add(valStr);
        }
        return makeXMLElement("size", attrs, content);
    }
    
    /**
     * Scale feature
     */
    private String createScaleXML() {
        List<String> attrs = new ArrayList<String>();
        KQMLObject valObj = attributes.get(Attribute.SCALE);
        if (valObj == null) {
            return "";
        }
        // value must be an ont type
        String attr = makeXMLAttribute("type", valObj.toString());
        attrs.add(attr);
        return makeXMLElement("scale", attrs, null);
    }

    //// OTHER
    
    /** 
     * Expands non-argument attribute values in {@link #shortValue} and returns the 
     * resulting KQMLList.
     */
    public void makeExpandedValue() {
        expandedValue = new KQMLList();
        ListIterator<KQMLObject> iterator = shortValue.listIterator();
        boolean expand = false;
        int index = 0;
        while (iterator.hasNext()) {
            KQMLObject item = iterator.next();
            String itemAsString = item.toString();
            if (index < 3) { // first 3 items are special
                expandedValue.add(item);
            } else if (itemAsString.startsWith(":")) { // attribute?
                expandedValue.add(item);
                // check if known
                if (Attribute.isAttribute(itemAsString)) {
                    // nop
                } else if (PolyAttribute.isPolyAttribute(itemAsString)) {
                    expand = true;
                } else {
                    Debug.warn("Unrecognized term attribute: " + itemAsString);
                    expand = true;
                }
            } else if (expand && isOntVar(itemAsString)) { // variable?
                KQMLList term = findTermByVar(itemAsString, context);
                if (term == null) {
                    expandedValue.add(item);
                } else {
                    KQMLList ontVal = pullCompleteOntInfo(term);
                    expandedValue.add(ontVal);
                }
                expand = false;
            } else {
                expandedValue.add(item);
                expand = false;
            }
            index++;
        }
    }

    public String toString() {
        return expandedValue.toString();
    }

}

