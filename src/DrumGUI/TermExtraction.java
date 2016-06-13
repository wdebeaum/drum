/*
 * TermExtraction.java
 *
 * $Id: TermExtraction.java,v 1.37 2016/06/12 16:25:34 lgalescu Exp $
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>, 8 Jan 2015
 */

package TRIPS.DrumGUI;

import java.util.ArrayList;
import java.util.LinkedHashMap;
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
        // :SPEC --> quantifier (aka "term constructor")
        SPEC(":SPEC"),
        // :PRO context-term-id --> ID for event describing modifier
        PRO(":PRO"),
        // :BASE id --> an assoc-with for compositional terms, eg, "the Erk gene"
        BASE(":BASE"),
        // :M-SEQUENCE context-term-id --> sequence of term IDs
        MSEQ(":M-SEQUENCE"),
        // :LOGICALOP-SEQUENCE context-term-id --> sequence of term IDs
        SEQ(":LOGICALOP-SEQUENCE"),
        // :OP ontType --> operator joining SEQ
        OP(":OPERATOR"),
        // :CELL-LINE context-term-id --> cell line
        CELL_LINE(":CELL-LINE"),
        // :ACTIVE bool --> activation
        ACTIVE(":ACTIVE"),
        // :SITE context-term-id: ID for site (residue, domain) on a protein
        SITE(":SITE"),
        // :DRUM list --> drum-specific info
        DRUM(":DRUM");
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
        // :DEGREE context-term-id: hyper-/hypo-/under-/over-
        DEGREE(":DEGREE"),
        // :FREQUENCY context-term-id: re-
        FREQUENCY(":FREQUENCY"),
        // :MODA auto-/trans-/homo-/hetero-/uni-/mono-/di-/tri-/poly-/multi-
        MODA(":MODA"),
        // :MODN de-/un-/non-/dis-
        MODN(":MODN"),
        // :INEVENT context-term-id: ID for event modifying this term
        INEVENT(":INEVENT"),
        // :LOC context-term-id: ID for cellular location term
        CELL_LOC(":LOC"),
        // :MUTATION context-term-id: ID for mutation term
        MUTATION(":MUTATION");
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

    /** List of attribute s */
    private LinkedHashMap<Attribute, KQMLObject> attributes;
    /** List of poly-attributes */
    private LinkedHashMap<PolyAttribute, ArrayList<KQMLObject>> polyAttributes;

    /**
     * Contructor.
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

        // make expanded value
        makeExpandedValue();
    }

    //// TERM OPERATIONS

        /**
     * Combines with another extraction.
     * Should be overridden by subclasses.
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
        KQMLObject rule = value.removeKeywordArg(":RULE");
        KQMLObject eRule = e.value.getKeywordArg(":RULE");
        value.add(":RULE");
        value.add(rule.toString() + "," + eRule.toString());
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
    
    //// TERM OPERATIONS
    
    /** 
     * Instantiates a sequence term to obtain its subterms.
     * @deprecated Individual terms are extracted already, so there is no need to do it here.
     *
     */
    @Deprecated
    private KQMLList instantiate(String var) {
        Debug.debug("instantiate(" + var + ") from " + value);
        KQMLList varTerm = findTermByVar(var, context);
        if (var == null) {
            // TODO: complain
            Debug.warn("var term not found!");
            return value;
        }
        // clone old value list and update roles
        KQMLList newValue = new KQMLList();
        for (KQMLObject o: value) {
            newValue.add(o);
        }
        //... remove operator info
        newValue.removeKeywordArg(":LOGICALOP-SEQUENCE");
        newValue.removeKeywordArg(":OPERATOR");
        //... update var
        String newValueVar = pullTermVar(value) + "_" + removePackage(var);
        newValue.set(1, new KQMLToken(newValueVar));
        //... update ontType
        String ontType = pullTermOntType(varTerm);
        newValue.set(2, new KQMLToken(ontType));
        //... update attributes
        KQMLList attributes = newValue.subList(3, newValue.size());
        for  (int i = 0; i < attributes.size(); i += 2) {
            String key = attributes.get(i).toString();
            KQMLObject newAttrValue = varTerm.getKeywordArg(key);
            if (newAttrValue != null) {
                attributes.set(i+1, newAttrValue);
            }
        }
        // done
        Debug.debug("instantatied: " + newValue);
        return newValue;
    }
    
    
    //// XML FORMATTING
    
    /** 
     * Returns an XML representation of the term. Typically, this will be an {@code <term>} XML element.
     */
    public String toXML() {
        if (attributes.get(Attribute.SEQ) != null) { // logical sequence
            return createSequenceTermXML();
        }
        if (attributes.get(Attribute.MSEQ) != null) { // complex sequence
            return createComplexTermXML();
        }
        if (ontType.equalsIgnoreCase("ONT::MUTATION")) { // info from :DRUM
            return createMutationTermXML();
        }
        if (ontType.equalsIgnoreCase("ONT::PROTEIN-FAMILY")) { // info from :DRUM
            return createProtfamTermXML();
        }
        return createTermXML();
    }

    /**
     * Returns a {@code <term>} XML element representing a primitive term.
     */
    private String createTermXML() {
        Debug.debug("pTERM(" + value + ")");
        String var = pullTermVar(value);
        String id = removePackage(var, false);
        Debug.debug("id: " + id);
        String parID = getParagraphID();
        Debug.debug("parID: " + parID);

        String text = removeTags(getTextSpan(start, end));
        Debug.debug("text[" + start + "," + end + "]: " + text);

        String ruleID = value.getKeywordArg(":RULE").toString();
        Debug.debug("ruleID: " + ruleID);

        // dbids
        String dbID = getDBTermIds(attributes.get(Attribute.DRUM));

        Debug.debug("pTERM_toXML(): ready");

        return "<" + exType + " " +
                "id=\"" + id + "\" " +
                (dbID == null ? "" : ("dbid=\"" + dbID + "\" ")) +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\" " +
                "paragraph=\"" + parID + "\" " +
                "uttnum=\"" + uttnum + "\" " +
                "lisp=\"" + getLispForm() + "\" " +
                "rule=\"" + ruleID + "\">"
                + "<type>" + ontType + "</type>"
                + createModsXML()
                + createFeaturesXML()
                + createNameXML()
                + createCorefXML()
                + createBaseXML()
                + "<text>" + escapeXML(text) + "</text>" +
                "</" + exType + ">";
    }

    /**
     * Returns a {@code <term>} XML element representing a sequence term.
     */
    private String createSequenceTermXML() {
        Debug.debug("sTERM(" + value + ")");
        String var = pullTermVar(value);
        String id = removePackage(var, false);
        String ontText = normalize(pullTermOntWord(value));
        String parID = getParagraphID();

        String text = removeTags(getTextSpan(start, end));

        String ruleID = value.getKeywordArg(":RULE").toString();

        KQMLObject sequence = attributes.get(Attribute.SEQ);
        KQMLObject operator = attributes.get(Attribute.OP);
        String aggregate = createAggregateXML((KQMLList) sequence, operator.toString());
        
        // FIXME: do sequences have :DRUM info???
        String dbID = getDBTermIds(attributes.get(Attribute.DRUM));

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
    private String createAggregateXML(KQMLList varSeq, String operator) {
        String result = "";
        for (KQMLObject var: varSeq) {
            result += "<member id=\"" + removePackage(var.toString(), false) + "\" />";
        }
        return "<aggregate operator=\"" + removePackage(operator, false) + "\">" + result + "</aggregate>";
    }

    /**
     * Returns a {@code <term>} XML element representing a sequence term.
     */
    private String createComplexTermXML() {
        Debug.debug("cTERM(" + value + ")");
        String var = pullTermVar(value);
        String id = removePackage(var, false);
        String ontText = normalize(pullTermOntWord(value));
        String parID = getParagraphID();

        String text = removeTags(getTextSpan(start, end));

        String ruleID = value.getKeywordArg(":RULE").toString();

        KQMLObject sequence = attributes.get(Attribute.MSEQ);
        String subterms = createComponentsXML((KQMLList) sequence);

        // FIXME: do sequences have :DRUM info???
        String dbID = getDBTermIds(attributes.get(Attribute.DRUM));

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
                + "<text normalization=\"" + escapeXML(ontText) + "\">" + escapeXML(text) + "</text>" +
                "</" + exType + ">";
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
        String ontText = normalize(pullTermOntWord(value));
        String parID = getParagraphID();

        String text = removeTags(getTextSpan(start, end));

        String ruleID = value.getKeywordArg(":RULE").toString();

        // FIXME: do sequences have :DRUM info???
        KQMLObject dsiInfo = attributes.get(Attribute.DRUM);
        if ((dsiInfo != null) && !(dsiInfo instanceof KQMLList)) {
            dsiInfo = null;
        }
        String mutation = "";
        if (dsiInfo != null) {
            KQMLList drumInfo = findTermByHead(":DRUM", (KQMLList) dsiInfo);
            if (drumInfo == null) {
                mutation = "<mutation>" + "ONT::TRUE" + "</mutation>";
            } else {
                Debug.debug("drumInfo:" + drumInfo);
                KQMLList mutTerms = findAllTermsByHead("MUTATION", drumInfo);
                if (mutTerms == null) {
                    mutation = "<mutation>" + "ONT::TRUE" + "</mutation>";
                } else {
                    Debug.debug("mutTerms:" + mutTerms);
                    for (KQMLObject mutObj : mutTerms) {
                        mutation += "<mutation>"
                                + parseMutationToXML((KQMLList) mutObj)
                                + "</mutation>";
                    }
                }
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
     * Returns a {@code <term>} XML element representing a protein family term.
     */
    private String createProtfamTermXML() {
        Debug.debug("pfTERM(" + value + ")");
        String var = pullTermVar(value);
        String id = removePackage(var, false);
        String ontText = normalize(pullTermOntWord(value));
        String parID = getParagraphID();

        String text = removeTags(getTextSpan(start, end));

        String ruleID = value.getKeywordArg(":RULE").toString();

        // dbids
        String dbID = getDBTermIds(attributes.get(Attribute.DRUM));

        // get members from :DRUM
        KQMLObject dsiInfo = attributes.get(Attribute.DRUM);
        if ((dsiInfo != null) && !(dsiInfo instanceof KQMLList)) {
            dsiInfo = null;
        }
        String members = "";
        if (dsiInfo != null) {
            KQMLList drumInfo = findTermByHead(":DRUM", (KQMLList) dsiInfo);
            if (drumInfo == null) {
            } else {
                Debug.debug("drumInfo:" + drumInfo);
                KQMLList pfTerms = findAllTermsByHead("TERM", drumInfo);
                // FIXME: for now, there should be only one TERM, but in the future this may need to be revised
                if (pfTerms == null) {
                } else {
                    Debug.debug("pfTerms:" + pfTerms);
                    KQMLList pfTerm = (KQMLList) pfTerms.get(0);
                    
                    // ok, we know memberType can only be ONT::PROTEIN...
                    String memberType = getKeywordArgString(":MEMBER-TYPE", pfTerm);
                    
                    // member DBIDs
                    KQMLObject memberIDs = pfTerm.getKeywordArg(":MEMBERS");
                    if (memberIDs != null) {
                        members += "<members>";
                        for (KQMLObject memberID : (KQMLList) memberIDs) {
                            members += "<member type=\"" + memberType + "\" "
                                    + "dbid=\"" + normalizeDBID(memberID.stringValue()) + "\" "
                                    + "/>";
                        }
                        members += "</members>";
                    }
                }
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
            name = normalize(nameObj.toString());
        } else if (nameObj instanceof KQMLList) {
            for (KQMLObject w: (KQMLList) nameObj) {
                if (name == null) 
                    name = "";
                else
                    name += " ";
                name += normalize(w.toString());
            }
        } else {
            // shouldn't happen
            Debug.warn("ILL-FORMED INPUT: " + nameObj);
        }

        if (name == null) {
            return "";
        }
        return "<name>" + name + "</name>";
    }
    
    /**
     * Returns a {@code <coref>} XML element representing the term referred to by this term, or
     * the empty string if no such information exists.
     */
    private String createCorefXML() {
        String result = "";
        if (refType != null) {
            result = "<coref type=\"" + refType + "\" " +
                    ((refVar != null) ? ("id=\"" + removePackage(refVar, false) + "\" ") : "")
                    + "/>";
        }
        return result;
    }

    /**
     * Returns a {@code <base>} XML element representing the term referred to by this term, or
     * the empty string if no such information exists.
     */
    private String createBaseXML() {
        KQMLObject baseObj = attributes.get(Attribute.BASE);
        if (baseObj == null) {
            return "";
        }
        return "<assoc-with id=\"" + removePackage(baseObj.stringValue(), false) + "\" " + "/>";
    }

    /**
     * Returns a {@code <mods>} XML element representing the term modifiers, or
     * the empty string if no such information exists.
     */
    private String createModsXML() {
        String mods = "";
        mods += createModsXML(PolyAttribute.DEGREE, "degree");
        mods += createModsXML(PolyAttribute.FREQUENCY, "frequency");
        mods += createModsXML(PolyAttribute.MODA, "mod");
        mods += createModsXML(PolyAttribute.MODN, "mod");

        return mods.equals("") ? "" : "<mods>" + mods + "</mods>";
    }

    /**
     * @deprecated
     */
    private String createDegreeModsXML() {
        ArrayList<KQMLObject> degrees = polyAttributes.get(PolyAttribute.DEGREE);
        if ((degrees == null) || degrees.isEmpty()) {
            return "";
        }
        String result = "";
        for (KQMLObject degTermVar : degrees) {
            if (!isOntVar(degTermVar.toString())) {
                Debug.warn(":DEGREE value: expected var, got " + degTermVar);
                result +=
                    "<degree>"
                    + removePackage(degTermVar.toString(), false) +
                    "</degree>";
            } else {
                KQMLList degTerm = findTermByVar(degTermVar.toString(), context);
                KQMLList ontVal = pullCompleteOntInfo(degTerm);
                int start = getKeywordArgInt(":START", degTerm);
                int end = getKeywordArgInt(":END", degTerm);
                String text = removeTags(getTextSpan(start, end));

                result += "<degree " +
                        "start=\"" + getOffset(start) + "\" " +
                        "end=\"" + getOffset(end) + "\"" + ">"
                        + "<type>" + ontVal.get(0) + "</type>"
                        + "<text>" + escapeXML(text) + "</text>" +
                        "</degree>";
            }
        }

        return result;
    }

    /**
     * @deprecated
     */
    private String createFrequencyModsXML() {
        ArrayList<KQMLObject> freqs = polyAttributes.get(PolyAttribute.FREQUENCY);
        if ((freqs == null) || freqs.isEmpty()) {
            return "";
        }
        String result = "";
        for (KQMLObject freqTermVar : freqs) {
            if (!isOntVar(freqTermVar.toString())) {
                Debug.warn(":FREQUENCY value: expected var, got " + freqTermVar);
                result +=
                    "<frequency>"
                    + removePackage(freqTermVar.toString(), false) +
                    "</frequency>";
            } else {
                KQMLList freqTerm = findTermByVar(freqTermVar.toString(), context);
                KQMLList ontVal = pullCompleteOntInfo(freqTerm);
                int start = getKeywordArgInt(":START", freqTerm);
                int end = getKeywordArgInt(":END", freqTerm);
                String text = removeTags(getTextSpan(start, end));

                result += "<frequency " +
                        "start=\"" + getOffset(start) + "\" " +
                        "end=\"" + getOffset(end) + "\"" + ">"
                        + "<type>" + ontVal.get(0) + "</type>"
                        + "<text>" + escapeXML(text) + "</text>" +
                        "</frequency>";
            }
        }

        return result;
    }

    /**
     * @deprecated
     */
    private String createAModsXML() {
        ArrayList<KQMLObject> mods = polyAttributes.get(PolyAttribute.MODA);
        if ((mods == null) || mods.isEmpty()) {
            return "";
        }
        String result = "";
        for (KQMLObject modTerm : mods) {
            if (modTerm instanceof KQMLList) {
                KQMLList modTermList = (KQMLList) modTerm;
                result += "<mod>"
                        + "<type>" + modTermList.get(0) + "</type>"
                        + "<value>" + removePackage(modTermList.get(1).toString(), false) + "</value>" +
                        "</mod>";
            } else if (isOntVar(modTerm.toString())) { // TODO remove (obsolete)
                KQMLList modTermFromContext = findTermByVar(modTerm.toString(), context);
                KQMLList ontVal = pullCompleteOntInfo(modTermFromContext);
                int start = getKeywordArgInt(":START", modTermFromContext);
                int end = getKeywordArgInt(":END", modTermFromContext);
                String text = removeTags(getTextSpan(start, end));

                result += "<mod " +
                        "start=\"" + getOffset(start) + "\" " +
                        "end=\"" + getOffset(end) + "\"" + ">"
                        + "<type>" + ontVal.get(0) + "</type>"
                        + "<text>" + escapeXML(text) + "</text>" +
                        "</mod>";
            } else { // should not happen!
                Debug.error("unexpected :MODA value: " + modTerm);
                result += "<mod>"
                                + removePackage(modTerm.toString(), false) +
                                "</mod>";
            }
        }

        return result;
    }

    /**
     * @deprecated
     */
    private String createNModsXML() {
        ArrayList<KQMLObject> mods = polyAttributes.get(PolyAttribute.MODN);
        if ((mods == null) || mods.isEmpty()) {
            return "";
        }
        String result = "";
        for (KQMLObject modTerm : mods) {
            if (modTerm instanceof KQMLList) {
                KQMLList modTermList = (KQMLList) modTerm;
                result += "<mod>"
                        + "<type>" + modTermList.get(0) + "</type>"
                        + "<value>" + removePackage(modTermList.get(1).toString(), false) + "</value>" +
                        "</mod>";
            } else if (isOntVar(modTerm.toString())) { // TODO remove (obsolete)
                KQMLList modTermFromContext = findTermByVar(modTerm.toString(), context);
                KQMLList ontVal = pullCompleteOntInfo(modTermFromContext);
                int start = getKeywordArgInt(":START", modTermFromContext);
                int end = getKeywordArgInt(":END", modTermFromContext);
                String text = removeTags(getTextSpan(start, end));

                result += "<mod " +
                        "start=\"" + getOffset(start) + "\" " +
                        "end=\"" + getOffset(end) + "\"" + ">"
                        + "<type>" + ontVal.get(0) + "</type>"
                        + "<text>" + escapeXML(text) + "</text>" +
                        "</mod>";
            } else { // should not happen!
                Debug.error("unexpected :MODN value: " + modTerm);
                result += "<mod>"
                        + removePackage(modTerm.toString(), false) +
                        "</mod>";
            }
        }

        return result;
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
                result += "<" + modType + ">"
                        + "<type>" + modPair.get(0) + "</type>"
                        + "<value>" + removePackage(modPair.get(1).toString(), false) + "</value>" +
                        "</" + modType + ">";
            } else if (isOntVar(modValue.toString())) { // TODO remove (obsolete)
                KQMLList modTerm = findTermByVar(modValue.toString(), context);
                KQMLList ontVal = pullCompleteOntInfo(modTerm);
                int start = getKeywordArgInt(":START", modTerm);
                int end = getKeywordArgInt(":END", modTerm);
                String text = removeTags(getTextSpan(start, end));
                result += "<" + modType + " " +
                        "start=\"" + getOffset(start) + "\" " +
                        "end=\"" + getOffset(end) + "\"" + ">"
                        + "<type>" + ontVal.get(0) + "</type>"
                        + "<text>" + escapeXML(text) + "</text>" +
                        "</" + modType + ">";
            } else { // should not happen!
                Debug.error("unexpected " + mod + " value: " + modValue);
                result += "<" + modType + ">"
                        + removePackage(modValue.toString(), false) +
                        "</" + modType + ">";
            }
        }

        return result;
    }

    /**
     * Returns a {@code <features>} XML element representing the term features,
     * or the empty string if no such information exists.
     */
    private String createFeaturesXML() {
        String features = "";
        features += createActivityFeatureXML();
        features += createLocationFeatureXML(); // cellular location
        features += createMutationFeatureXML(); // mutations (for proteins, etc.)
        features += createDomSiteFeatureXML(); // domain
        features += createResSiteFeatureXML(); // residue
        features += createCelllineXML(); // cell-line
        features += createIneventFeaturesXML(); // TODO: replace w/ more specific features: binding, etc

        return features.equals("")
            ? ""
            : "<features>" + features + "</features>";
    }
    
    private String createActivityFeatureXML() {
        KQMLObject isActive = attributes.get(Attribute.ACTIVE);
        if (isActive == null)
            return "";
        return "<active>" + removePackage(isActive.toString(), false) + "</active>";
    }

    private String createLocationFeatureXML() {
        ArrayList<KQMLObject> locations = polyAttributes.get(PolyAttribute.CELL_LOC);
        if ((locations == null) || locations.isEmpty()) {
            return "";
        }
        // TODO: figure out what situations w/ multiple locations look like
        String result = "";
        for (KQMLObject location : locations) {
            if (!isOntVar(location.toString())) {
                Debug.warn(":LOCATION value: expected var, got " + location);
            } else {
                Extraction ekbTerm = ekbFindExtraction(location.toString());
                if (ekbTerm != null) {
                    result += "<location id=\"" + removePackage(location.toString(), false) + "\" />";
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
        KQMLObject sitePos = term.getKeywordArg(":AA-INDEX");
        result += "<pos>" + sitePos.toString() + "</pos>";
        // optional
        KQMLObject from = term.getKeywordArg(":OLD");
        if (from != null) {
            String aaFrom = parseAAToXML((KQMLList) from);
            result += "<aa-from>" + aaFrom + "</aa-from>";
        }
        // obligatory
        KQMLObject to = term.getKeywordArg(":NEW");
        String aaTo = parseAAToXML((KQMLList) to);
        result += "<aa-to>" + aaTo + "</aa-to>";

        return result;
    }

    private String parseMutationInsToXML(KQMLList term) {
        Debug.debug("insertion: " + term);
        KQMLObject from = term.getKeywordArg(":LOWER");
        KQMLObject to = term.getKeywordArg(":UPPER");
        KQMLObject insert = term.getKeywordArg(":NEW");
        String aaFrom = parseAASiteToXML((KQMLList) from);
        String aaTo = parseAASiteToXML((KQMLList) to);
        String insertXML = parseAAListXML((KQMLList) insert);
        return "<pos-from>" + aaFrom + "</pos-from>"
                + "<pos-to>" + aaTo + "</pos-to>"
                + "<insert>" + insertXML + "</insert>";
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
        KQMLObject aaName = term.getKeywordArg(":NAME");
        KQMLObject aaCode = term.getKeywordArg(":LETTER");

        // FIXME: name and code have quotes! is this right???
        return "<aa>"
                + ((aaName == null) ? "" : ("<name>" + aaName.toString().replaceAll("\"", "") + "</name>"))
                + ((aaCode == null) ? "" : "<code>" + (aaCode.toString().replaceAll("\"", "") + "</code>")) +
                "</aa>";

    }

    private String parseAASiteToXML(KQMLList term) {
        Debug.debug("aa-site: " + term);
        KQMLObject aaName = term.getKeywordArg(":NAME");
        KQMLObject aaCode = term.getKeywordArg(":LETTER");
        KQMLObject sitePos = term.getKeywordArg(":INDEX");

        // FIXME: name and code have quotes! is this right???
        return "<site>"
                + ((aaName == null) ? "" : ("<name>" + aaName.toString().replaceAll("\"", "") + "</name>"))
                + ((aaCode == null) ? "" : "<code>" + (aaCode.toString().replaceAll("\"", "") + "</code>"))
                + ((aaName == null) ? "" : ("<pos>" + sitePos.toString() + "</pos>")) +
                "</site>";
    }

    /**
     * Creates residue site term property.
     * 
     * @return
     */
    private String createResSiteFeatureXML() {
        KQMLObject dsiInfo = attributes.get(Attribute.DRUM);
        // Debug.debug("dsiInfo:" + dsiInfo);
        if (! (dsiInfo instanceof KQMLList)) {
            return "";
        }
        KQMLList drumInfo = findTermByHead(":DRUM", (KQMLList) dsiInfo);
        if (drumInfo == null) {
            return "";
        }
        // Debug.debug("drumInfo:" + drumInfo);
        KQMLList siteTerm = findTermByHead("AA-SITE", drumInfo);
        if (siteTerm == null) {
            return "";
        }
        // Debug.debug("siteTerm:" + siteTerm);
        return parseAASiteToXML(siteTerm);
    }

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
                result += "<event id=\"" + removePackage(inEvVar.toString(), false) + "\" />";
            }
        }

        return "<inevent>" + result + "</inevent>";
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
        String ontText = (ontInfo.size() > 1) ? normalize(ontInfo.get(1).toString()) : "";
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

