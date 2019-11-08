/*
 * TermExtraction.java
 *
 * $Id: TermExtraction.java,v 1.65 2019/11/07 19:54:11 lgalescu Exp $
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>, 8 Jan 2015
 */

package TRIPS.DrumGUI;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLString;
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
     */
    protected enum Attribute {
        // :NAME symbol --> name of term (in W:: package)
        NAME(":NAME"),
        // :SPEC --> quantifier 
        SPEC(":SPEC"),
        // :PRO id --> ID for event describing modifier
        PRO(":PRO"),
        // :BASE id --> an assoc-with for compositional terms, eg, "the Erk gene"
        BASE(":BASE"),
        // {DRUM} :M-SEQUENCE id --> sequence of term IDs
        MSEQ(":M-SEQUENCE"),
        // {DRUM} :LOGICALOP-SEQUENCE id --> sequence of term IDs
        LSEQ(":LOGICALOP-SEQUENCE"),
        // ordinary sequences
        SEQ(":SEQUENCE"), // sequence
        // :OP ontType --> operator joining SEQ
        OP(":OPERATOR"),
        // :EXCEPT --> works in conjunction w/ SEQ ; ** FIXME: currently not provided! **
        SEQ_EXC(":EXCEPT"),
        // {DRUM} :CELL-LINE id --> cell line
        CELL_LINE(":CELL-LINE"),
        // {DRUM} :ACTIVE bool --> activation
        ACTIVE(":ACTIVE"),
        // {DRUM} :SITE id: ID for site (residue, domain) on a protein
        SITE(":SITE"),
        // :LOC id :LOCMOD ontType --> location
        LOC(":LOC"),
        LOCMOD(":LOCMOD"), // location modifier
        LOCATION(":LOCATION"), // LF-term attribute for :LOC
        //
        ASSOC_POSS(":ASSOC-POSS"),
        // various 
        SIZE(":SIZE"),
        SCALE(":SCALE"),
        QUAN(":QUAN"), // quantifier
        REFSET(":REFSET"), // reference set for parts
        ID_AS(":IDENTIFIED-AS"),
        // numbers
        VALUE(":VALUE"), // numbers
        MIN(":MIN"), // numbers
        MAX(":MAX"), // numbers
        MOD(":MOD"), // numbers TODO: find out if it applied to other terms
        VALSEQ(":VAL"), // sequence of numbers
        QUANTITY(":QUANTITY"), // quantities
        AMOUNT(":AMOUNT"), // quantities
        UNIT(":UNIT"), // quantities
        RATE_QUAN(":REPEATS"), // rates
        RATE_OVER(":OVER-PERIOD"), // rates
        // temporal expressions
        DOW(":DAY-OF-WEEK"), // time
        DAY(":DAY"), // time
        MONTH(":MONTH"), // time
        YEAR(":YEAR"), // time
        TIME(":TIME"), // time
        TIMEMOD(":TIMEMOD"), // time modifier
        PHASE(":PHASE"), // another time modifier
        // [:FROM id] :TO id --> {CWMS} time-range attributes
        FROM(":FROM"),
        TO(":TO"),
        ;
        private String attrName;
        private Attribute(String name) { attrName = name; }
        public String toString() { return attrName; }

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
        // :MUTATION id: ID for mutation term
        MUTATION(":MUTATION"),
        // {CWMS} the following are LF attributes, or substitutes thereof
        QUAL(":QUAL"),
        ASSOC(":ASSOC")
       ;
        private String attrName;

        private PolyAttribute(String name) {
            attrName = name;
        }
        
        /**
         * Stores alternative names of poly-attributes.
         * For now we only handle one alternative per modifier.
         */
        private static HashMap<PolyAttribute, String> altNames;
        static {
            altNames = new HashMap<PolyAttribute, String>();
            altNames.put(ASSOC, ":ASSOC-WITH");
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
                /* LG20181020 we don't do this anymore
                if (aValue instanceof KQMLList) {
                    KQMLList valueList = (KQMLList) aValue;
                    if (((KQMLToken) valueList.get(0)).equalsIgnoreCase(":*")) {
                        Debug.warn("Removed [:*] from " + valueList);
                        valueList.remove(0);
                    }
                }
                */
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
                String altName = PolyAttribute.altNames.get(attr);
                iterator = shortValue.listIterator();
                ArrayList<KQMLObject> attrValues = new ArrayList<KQMLObject>();
                while (iterator.hasNext()) {
                    String key = iterator.next().toString();
                    if (key.equalsIgnoreCase(attrName) || key.equalsIgnoreCase(altName)) {
                        // look ahead
                        int nextIndex = iterator.nextIndex();
                        KQMLObject aValue = shortValue.get(nextIndex);
                        Debug.debug("Found poly-attribute: " + key + " " + aValue);
                        if ((aValue instanceof KQMLToken) && ((KQMLToken) aValue).equalsIgnoreCase("-")) {
                            Debug.warn("Removed " + key + " " + aValue);
                            iterator.remove();
                            iterator.next();
                            iterator.remove();
                            continue;
                        }
                        /* LG20181020 we don't do this anymore
                        if (aValue instanceof KQMLList) {
                            KQMLList valueList = (KQMLList) aValue;
                            if (((KQMLToken) valueList.get(0)).equalsIgnoreCase(":*")) {
                                Debug.warn("Removed [:*] from " + valueList);
                                valueList.remove(0);
                            }
                        }
                        */
                        if (!attrValues.contains(aValue)) {
                            attrValues.add(aValue);
                        }
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
            return xml_sequenceTerm();
        }
        if (attributes.get(Attribute.MSEQ) != null) { // complex sequence
            return xml_complexTerm();
        }
        if (attributes.get(Attribute.SEQ) != null) { // logical sequence
            return xml_sequenceTerm();
        }
        if (ontType.equalsIgnoreCase("ONT::MUTATION")) { // info from drumTerms
            return xml_mutationTerm();
        }
        return xml_term();
    }

    /**
     * XML contents (sub-elements) common to all TERM extractions.
     * 
     * @return list of XML elements
     */
    protected List<String> xml_commonContents() {
        List<String> conts = super.xml_commonContents();
        if (attributes.get(Attribute.SPEC) != null)
            conts.add(xml_element("spec", "", attributes.get(Attribute.SPEC).toString()));
        conts.add(xml_name());
        conts.add(xml_mods());
        conts.add(xml_features());
        conts.add(xml_coref());
        return conts;
    }
    
    /**
     * Returns a {@code <term>} XML element representing a primitive term.
     */
    private String xml_term() {
        Debug.debug("pTERM(" + value + ")");
        
        List<String> attrs = xml_commonAttributes();
        List<String> conts = xml_commonContents();

        conts.add(xml_base());

        if (ExtractionFactory.getProperty("extractions.mode").equals("DRUM")) {
            attrs.add(xml_attribute("dbid", getDBTermIds()));
            conts.add(xml_drumTerms());
        } 
        if (ExtractionFactory.getProperty("extractions.mode").equals("CWMS")) {
            conts.add(xml_grounding());
        } 

        conts.add(xml_equals());
        conts.add(xml_assocs());
        conts.add(xml_assocPoss());
        conts.add(xml_qualifiers());
        conts.add(xml_size());
        conts.add(xml_quantifier());
        conts.add(xml_refset());
        conts.add(xml_scale());
        conts.add(xml_location());
        conts.add(xml_timex());
        conts.add(xml_value());
        conts.add(xml_unit());
        conts.add(xml_amount());
        conts.add(xml_quantity());
        if (ontType.equalsIgnoreCase("ONT::RATE"))
            conts.add(xml_rate());
        conts.add(xml_timerange());
        // sequences of numbers
        conts.add(xml_valseq());

        return xml_element(exType, attrs, conts);
    }
    /**
     * Returns a domain-specific (e.g., {@code drum-terms}) XML element containing a set of grounding information terms.
     * 
     */
    protected String xml_grounding() {
        List<String> conts = new ArrayList<String>();
        for (KQMLList term : dsTerms) {
            conts.add(xml_cwmsTerm(term));
        }
        return xml_element("grounding", null, conts);
    }

    /**
     * Returns a {@code country} XML element containing grounding information for country names.
     * <p>
     * Attributes: {@code code}, {@code name}
     * CAPITAL, REGION, SUBREGION, DEMONYM
     */
    protected String xml_cwmsTerm(KQMLList dsTerm) {
        if (dsTerm == null)
            return "";
        String termType = pullTermHead(dsTerm).toLowerCase();
        List<String> attrs = new ArrayList<String>();
        // id (may be missing)
        KQMLObject dbID = dsTerm.getKeywordArg(":ID");
        if (dbID != null)
            attrs.add(xml_attribute("id", normalizeDBID(dbID.toString())));
        // official name
        KQMLObject name = dsTerm.getKeywordArg(":NAME");
        if (name != null)
            attrs.add(xml_attribute("name", xml_escape(name.stringValue())));
        // code (for countries)
        KQMLObject code = dsTerm.getKeywordArg(":CODE");
        if (code != null)
            attrs.add(xml_attribute("code", xml_escape(code.stringValue())));
        // country code (for capitals)
        KQMLObject c_code = dsTerm.getKeywordArg(":COUNTRY");
        if (c_code != null)
            attrs.add(xml_attribute("country-code", xml_escape(c_code.stringValue())));
        // regions, sub-regions and demonyms have :COUNTRIES
        KQMLObject c_codes = dsTerm.getKeywordArg(":COUNTRIES");
        if (c_codes != null) {
            attrs.add(xml_attribute("country-codes", xml_escape(c_codes.stringValue())));
        }
        // status may be missing
        KQMLObject status = dsTerm.getKeywordArg(":STATUS");
        if (status != null)
            attrs.add(xml_attribute("status", status.toString()));
        // source
        KQMLObject source = dsTerm.getKeywordArg(":SOURCE");
        if (source != null)
            attrs.add(xml_attribute("source", xml_escape(source.stringValue())));
        // matches may be missing
        KQMLObject matches = dsTerm.getKeywordArg(":MATCHES");
        String matchedName = null;
        if (matches != null) {
            KQMLList firstMatch = (KQMLList) ((KQMLList) matches).get(0);
            matchedName = firstMatch.getKeywordArg(":MATCHED").stringValue();
            attrs.add(xml_attribute("matched-name", xml_escape(matchedName)));
            // score may be missing
            KQMLObject matchScore = firstMatch.getKeywordArg(":SCORE");
            if (matchScore != null)
                attrs.add(xml_attribute("match-score", matchScore.toString()));
            // status may be missing; also, it may be a string rather than a token
            KQMLObject matchStatus = firstMatch.getKeywordArg(":STATUS");
            if (matchStatus != null) {
                if (matchStatus instanceof KQMLString) {
                    attrs.add(xml_attribute("status", matchStatus.stringValue()));                    
                } else {
                    attrs.add(xml_attribute("status", matchStatus.toString()));
                }
            }
            // source
            KQMLObject matchedSource = firstMatch.getKeywordArg(":SOURCE");
            if (matchedSource != null)
                attrs.add(xml_attribute("source", xml_escape(matchedSource.stringValue())));
        }

        return xml_element(termType, attrs, null);
    }

    /**
     * Returns a {@code place} XML element containing grounding (geographic) information for place names.
     * <p>
     * Attributes: {@code id}, {@code source}
     * 
     */
    @Deprecated
    protected String xml_cwmsPlaceTerm(KQMLList dsTerm) {
        if (dsTerm == null)
            return "";

        List<String> attrs = new ArrayList<String>();
        KQMLObject dbID = dsTerm.getKeywordArg(":ID");
        if (dbID != null)
            attrs.add(xml_attribute("id", normalizeDBID(dbID.toString())));
        // source
        KQMLObject source = dsTerm.getKeywordArg(":SOURCE");
        if (source != null)
            attrs.add(xml_attribute("source", xml_escape(source.stringValue())));

        return xml_element("place", attrs, null);
    }

    /**
     * Equals (identified-as)
     */
    private String xml_equals() {
        KQMLObject idAs = attributes.get(Attribute.ID_AS);
        if (idAs == null) return "";
        
        return xml_elementWithID("equals", idAs.toString());
    }

    /**
     * Units for quantities
     */
    private String xml_unit() {
        KQMLObject unit = attributes.get(Attribute.UNIT);
        if (unit == null) return "";
        
        KQMLList unitType = null;
        if (unit instanceof KQMLList)
            unitType = (KQMLList) unit;
        
        return xml_element("unit", 
                xml_attribute("type", ontType(unit)), 
                (unit != null) ? removePackage(ontWord(unitType)) : "");
    }

    /**
     * Amounts for quantities
     */
    private String xml_amount() {
        KQMLObject value = attributes.get(Attribute.AMOUNT);
        if (value == null) return "";
        
        if (isOntVar(value.toString())) {
            String var = value.toString();
            if (ekbFindExtraction(var) != null) { 
                return xml_elementWithID("amount", var);
            } else { // we need to define the item here
                return xml_lfTerm("amount", var);
            }
        } else { // an actual value
            return xml_element("amount", null, value.toString());
        }
     }

    /**
     * Quantities
     */
    private String xml_quantity() {
        KQMLObject value = attributes.get(Attribute.QUANTITY);
        if (value == null) 
            return "";
        
        if (isOntVar(value.toString())) {
            String var = value.toString();
            if (ekbFindExtraction(var) != null) { 
                return xml_elementWithID("quantity", var);
            } else { // we need to define the item here
                return xml_lfTerm("quantity", var);
            }
        } else { // an actual value
            return xml_element("quantity", null, value.toString());
        }
     }

    /**
     * Rates
     */
    private String xml_rate() {
        KQMLObject quan = attributes.get(Attribute.RATE_QUAN);
        KQMLObject over = attributes.get(Attribute.RATE_OVER);
        if (quan != null && over != null)
            return xml_elementWithID("quantity", quan.toString()) 
                    + xml_elementWithID("over-quantity", over.toString());
        return "";
    }

    /**
     * Value expressions for numbers
     */
    private String xml_value() {
        if (!(ontType.equalsIgnoreCase("ONT::NUMBER") || ontType.equalsIgnoreCase("ONT::NUMBER-UNIT"))) {
            return "";
        }
        // number with value
        KQMLObject value = attributes.get(Attribute.VALUE);
        if (value != null) 
             return xml_element("value", "", value.toString());
        // number greater than, less than, or between values
        List<String> conts = new ArrayList<String>();
        List<String> attrs = new ArrayList<String>();
        KQMLObject min = attributes.get(Attribute.MIN);
        if (min != null)
            conts.add(xml_element("min", "", min.toString()));
        KQMLObject max = attributes.get(Attribute.MAX);
        if (max != null)
            conts.add(xml_element("max", "", max.toString()));
        // number that is more or less than another
        KQMLObject mod = attributes.get(Attribute.MOD);
        if (mod != null){
           if (isOntVar(mod.toString())) {
               KQMLList modTerm = findTermByVar(mod.toString(), context);
               if (modTerm != null) {
                   KQMLObject modOntType = pullFullOntType(modTerm);
                   String ontType = ontType(modOntType);
                   // TODO: assert that this is ONT::QMODIFIER
                   String ontWord = ontWord(modOntType);
                   // TODO: assert that figureVar.equalsIgnoreCase(id)
                   KQMLObject ground = modTerm.getKeywordArg(":GROUND");
                   String groundVar = ground.toString();
                   attrs.add(xml_attribute("mod", removePackage(ontWord)));
                   attrs.add(xml_attribute("id", removePackage(ground.toString())));
               }
           }
        }
        
        return xml_element("value", attrs, conts);
    }

    /**
     * Value expressions for sequences of numbers
     */
    private String xml_valseq() {
        List<String> conts = new ArrayList<String>();
        KQMLObject valseq = attributes.get(Attribute.VALSEQ);
        Debug.debug("valseq(" + valseq + ")");
        if ((valseq != null) && (valseq instanceof KQMLList)) {
            for (KQMLObject val : (KQMLList) valseq)
                conts.add(xml_element("item", null, val.toString()));
        }
        return xml_element("values", null, conts);
    }

    /**
     * Returns a {@code <term>} XML element representing a sequence term.
     */
    private String xml_sequenceTerm() {
        Debug.debug("sTERM(" + value + ")");
        
        List<String> attrs = xml_commonAttributes();
        
        attrs.add(xml_attribute("dbid", getDBTermIds()));

        List<String> conts = xml_commonContents();
        KQMLObject sequence = attributes.get(Attribute.LSEQ);
        if (sequence == null) { // CWMS
            sequence = attributes.get(Attribute.SEQ);
        }
        // fix for :LOGICALOP-SEQUENCE -- sometimes we don't get a list
        if (sequence instanceof KQMLToken) {
            KQMLList new_seq = new KQMLList();
            new_seq.add(sequence);
            sequence = new_seq;
        }
        String operator = null;
        KQMLObject op = attributes.get(Attribute.OP);
        if (op != null)
            operator = op.toString();
        conts.add(xml_aggregate(operator, 
                (KQMLList) sequence, 
                (KQMLToken) attributes.get(Attribute.SEQ_EXC)));

        return xml_element(exType, attrs, conts);
    }

    /**
     * Returns an {@code <aggregate>} XML element representing an aggregation, via a logical operation, of terms.
     */
    private String xml_aggregate(String operator, KQMLList varSeq, KQMLToken varExc) {
        List<String> attrs = new ArrayList<String>();
        List<String> conts = new ArrayList<String>();
        String op = removePackage(operator, false);
        for (KQMLObject var: varSeq) 
            conts.add(xml_elementWithID("member", var.toString()));
        if (varExc != null) {
            conts.add(xml_elementWithID("except", varExc.toString()));
            op = "BUT-NOT";
        }
        attrs.add(xml_attribute("operator", op));
        return xml_element("aggregate", attrs, conts);
    }

    /**
     * Returns a {@code <term>} XML element representing a sequence term.
     */
    private String xml_complexTerm() {
        Debug.debug("cTERM(" + value + ")");
        
        List<String> attrs = xml_commonAttributes();
        attrs.add(xml_attribute("dbid", getDBTermIds()));
 
        List<String> conts = xml_commonContents();
        conts.add(xml_components((KQMLList) attributes.get(Attribute.MSEQ)));
        conts.add(xml_drumTerms());
 
        return xml_element(exType, attrs, conts);
   }

    /**
     * Returns a {@code members} element containing members of a protein family.
     * 
     * @param term
     * @return
     */
    private String xml_famMembers(KQMLList term) {
        List<String> attrs = new ArrayList<String>();
        List<String> conts = new ArrayList<String>();
        KQMLObject memberIDs = term.getKeywordArg(":MEMBERS");
        if (memberIDs != null) {
            attrs.add(xml_attribute("type", getKeywordArgString(":MEMBER-TYPE", term)));
            for (KQMLObject memberID : (KQMLList) memberIDs) {
                conts.add(xml_element("member", xml_attribute("dbid", normalizeDBID(memberID.stringValue())), null));
            }
        }
        return xml_element("members", attrs, conts);
    }

    /**
     * Returns a {@code <components>} XML element containing references (term
     * IDs) to components of a complex term.
     */
    private String xml_components(KQMLList varSeq) {
        List<String> conts = new ArrayList<String>();
        for (KQMLObject var : varSeq) {
            conts.add(xml_elementWithID("component", var.toString()));
        }
        return xml_element("components", null, conts);
    }

    /**
     * Returns a {@code <term>} XML element representing a mutation term.
     * <b>
     * Domains: DRUM.
     */
    private String xml_mutationTerm() {
        Debug.debug("mTERM(" + value + ")");
        
        List<String> attrs = xml_commonAttributes();

        List<String> conts = xml_commonContents();
        if (dsTerms.isEmpty()) {
            conts.add(xml_element("mutation", "", "ONT::TRUE"));
        } else {
            for (KQMLList drumTerm : dsTerms) {
                if (!pullTermHead(drumTerm).equalsIgnoreCase("MUTATION")) {
                    continue;
                }
                conts.add(xml_element("mutation", "", xml_mutation(drumTerm)));
            }
        }
        return xml_element(exType, attrs, conts);
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
    protected String xml_drumTerm(KQMLList dsTerm) {
        if (dsTerm == null)
            return "";
        
        List<String> attrs = xml_drumTerm_attributes(dsTerm);     
        List<String> conts = xml_drumTerm_content(dsTerm);
        conts.add(xml_famMembers(dsTerm));
        
        return xml_element("drum-term", attrs, conts);                
    }

    /**
     * Returns a {@code <name>} XML element representing the name of the term,
     * or the empty string if no such information exists.
     */
    private String xml_name() {
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
                : xml_element("name", "", xml_escape(name));
    }
    
    /**
     * Returns a {@code <coref>} XML element representing the term referred to by this term, or
     * the empty string if no such information exists.
     */
    private String xml_coref() {
        return (refVar == null) ? "" 
                : xml_element("coref", xml_attribute("id", removePackage(refVar, false)), null);
    }

    /**
     * Returns a {@code <base>} XML element representing the term referred to by this term, or
     * the empty string if no such information exists.
     */
    private String xml_base() {
        KQMLObject baseObj = attributes.get(Attribute.BASE);
        return (baseObj == null) ? "" 
                : xml_element("coref", xml_attribute("id", removePackage(baseObj.stringValue(), false)), null);
    }

    /**
     * Returns a {@code <mods>} XML element representing the term modifiers, or
     * the empty string if no such information exists.
     */
    private String xml_mods() {
        List<String> conts = new ArrayList<String>();
        conts.add(xml_mods(PolyAttribute.DEGREE, "degree"));
        conts.add(xml_mods(PolyAttribute.FREQUENCY, "frequency"));
        conts.add(xml_mods(PolyAttribute.MODA, "mod"));
        conts.add(xml_mods(PolyAttribute.MODN, "mod"));
        return xml_element("mods", null, conts);
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
    private String xml_mods(PolyAttribute mod, String modType) {
        ArrayList<KQMLObject> mods = polyAttributes.get(mod);
        if ((mods == null) || mods.isEmpty()) 
            return "";
        
        String result = "";
        for (KQMLObject modValue : mods) {
            if (modValue instanceof KQMLList) {
                KQMLList modValueList = (KQMLList) modValue;
                List<String> conts = new ArrayList<String>();
                conts.add(xml_element("type", "", ontType(modValueList)));
                conts.add(xml_element("value", "", removePackage(ontWord(modValueList), false)));
                result += xml_element(modType, null, conts);
            } else if (isOntVar(modValue.toString())) {
                String var = modValue.toString();
                if (ekbFindExtraction(var) != null) { 
                    result += xml_elementWithID(modType, var);
                } else { // we need to define the item here
                    result += xml_lfTerm(modType, var);
                }
            } else { // should not happen!
                Debug.error("unexpected " + mod + " value: " + modValue);
                result += xml_element(modType, "", removePackage(modValue.toString(), false));
            }
        }

        return result;
    }

    /**
     * Qualifiers
     */
    private String xml_qualifiers() {
        return xml_element("qualifiers", "", xml_qualifiers(PolyAttribute.QUAL, "qual"));
    }
    
    /**
     * Qualifier
     * 
     * @param qual
     * @param tag
     * @return
     */
    private String xml_qualifiers(PolyAttribute qual, String tag) {
        ArrayList<KQMLObject> quals = polyAttributes.get(qual);
        if ((quals == null) || quals.isEmpty()) 
            return "";
        
        String result = "";
        for (KQMLObject valObj : quals) {
            if (valObj instanceof KQMLList) {
                KQMLList valType = (KQMLList) valObj;
                result += xml_element(tag, xml_attribute("type", ontType(valType)), null);
            } else if (isOntVar(valObj.toString())) {
                String var = valObj.toString();
                if (ekbFindExtraction(var) != null) { 
                    result += xml_elementWithID(tag, var);
                } else { // we need to define the item here
                    result += xml_lfTerm(tag, var);
                }
            } else { // should not happen!
                Debug.error("unexpected " + qual + " value: " + valObj);
                result += xml_element(tag, "", removePackage(valObj.toString(), false));
            }
        }
        return result;
    }

    /**
     * Assoc-with
     * 
     * @return
     */
    private String xml_assocs() {
        ArrayList<KQMLObject> assocs = polyAttributes.get(PolyAttribute.ASSOC);
        if ((assocs == null) || assocs.isEmpty()) 
            return "";
        
        String result = "";
        for (KQMLObject valObj : assocs) {
            if (valObj instanceof KQMLList) {
                KQMLList valType = (KQMLList) valObj;
                result += xml_element("assoc-with", xml_attribute("type", ontType(valType)), null);
            } else if (isOntVar(valObj.toString())) {
                String var = valObj.toString();
                if (ekbFindExtraction(var) != null) { 
                    result += xml_elementWithID("assoc-with", var);
                } else { // we need to define the item here
                    result += xml_lfTerm("assoc-with", var);
                }
            } else { // should not happen!
                Debug.warn("unexpected " + PolyAttribute.ASSOC + " value: " + valObj);
                result += xml_element("assoc-with", null, removePackage(valObj.toString(), false));
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
    private String xml_assocPoss() {
        KQMLObject assoc = attributes.get(Attribute.ASSOC_POSS);
        if (assoc == null) 
            return "";
        
        String var = assoc.toString();
        if (isOntVar(var)) {
            if (ekbFindExtraction(var) != null) { 
                return xml_elementWithID("poss-by", var);
            } else { // we need to define the item here
                return xml_lfTerm("poss-by", var);
            }
        } else { // should not happen!
            Debug.warn("unexpected " + Attribute.ASSOC_POSS + " value: " + var);
            return xml_element("poss-by", null, removePackage(var, false));
        }
    }

    /**
     * XML element for quantifier.
     * 
     * @return
     */
    private String xml_quantifier() {
        KQMLObject quan = attributes.get(Attribute.QUAN);
        if (quan == null) 
            return "";
        
        String var = quan.toString();
        if (isOntVar(var)) {
            if (ekbFindExtraction(var) != null) { 
                return xml_elementWithID("quantifier", var);
            } else { // we need to define the item here
                return xml_lfTerm("quantifier", var);
            }
        } else if (quan instanceof KQMLToken) { // likely an ont type
            return xml_element("quantifier", xml_attribute("type", var), null);
        } else { // should not happen!
            Debug.warn("unexpected " + Attribute.QUAN + " value: " + var);
            return "";
        }
    }

    /**
     * XML element for refset.
     * 
     * @return
     */
    private String xml_refset() {
        KQMLObject quan = attributes.get(Attribute.REFSET);
        if (quan == null) 
            return "";
        
        String var = quan.toString();
        if (isOntVar(var)) {
            if (ekbFindExtraction(var) != null) { 
                return xml_elementWithID("refset", var);
            } else { // we need to define the item here
                return xml_lfTerm("refset", var);
            }
        } else if (quan instanceof KQMLToken) { // likely an ont type
            return xml_element("refset", xml_attribute("type", var), null);
        } else { // should not happen!
            Debug.warn("unexpected " + Attribute.REFSET + " value: " + var);
            return "";
        }
    }


    /**
     * Returns a {@code <features>} XML element representing the term features,
     * or the empty string if no such information exists.
     */
    private String xml_features() {
        List<String> conts = new ArrayList<String>();
        conts.add(xml_inevent());
        conts.add(xml_time()); // time
        if (ExtractionFactory.getProperty("extractions.mode").equals("DRUM")) {
            conts.add(xml_active());
            conts.add(xml_location()); // {DRUM} cellular location
            conts.add(xml_mutation()); // {DRUM} mutations (for proteins, etc.)
            conts.add(xml_site()); // {DRUM} domain
            conts.add(xml_residue()); // {DRUM} residue
            conts.add(xml_cellline()); // {DRUM} cell-line
        }
        return xml_element("features", null, conts);
    }
    
    private String xml_active() {
        KQMLObject isActive = attributes.get(Attribute.ACTIVE);
        if (isActive == null)
            return "";
        return xml_element("active", null, removePackage(isActive.toString(), false));
    }
    
    /**
     * Time expressions
     * @return
     */
    private String xml_timex() {
        List<String> attrs = new ArrayList<String>();
        List<String> conts = new ArrayList<String>();
        KQMLObject dow = attributes.get(Attribute.DOW);
        if (dow != null) {
            String var = dow.toString();
            Extraction ekbTerm = ekbFindExtraction(var);
            KQMLList term = (ekbTerm != null) ? ekbTerm.getValue() : findTermByVar(var, context);
            if (term != null) {
                conts.add(xml_element("dow", null, removePackage(ontWord(pullFullOntType(term)))));
            } else { // something's wrong
                conts.add(xml_lfTerm("from-time", var));
            }

        }
        KQMLObject day = attributes.get(Attribute.DAY);
        if (day != null) {
            conts.add(xml_element("day", null, day.toString()));
        }
        KQMLObject month = attributes.get(Attribute.MONTH);
        if (month != null) {
            String mtext = "";
            if (month instanceof KQMLList) {
                mtext = normalizeOnt(ontWord(month));
            }
            conts.add(xml_element("month", null, mtext));
        }
        KQMLObject year = attributes.get(Attribute.YEAR);
        if (year != null) {
            conts.add(xml_element("year", null, year.toString()));
        }
        if (conts.isEmpty()) 
            return "";
        
        KQMLObject phase = attributes.get(Attribute.PHASE);
        if (phase != null) {
            String mod = removePackage(ontWord(phase));
            if (!mod.isEmpty()) {
                attrs.add(xml_attribute("mod", mod));
            }
        }

        attrs.add(xml_attribute("type","DATE"));
        return xml_element("timex", attrs, conts);
    }

    /**
     * Time expressions for time ranges
     * @return
     */
    private String xml_timerange() {
        if (! ontType.equalsIgnoreCase("ONT::TIME-RANGE")) {
            return "";
        }

        //List<String> attrs = new ArrayList<String>();
        List<String> conts = new ArrayList<String>();
        KQMLObject from = attributes.get(Attribute.FROM);
        if (from != null) {
            String var = from.toString();
            if (ekbFindExtraction(var) != null) {
                conts.add(xml_elementWithID("from-time", var, null));
            } else { // we need to define the item here
                conts.add(xml_lfTerm("from-time", var));
            }
        }
        KQMLObject to = attributes.get(Attribute.TO);
        if (to != null) {
            String var = to.toString();
            if (ekbFindExtraction(var) != null) {
                conts.add(xml_elementWithID("to-time", var, null));
            } else { // we need to define the item here
                conts.add(xml_lfTerm("to-time", var));
            }
        }

        //attrs.add(xml_attribute("type","DURATION"));
        //return xml_element("timex", attrs, conts);
        return join("",conts);
    }

    /**
     * Time feature
     * @return
     */
    private String xml_time() {
        KQMLObject time = attributes.get(Attribute.TIME);
        if (time == null)
            return "";
        
        List<String> attrs = new ArrayList<String>();
        KQMLObject modType = attributes.get(Attribute.TIMEMOD);
        if (modType != null) {
            String mod = ontType(modType);
            if (!mod.isEmpty()) {
                attrs.add(xml_attribute("mod", mod));
            }
        }
        
        if (isOntVar(time.toString())) {
            String var = time.toString();
            if (ekbFindExtraction(var) != null) { 
                return xml_elementWithID("time", var, attrs);
            } else { // we need to define the item here
                return xml_lfTerm("time", var, attrs);
            }
        } else { // should not happen!
            Debug.error("unexpected " + Attribute.TIME + " value: " + time);
            return xml_element("time", "", removePackage(time.toString(), false)); 
        }
    }

    // TODO: multiple locations (make it poly-attribute)
    private String xml_location() {
        KQMLObject loc = attributes.get(Attribute.LOC);
        if (loc == null) {
            loc = attributes.get(Attribute.LOCATION);
            if (loc == null)
                return "";
        }

        List<String> attrs = new ArrayList<String>();
        KQMLObject modType = attributes.get(Attribute.LOCMOD);
        if (modType != null) {
            String mod = ontType(modType);
            if (!mod.isEmpty()) {
                attrs.add(xml_attribute("mod", mod));
            }
        }
        
        String result = "";
        if (isOntVar(loc.toString())) {
            String var = loc.toString();
            if (ekbFindExtraction(var) != null) {
                result += xml_elementWithID("location", var, attrs);
            } else { // we need to define the item here
                result += xml_lfTerm("location", var, attrs);
            }
        } else {
            Debug.warn("unexpected " + Attribute.LOC + " value: " + loc);
        }
        return result;
    }
    
    /**
     * Creates explicit site (eg, domain) property for the term.
     * 
     */
    private String xml_site() {
        KQMLObject varObj = attributes.get(Attribute.SITE);
        if (varObj == null) 
            return "";
        String var = varObj.toString();
        if (isOntVar(var)) {
            if (ekbFindExtraction(var) != null) {
                return xml_elementWithID("site", var);
            } else { // we need to define the item here
                return xml_lfTerm("site", var);
            }
        } else {
            Debug.warn("unexpected " + Attribute.SITE + " value: " + var);
            return "";
        }
    }

    /**
     * 
     * @return
     */
    private String xml_mutation() {
        ArrayList<KQMLObject> mutations = polyAttributes.get(PolyAttribute.MUTATION);
        if ((mutations == null) || mutations.isEmpty()) 
            return "";
        
        Debug.debug("mutations: " + mutations);
        String result = "";
        for (KQMLObject varObj : mutations) {
            // can be either ONT::TRUE or a term var
            String var = varObj.toString();
            if (isOntBool(var)) {
                Debug.warn(":MUTATION value is bool only: " + var);
                result +=  xml_element("mutation", "", removePackage(var));
            } else if (isOntVar(var)) {
                if (ekbFindExtraction(var) != null) {
                    return xml_elementWithID("mutation", var);
                } else { // we need to define the item here
                    return xml_lfTerm("mutation", var);
                }
            } else {
                Debug.warn("unexpected " + PolyAttribute.MUTATION + " value: " + var);
                result +=  xml_element("mutation", "", removePackage(var, false));
            }
        }

        return result;
    }

    private String xml_mutation(KQMLList term) {
        Debug.debug("mutation: " + term);
        KQMLObject mType = term.getKeywordArg(":TYPE");
        String result = xml_element("type", "", mType.toString());
        if (mType.toString().equalsIgnoreCase("DELETION")) {
            result += xml_deletionMutation(term);
        }
        if (mType.toString().equalsIgnoreCase("SUBSTITUTION")) {
            result += xml_substitutionMutation(term);
        }
        if (mType.toString().equalsIgnoreCase("INSERTION")) {
            result += xml_insertionMutation(term);
        }
        return result;
    }

    private String xml_deletionMutation(KQMLList term) {
        Debug.debug("deletion: " + term);
        String result = "";
        KQMLObject posFrom = term.getKeywordArg(":LOWER");
        if (posFrom != null) {
            result += xml_element("pos-from", "", xml_AASite((KQMLList) posFrom));
        }
        KQMLObject posTo = term.getKeywordArg(":UPPER");
        if (posTo != null) {
            result += xml_element("pos-to", "", xml_AASite((KQMLList) posTo));
        }
        KQMLObject pos = term.getKeywordArg(":AA-INDEX");
        if (pos != null) {
            result += xml_element("pos", "", pos.toString());
        }
        KQMLObject posAA = term.getKeywordArg(":OLD");
        if (posAA != null) {
            result += xml_element("aa-from", "", xml_AA((KQMLList) posAA));
        }
        return result;
    }

    private String xml_substitutionMutation(KQMLList term) {
        Debug.debug("substitution: " + term);
        String result = "";

        // obligatory
        result += xml_element("pos", "", term.getKeywordArg(":AA-INDEX").toString());
        // optional
        KQMLObject from = term.getKeywordArg(":OLD");
        if (from != null) {
            result += xml_element("aa-from", "", xml_AA((KQMLList) from));
        }
        // obligatory
        result += xml_element("aa-to", "", xml_AA((KQMLList) term.getKeywordArg(":NEW")));

        return result;
    }

    private String xml_insertionMutation(KQMLList term) {
        Debug.debug("insertion: " + term);
        String result = "";
        result += xml_element("pos-from", "", xml_AASite((KQMLList) term.getKeywordArg(":LOWER")));
        result += xml_element("pos-to", "", xml_AASite((KQMLList) term.getKeywordArg(":UPPER")));
        result += xml_element("insert", "", xml_AAList((KQMLList) term.getKeywordArg(":NEW")));
        return result;
    }

    private String xml_AAList(KQMLList term) {
        Debug.debug("aa-list: " + term);
        String result = "";
        for (KQMLObject aaTerm : term) {
            result += xml_AA((KQMLList) aaTerm);
        }
        return result;
    }

    private String xml_AA(KQMLList term) {
        Debug.debug("aa: " + term);
        List<String> conts = new ArrayList<String>();
        conts.add(xml_element("name", "", term.getKeywordArg(":NAME").stringValue()));
        conts.add(xml_element("code", "", term.getKeywordArg(":LETTER").stringValue()));
        return xml_element("aa", null, conts);

    }

    private String xml_AASite(KQMLList term) {
        Debug.debug("aa-site: " + term);
        List<String> conts = new ArrayList<String>();
        conts.add(xml_element("name", "", term.getKeywordArg(":NAME").stringValue()));
        conts.add(xml_element("code", "", term.getKeywordArg(":LETTER").stringValue()));
        conts.add(xml_element("pos", "", term.getKeywordArg(":INDEX").toString()));
        return xml_element("site", null, conts);
    }

    /**
     * Creates residue site term property.
     * 
     * @return
     */
    private String xml_residue() {
        if (dsTerms.isEmpty()) {
            return "";
        }
        for (KQMLList drumTerm : dsTerms) {
            // we only look for the first AA-SITE term; there shouldn't be more than one!
            if (pullTermHead(drumTerm).equalsIgnoreCase("AA-SITE")) {
                return xml_AASite(drumTerm);
            }
        }
        return "";
    }

    /**
     * Returns a list of {@code inevent} "features".
     * TODO: re-think whether these are really features or something else!
     */
    private String xml_inevent() {
        ArrayList<KQMLObject> inEvents = polyAttributes.get(PolyAttribute.INEVENT);
        // Debug.warn("poly :INEVENT of " + id + " =  " + inEvents);
        if ((inEvents == null) || inEvents.isEmpty()) 
            return "";
        
        String result = "";
        for (KQMLObject varObj : inEvents) {
            String var = varObj.toString();
            if (isOntVar(var)) {
                // TODO: get more info?
                if (ekbFindExtraction(var) != null) {
                    return xml_elementWithID("inevent", var);
                } else { // we need to define the item here
                    return xml_lfTerm("inevent", var);
                }
            } else {
                Debug.error("unexpected " + PolyAttribute.INEVENT + " value: " + var);
            }
        }

        return result;
    }

    /**
     * Returns a {@code <cell-line>} XML element representing cell-line
     * information attached to the term, or the empty string if no such
     * information exists.
     */
    private String xml_cellline() {
        KQMLObject varObj = attributes.get(Attribute.CELL_LINE);
        if (varObj == null) 
            return "";
        
        String var = varObj.toString();
        if (isOntVar(var)) {
            if (ekbFindExtraction(var) != null) {
                return xml_elementWithID("cell-line", var);
            } else { // we need to define the item here
                return xml_lfTerm("cell-line", var);
            }
        } else {
            Debug.error("unexpected " + Attribute.CELL_LINE + " value: " + var);
            return "";
        }
    }
    
    /**
     * Size feature
     */
    private String xml_size() {
        KQMLObject varObj = attributes.get(Attribute.SIZE);
        if (varObj == null) 
            return "";
        // value could be an ont type or a var
        String var = varObj.toString();
        if (isOntVar(var)) {
            if (ekbFindExtraction(var) != null) {
                return xml_elementWithID("size", var);
            } else { // we need to define the item here
                return xml_lfTerm("size", var);
            }
        } else { // must be an ont type
            return xml_element("size", xml_attribute("type", var), null);
        }
    }
    
    /**
     * Scale feature
     */
    private String xml_scale() {
        KQMLObject valObj = attributes.get(Attribute.SCALE);
        if (valObj == null)
            return "";
        // value must be an ont type
        return xml_element("scale", xml_attribute("type", valObj.toString()), null);
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
                    expandedValue.add(pullFullOntType(term));
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

