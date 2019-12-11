/*
 * EventExtraction.java
 *
 * $Id: CausalityExtraction.java,v 1.17 2019/12/11 04:25:43 lgalescu Exp $
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>, 8 Jan 2015
 */

package TRIPS.DrumGUI;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLToken;

/**
 * Class for representing and handling CC (Causal Connection) extractions.
 * 
 * @author lgalescu
 *
 */
public class CausalityExtraction extends Extraction {

    /** Roles (event arguments) */
    private enum Role {
        // Roles:
        // CAUSE
        FACTOR(":FACTOR"),
        // sequence of EVENTs leading to outcome (only for type BY-MEANS-OF)
        FACTOR_SEQUENCE(":FACTOR-SEQUENCE"),
        // EFFECT
        OUTCOME(":OUTCOME"),
        /** */
        AGENT(":AGENT"),
        /** */
        AGENT1(":AGENT1"),
        /** */
        AFFECTED(":AFFECTED"),
        /** */
        AFFECTED1(":AFFECTED1"),
        /** */
        AFFECTED_RESULT(":AFFECTED-RESULT"),
        /** */
        NEUTRAL(":NEUTRAL"),
        /** */
        NEUTRAL1(":NEUTRAL1"),
        /** */
        FORMAL(":FORMAL");
        private String roleName;
        private Role(String name) { roleName = name; }
        public String toString() { return roleName; }
        public static boolean isRole(String item) {
            for (Role r: Role.values()) {
                if (r.roleName.equalsIgnoreCase(item)) return true;
            }
            return false;
        }
    };

    /** Features */
    protected enum Feature {
        // :LOC id :LOCMOD ontType --> location
        LOC(":LOC"),
        LOCMOD(":LOCMOD"),
        LOCATION(":LOCATION"), // LF-term attribute for :LOC
        // :TIME :TIMEMOD
        TIME(":TIME"), // time
        TIMEMOD(":TIMEMOD"),
        // :EXTENT
        EXTENT(":EXTENT")
        ;
        private String featureName;
        private Feature(String name) { featureName = name; }
        public String toString() { return featureName; }
        public static boolean isFeature(String item) {
            for (Feature f: Feature.values()) {
                if (f.featureName.equalsIgnoreCase(item)) return true;
            }
            return false;
        }
    };


    /** Event modifiers */
    private enum Modifier {
        // :NEGATION true|false --> negation
        NEGATION(":NEGATION"),
        // :POLARITY negative|positive --> polarity
        POLARITY(":POLARITY"),
        // :FORCE token
        FORCE(":FORCE"),
        // :MODALITY VAR
        MODALITY(":MODALITY"),
        // :EPI VAR
        EPI(":EPI");
        private String modName;

        private Modifier(String name) {
            modName = name;
        }

        public String toString() {
            return modName;
        }

        public static boolean isModifier(String item) {
            for (Modifier m : Modifier.values()) {
                if (m.modName.equalsIgnoreCase(item))
                    return true;
            }
            return false;
        }
    };

    /** Event poly-modifiers (stackable) */
    private enum PolyModifier {
        // DEGREE: hyper-/hypo-/under-/over-
        DEGREE(":DEGREE"),
        // FREQUENCY: re-
        FREQUENCY(":FREQUENCY"),
        // MODA: auto-/trans-/homo-/hetero-/uni-/mono-/di-/tri-/poly-/multi-
        MODA(":MODA"),
        // MODN: de-/un-/non-/dis-
        MODN(":MODN"),
        ASSOC(":ASSOC")
        ;
        private String modName;

        /**
         * Stores alternative names of poly-modifiers.
         * For now we only handle one alternative per modifier.
         */
        private static HashMap<PolyModifier, String> altNames;
        static {
            altNames = new HashMap<PolyModifier, String>();
            altNames.put(ASSOC, ":ASSOC-WITH");
        }
        
        private PolyModifier(String name) {
            modName = name;
        }

        public String toString() {
            return modName;
        }

        public static boolean isModifier(String item) {
            for (PolyModifier m : PolyModifier.values()) {
                if (m.modName.equalsIgnoreCase(item))
                    return true;
            }
            return false;
        }
    };

    /* roles list (in order): mapping from role names to term vars */
    private LinkedHashMap<Role, KQMLObject> roles;
    /* features list */
    private LinkedHashMap<Feature, KQMLObject> features;
    /* mods list */
    private LinkedHashMap<Modifier, KQMLObject> mods;
    /* poly-mods list */
    private LinkedHashMap<PolyModifier, ArrayList<KQMLObject>> polyMods;


    public CausalityExtraction(DrumKB ekb, KQMLList value, KQMLList context, int uttnum) {
        super(ekb, value, context, uttnum);

        // get roles, features, modifiers
        pullRoles();
        pullFeatures();
        pullModifiers();
        pullPolyModifiers();

        // expandedValue
        makeExpandedValue();
    }

    //// GETTERS

    /**
     * Gets the list of roles.
     */
    public LinkedHashMap<Role, KQMLObject> getRoles() {
        return roles;
    }
    
    /**
     * Gets the list of features.
     */
    public LinkedHashMap<Feature, KQMLObject> getFeatures() {
        return features;
    }
    
    /**
     * Gets the list of modifiers.
     */
    public LinkedHashMap<Modifier, KQMLObject> getMods() {
        return mods;
    }
    
    /**
     * Gets the list of poly-modifiers.
     */
    public LinkedHashMap<PolyModifier, ArrayList<KQMLObject>> getPolyMods() {
        return polyMods;
    }

    /**
     * @return the negation
     */
    public KQMLObject getNegation() {
        return mods.get(Modifier.NEGATION);
    }

    /**
     * @return the polarity
     */
    public KQMLObject getPolarity() {
        return mods.get(Modifier.POLARITY);
    }

    /**
     * @return the force
     */
    public KQMLObject getForce() {
        return mods.get(Modifier.FORCE);
    }

    /**
     * @return the modality
     */
    public KQMLObject getModality() {
        return mods.get(Modifier.MODALITY);
    }

    /**
     * @return the epistemic modality
     */
    public KQMLObject getEpiModality() {
        return mods.get(Modifier.EPI);
    }

    //// PULLERS
    
    /**
     * Pulls roles from an LF term.
     */
    private void pullRoles() {
        roles = new LinkedHashMap<Role, KQMLObject>();
        for (Role role : Role.values()) {
            KQMLObject value = shortValue.getKeywordArg(role.toString());
            if ((value != null) && !value.toString().equals("-")) {
                if (isOntVar(value.toString())) {
                    roles.put(role, value);
                } else if (value instanceof KQMLList) { // only for :FACTOR-SEQUENCE, but we don't check
                    roles.put(role, value);
                } else {
                    Debug.warn("Unexpected role value: " + role + " " + value);
                }
            }
        }
    }

    /**
     * Pulls features.
     */
    private void pullFeatures() {
        features = new LinkedHashMap<Feature, KQMLObject>();
        for (Feature feat : Feature.values()) {
            KQMLObject aValue = shortValue.getKeywordArg(feat.toString());
            // note: we ignore dash values, which we interpret as "undefined"
            if ((aValue != null) && !aValue.toString().equals("-")) {
                features.put(feat, aValue);
            }
        }
    }


    /**
     * Pulls modifiers.
     */
    private void pullModifiers() {
        mods = new LinkedHashMap<Modifier, KQMLObject>();
        for (Modifier mod : Modifier.values()) {
            KQMLObject value = shortValue.getKeywordArg(mod.toString());
            if ((value != null) && !value.toString().equals("-")) {
                mods.put(mod, value);
            }
        }
    }

    /**
     * Pulls poly-modifiers (modifiers that can be repeated with different values).
     */
    private void pullPolyModifiers() {
        polyMods = new LinkedHashMap<PolyModifier, ArrayList<KQMLObject>>();
        try {
            ListIterator<KQMLObject> iterator;
            for (PolyModifier mod : PolyModifier.values()) {
                String modName = mod.toString();
                String altName = PolyModifier.altNames.get(mod);
                iterator = shortValue.listIterator();
                ArrayList<KQMLObject> modValues = new ArrayList<KQMLObject>();
                while (iterator.hasNext()) {
                    String key = iterator.next().toString();
                    if (key.equalsIgnoreCase(modName) || key.equalsIgnoreCase(altName)) {
                        // look ahead
                        int nextIndex = iterator.nextIndex();
                        KQMLObject value = shortValue.get(nextIndex);
                        // Debug.debug("Found poly-modifier: " + key + " " + value);
                        if ((value instanceof KQMLToken) && ((KQMLToken) value).equalsIgnoreCase("-")) {
                            Debug.warn("Removed " + key + " " + value);
                            iterator.remove();
                            iterator.next();
                            iterator.remove();
                            continue;
                        }
                        /* LG20181020 we don't do this anymore
                        if (value instanceof KQMLList) { // fix
                            KQMLList valueList = (KQMLList) value;
                            if (((KQMLToken) valueList.get(0)).equalsIgnoreCase(":*")) {
                                Debug.warn("Removed [:*] from " + valueList);
                                valueList.remove(0);
                            }
                        }
                        */
                        if (! modValues.contains(value)) {
                            modValues.add(value);
                        }
                        iterator.next();
                    }
                }
                if (!modValues.isEmpty()) {
                    polyMods.put(mod, modValues);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // // COMPARISONS and MERGING

    /**
     * Compares the specified extraction with this one for equality.
     * Two {@link Extraction} objects are equal if their {@link #exType}, {@link #id} and {@link #ontType} members are
     * equal. For events,
     * they must also have the same arguments.
     */
    public boolean equals(CausalityExtraction e) {
        if (!super.equals(e))
            return false;
        if (!getNegation().equals(e.getNegation()))
            return false;
        // common roles must match
        LinkedHashMap<Role, KQMLObject> eRoles = e.getRoles();
        for (Role r : roles.keySet()) {
            if (!roles.get(r).equals(eRoles.get(r)))
                return false;
        }
        return true;
    }

    /**
     * Combines with another extraction.
     * Should be overridden by subclasses.
     */
    protected void combineWith(Extraction other) {
        if (!this.equals(other)) {
            Debug.error("Cannot update: extractions not equal: this=" + shortValue + " other=" + other.shortValue);
            return;
        }
        CausalityExtraction e = (CausalityExtraction) other;
        Debug.debug("Extraction before update: " + shortValue);
        Debug.debug("Update from: " + e.shortValue);

        // check roles (we already know the common ones are equal)
        LinkedHashMap<Role, KQMLObject> eRoles = e.getRoles();
        // create a new list to make sure its keySet is sorted in natural order
        LinkedHashMap<Role, KQMLObject> newRoles = new LinkedHashMap<Role, KQMLObject>();
        boolean updated = false;
        for (Role r : Role.values()) {
            KQMLObject v1 = roles.get(r);
            KQMLObject v2 = eRoles.get(r);
            if (v1 != null) {
                newRoles.put(r, v1);
            } else if (v2 != null) {
                newRoles.put(r, v2);
                shortValue.add(r.toString());
                shortValue.add(v2);
                Debug.debug("Updated: " + r);
                updated = true;
            }
        }
        if (updated) {
            roles = newRoles;
        }

        // check features; issue error if any are different, but keep going anyway
        LinkedHashMap<Feature, KQMLObject> eFeats = e.getFeatures();
        for (Feature attr: eFeats.keySet()) {
            KQMLObject newVal = eFeats.get(attr);
            if (newVal == null) continue;
            KQMLObject oldVal = features.get(attr);
            if (oldVal == null) {
                features.put(attr, newVal);
                shortValue.add(attr.toString());
                shortValue.add(newVal);
                Debug.debug("Updated: " + attr + " to: " + newVal);
            } else if (!oldVal.equals(newVal)) {
                Debug.error("Conflicting values for " + attr + ":\n\told: " + oldVal + "\n\tnew: " + newVal
                        + "\n\t=> Will keep original.");
            }

        }
    
        // check mods
        LinkedHashMap<Modifier, KQMLObject> eMods = e.getMods();
        for (Modifier attr : eMods.keySet()) {
            KQMLObject newVal = eMods.get(attr);
            if (newVal == null)
                continue;
            KQMLObject oldVal = mods.get(attr);
            if (oldVal == null) {
                mods.put(attr, newVal);
                shortValue.add(attr.toString());
                shortValue.add(newVal);
                Debug.debug("Updated: " + attr + " to: " + newVal);
            } else if (!oldVal.equals(newVal))
                Debug.error("Conflicting values for " + attr + "\n\told: " + oldVal + "\n\tnew: " + newVal
                        + "\n\t=> Will keep original.");
        }

        // check poly-mods
        LinkedHashMap<PolyModifier, ArrayList<KQMLObject>> ePolyMods = e.getPolyMods();
        for (PolyModifier attr : ePolyMods.keySet()) {
            ArrayList<KQMLObject> pmVals2 = ePolyMods.get(attr);
            if (pmVals2 == null)
                continue;
            ArrayList<KQMLObject> pmVals1 = polyMods.get(attr);
            if (pmVals1 == null) {
                pmVals1 = new ArrayList<KQMLObject>();
                polyMods.put(attr, pmVals1);
            }
            for (KQMLObject v2 : pmVals2) {
                if (!pmVals1.contains(v2)) {
                    pmVals1.add(v2);
                    shortValue.add(attr.toString());
                    shortValue.add(v2);
                    Debug.debug("Updated: " + attr + " to: " + polyMods.get(attr));
                }
            }
        }

        // update context
        KQMLList eCtxt = e.getContext();
        for (KQMLObject o : eCtxt) {
            if (context.indexOf(o) < 0) {
                context.add(o);
            }
        }

        // update boundaries
        int eStart = e.getStart();
        if (eStart < start) {
            start = eStart;
        }
        int eEnd = e.getEnd();
        if (eEnd > end) {
            end = eEnd;
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

    //// XML FORMATTING
    
    /** 
     * Returns an XML representation of the event. Typically, this will be an {@code <event>} XML element.
     * However, if the event is not primitive, i.e., when at least one role is filled by multiple entities/terms, the 
     * result is a sequence of {@code <event>} XML elements. 
     */
    public String toXML() {
        List<String> attrs = xml_commonAttributes();
        List<String> conts = xml_commonContents();

        conts.add(xml_assocs());
        conts.add(xml_mods());
        conts.add(xml_args());
        conts.add(xml_time());
        conts.add(xml_extent());
        conts.add(xml_location());     
        
        return xml_element(exType, attrs, conts);
    }

    /**
     * XML contents (sub-elements) common to all EVENT extractions.
     * 
     * @return list of XML elements
     */
    protected List<String> xml_commonContents() {
        List<String> conts = super.xml_commonContents();
        conts.add(xml_negation());
        conts.add(xml_polarity());
        conts.add(xml_modality());
        conts.add(xml_epimodality());
        conts.add(xml_force());
        return conts;
    }
    

    /**
     * Returns a list of XML elements representing information about all event
     * arguments.
     * 
     * @see #createArgXML(int, Role, String)
     */
    private String xml_args() {
        if (roles.isEmpty()) {
            return "";
        }
        String result = "";
        for (Role role: roles.keySet()) {
            result += xml_arg(role, roles.get(role));
        }
        return result;
    }

    /**
     * Returns an XML element representing information about an argument.
     * 
     * @see #xml_args()
     */
    private String xml_arg(Role role, KQMLObject roleValue) {
        Debug.debug("arg(" + role + "," + roleValue + ")");
        if (roleValue == null) {
            // shouldn't happen!
            Debug.warn("unexpected role: " + role);
            return "";
        }
        if (roleValue instanceof KQMLList) {
            return xml_seqarg(role, (KQMLList) roleValue);
        }
        String idVar = roleValue.toString();
        // we obtain the arg from the EKB, if we can; otherwise, we look for the term in the context
        Extraction ekbTerm = ekbFindExtraction(idVar);
        KQMLList term = (ekbTerm != null) ? ekbTerm.getValue() : findTermByVar(idVar, context);
	if (term == null) {
            // shouldn't happen, but occasionally terms are missing from the LF!
            Debug.warn("LF term not found: " + idVar);
            return "";
	}
        int start = getKeywordArgInt(":START", term);
        int end = getKeywordArgInt(":END", term);

        List<String> attrs = new ArrayList<String>();
        attrs.add(xml_attribute("id", removePackage(idVar)));
        attrs.add(xml_attribute("role", role.toString()));
        attrs.add(xml_attribute("start", String.valueOf(getOffset(start))));
        attrs.add(xml_attribute("end", String.valueOf(getOffset(end))));

        if (ekbTerm != null) {
            return xml_element("arg", attrs, null);
        } else {
            KQMLObject termType = pullFullOntType(term);

            List<String> conts = new ArrayList<String>();
            conts.add(xml_element("type", "", ontType(termType)));
            conts.add(xml_element("text",
                    xml_attribute("normalization", xml_escape(normalizeOnt(ontWord(termType)))),
                    xml_escape(removeTags(getTextSpan(start, end)))));

            return xml_element("arg", attrs, conts);

        }
    }
    
    /**
     * Returns an XML element representing information about a sequence argument.
     * <p>
     * Currently this applies only to {@link CausalityExtraction.Role#FACTOR_SEQUENCE}
     * 
     * @see #createArgXML()
     */
    private String xml_seqarg(Role role, KQMLList roleValue) {
        Debug.debug("arg(" + role + "," + roleValue + ")");
        if (roleValue == null) {
            // shouldn't happen!
            Debug.warn("unexpected role: " + role);
            return "";
        }
        String seqArgs = "";
        int seqIndex = 0;
        for (KQMLObject tVar : roleValue) { // TODO: check that tVar is, indeed, an OntVar!
            String idVar = tVar.toString();
            // we obtain the arg from the EKB, if we can; otherwise, we look for the term in the context
            Extraction ekbTerm = ekbFindExtraction(idVar);
            KQMLList term = (ekbTerm != null) ? ekbTerm.getValue() : findTermByVar(idVar, context);
            int start = getKeywordArgInt(":START", term);
            int end = getKeywordArgInt(":END", term);

            List<String> attrs = new ArrayList<String>();
            attrs.add(xml_attribute("id", removePackage(idVar)));
            attrs.add(xml_attribute("role", role.toString()));
            attrs.add(xml_attribute("seqno", String.valueOf(++seqIndex)));
            attrs.add(xml_attribute("start", String.valueOf(getOffset(start))));
            attrs.add(xml_attribute("end", String.valueOf(getOffset(end))));

            if (ekbTerm != null) {                
                seqArgs += xml_element("arg", attrs, null);
            } else {
                KQMLObject termType = pullFullOntType(term);

                List<String> conts = new ArrayList<String>();
                conts.add(xml_element("type", "", ontType(termType)));
                conts.add(xml_element("text",
                        xml_attribute("normalization", xml_escape(normalizeOnt(ontWord(termType)))),
                        xml_escape(removeTags(getTextSpan(start, end)))));

                seqArgs += xml_element("arg", attrs, conts);
            }
        }

        return seqArgs;
    }

    /**
     * Returns a {@code <negation>} XML element representing the value of the negation modifier for the
     * event. If that value is {@code null}, returns the empty string.
     * 
     * @see Modifier#NEGATION
     */
    private String xml_negation() {
        KQMLObject value = getNegation();
        if (value == null)  return "";
        return xml_element("negation", "", value.toString());
    }
    
    /**
     * Returns a {@code <polarity>} XML element representing the of the negation modifier for the
     * event. If that value is {@code null}, returns the empty string.
     * 
     * @see Modifier#POLARITY
     */
    private String xml_polarity() {
        KQMLObject value = getPolarity();
        if (value == null)  return "";
        return xml_element("polarity", "", value.toString());
    }
    
    /**
     * Returns a {@code <force>} XML element representing the value of the force modifier for the
     * event. If that value is {@code null}, returns the empty string.
     * 
     * @see Modifier#FORCE
     */
    private String xml_force() {
        KQMLObject value = getForce();
        if (value == null)  return "";
        return xml_element("force", "", value.toString());
    }

    /**
     * Returns a {@code <modality>} XML element representing the modality associated with the
     * event. If that value is {@code null}, returns the empty string.
     * 
     * @see Modifier#MODALITY
     */
    private String xml_modality() {
        KQMLObject value = getModality();
        if (value == null) return "";
        // value is an ONT type (should not remove package!)
	if (value instanceof KQMLList) {
	    return xml_element("modality", "", ontType((KQMLList) value));
	} else if (value.toString().toUpperCase().startsWith("ONT::")) {
	    // new development; shouldn't be this way!
	    return xml_element("modality", "", value.toString());	    
	} else {
	    // it may be just ONT::DO, but we'll complain
	    Debug.error("unexpected modality value: " + value);
	    return "";
	}
    }

    /**
     * Returns a {@code <epistemic-modality>} XML element representing the epistemic modality associated with the
     * event. If that value is {@code null}, returns the empty string.
     * 
     * @see Modifier#EPI
     */
    private String xml_epimodality() {
        KQMLObject value = getEpiModality();
        if (value == null)  return "";
        return xml_element("epistemic-modality", "", value.toString());
    }

    /**
     * Time feature
     * @return
     */
    private String xml_time() {
        KQMLObject time = features.get(Feature.TIME);
        if (time == null)
            return "";
        
        List<String> attrs = new ArrayList<String>();
        KQMLObject modType = features.get(Feature.TIMEMOD);
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
            Debug.error("unexpected " + Feature.TIME + " value: " + time);
            return xml_element("time", "", removePackage(time.toString(), false));
        }
    }

    /**
     * Extent feature
     * @return
     */
    private String xml_extent() {
        KQMLObject extent = features.get(Feature.EXTENT);
        if (extent == null)
            return "";
        
        // modifier?
        if (isOntVar(extent.toString())) {
            String var = extent.toString();
            List<String> attrs = new ArrayList<String>();
            KQMLList modifierTerm = findTermByVar(var, context);
            if (modifierTerm != null) {
                String modOntType = pullOntType(modifierTerm);
                KQMLObject figure = modifierTerm.getKeywordArg(":FIGURE");
                String figureVar = figure.toString();
                // TODO: assert that figureVar.equalsIgnoreCase(id)
                KQMLObject ground = modifierTerm.getKeywordArg(":GROUND");
                String groundVar = ground.toString();
                KQMLList groundTerm = findTermByVar(groundVar, context);
                String groundOntType = pullOntType(groundTerm);
                // we handle cases individually
                if (modOntType.equalsIgnoreCase("ONT::EVENT-DURATION-MODIFIER")
                        && groundOntType.equalsIgnoreCase("ONT::TIME-LOC")) 
                {
                    attrs.add(xml_attribute("mod", modOntType));
                    if (ekbFindExtraction(groundVar) != null) {
                        return xml_elementWithID("time", groundVar, attrs);
                    } else { // we need to define the item here
                        return xml_lfTerm("time", groundVar, attrs);
                    }
                }
            }
        } 
        // else // should not happen!
        Debug.error("unexpected " + Feature.EXTENT + " value: " + extent);
        return xml_element("extent", "", removePackage(extent.toString(), false));
    }
        
    /**
     * Returns a {@code <location>} XML element representing location information attached to the event, 
     * or the empty string if no such information exists. 
     */
    private String xml_location() {
        KQMLObject loc = features.get(Feature.LOC);
        if (loc == null) {
            loc = features.get(Feature.LOCATION);
            if (loc == null)
                return "";
        }
        
        List<String> attrs = new ArrayList<String>();
        KQMLObject modType = features.get(Feature.LOCMOD);
        if (modType != null) {
            String mod = ontType(modType);
            if (!mod.isEmpty()) {
                attrs.add(xml_attribute("mod", mod));
            }
        }
        
        if (isOntVar(loc.toString())) {
            String var = loc.toString();
            if (ekbFindExtraction(var) != null) {
                return xml_elementWithID("location", var, attrs);
            } else { // we need to define the item here
                return xml_lfTerm("location", var, attrs);
            }
        } else {
            Debug.warn("unexpected location value: " + loc);
            return "";
        }
    }

    /**
     * Assoc-with
     * 
     * @return
     */
    private String xml_assocs() {
        ArrayList<KQMLObject> assocs = polyMods.get(PolyModifier.ASSOC);
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
                Debug.warn("unexpected " + PolyModifier.ASSOC + " value: " + valObj);
                result += xml_element("assoc-with", null, removePackage(valObj.toString(), false));
            }
        }
        return result;
    }

    /**
     * Returns a {@code <mods>} XML element representing the term modifiers, or
     * the empty string if no such information exists.
     */
    private String xml_mods() {
        String mods = "";
        mods += xml_mods(PolyModifier.DEGREE, "degree");
        mods += xml_mods(PolyModifier.FREQUENCY, "frequency");
        mods += xml_mods(PolyModifier.MODA, "mod");
        mods += xml_mods(PolyModifier.MODN, "mod");
        return mods.equals("") ? "" : "<mods>" + mods + "</mods>";
    }

    /**
     * Creates a list of XML elements of the form {@code <X><type>T</type><value>V</value></X>}, where {@code X} is the
     * value of {@code modType}. An element of this form is created for each instance of a poly-modifier of type
     * {@code mod}.
     * 
     * @param mod
     * @param modType
     * @return
     */
    private String xml_mods(PolyModifier mod, String modType) {
        ArrayList<KQMLObject> mods = polyMods.get(mod);
        if ((mods == null) || mods.isEmpty()) return "";
        
        String result = "";
        for (KQMLObject modValue : mods) {
            if (modValue instanceof KQMLList) {
                KQMLList modValueList = (KQMLList) modValue;
                List<String> conts = new ArrayList<String>();
                conts.add(xml_element("type", "", ontType(modValueList)));
                conts.add(xml_element("value", "", removePackage(ontWord(modValueList), false)));
                result += xml_element(modType, null, conts);
            } else if (isOntVar(modValue.toString())) { // TODO remove (obsolete)
                KQMLList modTerm = findTermByVar(modValue.toString(), context);
                int start = getKeywordArgInt(":START", modTerm);
                int end = getKeywordArgInt(":END", modTerm);
                List<String> attrs = new ArrayList<String>();
                attrs.add(xml_attribute("start", String.valueOf(getOffset(start))));
                attrs.add(xml_attribute("end", String.valueOf(getOffset(end))));
                List<String> conts = new ArrayList<String>();
                conts.add(xml_element("type", "", pullOntType(modTerm)));
                conts.add(xml_element("text", "", xml_escape(removeTags(getTextSpan(start, end)))));
                result += xml_element(modType, attrs, conts);
            } else { // should not happen!
                Debug.error("unexpected " + mod + " value: " + modValue);
                result += xml_element(modType, "", removePackage(modValue.toString(), false));
            }
        }
        return result;
    }


    //// LISP FORMATTING
    
    
    /**
     * Expands non-argument attribute values in shortValue and returns the
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
                if (Role.isRole(itemAsString) // arg
                        || Feature.isFeature(itemAsString) // feature
                        || Modifier.isModifier(itemAsString) // mod
                        || PolyModifier.isModifier(itemAsString)) // poly-mod
                { 
                    expand = false;
                } else {
                    Debug.warn("Unrecognized CC attribute: " + itemAsString);
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

    
    //// OTHER
    
    protected HashSet<String> findAllVars () {
        HashSet<String> vars = new HashSet<String>();
        vars.add(pullTermVar(value));
        for (Role role : roles.keySet()) {
            vars.add(roles.get(role).toString());
        }
        return vars;
    }


}
