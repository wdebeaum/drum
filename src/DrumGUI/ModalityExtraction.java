/*
 * EventExtraction.java
 *
 * $Id: ModalityExtraction.java,v 1.28 2019/12/11 04:25:43 lgalescu Exp $
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>, 8 Jan 2015
 */

package TRIPS.DrumGUI;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLToken;

/**
 * Class for representing and handling MODALITY extractions.
 * 
 * @author lgalescu
 *
 */
public class ModalityExtraction extends Extraction {

    /** Roles (event arguments) */
    private enum Role {
        // Roles:
        // AGENT -- obsolete?
        AGENT(":AGENT"),
        // EXPERIENCER
        EXPERIENCER(":EXPERIENCER"),
        // EVENT
        EVENT(":EVENT"),
        // EPI
        EPI(":EPI"),
        // NEUTRAL
        NEUTRAL(":NEUTRAL"),
        // NEUTRAL1
        NEUTRAL1(":NEUTRAL1"),
        // FORMAL
        FORMAL(":FORMAL"),
        // PURPOSE
        PURPOSE(":PURPOSE");
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

    /** modifiers */
    private enum Modifier {
        // :NEGATION true|false --> negation
        NEGATION(":NEGATION"),
        // :POLARITY negative|positive --> polarity
        POLARITY(":POLARITY");
        private String modName;

        private Modifier(String name) {
            modName = name;
        }

        public String toString() {
            return modName;
        }

        public static boolean isModifier(String item) {
            Modifier m = fromString(item);
            return (m != null);
        }

        public static Modifier fromString(String item) {
            for (Modifier m : Modifier.values()) {
                if (m.modName.equalsIgnoreCase(item))
                    return m;
            }
            return null;
        }

        public boolean hasReferent() {
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
        MODN(":MODN");
        private String modName;

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
    /* mods list */
    private LinkedHashMap<Modifier, KQMLObject> mods;
    /* poly-mods list */
    private LinkedHashMap<PolyModifier, ArrayList<KQMLObject>> polyMods;


    public ModalityExtraction(DrumKB ekb, KQMLList value, KQMLList context, int uttnum) {
        super(ekb, value, context, uttnum);

        // get roles, features, modifiers
        pullRoles();
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
                } else {
                    Debug.warn("Unexpected role value: " + role + " " + value);
                }
            }
        }
    }

    /**
     * Pulls modifiers.
     */
    private void pullModifiers() {
        mods = new LinkedHashMap<Modifier, KQMLObject>();
        for (Modifier mod : Modifier.values()) {
            KQMLObject aValue = shortValue.getKeywordArg(mod.toString());
            if ((aValue != null) && !aValue.toString().equals("-")) {
                mods.put(mod, aValue);
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
                iterator = shortValue.listIterator();
                ArrayList<KQMLObject> modValues = new ArrayList<KQMLObject>();
                while (iterator.hasNext()) {
                    String key = iterator.next().toString();
                    if (key.equalsIgnoreCase(mod.toString())) {
                        // look ahead
                        int nextIndex = iterator.nextIndex();
                        KQMLObject aValue = shortValue.get(nextIndex);
                        // Debug.debug("Found poly-modifier: " + key + " " + value);
                        if ((aValue instanceof KQMLToken) && ((KQMLToken) aValue).equalsIgnoreCase("-")) {
                            Debug.warn("Removed " + key + " " + aValue);
                            iterator.remove();
                            iterator.next();
                            iterator.remove();
                            continue;
                        }
                        /* LG20181020 we don't do this anymore
                        if (aValue instanceof KQMLList) { // fix
                            KQMLList valueList = (KQMLList) aValue;
                            if (((KQMLToken) valueList.get(0)).equalsIgnoreCase(":*")) {
                                Debug.warn("Removed [:*] from " + valueList);
                                valueList.remove(0);
                            }
                        }
                        */
                        modValues.add(aValue);
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

    //// XML FORMATTING
    
    /** 
     * Returns an XML representation of the event. Typically, this will be an {@code <event>} XML element.
     * However, if the event is not primitive, i.e., when at least one role is filled by multiple entities/terms, the 
     * result is a sequence of {@code <event>} XML elements. 
     */
    public String toXML() {
        String var = pullTermVar(value);
        String id = removePackage(var);
        String parID = getParagraphID();
        String text = removeTags(getTextSpan(start, end));

        String ruleID = getKeywordArgString(":RULE", value);    
        if (ruleID == null) {
            Debug.warn("No :RULE? value=" + value);
        }
        
        return "<" + exType + " " +
                "id=\"" + id + "\" " +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\" " +
                "paragraph=\"" + parID + "\" " +
                "uttnum=\"" + uttnum + "\" " +
                "lisp=\"" + getLispForm() + "\" " +
                "rule=\"" + ruleID + "\">"
                + "<type>" + ontType + "</type>"
                + xml_negation()
                + xml_polarity()
                + xml_args()
                + "<text>" + xml_escape(text) + "</text>" +
                "</" + exType + ">";
    }

    /**
     * Returns a list of XML elements representing information about all event
     * arguments.
     * 
     * @see #xml_arg(int, Role, String)
     */
    private String xml_args() {
        if (roles.isEmpty()) {
            return "";
        }
        int index = 1;
        if ((roles.size() == 1) && (roles.get(Role.AGENT) == null)) {
            index++;
        }
        String result = "";
        for (Role role: roles.keySet()) {
            result += xml_arg(index++, role, roles.get(role).toString());
        }
        return result;
    }

    /**
     * Returns an XML element representing information about an` argument. The XML tag is either 
     * {@code arg1} or {@code arg2}, depending on the argument type.
     * 
     * @see #xml_args()
     */
    private String xml_arg(int roleIndex, Role role, String var) {
        Debug.debug("arg(" + role + "," + var + ")");
        if (var == null) {
            // shouldn't happen!
            Debug.warn("unexpected role: " + role);
            return "";
        }
        String id = removePackage(var);
        // we obtain the arg from the EKB, if we can; otherwise, we look for the term in the context
        Extraction ekbTerm = ekbFindExtraction(var);
        KQMLList term = (ekbTerm != null) ? ekbTerm.getValue() : findTermByVar(var, context);
	if (term == null) {
            // shouldn't happen, but occasionally terms are missing from the LF!
            Debug.warn("LF term not found: " + var);
            return "";
	}
        KQMLObject termType = pullFullOntType(term);
        int start = getKeywordArgInt(":START", term);
        int end = getKeywordArgInt(":END", term);

        List<String> attrs = new ArrayList<String>();
        attrs.add(xml_attribute("id", removePackage(var)));
        attrs.add(xml_attribute("role", role.toString()));
        attrs.add(xml_attribute("start", String.valueOf(getOffset(start))));
        attrs.add(xml_attribute("end", String.valueOf(getOffset(end))));

        List<String> conts = new ArrayList<String>();
        conts.add(xml_element("type", "", ontType(termType)));
        conts.add(xml_element("text",
                xml_attribute("normalization", xml_escape(normalizeOnt(ontWord(termType)))),
                xml_escape(removeTags(getTextSpan(start, end)))));

        return xml_element("arg" + roleIndex, attrs, conts);
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
                String text = removeTags(getTextSpan(start, end));
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
            } else if (itemAsString.startsWith(":")) { // attribute
                expandedValue.add(item);
                if (Role.isRole(itemAsString)
                        || (Modifier.isModifier(itemAsString) && Modifier.fromString(itemAsString).hasReferent()))
                {
                    expand = false;
                } else { // expandable modifier
                    if (!Modifier.isModifier(itemAsString) && !PolyModifier.isModifier(itemAsString)) {
                        Debug.warn("Unrecognized event attribute: " + itemAsString);
                    }
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
