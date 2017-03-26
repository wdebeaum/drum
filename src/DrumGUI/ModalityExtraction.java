/*
 * EventExtraction.java
 *
 * $Id: ModalityExtraction.java,v 1.24 2017/03/25 20:55:51 lgalescu Exp $
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>, 8 Jan 2015
 */

package TRIPS.DrumGUI;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
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

    /** Event modifiers */
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
                iterator = shortValue.listIterator();
                ArrayList<KQMLObject> modValues = new ArrayList<KQMLObject>();
                while (iterator.hasNext()) {
                    String key = iterator.next().toString();
                    if (key.equalsIgnoreCase(mod.toString())) {
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
                        if (value instanceof KQMLList) { // fix
                            KQMLList valueList = (KQMLList) value;
                            if (((KQMLToken) valueList.get(0)).equalsIgnoreCase(":*")) {
                                Debug.warn("Removed [:*] from " + valueList);
                                valueList.remove(0);
                            }
                        }
                        modValues.add(value);
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
        Debug.debug("toXML: ready");

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
                + createNegationXML()
                + createPolarityXML()
                + createArgsXML()
                + "<text>" + escapeXML(text) + "</text>" +
                "</" + exType + ">";
    }

    /**
     * Returns a list of XML elements representing information about all event
     * arguments.
     * 
     * @see #createArgXML(int, Role, String)
     */
    private String createArgsXML() {
        if (roles.isEmpty()) {
            return "";
        }
        int index = 1;
        if ((roles.size() == 1) && (roles.get(Role.AGENT) == null)) {
            index++;
        }
        String result = "";
        for (Role role: roles.keySet()) {
            result += createArgXML(index++, role, roles.get(role).toString());
        }
        return result;
    }

    /**
     * Returns an XML element representing information about an event argument. The XML tag is either 
     * {@code arg1} or {@code arg2}, depending on the argument type.
     * 
     * @see #createArgsXML()
     */
    private String createArgXML(int roleIndex, Role role, String var) {
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
        KQMLList ontInfo = pullCompleteOntInfo(term);
        String ontText = (ontInfo.size() > 1) ? normalizeOnt(ontInfo.get(1).toString()) : "";
        int start = getKeywordArgInt(":START", term);
        int end = getKeywordArgInt(":END", term);
        String text = removeTags(getTextSpan(start, end));
        String tag = "arg" + roleIndex;

        Debug.debug("createArgXML: ready");

        return "<" + tag + " " +
                "id=\"" + id + "\" " +
                "role=\"" + role + "\" " +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\"" + ">"
                + "<type>" + ontInfo.get(0) + "</type>"
                + "<text normalization=\"" + escapeXML(ontText) + "\">" + escapeXML(text) + "</text>" +
                "</" + tag + ">";
    }
    
    /**
     * Returns a {@code <negation>} XML element representing the value of the negation modifier for the
     * event. If that value is {@code null}, returns the empty string.
     * 
     * @see Modifier#NEGATION
     */
    private String createNegationXML() {
        KQMLObject negation = getNegation();
        if (negation != null) {
            return "<negation>" + negation.toString() + "</negation>";
        }
        return "";
    }
    
    /**
     * Returns a {@code <polarity>} XML element representing the of the negation modifier for the
     * event. If that value is {@code null}, returns the empty string.
     * 
     * @see Modifier#POLARITY
     */
    private String createPolarityXML() {
        KQMLObject polarity = getPolarity();
        if (polarity != null) {
            return "<polarity>" + polarity.toString() + "</polarity>";
        }
        return "";
    }
    
    /**
     * Returns a {@code <mods>} XML element representing the term modifiers, or
     * the empty string if no such information exists.
     */
    private String createModsXML() {
        String mods = "";
        mods += createModsXML(PolyModifier.DEGREE, "degree");
        mods += createModsXML(PolyModifier.FREQUENCY, "frequency");
        mods += createModsXML(PolyModifier.MODA, "mod");
        mods += createModsXML(PolyModifier.MODN, "mod");

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
    private String createModsXML(PolyModifier mod, String modType) {
        ArrayList<KQMLObject> mods = polyMods.get(mod);
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
