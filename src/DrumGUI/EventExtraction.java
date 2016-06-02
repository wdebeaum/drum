/*
 * EventExtraction.java
 *
 * $Id: EventExtraction.java,v 1.44 2016/06/02 16:10:40 lgalescu Exp $
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
 * Class is for representing and handling EVENT extractions.
 * 
 * @author lgalescu
 *
 */
public class EventExtraction extends Extraction {

    /** Roles (event arguments) */
    protected enum Role {
        // Roles:
        // AGENT, AFFECTED
        // AFFECTED, AFFECTED1
        // AGENT, AFFECTED-CREATE
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

    /** Event features (domain-specific) */
    protected enum Feature {
        // :CELL-LINE context-term-id --> cell line
        CELL_LINE(":CELL-LINE"),
        // :LOCMOD ontType :LOC context-term-id --> cell component/location
        LOCMOD(":LOCMOD"),
        CELL_LOC(":LOC"),
        // [:SITEMOD ontType] :SITE context-term-id --> eg, at/SITEMOD Y200/SITE
        SITEMOD(":SITEMOD"),
        SITE(":SITE"),
        // [:FROM context-term-id] :TO context-term-id: cell components/locations
        FROM(":FROM"),
        TO(":TO");
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
    protected enum Modifier {
        // :NEGATION true|false (E is the case vs. !E is the case)
        NEGATION(":NEGATION"),
        // :POLARITY negative|positive
        POLARITY(":POLARITY"),
        // :FORCE token
        FORCE(":FORCE"),
        // :MODALITY VAR
        MODALITY(":MODALITY"),
        // :EPI VAR
        EPI(":EPI");
        // :METHOD
        // METHOD(":METHOD");

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
            boolean result = (this == MODALITY) || (this == EPI);
            return result;
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
        public String toString() { return modName; }

        public static boolean isModifier(String item) {
            for (PolyModifier m : PolyModifier.values()) {
                if (m.modName.equalsIgnoreCase(item))
                    return true;
            }
            return false;
        }
    };

    /**
     * Roles list. It contains a mapping from role names to term variables. The
     * roles in the keySet are maintained in their natural order.
     */
    private LinkedHashMap<Role, KQMLObject> roles;
    /* features list */
    private LinkedHashMap<Feature, KQMLObject> features;
    /* mods list */
    private LinkedHashMap<Modifier, KQMLObject> mods;
    /* poly-mods list */
    private LinkedHashMap<PolyModifier, ArrayList<KQMLObject>> polyMods;
    /** sub-events: inferred by expanding sequence arguments */
    protected ArrayList<EventExtraction> subEvents;


    public EventExtraction(DrumKB ekb, KQMLList value, KQMLList context, int uttnum) {
        super(ekb, value, context, uttnum);

        // get roles, features, modifiers
        pullRoles();
        pullFeatures();
        pullModifiers();
        pullPolyModifiers();
        
        // sub-events -- 20160316 LG took out; TODO: remove completely
        // subEvents = makeSubEventsMaybe();

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

    /**
     * @return the method
     */
    // public KQMLObject getMethod() {
    // return mods.get(Modifier.METHOD);
    // }

    /**
     * Gets the list of sub-events.
     */
    public ArrayList<EventExtraction> getSubEvents() {
        return subEvents;
    }

    //// PULLERS
    
    /**
     * Pulls roles.
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
     * Pulls features.
     */
    private void pullFeatures() {
        features = new LinkedHashMap<Feature, KQMLObject>();
        for (Feature feat : Feature.values()) {
            KQMLObject value = shortValue.getKeywordArg(feat.toString());
            // note: we ignore dash values, which we interpret as "undefined"
            if ((value != null) && !value.toString().equals("-")) {
                if (value instanceof KQMLList) {
                    KQMLList valueList = (KQMLList) value;
                    if (((KQMLToken) valueList.get(0)).equalsIgnoreCase(":*")) {
                        Debug.warn("Removed [:*] from " + valueList);
                        valueList.remove(0);
                    }
                }
                features.put(feat, value);
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
                if (value instanceof KQMLList) {
                    KQMLList valueList = (KQMLList) value;
                    if (((KQMLToken) valueList.get(0)).equalsIgnoreCase(":*")) {
                        Debug.warn("Removed [:*] from " + valueList);
                        valueList.remove(0);
                    }
                }
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
        } catch (Exception e) { e.printStackTrace();}
    }


    //// EVENT-EVENT OPERATIONS 
    
    /**
     * Compares the specified extraction with this one for equality.
     * Two {@link Extraction} objects are equal if their {@link #exType}, 
     * {@link #id} and {@link #ontType} members are equal. For events,
     * they must also have the same arguments.
     */
    public boolean equals(EventExtraction e) {
        if (!super.equals(e))
            return false;
        if (! getNegation().equals(e.getNegation()))
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
        EventExtraction e = (EventExtraction) other;
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
        for (KQMLObject o: eCtxt) {
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

        // re-generate sub-events -- 20160316 LG took out; TODO: remove completely
        // subEvents = makeSubEventsMaybe();

        // update expandedValue
        makeExpandedValue();

        // done
        Debug.debug("Extraction after update: " + shortValue);
    }
    
    //// SUB-EVENTS
    
    /**
     * Makes list of sub-events by expanding sequence arguments.
     * 
     * @return list of sub-events (may be empty)
     */
    private ArrayList<EventExtraction> makeSubEventsMaybe() {
        // collect roles that need expansion; roleExpansions: { role => [var, ...], ... }
        LinkedHashMap<Role, ArrayList<String>> roleExpansions =
            new LinkedHashMap<Role, ArrayList<String>>();
        for (Role role: roles.keySet()) {
            String rVar = roles.get(role).toString();
            // TODO: eventually everything ought to be looked up in the EKB; this is just a start
            Extraction termEx = ekbFindExtraction(rVar);
            KQMLList seq;
            if (termEx != null) {
                if (!(termEx instanceof TermExtraction)) {
                    continue;
                }
                KQMLObject seqAttr = ((TermExtraction) termEx).getAttribute(TermExtraction.Attribute.SEQ);
                if (seqAttr == null) {
                    continue;
                }
                seq = (KQMLList) seqAttr; // TODO: check that we have a list?
            } else {
                KQMLList term = findTermByVar(rVar, context);
                KQMLObject seqObj = term.getKeywordArg(":SEQUENCE");
                if (seqObj == null) {
                    continue;
                }
                seq = (KQMLList) seqObj; // TODO: check that we have a list?
            }
            ArrayList<String> roleVars = new ArrayList<String>();
            for (KQMLObject member : seq) {
                Debug.debug("Role expansion: " + role + " " + rVar + " => " + member);
                roleVars.add(member.toString());
            }
            roleExpansions.put(role, roleVars);
        }
        // stop if we don't have any expansions
        if (roleExpansions.isEmpty()) {
            Debug.debug("No sequence expansion for event " + id);
            return null;
        }
        // check for cases where we don't want to expand (cumulative arguments)
        // for now: BIND, BINDEXPT, INTERACT and BREAK, with a single sequence role,
        // either :AGENT or :AFFECTED
        if ((roles.keySet().size() == 1)
                && (roles.containsKey(Role.AGENT) || roles.containsKey(Role.AFFECTED))
                && ((ontType.equalsIgnoreCase("ONT::BIND")
                        || ontType.equalsIgnoreCase("ONT::BINDEXPT")
                        || ontType.equalsIgnoreCase("ONT::INTERACT")
                        || ontType.equalsIgnoreCase("ONT::BREAK")
                )))
        {
            Debug.debug("No sequence expansion for event " + id + " (" + ontType + " " + roles.keySet() + ")");
            return null;
        }

        // get on with it: we start with the current {@code value} and iteratively replace the value of each role in
        // {@code roleExpansions} with one of the variables in the aggregate.
        return inferSubeventsByDistributiveExpansion(roleExpansions);
    }

    /**
     * Infers new events by distributive expansion of aggregate role values.
     * <p>
     * NOTE: This function should call {@code inferSubeventsByDistributiveExpansion(Role, ArrayList)} for each role with
     * aggregate values. However, currently it does all expansions on its own, in one fell swoop.
     * 
     * @see #inferSubeventsByDistributiveExpansion(Role, ArrayList)
     * @param roleExpansions
     *            list of role expansions
     * @return list of inferred events (an empty list if {@code roleExpansions} is empty or {@code null})
     */
    // TODO: i think i should add an explicit relation bw e1 and e1.1, e1.2, etc.
    // eg: (REL e1 :LOGICALOP-SEQUENCE (e1.1 e1.2 ...))
    // this wouldn't work now for 2 reasons:
    // 1. we don't have a REL extraction type
    // 2. i am now expansing all roles simultaneously, which makes it impossible to define the type of the raised SEQ
    // operator. instead, i should do it role by role, and infer all REL relations in turn.
    private ArrayList<EventExtraction> inferSubeventsByDistributiveExpansion(
            LinkedHashMap<Role, ArrayList<String>> roleExpansions) {
        ArrayList<EventExtraction> result = new ArrayList<EventExtraction>();
        if ((roleExpansions == null) || (roleExpansions.isEmpty())) {
            return result;
        }
        Debug.debug("Invoked inferSubeventsByDistributiveExpansion1 for event " + id);
        ArrayList<KQMLList> valueExpansions = new ArrayList<KQMLList>();
        valueExpansions.add(value);
        int n = 1;
        for (Role role : roleExpansions.keySet()) {
            for (int i = 0; i < n; i++) {
                KQMLList val = valueExpansions.remove(0);
                int pos = val.indexOfIgnoreCase(role.toString());
                int k = 0;
                for (String var : roleExpansions.get(role)) {
                    // clone old value list
                    KQMLList newVal = new KQMLList();
                    for (KQMLObject o : val) {
                        newVal.add(o);
                    }
                    // replace var
                    newVal.set(1, new KQMLToken(expandVar(pullTermVar(val), ++k)));
                    // and replace role var
                    newVal.set(pos + 1, new KQMLToken(var));
                    valueExpansions.add(newVal);
                    Debug.debug("newVal: " + newVal);
                }
            }
            n *= roleExpansions.get(role).size();
        }
        Debug.debug("got " + n + " expansions");

        for (KQMLList val : valueExpansions) {
            EventExtraction sub = new EventExtraction(ekb, val, context, uttnum);
            if (sub == null) {
                Debug.debug("FAILED to create sub-event!");
                continue;
            }

            result.add(sub);
        }

        return result;

    }

    /**
     * Infers new events by distributive expansion of an aggregate role.
     * 
     * <p>
     * For example, from event <blockquote>
     * 
     * <pre>
     * (EVENT e1 t :role av1 &rest)
     * </pre>
     * 
     * </blockquote> and term <blockquote>
     * 
     * <pre>
     * (TERM av1 type1 :LOGICALOP-SEQUENCE (x1 x2))
     * </pre>
     * 
     * </blockquote> the following events will be inferred: <blockquote>
     * 
     * <pre>
     * (EVENT e1.1 t :role x1 &rest)
     * (EVENT e1.2 t :role x2 &rest)
     * </pre>
     * 
     * </blockquote>
     * 
     * @param role
     *            role expanded
     * @param roleValues
     *            values by which {@code role} is expanded
     * @return list of inferred events (an empty list if {@code roleValues} is empty or {@code null})
     */
    // TODO: i think i should add an explicit relation bw e1 and e1.1, e1.2, etc.
    // eg: (REL e1 :LOGICALOP-SEQUENCE (e1.1 e1.2 ...))
    // this wouldn't work now for 2 reasons:
    // 1. we don't have a REL extraction type
    // 2. i am now expansing all roles simultaneously, which makes it impossible to define the type of the raised SEQ
    // operator. instead, i should do it role by role, and infer all REL relations in turn.
    private ArrayList<EventExtraction> inferSubeventsByDistributiveExpansion(Role role, ArrayList<String> roleValues) {
        ArrayList<EventExtraction> result = new ArrayList<EventExtraction>();
        if ((roleValues == null) || (roleValues.isEmpty())) {
            return result;
        }
        ArrayList<KQMLList> valueExpansions = new ArrayList<KQMLList>();
        int n = roleValues.size();
        int pos = value.indexOfIgnoreCase(role.toString()); // role key position
        int k = 0;
        for (String var : roleValues) {
            // clone old value list
            KQMLList newVal = new KQMLList();
            for (KQMLObject o : value) {
                newVal.add(o);
            }
            // replace id w/ id.k
            newVal.set(1, new KQMLToken(expandVar(id, ++k)));
            // and replace role var
            newVal.set(pos + 1, new KQMLToken(var));
            Debug.debug("newVal: " + newVal);
            // record new event value
            valueExpansions.add(newVal);
        }
        Debug.debug("got " + n + " expansions");

        // create new events
        for (KQMLList val : valueExpansions) {
            EventExtraction sub = new EventExtraction(ekb, val, context, uttnum);
            if (sub == null) {
                Debug.debug("FAILED to create sub-event!");
                continue;
            }

            result.add(sub);
        }

        return result;

    }

    //// XML FORMATTING
    
    /** 
     * Returns an XML representation of the event. Typically, this will be an {@code <event>} XML element.
     * However, if the event is not primitive, i.e., when at least one role is filled by multiple entities/terms, the 
     * result is a sequence of {@code <event>} XML elements. 
     */
    public String toXML() {
        // if we have sub-events, concatenate them and return the result
        if ((subEvents != null) && (!subEvents.isEmpty())) {
            String result = "";
            for (EventExtraction sub: subEvents) {
                if (! result.equals("")) { result += "\n"; }
                result += sub.toXML();
            }
            return result;
        }

        // special cases
        if (ontType.equals("ONT::S-CONJOINED") || ontType.equals("ONT::VP-CONJOINED")) {
            return createConjoinedEventXML();
        }

        // otherwise, we create a new EVENT element for this
        String var = pullTermVar(value);
        String id = removePackage(var);
        String parID = getParagraphID();
        String text = removeTags(getTextSpan(start, end));
        Debug.debug("toXML: ready");

        String ruleID = getKeywordArgString(":RULE", value);    
        if (ruleID == null) {
            Debug.warn("No :RULE? value=" + value);
        }
        
        return "<" + exType + " " + "id=\"" + id + "\" " +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\" " +
                "paragraph=\"" + parID + "\" " +
                "uttnum=\"" + uttnum + "\" " +
                "lisp=\"" + getLispForm() + "\" " +
                "rule=\"" + ruleID + "\">"
                + "<type>" + ontType + "</type>"
                // + "<value>" + value + "</value>"
                + createNegationXML()
                + createPolarityXML()
                + createForceXML()
                + createModalityXML()
                + createEpiModalityXML()
                // + createMethodXML()
                + createModsXML()
                + createPredicateXML()
                + createArgsXML()
                + createSiteXML()
                + createLocationXML()
                + createTransportFromXML()
                + createTransportToXML()
                + createCelllineXML()
                + "<text>" + escapeXML(text) + "</text>" +
                "</" + exType + ">";
    }

    private String createConjoinedEventXML() {
        KQMLObject sequence = value.getKeywordArg(":SEQUENCE");
        if (sequence == null) {
            return "";
        }
        KQMLObject opObj = value.getKeywordArg(":OPERATOR");
        String operator = (opObj == null) ? "NOOP" : opObj.stringValue();
        String aggregate = createAggregateXML((KQMLList) sequence, operator);

        String var = pullTermVar(value);
        String id = removePackage(var);
        String parID = getParagraphID();
        String text = removeTags(getTextSpan(start, end));
        Debug.debug("toXML: ready");

        String ruleID = getKeywordArgString(":RULE", value);
        if (ruleID == null) {
            Debug.warn("No :RULE? value=" + value);
        }

        return "<" + exType + " " + "id=\"" + id + "\" " +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\" " +
                "paragraph=\"" + parID + "\" " +
                "uttnum=\"" + uttnum + "\" " +
                "lisp=\"" + getLispForm() + "\" " +
                "rule=\"" + ruleID + "\">"
                + "<type>" + ontType + "</type>"
                + createNegationXML()
                + createPolarityXML()
                + createModalityXML()
                + createEpiModalityXML()
                + aggregate
                + "<text>" + escapeXML(text) + "</text>" +
                "</" + exType + ">";
    }

    /**
     * Returns an {@code <aggregate>} XML element representing an aggregation, via a logical operation, of terms.
     */
    private String createAggregateXML(KQMLList varSeq, String operator) {
        String result = "";
        for (KQMLObject var : varSeq) {
            result += "<member id=\"" + removePackage(var.toString(), false) + "\" />";
        }
        return "<aggregate operator=\"" + removePackage(operator, false) + "\">" + result + "</aggregate>";
    }

    /**
     * Returns a {@code <predicate>} XML element representing information about
     * the event's predicate.
     */
    private String createPredicateXML() {
        String var = pullTermVar(value);
        String id = removePackage(var);
        KQMLList contextTerm = findTermByVar(getBaseVar(var), context);
        String textNorm = "";
        String predType;
        if (contextTerm != null) {
            KQMLList ontInfo = pullCompleteOntInfo(contextTerm);
            if (ontInfo.size() > 1) {
                // complete context term
                String ontText = removePackage(ontInfo.get(1).toString());
                predType = ontInfo.get(0).toString();
                textNorm = " normalization=\"" + escapeXML(ontText) + "\"";
            } else {
                // incomplete context term
                predType = ontType;
            }
        } else {
            // when events are inferred rather than read off the LF, we have no context term
            predType = ontType;
        }
        int start = getKeywordArgInt(":START", contextTerm);
        int end = getKeywordArgInt(":END", contextTerm);
        String text = removeTags(getTextSpan(start, end));
        Debug.debug("createPredicateXML: ready");

        return "<predicate " +
                "id=\"" + id + "\" " +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\"" + ">"
                + "<type>" + predType + "</type>"
                + "<text" + textNorm + ">" + escapeXML(text) + "</text>" +
                "</predicate>";
    }

    /**
     *  Returns a list of XML elements representing information about all event arguments.
     *  
     *  @see #createArgXML(int, Role, String)
     */
    private String createArgsXML() {
        if (roles.isEmpty()) {
            return "";
        }
        int index = 1;
        // if there is a single role and it's not AGENT, we put it in the 2nd arg slot
        if ((roles.size() == 1) && (roles.get(Role.AGENT) == null) && (roles.get(Role.AGENT1) == null)) {
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
        String ontText = (ontInfo.size() > 1) ? normalize(ontInfo.get(1).toString()) : "";
        int start = getKeywordArgInt(":START", term);
        int end = getKeywordArgInt(":END", term);
        String text = removeTags(getTextSpan(start, end));
        String tag = "arg" + roleIndex;

        String dbID = getDBTermIds(term.getKeywordArg(":DRUM"));

        Debug.debug("createArgXML: ready");

        return "<" + tag + " " +
                "id=\"" + id + "\" " +
                "role=\"" + role + "\" " +
                (dbID == null ? "" : ("dbid=\"" + dbID + "\" ")) +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\"" + ">"
                + "<type>" + ontInfo.get(0) + "</type>"
                + "<text normalization=\"" + escapeXML(ontText) + "\">" + escapeXML(text) + "</text>" +
                "</" + tag + ">";
    }
    
    /**
     * Returns a {@code <site>} XML element representing molecular site information attached to the event, 
     * or the empty string if no such information exists. 
     */
    private String createSiteXML() {
        KQMLObject sitevalObj = features.get(Feature.SITE);
        KQMLObject sitemodObj = features.get(Feature.SITEMOD);
        // FIXME: this is currently ignored; it may be null!

        if (sitevalObj == null) {
            return "";
        }

        String siteval = sitevalObj.toString();
        //String sitemod = sitemodObj.toString();
        String id = removePackage(siteval);
        KQMLList varTerm = findTermByVar(siteval, context);
        KQMLList ontInfo = pullCompleteOntInfo(varTerm);
        String ontText = (ontInfo.size() > 1) ? normalize(ontInfo.get(1).toString()) : "";
        int start = getKeywordArgInt(":START", varTerm);
        int end = getKeywordArgInt(":END", varTerm);
        String text = removeTags(getTextSpan(start, end));
        Debug.debug("createSiteXML: ready");

        return "<site " +
                "id=\"" + id + "\" " +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\"" + ">"
                + "<type>" + ontInfo.get(0) + "</type>"
                // + "<mod>" + sitemod + "</mod>" // TODO
                + "<text normalization=\"" + escapeXML(ontText) + "\">" + escapeXML(text) + "</text>" +
                "</site>";
    }

    /**
     * Returns a {@code <location>} XML element representing location information attached to the event, 
     * or the empty string if no such information exists. 
     */
    private String createLocationXML() {
        KQMLObject locvalObj = features.get(Feature.CELL_LOC);
        KQMLObject locmodObj = features.get(Feature.LOCMOD);

        if (locvalObj == null) {
            return "";
        }

        String locval = locvalObj.toString();
        String locmod = null;
        if (locmodObj instanceof KQMLList) {
            locmod = ((KQMLList) locmodObj).get(0).toString();
        } else if (locmodObj instanceof KQMLToken) { // TODO: remove (obsolete)
            locmod = locmodObj.toString();
        }
        String id = removePackage(locval);
        KQMLList locTerm = findTermByVar(locval, context);
        KQMLList ontInfo = pullCompleteOntInfo(locTerm);
        String ontText = (ontInfo.size() > 1) ? normalize(ontInfo.get(1).toString()) : "";
        int start = getKeywordArgInt(":START", locTerm);
        int end = getKeywordArgInt(":END", locTerm);
        String text = removeTags(getTextSpan(start, end));
        Debug.debug("createLocationXML: ready");

        return "<location " +
                "id=\"" + id + "\" " +
                "mod=\"" + removePackage(locmod, false) + "\" " +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\"" + ">"
                + "<type>" + ontInfo.get(0) + "</type>"
                + "<text normalization=\"" + escapeXML(ontText) + "\">" + escapeXML(text) + "</text>" +
                "</location>";
    }

    /**
     * Returns a {@code <cell-line>} XML element representing cell-line
     * information attached to the event, or the empty string if no such
     * information exists.
     */
    private String createCelllineXML() {
        KQMLObject clVarObj = features.get(Feature.CELL_LINE);
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

    /**
     * Returns a {@code <from>} XML element representing the source location
     * information attached to the event, or the empty string if no such
     * information exists. Only events of movement will have this feature.
     */
    private String createTransportFromXML() {
        KQMLObject locVarObj = features.get(Feature.FROM);
        if (locVarObj == null) {
            // we must not have :from info!
            return "";
        }
        String locVar = locVarObj.toString();
        String id = removePackage(locVar);
        KQMLList locTerm = findTermByVar(locVar, context);
        KQMLList ontInfo = pullCompleteOntInfo(locTerm);
        String ontText = (ontInfo.size() > 1) ? normalize(ontInfo.get(1).toString()) : "";
        int start = getKeywordArgInt(":START", locTerm);
        int end = getKeywordArgInt(":END", locTerm);
        String text = removeTags(getTextSpan(start, end));
        Debug.debug("createTransportFromXML: ready");

        return "<from-location " +
                "id=\"" + id + "\" " +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\"" + ">"
                + "<type>" + ontInfo.get(0) + "</type>"
                + "<text normalization=\"" + escapeXML(ontText) + "\">" + escapeXML(text) + "</text>" +
                "</from-location>";
    }

    /**
     * Returns a {@code <to>} XML element representing the target location
     * information attached to the event, or the empty string if no such
     * information exists. Only events of movement will have this feature.
     */
    private String createTransportToXML() {
        KQMLObject locVarObj = features.get(Feature.TO);
        if (locVarObj == null) {
            // we must not have :from info!
            return "";
        }
        String locVar = locVarObj.toString();
        String id = removePackage(locVar);
        KQMLList locTerm = findTermByVar(locVar, context);
        KQMLList ontInfo = pullCompleteOntInfo(locTerm);
        String ontText = (ontInfo.size() > 1) ? normalize(ontInfo.get(1).toString()) : "";
        int start = getKeywordArgInt(":START", locTerm);
        int end = getKeywordArgInt(":END", locTerm);
        String text = removeTags(getTextSpan(start, end));
        Debug.debug("createTransportToXML: ready");

        return "<to-location " +
                "id=\"" + id + "\" " +
                "start=\"" + getOffset(start) + "\" " +
                "end=\"" + getOffset(end) + "\"" + ">"
                + "<type>" + ontInfo.get(0) + "</type>"
                + "<text normalization=\"" + escapeXML(ontText) + "\">" + escapeXML(text) + "</text>" +
                "</to-location>";
    }

    /**
     * Returns a {@code <negation>} XML element representing the value of the negation modifier for the
     * event. If that value is {@code null}, returns the empty string.
     * 
     * @see Modifier#NEGATION
     */
    private String createNegationXML() {
        KQMLObject value = getNegation();
        if (value != null) {
            return "<negation>" + value.toString() + "</negation>";
        }
        return "";
    }
    
    /**
     * Returns a {@code <polarity>} XML element representing the value of the polarity modifier for the
     * event. If that value is {@code null}, returns the empty string.
     * 
     * @see Modifier#POLARITY
     */
    private String createPolarityXML() {
        KQMLObject value = getPolarity();
        if (value != null) {
            return "<polarity>" + value.toString() + "</polarity>";
        }
        return "";
    }

    /**
     * Returns a {@code <force>} XML element representing the value of the force modifier for the
     * event. If that value is {@code null}, returns the empty string.
     * 
     * @see Modifier#FORCE
     */
    private String createForceXML() {
        KQMLObject value = getForce();
        if (value != null) {
            return "<force>" + value.toString() + "</force>";
        }
        return "";
    }

    /**
     * Returns a {@code <modality>} XML element representing the modality associated with the
     * event. If that value is {@code null}, returns the empty string.
     * 
     * @see Modifier#MODALITY
     */
    private String createModalityXML() {
        KQMLObject value = getModality();
        if (value != null) {
            // typically this is a list; output is an ONT type (should not remove package!)
            if (value instanceof KQMLList) {
                value = ((KQMLList) value).get(1);
            }
            // other times it is just the ONT type
            return "<modality>" + value.toString() + "</modality>";
        }
        return "";
    }

    /**
     * Returns a {@code <epistemic-modality>} XML element representing the epistemic modality associated with the
     * event. If that value is {@code null}, returns the empty string.
     * 
     * @see Modifier#EPI
     */
    private String createEpiModalityXML() {
        KQMLObject value = getEpiModality();
        if (value != null) { // must be a variable
            return "<epistemic-modality id=\"" + removePackage(value.toString()) + "\" />";
        }
        return "";
    }
    
    /**
     * Returns a {@code <method>} XML element representing the method by which this event is realized. If that value is
     * {@code null}, returns the empty string.
     * 
     * @see Modifier#METHOD
     */
    // private String createMethodXML() {
    // KQMLObject method = getMethod();
    // if (method != null) {
    // return "<method>" + method.toString() + "</method>";
    // }
    // return "";
    // }

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
                        || Feature.isFeature(itemAsString)
                        || (Modifier.isModifier(itemAsString) && Modifier.fromString(itemAsString).hasReferent()))
                {
                    expand = false;
                } else { // expandable modifier
                    if (!Modifier.isModifier(itemAsString) && !PolyModifier.isModifier(itemAsString)) {
                        Debug.warn("Unrecognized event attribute: " + itemAsString);
                    }
                    expand = true;
                }
            } else if (expand && isOntVar(itemAsString)) { // variable
                KQMLList term = findTermByVar(itemAsString, context);
                if (term == null) {
                    expandedValue.add(item);
                } else {
                    KQMLList ontVal = pullCompleteOntInfo(term);
                    expandedValue.add(ontVal);
                }
                expand = false;
            } else { // default
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
