/*
 * EventExtraction.java
 *
 * $Id: EventExtraction.java,v 1.74 2019/12/11 04:25:43 lgalescu Exp $
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
        // AGENT, AFFECTED-RESULT
        // etc
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
        NEUTRAL2(":NEUTRAL2"),
        /** */
        EXPERIENCER(":EXPERIENCER"),
        /** */
        BENEFICIARY(":BENEFICIARY"),
        /** */
        FORMAL(":FORMAL"),
        /** */
        RESULT(":RES");
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
        TENSE(":TENSE"),
        // spec, for nominalizations
        SPEC(":SPEC"),
        // {DRUM} :CELL-LINE id --> cell line
        CELL_LINE(":CELL-LINE"),
        // {DRUM} :SITE id [:SITEMOD ontType] --> eg, at/SITEMOD Y200/SITE
        SITE(":SITE"),
        SITEMOD(":SITEMOD"),
        // :LOC id :LOCMOD ontType --> location
        LOC(":LOC"),
        LOCMOD(":LOCMOD"),
        LOCATION(":LOCATION"), // LF-term attribute for :LOC
        LOC1(":LOC1"),
        LOCMOD1(":LOCMOD1"),
        // {CWMS} :SOURCE and :RESULT --> location
        SOURCE(":SOURCE"),
        RESULT(":RESULT"),
        RESULT1(":RESULT1"),
        // [:FROM id] :TO id --> {DRUM} cell components/locations
        FROM(":FROM"),
        TO(":TO"),
        // :TIME :TIMEMOD
        TIME(":TIME"), // time
        TIMEMOD(":TIMEMOD"),
        // :EXTENT
        EXTENT(":EXTENT"),
        // modifiers
        MOD(":MOD"),
        MOD1(":MOD1")
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
        private Modifier(String name) { modName = name; }
        public String toString() { return modName; }

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
        MODN(":MODN"),
        // :INEVENT id: ID for event in which this event participates in some role
        INEVENT(":INEVENT"),
        // {CWMS} the following are LF attributes, or substitutes thereof
        QUAL(":QUAL"), // FIXME not sure that this is, in fact, produced
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
        
        packRules();

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
            KQMLObject aValue = shortValue.getKeywordArg(feat.toString());
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
            KQMLObject aValue = shortValue.getKeywordArg(mod.toString());
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
                String altName = PolyModifier.altNames.get(mod);
                iterator = shortValue.listIterator();
                ArrayList<KQMLObject> modValues = new ArrayList<KQMLObject>();
                while (iterator.hasNext()) {
                    String key = iterator.next().toString();
                    if (key.equalsIgnoreCase(modName) || key.equalsIgnoreCase(altName)) {
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
                        if (aValue instanceof KQMLList) {
                            KQMLList valueList = (KQMLList) aValue;
                            if (((KQMLToken) valueList.get(0)).equalsIgnoreCase(":*")) {
                                Debug.warn("Removed [:*] from " + valueList);
                                valueList.remove(0);
                            }
                        }
                        */
                        if (! modValues.contains(aValue)) {
                            modValues.add(aValue);
                        }
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
        KQMLObject eRule = e.value.getKeywordArg(":RULE");
        if (eRule != null) {
            value.add(":RULE");
            value.add(eRule.toString());
            packRules();
        }

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
     *
     * @deprecated We don't do this inference (here) any more.
     */
    @Deprecated
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
                KQMLObject seqAttr = ((TermExtraction) termEx).getAttribute(TermExtraction.Attribute.LSEQ);
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
     * NOTE: This function should call {@link #inferSubeventsByDistributiveExpansion(Role, ArrayList)} for each role
     * with aggregate values. However, currently it does all expansions on its own, in one fell swoop.
     * 
     * @see #inferSubeventsByDistributiveExpansion(Role, ArrayList)
     * @param roleExpansions
     *            list of role expansions
     * @return list of inferred events (an empty list if {@code roleExpansions} is empty or {@code null})
     * 
     * @deprecated We don't do this inference (here) any more.
     */
    // TODO: i think i should add an explicit relation bw e1 and e1.1, e1.2, etc.
    // eg: (REL e1 :LOGICALOP-SEQUENCE (e1.1 e1.2 ...))
    // this wouldn't work now for 2 reasons:
    // 1. we don't have a REL extraction type
    // 2. i am now expansing all roles simultaneously, which makes it impossible to define the type of the raised SEQ
    // operator. instead, i should do it role by role, and infer all REL relations in turn.
    @Deprecated
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
     * 
     * @deprecated We don't do this inference (here) any more.
     */
    // TODO: i think i should add an explicit relation bw e1 and e1.1, e1.2, etc.
    // eg: (REL e1 :LOGICALOP-SEQUENCE (e1.1 e1.2 ...))
    // this wouldn't work now for 2 reasons:
    // 1. we don't have a REL extraction type
    // 2. i am now expansing all roles simultaneously, which makes it impossible to define the type of the raised SEQ
    // operator. instead, i should do it role by role, and infer all REL relations in turn.
    @Deprecated
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
        Debug.debug("event: " + value);
        // if we have sub-events, concatenate them and return the result
        if ((subEvents != null) && (!subEvents.isEmpty())) {
            String result = "";
            for (EventExtraction sub: subEvents) {
                if (! result.equals("")) { result += "\n"; }
                result += sub.toXML();
            }
            return result;
        }

        // special cases (not sure that these still happen!)
        if (ontType.equals("ONT::S-CONJOINED") || ontType.equals("ONT::VP-CONJOINED")) {
            return xml_sequence();
        }
        
        // otherwise, we create a new EVENT element for this
        List<String> attrs = xml_commonAttributes();
        List<String> conts = xml_commonContents();

        // for PTMs we want a name
        if (ontType.equalsIgnoreCase("ONT::PTM")) {
            if (!dsTerms.isEmpty()) {
                // just get the first one
                KQMLList firstTerm = dsTerms.get(0);
                KQMLObject nameObj = firstTerm.getKeywordArg(":name");
                if (nameObj != null) {
                    conts.add(xml_element("name", "", nameObj.stringValue()));
                }
            }
        }
        
        conts.add(xml_assocs());
        conts.add(xml_mods());
        conts.add(xml_qualifiers());
        conts.add(xml_features());
        conts.add(xml_time());
        conts.add(xml_extent());
        conts.add(xml_location());     
        conts.add(xml_location1());     
        conts.add(xml_from());
        conts.add(xml_to());
        conts.add(xml_source());
        conts.add(xml_result());
        conts.add(xml_result1());
        if (ExtractionFactory.getProperty("extractions.mode").equals("DRUM")) {
            conts.add(xml_site());
            conts.add(xml_cellline());
            conts.add(xml_drumTerms());
        }
        return xml_element(exType, attrs, conts);
    }

    /**
     * XML contents (sub-elements) common to all EVENT extractions.
     * 
     * @return list of XML elements
     */
    protected List<String> xml_commonContents() {
        List<String> conts = super.xml_commonContents();
        if (features.get(Feature.SPEC) != null)
            conts.add(xml_element("spec", "", features.get(Feature.SPEC).toString()));
        conts.add(xml_negation());
        conts.add(xml_polarity());
        conts.add(xml_modality());
        conts.add(xml_epimodality());
        conts.add(xml_force());
        conts.add(xml_tense());
        conts.add(xml_predicate());
        conts.add(xml_args());
        return conts;
    }
    
    private String xml_sequence() {
        KQMLObject sequence = value.getKeywordArg(":SEQUENCE");
        if (sequence == null) 
            return "";
        
        List<String> attrs = xml_commonAttributes();
        List<String> conts = xml_commonContents();

        KQMLObject opObj = value.getKeywordArg(":OPERATOR");
        String operator = (opObj == null) ? "NOOP" : opObj.stringValue();
        conts.add(xml_aggregate(operator, (KQMLList) sequence));
        
        return xml_element(exType, attrs, conts);
    }

    /**
     * Returns an {@code <aggregate>} XML element representing an aggregation, via a logical operation, of terms.
     * 
     * TODO: do we need :EXCEPT here?
     */
    private String xml_aggregate(String operator, KQMLList varSeq) {
        List<String> attrs = new ArrayList<String>();
        List<String> conts = new ArrayList<String>();
        String op = removePackage(operator, false);
        for (KQMLObject var : varSeq) 
            conts.add(xml_elementWithID("member", var.toString()));
        attrs.add(xml_attribute("operator", op));
        return xml_element("aggregate", attrs, conts);
    }

    /**
     * Returns a {@code <predicate>} XML element representing information about
     * the event's predicate.
     */
    private String xml_predicate() {
        String var = pullTermVar(value);

        KQMLList contextTerm = findTermByVar(getBaseVar(var), context);
        String predType = ontType; // default
        String textAttr = "";
        // if we do have a context term, we pull info from there
        // update 12/03/2019: nowadays the original (LF) ont-type is stored in the :ONT attribute
        if (contextTerm != null) {
            KQMLObject termType = contextTerm.getKeywordArg(":ONT");
            if (termType != null) {
                String termOntWord = ontWord(termType);
                if (!termOntWord.isEmpty()) {
                    textAttr = xml_attribute("normalization", xml_escape(normalizeOnt(termOntWord)));
                }
                String termOntType = ontType(termType);
                if (!termOntType.isEmpty()) 
                    predType = termOntType;
            }
        }
        int start = getKeywordArgInt(":START", contextTerm);
        int end = getKeywordArgInt(":END", contextTerm);
        
        List<String> attrs = new ArrayList<String>();
        attrs.add(xml_attribute("start", String.valueOf(getOffset(start))));
        attrs.add(xml_attribute("end", String.valueOf(getOffset(end))));

        List<String> conts = new ArrayList<String>();
        conts.add(xml_element("type", "", predType));
        conts.add(xml_element("text", textAttr, xml_escape(removeTags(getTextSpan(start, end)))));

        return xml_element("predicate", attrs, conts);
    }

    /**
     *  Returns a list of XML elements representing information about all event arguments.
     *  
     *  @see #xml_arg(int, Role, String)
     */
    private String xml_args() {
        if (roles.isEmpty())
            return "";
        
        int index = 1;
        // if there is a single role and it's not AGENT, we put it in the 2nd arg slot
        if ((roles.size() == 1) && (roles.get(Role.AGENT) == null) && (roles.get(Role.AGENT1) == null)) {
            index++;
        }
        String result = "";
        for (Role role: roles.keySet()) {
            result += xml_arg(index++, role, roles.get(role).toString());
        }
        return result;
    }

    /**
     * Returns an XML element representing information about an event argument. The XML tag is either 
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
     * Tense feature
     * @return
     */
    private String xml_tense() {
        KQMLObject tense = features.get(Feature.TENSE);
        if (tense == null)
            return "";
        // should always be a symbol
        return xml_element("tense", "", removePackage(tense.toString(), false));
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
            } else { 
                // check if there is a RELN term associating the event with a time-loc
                KQMLList lfTerm = findTermByVar(var, context);
                if (lfTerm != null) {
                    String termType = pullOntType(lfTerm);
                    String figureVar = getKeywordArgString(":FIGURE", lfTerm);
                    // TODO: assert that figureVar.equalsIgnoreCase(id)
                    String groundVar = getKeywordArgString(":GROUND", lfTerm);
                    KQMLList groundTerm = findTermByVar(groundVar, context);
                    String groundOntType = pullOntType(groundTerm);
                    // we handle cases individually
                    if (isOntTime(groundOntType)) //&&
                       // (termType.equalsIgnoreCase("ONT::BEFORE") || termType.equalsIgnoreCase("ONT::AFTER")
                       // ))
                    {
                        attrs.add(xml_attribute("mod", termType));
                        if (ekbFindExtraction(groundVar) != null) {
                            return xml_elementWithID("time", groundVar, attrs);
                        } else { // we need to define the item here
                            return xml_lfTerm("time", groundVar, attrs);
                        }
                    }
                }
            }
        } 
        // should not happen!
        Debug.error("unexpected " + Feature.TIME + " value: " + time);
        return xml_element("time", "", removePackage(time.toString(), false));
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
                String figureVar = getKeywordArgString(":FIGURE", modifierTerm);
                // TODO: assert that figureVar.equalsIgnoreCase(id)
                String groundVar = getKeywordArgString(":GROUND", modifierTerm);
                KQMLList groundTerm = findTermByVar(groundVar, context);
                String groundOntType = pullOntType(groundTerm);
                // we handle cases individually
                if (modOntType.equalsIgnoreCase("ONT::EVENT-DURATION-MODIFIER")
                        && this.isOntTime(groundOntType))
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
        } 
        Debug.warn("unexpected location value: " + loc);
        return "";
    }

    /**
     * Returns a {@code <location>} XML element representing location information attached to the event, 
     * or the empty string if no such information exists. 
     */
    private String xml_location1() {
        KQMLObject loc = features.get(Feature.LOC1);
        if (loc == null) {
            return "";
        }
        
        List<String> attrs = new ArrayList<String>();
        KQMLObject modType = features.get(Feature.LOCMOD1);
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
        } 
        Debug.warn("unexpected location value: " + loc);
        return "";
    }

    /**
     * Returns a {@code <site>} XML element representing molecular site information attached to the event, 
     * or the empty string if no such information exists. 
     */
    private String xml_site() {
        KQMLObject varObj = features.get(Feature.SITE);
        if (varObj == null) 
            return "";

        String var = varObj.toString();
        if (isOntVar(var)) {
            if (ekbFindExtraction(var) != null) {
                return xml_elementWithID("site", var);
            } else { // we need to define the item here
                return xml_lfTerm("site", var);
            }
        } 
        Debug.error("unexpected " + Feature.SITE + " value: " + var);
        return "";
    }

    /**
     * Returns a {@code <cell-line>} XML element representing cell-line
     * information attached to the event, or the empty string if no such
     * information exists.
     */
    private String xml_cellline() {
        KQMLObject varObj = features.get(Feature.CELL_LINE);
        if (varObj == null) 
            return "";
        
        String var = varObj.toString();
        if (isOntVar(var)) {
            if (ekbFindExtraction(var) != null) {
                return xml_elementWithID("cell-line", var);
            } else { // we need to define the item here
                return xml_lfTerm("cell-line", var);
            }
        } 
        Debug.error("unexpected " + Feature.CELL_LINE + " value: " + var);
        return "";
    }

    /**
     * Returns a {@code <from>} XML element representing the source location
     * information attached to the event, or the empty string if no such
     * information exists. Only events of movement will have this feature.
     */
    private String xml_from() {
        KQMLObject varObj = features.get(Feature.FROM);
        if (varObj == null) 
            return "";

        String var = varObj.toString();
        if (isOntVar(var)) {
            if (ekbFindExtraction(var) != null) {
                return xml_elementWithID("from", var);
            } else { // we need to define the item here
                return xml_lfTerm("from", var);
            }
        } 
        Debug.error("unexpected " + Feature.FROM + " value: " + var);
        return "";
    }

    /**
     * Returns a {@code <to>} XML element representing the target location
     * information attached to the event, or the empty string if no such
     * information exists. Only events of movement will have this feature.
     */
    private String xml_to() {
        KQMLObject varObj = features.get(Feature.TO);
        if (varObj == null) 
            return "";
        
        String var = varObj.toString();
        if (isOntVar(var)) {
            if (ekbFindExtraction(var) != null) {
                return xml_elementWithID("to", var);
            } else {
                // default: we define the item here in full
                return xml_lfTerm("to", var);
            }
        } 
        Debug.error("unexpected " + Feature.TO + " value: " + var);
        return "";
    }

    /**
     * Source
     * @return
     */
    private String xml_source() {
        KQMLObject varObj = features.get(Feature.SOURCE);
        if (varObj == null) 
            return "";
        
        String var = varObj.toString();
        if (isOntVar(var)) {
            if (ekbFindExtraction(var) != null) {
                return xml_elementWithID("source", var);
            } else {
                // first check if we have a relation to another extraction
                KQMLList varTerm = findTermByVar(var, context);
                KQMLObject groundVar = varTerm.getKeywordArg(":GROUND");
                if (groundVar != null) {
                    String gVar = groundVar.toString();
                    if (ekbFindExtraction(gVar) != null) {
                        List<String> attrs = new ArrayList<String>();
                        attrs.add(xml_attribute("mod", removePackage(pullOntWord(varTerm))));
                        return xml_elementWithID("source", gVar, attrs);
                    }
                }
                // default: we define the item here in full
                return xml_lfTerm("source", var);
            }
        }
        Debug.error("unexpected " + Feature.SOURCE + " value: " + var);
        return "";
    }

    /**
     * Result
     * @return
     */
    private String xml_result() {
        KQMLObject varObj = features.get(Feature.RESULT);
        if (varObj == null) 
            return "";
        
        String var = varObj.toString();
        if (isOntVar(var)) {
            if (ekbFindExtraction(var) != null) {
                return xml_elementWithID("result", var);
            } else {
                // first check if we have a relation to another extraction
                KQMLList varTerm = findTermByVar(var, context);
                KQMLObject groundVar = varTerm.getKeywordArg(":GROUND");
                if (groundVar != null) {
                    String gVar = groundVar.toString();
                    if (ekbFindExtraction(gVar) != null) {
                        List<String> attrs = new ArrayList<String>();
                        attrs.add(xml_attribute("mod", removePackage(pullOntWord(varTerm))));
                        return xml_elementWithID("result", gVar, attrs);
                    }
                }
                // default: we define the item here in full
                return xml_lfTerm("result", var);
            }
        } 
        Debug.error("unexpected " + Feature.RESULT + " value: " + var);
        return "";
    }

    /**
     * Result
     * @return
     */
    private String xml_result1() {
        KQMLObject varObj = features.get(Feature.RESULT1);
        if (varObj == null) 
            return "";
        
        String var = varObj.toString();
        if (isOntVar(var)) {
            if (ekbFindExtraction(var) != null) {
                return xml_elementWithID("result", var);
            } else {
                // first check if we have a relation to another extraction
                KQMLList varTerm = findTermByVar(var, context);
                KQMLObject groundVar = varTerm.getKeywordArg(":GROUND");
                if (groundVar != null) {
                    String gVar = groundVar.toString();
                    if (ekbFindExtraction(gVar) != null) {
                        List<String> attrs = new ArrayList<String>();
                        attrs.add(xml_attribute("mod", removePackage(pullOntWord(varTerm))));
                        return xml_elementWithID("result", gVar, attrs);
                    }
                }
                // default: we define the item here in full
                return xml_lfTerm("result", var);
            }
        } 
        Debug.error("unexpected " + Feature.RESULT1 + " value: " + var);
        return "";
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
     * Returns a {@code <polarity>} XML element representing the value of the polarity modifier for the
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
        if (value == null) return "";
        // it must be a variable
        return xml_elementWithID("epistemic-modality", value.toString());
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
        List<String> conts = new ArrayList<String>();
        conts.add(xml_mods(PolyModifier.DEGREE, "degree"));
        conts.add(xml_mods(PolyModifier.FREQUENCY, "frequency"));
        conts.add(xml_mods(PolyModifier.MODA, "mod"));
        conts.add(xml_mods(PolyModifier.MODN, "mod"));
        conts.add(xml_mod(Feature.MOD, "mod"));
        conts.add(xml_mod(Feature.MOD1, "mod"));
        return xml_element("mods", null, conts);
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
        if ((mods == null) || mods.isEmpty()) {
            return "";
        }
        String result = "";
        for (KQMLObject modValue : mods) {
            if (modValue instanceof KQMLList) {
                KQMLList modValueList = (KQMLList) modValue;
                List<String> conts = new ArrayList<String>();
                conts.add(xml_element("type", "", ontType(modValueList)));
                conts.add(xml_element("value", "", removePackage(ontWord(modValueList), false)));
                result += xml_element(modType, null, conts);
            } else if (isOntVar(modValue.toString())) { // TODO remove (obsolete)
                String var = modValue.toString();
                if (ekbFindExtraction(var) != null) { 
                    result += xml_elementWithID(modType, var);
                } else { 
                    // first check if we have a relation to another extraction
                    KQMLList varTerm = findTermByVar(var, context);
                    KQMLObject groundVar = varTerm.getKeywordArg(":GROUND");
                    if (groundVar != null) {
                        String gVar = groundVar.toString();
                        if (ekbFindExtraction(gVar) != null) {
                            List<String> attrs = new ArrayList<String>();
                            attrs.add(xml_attribute("mod", removePackage(pullOntWord(varTerm))));
                            return xml_elementWithID(modType, gVar, attrs);
                        }
                    }
                    // default: we define the item here in full
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
     * Modifier
     * 
     */
    private String xml_mod(Feature mod, String modType) {
        KQMLObject modObj = features.get(mod);
        if (modObj == null)
            return "";

        String var = modObj.toString();
        if (isOntVar(var)) {
            // sometimes a mod is mapped to :QUAL
            ArrayList<KQMLObject> quals = polyMods.get(PolyModifier.QUAL);
            if (quals != null) {
                for (KQMLObject valObj : quals) {
                    if (isOntVar(valObj.toString())) {
                        if (var.equalsIgnoreCase(valObj.toString())) {
                            return "";
                        }
                    }
                }
            }
            
            if (ekbFindExtraction(var) != null) {
                return xml_elementWithID(modType, var);
            } else { // we need to define the item here
                return xml_lfTerm(modType, var);
            }
        } 
        Debug.error("unexpected " + mod + " value: " + var);
        return "";
    }
    
    /**
     * Qualifiers
     */
    private String xml_qualifiers() {
        return xml_element("qualifiers", "", xml_qualifiers(PolyModifier.QUAL, "qual"));
    }
    
    /**
     * Qualifier
     * 
     * @param qual
     * @param tag
     * @return
     */
    private String xml_qualifiers(PolyModifier qual, String tag) {
        ArrayList<KQMLObject> quals = polyMods.get(qual);
        if ((quals == null) || quals.isEmpty()) {
            return "";
        }
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
     * Returns a {@code <features>} XML element representing the event features,
     * or the empty string if no such information exists.
     * <p>
     * Note: features are characteristics of the event that can be negated (in which case
     * they will appear in a {@code <not-features>} XML element).
     */
    private String xml_features() {
        List<String> conts = new ArrayList<String>();
        conts.add(xml_inevent());
        return xml_element("features", null, conts);
    }

    /**
     * Returns a list of {@code inevent} features.
     */
    private String xml_inevent() {
        ArrayList<KQMLObject> inEvents = polyMods.get(PolyModifier.INEVENT);
        //Debug.warn("poly :INEVENT of " + id + " =  " + inEvents);
        if ((inEvents == null) || inEvents.isEmpty()) 
            return "";
        
        String result = "";
        for (KQMLObject varObj : inEvents) {
            if (isOntVar(varObj.toString())) {
                // TODO: validate? get more info?
                result += xml_elementWithID("inevent", varObj.toString());
            } else {
                Debug.warn(":INEVENT value: expected var, got " + varObj);
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
                        || Modifier.isModifier(itemAsString)
                        || PolyModifier.isModifier(itemAsString))
                {
                    expand = false;
                } else { // expandable modifier
                    Debug.warn("Unrecognized event attribute: " + itemAsString);
                    expand = true;
                }
            } else if (expand && isOntVar(itemAsString)) { // variable
                KQMLList term = findTermByVar(itemAsString, context);
                if (term == null) {
                    expandedValue.add(item);
                } else {
                    expandedValue.add(pullFullOntType(term));
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
