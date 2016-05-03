/*
 * SubscriptionPattern.java
 *
 * George Ferguson, ferguson@cs.rochester.edu,  3 Apr 2000
 * $Id: SubscriptionPattern.java,v 1.2 2010/04/22 03:30:20 lgalescu Exp $
 */

package TRIPS.Facilitator;

import java.util.Vector;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLToken;

/**
 * Subscription patterns are matched against messages as followed:
 *  - The element * (asterisk) in the pattern is a wildcard that matches
 *    any single element (list or atom or whatever).
 *  - Embedded lists in the pattern are matched recursively.
 *  - The element &key in the pattern indicates that the remaining
 *    elements (at this level of embedding) are keyword/value patterns.
 *    These are considered to match if for each key/value pattern in
 *    the pattern, the key appears in the performative (at this level
 *    of embedding) and the corresponding value in the message matches
 *    the corresponding value in the pattern (recursively).
 *  - The keyword parameter :sender is treated specially: it is
 *    considered to match if either it names the sender of the message
 *    (as given by :sender or from the canonical name of the client
 *    who sent the message), or if it names a group to which the
 *    sender belongs.
 *  - Other elements (strings, numbers, whatever) must match literally
 *    using EQUALS.
 */
abstract public class SubscriptionPattern {
    /**
     * All SubscriptionPatterns must implement this method.
     */
    abstract public boolean matches(KQMLPerformative msg);
    /**
     * This method is the interface to the pattern compiler.
     * Given a specification (probably string or list), it tries to
     * design a matcher that will work efficiently for the pattern.
     * It does this by looking for several special cases such as:
     * ``*'': This pattern matches anything.
     * ``(* &key :content *)'': This matches anything with :content.
     * ``(* &key :content (word . *))'': Matches anything whose :content
     *                                   starts with ``word''.
     * ``(tell &key :sender foo)'': Matches anything whose :sender is ``foo''
     *                              (or belongs to foo if foo names a group).
     *
     * If none of these apply, then this method returns a general-purpose
     * matcher that expands the pattern and matches elements recursively.
     */
    static public SubscriptionPattern fromSpec(Object spec, Registry registry) {
	Debug.debug("SubscriptionPattern.fromSpec: spec=" + spec);
	// Atomic pattern
	if (spec instanceof KQMLToken) {
	    String s = ((KQMLToken)spec).stringValue();
	    // Usually this will be *
	    if (s.equals("*")) {
		Debug.debug("SubscriptionPattern.fromSpec: * matches anything");
		return new MatchAnythingPattern();
	    } else {
		// Non-* atomic pattern cannot match performative
		Debug.debug("SubscriptionPattern.fromSpec: non-* atomic pattern");
		return new MatchNothingPattern();
	    }
	} else if (spec instanceof KQMLList) {
	    KQMLList l = (KQMLList)spec;
	    if (l.size() == 0) {
		// Null list cannot be a performative
		Debug.debug("SubscriptionPattern.fromSpec: empty list");
		return new MatchNothingPattern();
	    } else {
		// We may have several conditions to test
		Vector matchers = new Vector();
		Object vobj = l.get(0);
		if (vobj instanceof KQMLToken) {
		    String verb = ((KQMLToken)vobj).stringValue();
		    if (verb.equals("*")) {
			// Verb pattern is "*"; nothing to test
			Debug.debug("SubscriptionPattern.fromSpec: verb is *");
		    } else {
			// Need to match verb
			Debug.debug("SubscriptionPattern.fromSpec: verb is " + verb);
			matchers.addElement(new MatchVerbPattern(verb));
		    }
		} else {
		    // Non-atomic verb cannot be performative
		    Debug.debug("SubscriptionPattern.fromSpec: non-atomic verb");
		    return new MatchNothingPattern();
		}
		int keyindex = findArg(l, "&key");
		if (keyindex == -1) {
		    // No &key specifier
		    if (l.size() == 3) {
			Object l1 = l.get(1);
			Object l2 = l.get(2);
			if (l1 instanceof KQMLToken &&
			    l2 instanceof KQMLToken) {
			    String s1 = ((KQMLToken)l1).stringValue();
			    String s2 = ((KQMLToken)l2).stringValue();
			    if (s1.equals(".") &&
				s2.equals("*")) {
				Debug.debug("SubscriptionPattern.fromSpec: match verb only");
				return (SubscriptionPattern)(matchers.elementAt(0));
			    } else {
				Debug.debug("SubscriptionPattern.fromSpec: no &key, length == 3, not (X . *)");
				return new MatchGeneralPattern(spec, registry);
			    }
			} else {
			    Debug.debug("SubscriptionPattern.fromSpec: no &key, length == 3");
			    return new MatchGeneralPattern(spec, registry);
			}
		    } else {
			Debug.debug("SubscriptionPattern.fromSpec: no &key, length != 3");
			return new MatchGeneralPattern(spec, registry);
		    }
		} else {
		    // We have an &key specifier
		    if (keyindex > 1) {
			// Args before &key match literally
			Debug.debug("SubscriptionPattern.fromSpec: args before &key");
			return new MatchGeneralPattern(spec, registry);
		    }
		    // Args after &key match by keyword
		    for (int i=keyindex+1; i < l.size(); i+=2) {
			Object kobj = l.get(i);
			if (!(kobj instanceof KQMLToken)) {
			    // Keyword arg is not an atom (exception?)
			    Debug.warn("SubscriptionPattern.fromSpec: keyword arg not atom");
			    return new MatchNothingPattern();
			}
			String keyword = ((KQMLToken)kobj).stringValue();
			if (i == l.size()) {
			    // Missing keyword parameter (exception?)
			    Debug.warn("SubscriptionPattern.fromSpec: missing keyword arg");
			    return new MatchNothingPattern();
			}
			Object pobj = l.get(i+1);
			if (pobj instanceof KQMLToken) {
			    String p = ((KQMLToken)pobj).stringValue();
			    if (p.equals("*")) {
				// Parameter must simply exist
				Debug.debug("SubscriptionPattern.fromSpec: parameter " + keyword + " must exist");
				matchers.addElement(new MatchParameterExistsPattern(keyword));
			    } else {
				// Parameter must match string
				Debug.debug("SubscriptionPattern.fromSpec: parameter " + keyword + " must be " + p);
				matchers.addElement(new MatchParameterStringPattern(keyword, p));
			    }
			} else if (pobj instanceof KQMLList) {
			    KQMLList pl = (KQMLList)pobj;
			    if (pl.size() == 3) {
				Object pl0 = pl.get(0);
				Object pl1 = pl.get(1);
				Object pl2 = pl.get(2);
				if (pl0 instanceof KQMLToken &&
				    pl1 instanceof KQMLToken &&
				    pl2 instanceof KQMLToken) {
				    String s0 = ((KQMLToken)pl0).stringValue();
				    String s1 = ((KQMLToken)pl1).stringValue();
				    String s2 = ((KQMLToken)pl2).stringValue();
				    if (s1.equals(".")) {
					if (s0.equals("*")) {
					    // Car of parm can be anything
					    Debug.debug("SubscriptionPattern.fromSpec: car of " + keyword + " can be anything");
					} else {
					    // Must match car of parm
					    Debug.debug("SubscriptionPattern.fromSpec: car of " + keyword + " must be " + s0);
					    matchers.addElement(new MatchParameterHeadPattern(keyword, s0));
					}
					if (s2.equals("*")) {
					    // Cdr of parm can be anything
					    Debug.debug("SubscriptionPattern.fromSpec: cdr of " + keyword + " can be anything");
					} else {
					    // This doesn't make sense...
					    Debug.debug("SubscriptionPattern.fromSpec: cdr of " + keyword + " must be " + s2);
					    return new MatchGeneralPattern(spec, registry);
					}
				    } else {
					// Have to match 3-element list
					Debug.debug("SubscriptionPattern.fromSpec: general match 3-element list");
					return new MatchGeneralPattern(spec, registry);
				    }
				} else {
				    // Non-strings require general matcher
				    Debug.debug("SubscriptionPattern.fromSpec: non-string in spec");
				    return new MatchGeneralPattern(spec, registry);
				}
			    } else {
				// Not * or (X . *) => general matcher
				Debug.debug("SubscriptionPattern.fromSpec: not * or (X . *)");
				return new MatchGeneralPattern(spec, registry);
			    }
			} else {
			    // Not list of string -> probably funny
			    Debug.warn("SubscriptionPattern.fromSpec: not list of string");
			    return new MatchGeneralPattern(spec, registry);
			}
		    }
		    // Now take all matchers and return conjunction
		    Debug.debug("SubscriptionPattern.fromSpec: returning conjunctive pattern");
		    //return new MatchConjunctionPattern((SubscriptionPattern[])(matchers.toArray()));
		    SubscriptionPattern[] pats = new SubscriptionPattern[matchers.size()];
		    for (int i=0; i < matchers.size(); i++) {
			pats[i] = (SubscriptionPattern)matchers.elementAt(i);
		    }
		    return new MatchConjunctionPattern(pats);
		}
	    }
	} else {
	    // Not an atom or a list => cannot be performative
	    Debug.warn("SubscriptionPattern.fromSpec: not atom or list");
	    return new MatchNothingPattern();
	}
    }
    /**
     * Returns the index of the first WHAT token in LIST, or -1
     * if not found. Matching is case-insensitive.
     */
    static int findArg(KQMLList list, String what) {
	int len = list.size();
	for (int i=0; i < len; i++) {
	    String str = list.get(i).toString();
	    if (str.equalsIgnoreCase(what)) {
		return i;
	    }
	}
	return -1;
    }
}

class MatchAnythingPattern extends SubscriptionPattern {
    public MatchAnythingPattern() {
    }
    public boolean matches(KQMLPerformative msg) {
	Debug.debug("MatchAnythingPattern.matches: returning true");
	return true;
    }
    public String toString() {
	return "MatchAnything";
    }
}

class MatchNothingPattern extends SubscriptionPattern {
    public MatchNothingPattern() {
    }
    public boolean matches(KQMLPerformative msg) {
	Debug.debug("MatchNothingPattern.matches: returning false");
	return false;
    }
    public String toString() {
	return "MatchNothing";
    }
}

class MatchVerbPattern extends SubscriptionPattern {
    protected String verb;
    public MatchVerbPattern(String verb) {
	this.verb = verb;
    }
    public boolean matches(KQMLPerformative msg) {
	Debug.debug("MatchVerbPattern.matches: msg=" + msg);
	Debug.debug("MatchVerbPattern.matches: verb=" + verb);
	return verb.equalsIgnoreCase(msg.getVerb());
    }
    public String toString() {
	return "MatchVerb[" + verb + "]";
    }
}

class MatchParameterExistsPattern extends SubscriptionPattern {
    protected String keyword;
    public MatchParameterExistsPattern(String keyword) {
	this.keyword = keyword;
    }
    public boolean matches(KQMLPerformative msg) {
	Object kobj = msg.getParameter(keyword);
	Debug.debug("MatchParameterExistsPattern.matches: kobj=" + kobj);
	return (kobj != null);
    }
    public String toString() {
	return "MatchExists[" + keyword + "]";
    }
}

class MatchParameterStringPattern extends SubscriptionPattern {
    protected String keyword;
    protected String value;
    public MatchParameterStringPattern(String keyword, String value) {
	this.keyword = keyword;
	this.value = value;
    }
    public boolean matches(KQMLPerformative msg) {
	Object kobj = msg.getParameter(keyword);
	Debug.debug("MatchParameterStringPattern.matches: kobj=" + kobj);
	if (kobj == null || !(kobj instanceof KQMLToken)) {
	    Debug.debug("MatchParameterStringPattern.matches: kobj null or not token; returning false");
	    return false;
	} else {
	    String kval = ((KQMLToken)kobj).stringValue();
	    Debug.debug("MatchParameterStringPattern.matches: kval=" + kval);
	    Debug.debug("MatchParameterStringPattern.matches: returning " + value.equalsIgnoreCase(kval));
	    return value.equalsIgnoreCase(kval);
	}
    }
    public String toString() {
	return "MatchParameterString[" + keyword + "," + value + "]";
    }
}

class MatchSenderPattern extends SubscriptionPattern {
    protected String sender;
    public MatchSenderPattern(String sender) {
	this.sender = sender;
    }
    public boolean matches(KQMLPerformative msg) {
	Object s = msg.getParameter(":sender");
	Debug.debug("MatchSenderPattern.matches: :sender=" + s);
	if (s != null && s instanceof KQMLToken) {
	    String str = ((KQMLToken)s).stringValue();
	    Debug.debug("MatchSenderPattern.matches: str=" + str);
	    Debug.debug("MatchSenderPattern.matches: returning " + sender.equalsIgnoreCase(str));
	    return sender.equalsIgnoreCase(str);
	}
	Debug.debug("MatchSenderPattern.matches: returning false");
	return false;
    }
    public String toString() {
	return "MatchSender[" + sender + "]";
    }
}

class MatchParameterHeadPattern extends SubscriptionPattern {
    protected String keyword;
    protected String head;
    public MatchParameterHeadPattern(String keyword, String head) {
	this.keyword = keyword;
	this.head = head;
    }
    public boolean matches(KQMLPerformative msg) {
	Debug.debug("MatchParameterHeadPattern.matches: msg=" + msg);
	Object pobj = msg.getParameter(keyword);
	Debug.debug("MatchParameterHeadPattern.matches: pobj=" + pobj);
	if (pobj instanceof KQMLList) {
	    KQMLList l = (KQMLList)pobj;
	    if (l.size() == 0) {
		Debug.debug("MatchParameterHeadPattern.matches: pobj is empty list; returning false");
		return false;
	    }
	    Object car = l.get(0);
	    if (car != null && car instanceof KQMLToken) {
		String carstr = ((KQMLToken)car).stringValue();
		Debug.debug("MatchParameterHeadPattern.matches: carstr=" + carstr);
		Debug.debug("MatchParameterHeadPattern.matches: returning " + head.equalsIgnoreCase(carstr));
		return head.equalsIgnoreCase(carstr);
	    }
	}
	Debug.debug("MatchParameterHeadPattern.matches: returning false");
	return false;
    }
    public String toString() {
	return "MatchParameterHead[" + keyword + "," + head + "]";
    }
}

class MatchConjunctionPattern extends SubscriptionPattern {
    protected SubscriptionPattern[] patterns;
    public MatchConjunctionPattern(SubscriptionPattern[] patterns) {
	this.patterns = patterns;
    }
    public boolean matches(KQMLPerformative msg) {
	Debug.debug("MatchConjunctionPattern.matches: msg=" + msg);
	for (int i=0; i < patterns.length; i++) {
	    Debug.debug("MatchConjunctionPattern.matches: patterns[" + i + "]=" + patterns[i]);
	    if (!patterns[i].matches(msg)) {
		Debug.debug("MatchConjunctionPattern.matches: returning false");
		return false;
	    }
	}
	Debug.debug("MatchConjunctionPattern.matches: returning true");
	return true;
    }
    public String toString() {
	StringBuffer buf = new StringBuffer("MatchConjunction[");
	for (int i=0; i < patterns.length; i++) {
	    buf.append(patterns[i].toString());
	    if (i < patterns.length - 1) {
		buf.append(",");
	    }
	}
	buf.append("]");
	return buf.toString();
    }
}

class MatchDisjunctionPattern extends SubscriptionPattern {
    protected SubscriptionPattern[] patterns;
    public MatchDisjunctionPattern(SubscriptionPattern[] patterns) {
	this.patterns = patterns;
    }
    public boolean matches(KQMLPerformative msg) {
	Debug.debug("MatchDisjunctionPattern.matches: msg=" + msg);
	for (int i=0; i < patterns.length; i++) {
	    Debug.debug("MatchDisjunctionPattern.matches: patterns[" + i + "]=" + patterns[i]);
	    if (patterns[i].matches(msg)) {
		Debug.debug("MatchDisjunctionPattern.matches: returning true");
		return true;
	    }
	}
	Debug.debug("MatchDisjunctionPattern.matches: returning false");
	return false;
    }
    public String toString() {
	StringBuffer buf = new StringBuffer("MatchDisjunction[");
	for (int i=0; i < patterns.length; i++) {
	    buf.append(patterns[i].toString());
	    if (i < patterns.length - 1) {
		buf.append(",");
	    }
	}
	buf.append("]");
	return buf.toString();
    }
}

//
// This class is the general (non-optimized) pattern matcher
//
class MatchGeneralPattern extends SubscriptionPattern {
    protected Object pattern;
    protected Registry registry;
    public MatchGeneralPattern(Object pattern, Registry registry) {
	this.pattern = pattern;
	this.registry = registry;
    }
    private static KQMLToken starToken = new KQMLToken("*");
    public boolean matches(KQMLPerformative msg) {
	return matches(this, pattern, msg.toList());
    }
    protected static boolean matches(MatchGeneralPattern sub,
				     Object pattern, Object thingo) {
	Debug.debug("MatchGeneralPattern.matches: pattern=" + pattern + ", thingo=" + thingo);
	if (pattern instanceof KQMLToken &&
	    ((KQMLToken)pattern).equals(starToken)) {
	    // Asterisk in pattern matches anything
	    Debug.debug("MatchGeneralPattern.matches: * pattern: returning true");
	    return true;
	} else if (pattern instanceof KQMLToken &&
		   thingo instanceof KQMLToken) {
	    // Two tokens: compare specially
	    String pstr = ((KQMLToken)pattern).toString();
	    String tstr = ((KQMLToken)thingo).toString();
	    if (pstr.equalsIgnoreCase(tstr)) {
		Debug.debug("MatchGeneralPattern.matches: tokens match: returning true");
		return true;
	    } else {
		Debug.debug("MatchGeneralPattern.matches: tokens don't match: returning false");
		return false;
	    }
	} else if (pattern.equals(thingo)) {
	    // Otherwise might match literally (note use of EQUALS here)
	    Debug.debug("MatchGeneralPattern.matches: literal equals: returning true");
	    return true;
	} else if (pattern instanceof KQMLList &&
		   thingo instanceof KQMLList) {
	    // Otherwise, if we have two lists we need to investigate
	    Debug.debug("MatchGeneralPattern.matches: comparing lists");
	    KQMLList patl = (KQMLList)pattern;
	    KQMLList thingol = (KQMLList)thingo;
	    int patlen = patl.size();
	    int keyarg = SubscriptionPattern.findArg(patl, "&key");
	    int dotarg = SubscriptionPattern.findArg(patl, ".");
	    if (keyarg != -1 && dotarg != -1) {
		// Both &key or dot specifier: should be caught by compiler...
		Debug.debug("MatchGeneralPattern.matches: both &key and . in pattern: returnin false");
		return false;
	    } else if (keyarg == -1 && dotarg == -1) {
		// No &key or dot specifier
		Debug.debug("MatchGeneralPattern.matches: no &key in pattern");
		// => Length of lists must match
		if (thingol.size() != patlen) {
		    Debug.debug("MatchGeneralPattern.matches: unequal length: returning false");
		    return false;
		}
		// And elements are matched recursively
		for (int i=0; i < patlen; i++) {
		    Debug.debug("MatchGeneralPattern.matches: matching element " + i + " recursively...");
		    if (!matches(sub, patl.get(i), thingol.get(i))) {
			Debug.debug("MatchGeneralPattern.matches: element " + i + " failed: returning false");
			return false;
		    }
		}
		Debug.debug("MatchGeneralPattern.matches: all elements matched: returning true");
		return true;
	    } else if (keyarg != -1) {
		// We have an &key specifier
		Debug.debug("MatchGeneralPattern.matches: &key present in pattern");
		// => Elements before it match recursively
		Debug.debug("MatchGeneralPattern.matches: matching positional elements");
		for (int i=0; i < keyarg; i++) {
		    Debug.debug("MatchGeneralPattern.matches: matching element " + i + " recursively...");
		    if (!matches(sub, patl.get(i), thingol.get(i))) {
			Debug.debug("MatchGeneralPattern.matches: element " + i + " failed: returning false");
			return false;
		    }
		}
		Debug.debug("MatchGeneralPattern.matches: positional elements ok");
		// No elements after &key (which would be funny but...)
		if (keyarg == patlen) {
		    Debug.debug("MatchGeneralPattern.matches: no elements after &key: returning true");
		    return true;
		}
		// Elements after match on a keyword basis
		for (int i=keyarg+1; i < patlen; i+=2) {
		    Debug.debug("MatchGeneralPattern.matches: doing keyarg at position " + i);
		    Object kobj = patl.get(i);
		    // Sanity check
		    if (!(kobj instanceof KQMLToken) ||
			i+1 == patlen) {
			// Should notice this when pattern is registered...
			Debug.debug("MatchGeneralPattern.matches: keyarg is not a KQMLToken: returning false");
			return false;
		    }
		    String key = ((KQMLToken)kobj).stringValue();
		    Debug.debug("MatchGeneralPattern.matches: key at position " + i + "=" + key);
		    Object pobj = patl.get(i+1);
		    Debug.debug("MatchGeneralPattern.matches: arg at position " + i + "+1=" + pobj);
		    Object mobj = thingol.getKeywordArg(key);
		    if (mobj == null) {
			Debug.debug("MatchGeneralPattern.matches: no " + key + " arg in thingo: returning false");
			return false;
		    }
		    Debug.debug("MatchGeneralPattern.matches: " + key + " arg in thingo=" + mobj);
		    if (key.equalsIgnoreCase(":sender")) {
			// Special case for :sender (match groups)
			Debug.debug("MatchGeneralPattern.matches: doing :sender match");
			if (!(pobj instanceof KQMLToken)) {
			    Debug.debug("MatchGeneralPattern.matches: pobj not KQMLToken: returning false");
			    return false;
			}
			if (!(mobj instanceof KQMLToken)) {
			    Debug.debug("MatchGeneralPattern.matches: mobj not KQMLToken: returning false");
			    return false;
			}
			String pval = ((KQMLToken)pobj).stringValue();
			String mval = ((KQMLToken)mobj).stringValue();
			Debug.debug("MatchGeneralPattern.matches: pval=" + pval + ", mval=" + mval);
			if (!matchesSender(sub, pval, mval)) {
			    Debug.debug("MatchGeneralPattern.matches: matchesSender failed: returning false");
			    return false;
			}
		    } else {
			// Other keywords' values match normally
			Debug.debug("MatchGeneralPattern.matches: matching recursively...");
			if (!matches(sub, pobj, mobj)) {
			    Debug.debug("MatchGeneralPattern.matches: recursive match failed: returning false");
			    return false;
			}
		    }
		}
		Debug.debug("MatchGeneralPattern.matches: all &key args matched: returning true");
		return true;
	    } else if (dotarg != -1) {
		// We have a "." specifier
		Debug.debug("MatchGeneralPattern.matches: dot present in pattern");
		// Not exactly one element after dot: should be an error
		if (dotarg != patlen-2) {
		    Debug.debug("MatchGeneralPattern.matches: pattern isn't dot-arg: returning false");
		    return false;
		}
		// => Elements before it match recursively
		Debug.debug("MatchGeneralPattern.matches: matching positional elements");
		for (int i=0; i < dotarg; i++) {
		    Debug.debug("MatchGeneralPattern.matches: matching element " + i + " recursively...");
		    if (!matches(sub, patl.get(i), thingol.get(i))) {
			Debug.debug("MatchGeneralPattern.matches: element " + i + " failed: returning false");
			return false;
		    }
		}
		Debug.debug("MatchGeneralPattern.matches: positional elements ok");
		// Final element matches remainder of list
		Object cdrpat = patl.get(dotarg+1);
		Object cdr = thingol.subList(dotarg+1, thingol.size());
		Debug.debug("MatchGeneralPattern.matches: cdrpat=" + cdrpat + ", cdr=" + cdr);
		Debug.debug("MatchGeneralPattern.matches: testing cdr recursively...");
		if (!matches(sub, cdrpat, cdr)) {
		    Debug.debug("MatchGeneralPattern.matches: recursive match failed: returning false");
		    return false;
		}
		Debug.debug("MatchGeneralPattern.matches: dot arg matched: returning true");
		return true;
	    }
	} else {
	    // Otherwise pattern can't match
	    Debug.debug("MatchGeneralPattern.matches: neither equals nor both lists: returning false");
	    return false;
	}
	Debug.debug("MatchGeneralPattern.matches: shouldn't get here: returning false");
	return false;
    }
    public static boolean matchesSender(MatchGeneralPattern sub,
					String pval, String mval) {
	Debug.debug("MatchGeneralPattern.matchesSender: pval=" + pval + ", mval=" + mval);
	// First check identical strings
	if (pval.equalsIgnoreCase(mval)) {
	    Debug.debug("MatchGeneralPattern.matchesSender: literally equal: returning true");
	    return true;
	}
	// Otherwise have to lookup in registry
	Sendable p = sub.registry.lookupClientByName(pval);
	Sendable m = sub.registry.lookupClientByName(mval);
	if (p == null) {
	    Debug.debug("MatchGeneralPattern.matchesSender: " + pval + " not in registry: returning false");
	    return false;
	}
	if (m == null) {
	    Debug.debug("MatchGeneralPattern.matchesSender: " + mval + " not in registry: returning false");
	    return false;
	}
	Debug.debug("MatchGeneralPattern.matchesSender: p=" + p + ", m=" + m);
	// Ok if pattern names a group and msg is from member of the group
	if (p.getClass() == ClientGroup.class) {
	    Debug.debug("MatchGeneralPattern.matchesSender: p is a ClientGroup");
	    ClientGroup g = (ClientGroup)p;
	    if (g.contains(m)) {
		Debug.debug("MatchGeneralPattern.matchesSender: " + g + " contains " + m + ": returning true");
		return true;
	    }
	}
	// Otherwise no good
	Debug.debug("MatchGeneralPattern.matchesSender: returning false");
	return false;
    }
    public String toString() {
	return "MatchGeneral[" + pattern + "]";
    }
}

