/*
 * Subscription.java
 *
 * George Ferguson, ferguson@cs.rochester.edu,  3 Apr 2000
 * $Id: Subscription.java,v 1.1.1.1 2005/01/14 19:48:08 ferguson Exp $
 */

package TRIPS.Facilitator;

import TRIPS.KQML.KQMLPerformative;

public class Subscription {
    //
    // Fields
    //
    protected Sendable client;
    protected SubscriptionPattern pattern;
    //
    // Constructor
    //
    public Subscription(Sendable client, Object pat, Registry registry) {
	Debug.debug("Subscription.<init>: client= " + client + ", pat=" + pat + ", registry=" + registry);
	this.client = client;
	this.pattern = SubscriptionPattern.fromSpec(pat, registry);
	Debug.debug("Subscription.<init>: done, pattern=" + pattern);
    }
    //
    // Accessors
    //
    public Sendable getClient() {
	return client;
    }
    public SubscriptionPattern getSubscriptionPattern() {
	return pattern;
    }
    //
    // Methods
    //
    public boolean matches(KQMLPerformative msg) {
	return pattern.matches(msg);
    }
    public void sendMessage(KQMLPerformative msg, Sendable fromWhom) {
	client.sendTo(msg, fromWhom);
    }
    //
    // Pretty-printing
    //
    public String toString() {
	return "Subscription[" + client + "," + pattern + "]";
    }
}

