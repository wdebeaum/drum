/*
 * SubscriptionManager.java
 *
 * George Ferguson, ferguson@cs.rochester.edu,  3 Apr 2000
 * $Id: SubscriptionManager.java,v 1.2 2019/06/19 00:50:24 lgalescu Exp $
 */

package TRIPS.Facilitator;

import java.util.Vector;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.NoSuchElementException;
import TRIPS.KQML.KQMLPerformative;

/**
 * A SubscriptionManager provides information dissemination on a subscription
 * basis. That is, clients registered with the Facilitator can subscribe
 * to information by providing a pattern that matches the messages they
 * wish to receive. Client messages are handled by the SubscriptionManager when
 * no :receiver is specified in the message. Each message handled by
 * the SubscriptionManager is matched against all recorded subscriptions. For
 * each match, the message is sent to the subscribing client.
 */
public class SubscriptionManager {
    //
    // Fields
    //
    Vector<Subscription> subscriptions = new Vector<Subscription>();
    Registry registry;
    //
    // Constructor
    //
    /**
     * Creates a new SubscriptionManager with no subscriptions.
     */
    public SubscriptionManager(Registry registry) {
	this.registry = registry;
    }
    //
    // Methods
    //
    public synchronized void subscribe(Sendable client, Object pattern) {
	Debug.debug("SubscriptionManager.subscribe: client=" + client + ", pattern=" + pattern);
	subscriptions.add(new Subscription(client, pattern, registry));
	Debug.debug("SubscriptionManager.subscribe: done");
    }
    public synchronized void unsubscribe(Sendable client, Object pattern) {
	Debug.warn("SubscriptionManager: haven't implemented unsubscribe yet");
    }
    /**
     * Removes all subscriptions for a client.
     */
    public synchronized void remove(Sendable client) {
	Debug.debug("SubscriptionManager.remove: " + client.getName());
	Iterator<Subscription> i = subscriptions.iterator();
	while (i.hasNext()) {
	    Subscription sub = i.next();
	    if (sub.getClient() == client) {
		Debug.debug("SubscriptionManager.remove: removed " + sub);
		i.remove();
	    }
	}

	Debug.debug("SubscriptionManager.remove: done");
    }
    /**
     * Gives a message to the SubscriptionMananger, meaning send it to any
     * subscribers whose pattern matches the message.
     */
    public void broadcast(KQMLPerformative msg, Sendable fromWhom) {
	Debug.debug("SubscriptionManager.broadcast: from=" + fromWhom + ", msg=" + msg);
	Enumeration e = subscriptions.elements();
	try {
	    while (e.hasMoreElements()) {
		Subscription sub = (Subscription)e.nextElement();
		Debug.debug("SubscriptionManager.broadcast: testing " + sub);
		if (sub.matches(msg)) {
		    Debug.debug("SubscriptionManager.broadcast: matched!");
		    sub.sendMessage(msg, fromWhom);
		}
	    }
	} catch (NoSuchElementException ex) {
	}

	Debug.debug("SubscriptionManager.broadcast: done");
    }
    //
    // Debugging
    //
    public Enumeration enumerateSubscriptions() {
	return subscriptions.elements();
    }
}
