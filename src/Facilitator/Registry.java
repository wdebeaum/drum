/*
 * Registry.java
 *
 * George Ferguson, ferguson@cs.rochester.edu,  4 Apr 2000
 * $Id: Registry.java,v 1.1.1.1 2005/01/14 19:48:08 ferguson Exp $
 */
package TRIPS.Facilitator;

import java.util.Hashtable;
import java.util.Enumeration;
import java.util.Vector;
import java.util.NoSuchElementException;

/**
 * This Registry provides both name->client lookups (method
 * lookupClientByName()) and also client->name lookups (method
 * lookupNameOfClient()). The former is one-to-one, the latter is
 * many-to-one since there can be multiple names registered for
 * a given client. However, we always return the "canonical" name, whatever
 * that is (I think it's the first registered name that hasn't been
 * unregistered).
 * <p>
 * The name->client lookups are used for delivering message to :receiver's.
 * The client->name lookups are used for filling in :sender when none is
 * is given.
 */
public class Registry {
    //
    // Fields
    //
    Hashtable clientsByName = new Hashtable();
    Hashtable entriesByClient = new Hashtable();
    //
    // Constructor
    //
    public Registry() {
    }
    //
    // Methods
    //
    /**
     * Add NAME for CLIENT to registry.
     *
     */
    public void add(String name, Sendable client) {
	name = name.toUpperCase();
	//Debug.debug("Registry.add: adding client=" + client + " with name \"" + name + "\"");
	// Store name->client mapping
	clientsByName.put(name, client);
	// Store reverse lookup info
	RegistryEntry entry = (RegistryEntry)entriesByClient.get(client);
	if (entry == null) {
	    entry = new RegistryEntry(client);
	    entriesByClient.put(client, entry);
	}
	entry.addName(name);
	//Debug.debug("Registry.add: done");
    }
    /**
     * Remove NAME for CLIENT from registry.
     *
     */
    public void remove(String name, Sendable client) {
	name = name.toUpperCase();
	//Debug.debug("Registry.remove: removing obj with name \"" + name + "\"");
	// Remove name->client mapping
	clientsByName.remove(name);
	// Remove reverse lookup info
	RegistryEntry entry = (RegistryEntry)entriesByClient.get(client);
	if (entry == null) {
	    Debug.error("Registry.remove: attempt to remove unregistered name \"" + "\" for client " + client);
	} else {
	    entry.removeName(name);
	}
	//Debug.debug("Registry.remove: done");
    }
    /**
     * Remove all names for CLIENT from registry.
     *
     */
    public void remove(Sendable client) {
	//Debug.debug("Registry.remove: removing client " + client);
	RegistryEntry entry = (RegistryEntry)entriesByClient.get(client);
	if (entry == null) {
	    // What to do here? NoSuchElementException?
	} else {
	    // Remove name->client mappings
	    Enumeration names = entry.enumerateNames();
	    try {
		while (names.hasMoreElements()) {
	    	    String name = (String)names.nextElement();
	    	    clientsByName.remove(name);
		}
	    } catch (NoSuchElementException ex) {
	    }
	    // Remove reverse lookup info
	    entriesByClient.remove(client);
	}
	//Debug.debug("Registry.remove: done");
    }
    /**
     * Return Client registered under NAME, or null if no client is registered
     * with that name.
     *
     */
    public Sendable lookupClientByName(String name) {
	return (Sendable)(clientsByName.get(name.toUpperCase()));
    }
    /**
     * If CLIENT is registered under one or more names, returns the ``canonical''
     * name (typically the first registered and not unregistered name), othewise
     * return null if no names are registered.
     *
     */
    public String lookupNameOfClient(Sendable client) {
	RegistryEntry entry = (RegistryEntry)entriesByClient.get(client);
	if (entry == null) {
	    return null;
	} else {
	    return entry.getCanonicalName();
	}
    }
    /**
     * Return an enumeration of all names of CLIENT. If CLIENT has never been
     * registered at all, returns an empty enumeration.
     *
     */
    public Enumeration enumerateNamesOfClient(Sendable client) {
	RegistryEntry entry = (RegistryEntry)entriesByClient.get(client);
	if (entry == null) {
	    Vector empty = new Vector();
	    return empty.elements();
	} else {
	    return entry.enumerateNames();
	}
    }
    /**
     * Return an enuemration of all registered Clients.
     *
     */
    public Enumeration enumerateClients() {
	return entriesByClient.keys();
    }
    /**
     * Return an enumeration of all names registerd to any Client.
     *
     */
    public Enumeration enumerateNames() {
	return clientsByName.keys();
    }
}

/**
 * RegistryEntry's store information about the multiple possible names that can
 * be registered for a Client.
 *
 */
class RegistryEntry {
    // Fields
    Sendable client;
    Vector names = new Vector();
    // Constructor
    RegistryEntry(Sendable client) {
	this.client = client;
    }
    // Methods
    public void addName(String name) {
    	names.addElement(name);
    }
    public void removeName(String name) {
    	names.removeElement(name);
    }
    public String getCanonicalName() {
        try {
            return (String)names.firstElement();
        } catch (NoSuchElementException ex) {
            return null;
        }
    }
    public Enumeration enumerateNames() {
        return names.elements();
    }
}
