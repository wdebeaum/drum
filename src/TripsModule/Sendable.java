/*
 * File: Sendable.java
 * Creator: George Ferguson
 * Created: Mon Jul 23 20:34:50 2007
 * Time-stamp: <Mon Jul 23 20:39:20 EDT 2007 ferguson>
 */

package TRIPS.TripsModule;

import TRIPS.KQML.KQMLPerformative;

/**
 * Interface implemented by class which can send KQML messages.
 * This ought to be implemented by StandardTripsModule, but it doesn't
 * matter that it isn't.
 */
public interface Sendable {
    public void send(KQMLPerformative msg);
}

