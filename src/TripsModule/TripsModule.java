/*
 * TripsModule.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 13 Dec 2000
 * $Id: TripsModule.java,v 1.1.1.1 2005/01/14 19:48:14 ferguson Exp $
 */

package TRIPS.TripsModule;

import TRIPS.KQML.KQMLReceiver;

/**
 * TripsModule represents a class of objects that are both Runnable
 * and KQMLReceiver's.
 * <p>
 * The ``StandardTripsModule'' class implements this interface and
 * provides additional functionality useful to TRIPS components. You
 * almost certainly want to extend that class rather than implement
 * this interface yourself, but it's here if for some reason you can't
 * derive your class from StandardTripsModule.
 */
public interface TripsModule extends Runnable, KQMLReceiver {
}
