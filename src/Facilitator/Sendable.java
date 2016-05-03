/*
 * Sendable.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 14 Dec 2000
 * $Id: Sendable.java,v 1.1.1.1 2005/01/14 19:48:08 ferguson Exp $
 */

package TRIPS.Facilitator;

import TRIPS.KQML.KQMLPerformative;

public interface Sendable {
    public String getName();
    public void sendTo(KQMLPerformative msg, Sendable fromWhom);
}
