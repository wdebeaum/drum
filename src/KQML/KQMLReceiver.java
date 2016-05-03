/*
 * KQMLReceiver.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 16 Feb 1999
 * Time-stamp: <Fri Aug 31 12:07:39 EDT 2001 ferguson>
 *
 * KQMLReceiver is the interface used to callback from a KQMLReaderThread.
 *
 * Note: REQUEST and REPLY are not performatives in the 1997 KQML spec.
 */

package TRIPS.KQML;

import java.io.IOException;

public interface KQMLReceiver {
    //
    // Called on EOF
    //
    public void receiveEOF();
    //
    // Incorrect messages
    //
    public void receiveMessageMissingVerb(KQMLPerformative msg);
    public void receiveMessageMissingContent(KQMLPerformative msg);
    //
    // Messages with content
    //
    public void receiveAskIf(KQMLPerformative msg, Object content);
    public void receiveAskAll(KQMLPerformative msg, Object content);
    public void receiveAskOne(KQMLPerformative msg, Object content);
    public void receiveStreamAll(KQMLPerformative msg, Object content);
    public void receiveTell(KQMLPerformative msg, Object content);
    public void receiveUntell(KQMLPerformative msg, Object content);
    public void receiveDeny(KQMLPerformative msg, Object content);
    public void receiveInsert(KQMLPerformative msg, Object content);
    public void receiveUninsert(KQMLPerformative msg, Object content);
    public void receiveDeleteOne(KQMLPerformative msg, Object content);
    public void receiveDeleteAll(KQMLPerformative msg, Object content);
    public void receiveUndelete(KQMLPerformative msg, Object content);
    public void receiveAchieve(KQMLPerformative msg, Object content);
    public void receiveUnachieve(KQMLPerformative msg, Object content);
    public void receiveAdvertise(KQMLPerformative msg, Object content);
    public void receiveUnadvertise(KQMLPerformative msg, Object content);
    public void receiveSubscribe(KQMLPerformative msg, Object content);
    public void receiveStandby(KQMLPerformative msg, Object content);
    public void receiveRegister(KQMLPerformative msg, Object content);
    public void receiveForward(KQMLPerformative msg, Object content);
    public void receiveBroadcast(KQMLPerformative msg, Object content);
    public void receiveTransportAddress(KQMLPerformative msg, Object content);
    public void receiveBrokerOne(KQMLPerformative msg, Object content);
    public void receiveBrokerAll(KQMLPerformative msg, Object content);
    public void receiveRecommendOne(KQMLPerformative msg, Object content);
    public void receiveRecommendAll(KQMLPerformative msg, Object content);
    public void receiveRecruitOne(KQMLPerformative msg, Object content);
    public void receiveRecruitAll(KQMLPerformative msg, Object content);
    public void receiveReply(KQMLPerformative msg, Object content);
    public void receiveRequest(KQMLPerformative msg, Object content);
    //
    // Messages without content
    //
    public void receiveEos(KQMLPerformative msg);
    public void receiveError(KQMLPerformative msg);
    public void receiveSorry(KQMLPerformative msg);
    public void receiveReady(KQMLPerformative msg);
    public void receiveNext(KQMLPerformative msg);
    public void receiveRest(KQMLPerformative msg);
    public void receiveDiscard(KQMLPerformative msg);
    public void receiveUnregister(KQMLPerformative msg);
    //
    // Any other performative
    //
    public void receiveOtherPerformative(KQMLPerformative msg);
    //
    // Exception handler
    //
    public void handleException(IOException ex);
}
