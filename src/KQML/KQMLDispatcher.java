/*
 * KQMLDispatcher.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 21 Dec 2000
 * Time-stamp: <Fri Aug 31 12:07:55 EDT 2001 ferguson>
 *
 * gf: 12 Dec 2000:
 *     - Added bogusKQMLCompatibility flag for MapleSim use
 */
package TRIPS.KQML;

import java.io.EOFException;
import java.io.IOException;
import java.util.Map;
import java.util.HashMap;

/**
 * A KQMLDispatcher simply sits in an infinite loop, reading KQML
 * performatives from a given KQMLReader stream and passing them back to
 * a receiver that implements the KQMLReceiver interface.
 * <p>
 * Note that since interruptable IO has been deprecated by Sun (c.f. Java
 * Bug Database #4103109 and #4154947), there's no point in implementing
 * "safe" versions of the Thread.stop() and Thread.suspend() methods
 * (which are also deprecated) since Thread.interrupt() won't interrupt
 * blocked IO and 99.999% of the time that you want to interrupt a
 * KQMLDispatcher, it will be blocked in read(). Sigh.
 * <p>
 * Note: REQUEST and REPLY are not performatives in the 1997 KQML spec.
 */
public class KQMLDispatcher extends Thread {
    //
    // Fields
    //
    protected KQMLReceiver receiver = null;
    protected KQMLReader reader = null;
    // Flag added to stifle warnings from bogus control-A's
    protected boolean bogusKQMLCompatibility = false;
    // Flag to support forced shutdown of connection
    protected boolean shutdownInitiated = false;
    // Counter used for naming instances of this class
    protected static int counter = 0;
    // Map from (uppercase) reply IDs to continuations to call when we get
    // :in-reply-to
    protected Map<String,KQMLContinuation> replyContinuations = new HashMap<String,KQMLContinuation>();
    //
    // Constructor
    //
    public KQMLDispatcher(KQMLReceiver rec, KQMLReader in) {
	// Save owner of this reader to receive messages
	receiver = rec;
	// Save stream connected to Facilitator
	reader = in;
	// Set our name for use in debugging
	setName("KQML-Dispatcher-" + counter++);
    }
    //
    // Accessors
    //
    public void setBogusKQMLCompatibility(boolean flag) {
	bogusKQMLCompatibility = flag;
    }
    public boolean getBogusKQMLCompatibility() {
	return bogusKQMLCompatibility;
    }
    //
    // Runnable method
    //
    public void run() {
	try {
	    while (true) {
		try {
		    // Read a performative
		    KQMLPerformative msg = reader.readPerformative();
		    // Go process it
		    dispatchMessage(msg);
		} catch (KQMLBadCharacterException ex){
		    if (!bogusKQMLCompatibility) {
			receiver.handleException(ex);
		    }
		} catch (KQMLException ex) {
		    receiver.handleException(ex);
		}
	    }
	} catch (EOFException ex) {
	    receiver.receiveEOF();
	} catch (IOException ex) {
	    if (!shutdownInitiated) {
		receiver.handleException(ex);
	    }
	}
    }
    private void warn(String msg) {
	System.err.println(msg);
    }
    //
    // This is the best we can do for interrupting a KQMLDispatcher.
    // See notes at top of file for more discussion.
    // This method will (presumably) be called by some other thread
    // than ourselves.
    //
    public void shutdown() {
	shutdownInitiated = true;
	try {
	    // This should throw an IO exception in the blocked read in run()
	    System.err.println("KQMLDispatcher.shutdown: " + Thread.currentThread() + ": closing reader");
	    // For some reason, simply closing the reader hangs, perhaps
	    // because it is locked by the thread doing the read. If so,
	    // this ``solution'' using interrupt() might really be a race
	    // condition.
	    this.interrupt();
	    reader.close();
	    System.err.println("KQMLDispatcher.shutdown: " + Thread.currentThread() + ": done");
	} catch(IOException ex) {
	}
    }
    //
    // Highest level message dispatching
    //
    protected void dispatchMessage(KQMLPerformative msg) {
	// All messages must have a verb
        String verb = msg.getVerb();
	if (verb == null) {
	    receiver.receiveMessageMissingVerb(msg);
	    return;
	}
	// check if msg is a reply we've been waiting for
	KQMLObject replyIDobj = msg.getParameter(":in-reply-to");
	if (replyIDobj != null) {
	    String replyID = replyIDobj.stringValue().toUpperCase();
	    if (replyContinuations.containsKey(replyID)) {
		replyContinuations.get(replyID).receive(msg);
		replyContinuations.remove(replyID);
		return;
	    }
	}
	// Many messages have :content
        if (verb.equalsIgnoreCase("ask-if") ||
            verb.equalsIgnoreCase("ask-all") ||
	    verb.equalsIgnoreCase("ask-one") ||
            verb.equalsIgnoreCase("stream-all") ||
            verb.equalsIgnoreCase("tell") ||
            verb.equalsIgnoreCase("untell") ||
            verb.equalsIgnoreCase("deny") ||
            verb.equalsIgnoreCase("insert") ||
            verb.equalsIgnoreCase("uninsert") ||
            verb.equalsIgnoreCase("delete-one") ||
            verb.equalsIgnoreCase("delete-all") ||
            verb.equalsIgnoreCase("undelete") ||
            verb.equalsIgnoreCase("achieve") ||
            verb.equalsIgnoreCase("unachieve") ||
            verb.equalsIgnoreCase("advertise") ||
            verb.equalsIgnoreCase("subscribe") ||
            verb.equalsIgnoreCase("standby") ||
            verb.equalsIgnoreCase("register") ||
            verb.equalsIgnoreCase("forward") ||
            verb.equalsIgnoreCase("broadcast") ||
            verb.equalsIgnoreCase("transport-address") ||
            verb.equalsIgnoreCase("broker-one") ||
            verb.equalsIgnoreCase("broker-all") ||
            verb.equalsIgnoreCase("recommend-one") ||
            verb.equalsIgnoreCase("recommend-all") ||
            verb.equalsIgnoreCase("recruit-one") ||
            verb.equalsIgnoreCase("recruit-all") ||
            verb.equalsIgnoreCase("reply") ||
            verb.equalsIgnoreCase("request")) {
	    // Note: REQUEST and REPLY are not in the 1997 KQML spec
	    Object content = msg.getParameter(":content");
	    if (content == null) {
		receiver.receiveMessageMissingContent(msg);
		return;
	    }
	    // Now dispatch based on verb
	    if (verb.equalsIgnoreCase("ask-if")) {
		receiver.receiveAskIf(msg, content);
	    } else if (verb.equalsIgnoreCase("ask-all")) {
		receiver.receiveAskAll(msg, content);
	    } else if (verb.equalsIgnoreCase("ask-one")) {
		receiver.receiveAskOne(msg, content);
	    } else if (verb.equalsIgnoreCase("stream-all")) {
		receiver.receiveStreamAll(msg, content);
	    } else if (verb.equalsIgnoreCase("tell")) {
		receiver.receiveTell(msg, content);
	    } else if (verb.equalsIgnoreCase("untell")) {
		receiver.receiveUntell(msg, content);
	    } else if (verb.equalsIgnoreCase("deny")) {
		receiver.receiveDeny(msg, content);
	    } else if (verb.equalsIgnoreCase("insert")) {
		receiver.receiveInsert(msg, content);
	    } else if (verb.equalsIgnoreCase("uninsert")) {
		receiver.receiveUninsert(msg, content);
	    } else if (verb.equalsIgnoreCase("delete-one")) {
		receiver.receiveDeleteOne(msg, content);
	    } else if (verb.equalsIgnoreCase("delete-all")) {
		receiver.receiveDeleteAll(msg, content);
	    } else if (verb.equalsIgnoreCase("undelete")) {
		receiver.receiveUndelete(msg, content);
	    } else if (verb.equalsIgnoreCase("achieve")) {
		receiver.receiveAchieve(msg, content);
	    } else if (verb.equalsIgnoreCase("unachieve")) {
		receiver.receiveUnachieve(msg, content);
	    } else if (verb.equalsIgnoreCase("advertise")) {
		receiver.receiveAdvertise(msg, content);
	    } else if (verb.equalsIgnoreCase("subscribe")) {
		receiver.receiveSubscribe(msg, content);
	    } else if (verb.equalsIgnoreCase("standby")) {
		receiver.receiveStandby(msg, content);
	    } else if (verb.equalsIgnoreCase("register")) {
		receiver.receiveRegister(msg, content);
	    } else if (verb.equalsIgnoreCase("forward")) {
		receiver.receiveForward(msg, content);
	    } else if (verb.equalsIgnoreCase("broadcast")) {
		receiver.receiveBroadcast(msg, content);
	    } else if (verb.equalsIgnoreCase("transport-address")) {
		receiver.receiveTransportAddress(msg, content);
	    } else if (verb.equalsIgnoreCase("broker-one")) {
		receiver.receiveBrokerOne(msg, content);
	    } else if (verb.equalsIgnoreCase("broker-all")) {
		receiver.receiveBrokerAll(msg, content);
	    } else if (verb.equalsIgnoreCase("recommend-one")) {
		receiver.receiveRecommendOne(msg, content);
	    } else if (verb.equalsIgnoreCase("recommend-all")) {
		receiver.receiveRecommendAll(msg, content);
	    } else if (verb.equalsIgnoreCase("recruit-one")) {
		receiver.receiveRecruitOne(msg, content);
	    } else if (verb.equalsIgnoreCase("recruit-all")) {
		receiver.receiveRecruitAll(msg, content);
	    } else if (verb.equalsIgnoreCase("reply")) {
		receiver.receiveReply(msg, content);
	    } else if (verb.equalsIgnoreCase("request")) {
		receiver.receiveRequest(msg, content);
	    }
	// Other messages have no content
	} else if (verb.equalsIgnoreCase("eos")) {
            receiver.receiveEos(msg);
	} else if (verb.equalsIgnoreCase("error")) {
	    receiver.receiveError(msg);
	} else if (verb.equalsIgnoreCase("sorry")) {
	    receiver.receiveSorry(msg);
        } else if (verb.equalsIgnoreCase("ready")) {
            receiver.receiveReady(msg);
        } else if (verb.equalsIgnoreCase("next")) {
            receiver.receiveNext(msg);
        } else if (verb.equalsIgnoreCase("rest")) {
            receiver.receiveRest(msg);
        } else if (verb.equalsIgnoreCase("discard")) {
            receiver.receiveDiscard(msg);
        } else if (verb.equalsIgnoreCase("unregister")) {
            receiver.receiveUnregister(msg);
        } else {
            receiver.receiveOtherPerformative(msg);
        }
    }
    // add a continuation to be called when we get the given reply ID
    public void addReplyContinuation(String replyID, KQMLContinuation cont) {
	replyContinuations.put(replyID.toUpperCase(), cont);
    }
}
