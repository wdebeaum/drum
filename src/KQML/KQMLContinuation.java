package TRIPS.KQML;

/**
 * A KQMLContinuation encapsulates something we should do when we receive a
 * particular kind of KQML message. In particular, extensions of
 * StandardTripsModule need to use a KQMLContinuation in order to use the
 * sendWithContinuation method.
 */
public interface KQMLContinuation {
    /**
     * Called when we receive a message appropriate for this continuation.
     */
    public void receive(KQMLPerformative msg);
}
