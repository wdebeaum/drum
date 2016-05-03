/*
 * RunnableWithExceptions.java
 *
 * William de Beaumont, wbeaumont@ihmc.us, 22 Jun 2011
 * $Id: RunnableWithExceptions.java,v 1.1 2011/06/22 17:17:36 wdebeaum Exp $
 */

package TRIPS.TripsModule;

import TRIPS.KQML.KQMLPerformative;
import javax.swing.SwingUtilities;

/**
 * Streamlines common cases of handling Exceptions in Runnables in a
 * StandardTripsModule. For example, in the receiveRequest method of a subclass
 * of StandardTripsModule:
 * <pre>
 * (new RunnableWithExceptions(this, msg) {
 *   public void runWithExceptions() throws Exception {
 *     // do stuff
 *   }
 * }).swingInvokeAndWait();
 * </pre>
 * Don't forget to add YourClass$$n.class to your Makefile, where n is 1 to the
 * number of anonymous classes (like the above) in YourClass.
 */
public abstract class RunnableWithExceptions implements Runnable {
  /** the module to send messages from */
  protected StandardTripsModule tripsModule;
  /** the message to reply to */
  protected KQMLPerformative msg;
  /** send (tell :content (ok)) on success? default true */
  protected boolean sendOK;
  /** send an error reply on exceptions? default true */
  protected boolean sendError;
  /** print a stack trace on exceptions? default true */
  protected boolean printStackTrace;
  /** did this succeed? i.e. did we see no Exceptions? */
  protected boolean success;
  public boolean getSuccess() { return success; }
  
  /**
   * Take some action. Implement this instead of run() to get the extra
   * Exception/success handling.
   */
  public abstract void runWithExceptions() throws Exception;

  public RunnableWithExceptions(StandardTripsModule tripsModule, KQMLPerformative msg, boolean sendOK, boolean sendError, boolean printStackTrace) {
    this.tripsModule = tripsModule;
    this.msg = msg;
    this.sendOK = sendOK;
    this.sendError = sendError;
    this.printStackTrace = printStackTrace;
    success = false;
  }

  public RunnableWithExceptions(StandardTripsModule tripsModule, KQMLPerformative msg, boolean sendOK, boolean sendError) {
    this(tripsModule, msg, sendOK, sendError, true);
  }

  public RunnableWithExceptions(StandardTripsModule tripsModule, KQMLPerformative msg, boolean sendOK) {
    this(tripsModule, msg, sendOK, true);
  }

  public RunnableWithExceptions(StandardTripsModule tripsModule, KQMLPerformative msg) {
    this(tripsModule, msg, true);
  }

  /**
   * Construct a RunnableWithExceptions that doesn't send any messages.
   */
  public RunnableWithExceptions(boolean printStackTrace) {
    this(null, null, false, false, printStackTrace);
  }

  /**
   * Construct a RunnableWithExceptions that just prints a stack trace on
   * Exceptions (no messages).
   */
  public RunnableWithExceptions() {
    this(true);
  }

  /**
   * Return a fresh copy of an "OK" reply. This is necessary because reply()
   * modifies its argument.
   */
  protected static KQMLPerformative ok() {
    KQMLPerformative ret = null;
    try {
      ret = KQMLPerformative.fromString("(tell :content (ok))");
    } catch (Exception ex) {
      // should never happen
    }
    return ret;
  }

  protected void handleException(Exception ex) {
    if (printStackTrace)
      ex.printStackTrace();
    if (sendError)
      tripsModule.errorReply(msg, ex.toString());
    success = false;
  }

  protected void handleSuccess() {
    if (sendOK)
      tripsModule.reply(msg, ok());
    success = true;
  }

  public void run() {
    try {
      runWithExceptions();
    } catch (Exception ex) {
      handleException(ex);
      return;
    }
    handleSuccess();
  }

  /**
   * Run this on the Swing thread, handling exceptions from invokeAndWait
   * itself the same way as exceptions from runWithExceptions.
   * @return true iff the run was successful
   */
  public boolean swingInvokeAndWait() {
    try {
      SwingUtilities.invokeAndWait(this);
    } catch (Exception ex) {
      handleException(ex);
    }
    return success;
  }
}
