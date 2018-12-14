package TRIPS.util.cwc;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

import TRIPS.KQML.*;
import TRIPS.TripsModule.StandardTripsModule;

public class StandardCWCModule extends StandardTripsModule {
  boolean alreadyDeclaredCapabilities;
  public WindowManager windowManager;

  public StandardCWCModule(String[] argv) {
    super(argv);
  }

  // override protected to public so that e.g. SwingWindowManager can reply
  @Override
  public void reply(KQMLPerformative msg, KQMLPerformative replymsg) {
    super.reply(msg, replymsg);
  }

  // ditto
  @Override
  public synchronized void send(KQMLPerformative msg) {
    super.send(msg);
  }

  // override protected to public so that WindowConfig can get parameters
  @Override
  public String getParameter(String parm) {
    return super.getParameter(parm);
  }

  // SWM also needs the module name so it can include it in IDs
  /** Get the name of this module. */
  public String getName() {
    return name;
  }

  /** Reply to msg with a report whose content is action; if msg is null, just
   * tell the report.
   */
  public void report(KQMLPerformative msg, KQMLObject action) {
    boolean replying = (msg != null);
    KQMLPerformative top = new KQMLPerformative(replying ? "reply" : "tell");
    KQMLPerformative report = new KQMLPerformative("report");
    report.setParameter(":content", action);
    top.setParameter(":content", report);
    if (replying) {
      reply(msg, top);
    } else {
      send(top);
    }
  }
  public void report(KQMLPerformative action) { report(null, action); }

  /** Report an answer with a single argument. */
  public void answer(KQMLPerformative msg, String key, KQMLObject val) {
    KQMLPerformative ans = new KQMLPerformative("answer");
    ans.setParameter(key, val);
    report(msg, ans);
  }

  public void answer(KQMLPerformative msg, String key, String val) {
    answer(msg, key, new KQMLToken(val));
  }

  /** Convert a Java exception to a KQML failure structure. */
  public static KQMLPerformative exceptionToFailure(Exception e) {
    if (e instanceof CWCException) {
      return ((CWCException)e).toFailure();
    }
    KQMLPerformative failure = new KQMLPerformative("failure");
    failure.setParameter(":type", "cannot-perform");
    KQMLPerformative reason = new KQMLPerformative("program-error");
    StringWriter stackTrace = new StringWriter();
    e.printStackTrace(new PrintWriter(stackTrace));
    reason.setParameter(":message", new KQMLString(stackTrace.toString()));
    failure.setParameter(":reason", reason);
    return failure;
  }

  /** Report the given exception as a failure. If msg is given, reply to it,
   * otherwise tell.
   */
  public void reportFailure(Exception e, KQMLPerformative msg) {
    KQMLPerformative failure = exceptionToFailure(e);
    report(msg, failure);
  }

  // just in case I mix up the args...
  public void reportFailure(KQMLPerformative msg, Exception e) {
    reportFailure(e, msg);
  }

  public void reportFailure(Exception e) { reportFailure(e, null); }

  /** Send all subscription messages. Called by init(). */
  public void subscribe() throws IOException {
    send(KQMLPerformative.fromString(
      "(subscribe :content (tell &key :content (i-am-here &key :who cwmsagent)))"));
    send(KQMLPerformative.fromString(
      "(subscribe :content (request &key :content (restart)))"));
  }

  /** Reset module state and resend some initial messages. Called by init(),
   * and upon receiving a restart request.
   */
  public void restart() {
    alreadyDeclaredCapabilities = false;
    try {
      send(KQMLPerformative.fromString(
	"(request :content (are-you-there :who cwmsagent))"));
    } catch (IOException e) {
      System.err.println("error attempting to send are-you-there: " + e.toString());
    }
    if (windowManager != null) windowManager.restart();
  }

  /** Send all define-service messages. */
  public void declareCapabilities() throws IOException {
    // to be implemented by modules with capabilities
  }

  /** Do declareCapabilities() unless we already did so since the last restart.
   */
  public void declareCapabilitiesOnce() throws IOException {
    if (alreadyDeclaredCapabilities) {
      return;
    }
    alreadyDeclaredCapabilities = true;
    declareCapabilities();
  }

  @Override
  public void init() {
    super.init();
    try {
      subscribe();
    } catch (IOException e) {
      System.err.println("error attempting to subscribe: " + e.toString());
      return;
    }
    restart();
  }

  @Override
  public void receiveRequest(KQMLPerformative msg, Object contentObj) {
    try {
      KQMLPerformative content = Args.getContentPerformative(msg, contentObj);
      String verb = content.getVerb().toLowerCase();
      if (!receiveRequest(msg, verb, content)) {
	throw new UnknownAction(verb);
      }
    } catch (Exception e) {
      reportFailure(e, msg);
    }
  }

  /** Slightly nicer interface for receiveRequest.
   * @param msg the whole message
   * @param verb the lowercase verb of the content performative
   * @param content the content performative (as KQMLPerformative not Object)
   * @return true iff the request was handled
   * Subclasses should override this like so:
   * <pre>
   * if (verb.equals("my-request-verb")) {
   *   ...
   * } else if (verb.equals("my-other-request-verb")) {
   *   ...
   * } else {
   *   return super.receiveRequest(msg, verb, content);
   * }
   * return true;
   * </pre>
   */
  public boolean receiveRequest(KQMLPerformative msg, String verb, KQMLPerformative content) throws CWCException, IOException {
    if (verb.equals("restart")) {
      restart();
      return true;
    } else if (windowManager != null) {
      return windowManager.receiveWindowManagementRequest(msg, verb, content);
    } else {
      return false;
    }
  }

  @Override
  public void receiveTell(KQMLPerformative msg, Object contentObj) {
    try {
      KQMLPerformative content = Args.getContentPerformative(msg, contentObj);
      String verb = content.getVerb().toLowerCase();
      if (!receiveTell(msg, verb, content)) {
	throw new UnknownAction(verb);
      }
    } catch (Exception e) {
      reportFailure(e, msg);
    }
  }

  /** Slightly nicer interface for receiveTell. See doc for receiveRequest. */
  public boolean receiveTell(KQMLPerformative msg, String verb, KQMLPerformative content) throws CWCException, IOException {
    if (verb.equals("i-am-here")) {
      KQMLToken who = Args.getTypedArgument(content, ":who", KQMLToken.class);
      if (who.toString().toLowerCase().equals( "cwmsagent")) {
	declareCapabilitiesOnce();
      } // don't care who else is here
      return true;
    } else {
      return false;
    }
  }
}
