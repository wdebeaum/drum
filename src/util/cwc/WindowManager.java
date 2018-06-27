package TRIPS.util.cwc;

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JFrame;
import javafx.stage.Stage;

import TRIPS.KQML.*;

/** Handle window management KQML messages for windows of type W. */
public abstract class WindowManager<W> {
  StandardCWCModule module;
  // NOTE: id2win maps from String instead of KQMLToken because KQMLToken
  // doesn't implement hashCode
  HashMap<String,W> id2win;
  HashMap<W,KQMLToken> win2id;
  String idPrefix;
  int nextIdNumber;

  public WindowManager(StandardCWCModule module) {
    this.module = module;
    id2win = new HashMap<String,W>();
    win2id = new HashMap<W,KQMLToken>();
    idPrefix = module.getName().toLowerCase() + "-win-";
    nextIdNumber = 0;
  }

  KQMLToken nextID() {
    KQMLToken id = new KQMLToken(idPrefix + nextIdNumber);
    nextIdNumber++;
    return id;
  }

  public W getWindow(KQMLToken id) throws UnknownObject {
    String idStr = id.stringValue().toLowerCase();
    W window = id2win.get(idStr);
    if (window == null) {
      throw new UnknownObject(idStr);
    }
    return window;
  }

  public KQMLToken getID(W window) {
    // TODO? check for null
    return win2id.get(window);
  }

  /** Handle a window management request sent to this.module. */
  public boolean receiveWindowManagementRequest(KQMLPerformative msg, String verb, KQMLPerformative content) throws CWCException {
    KQMLPerformative answer = new KQMLPerformative("answer");
    if (verb.equals("get-windows")) {
      answer.setParameter(":windows", getWindowIDs());
    } else if (verb.equals("describe")) {
      KQMLToken id = Args.getTypedArgument(content, ":what", KQMLToken.class);
      answer.setParameter(":description", describeWindow(id));
    } else if (verb.equals("configure")) {
      configureWindow(content);
      answer = null;
    } else if (verb.equals("close")) {
      KQMLToken id = Args.getTypedArgument(content, ":what", KQMLToken.class);
      closeWindow(id);
      answer = null;
    } else {
      return false;
    }
    if (answer != null) {
      KQMLPerformative report = new KQMLPerformative("report");
      report.setParameter(":content", answer);
      KQMLPerformative reply = new KQMLPerformative("reply");
      reply.setParameter(":content", report);
      module.reply(msg, reply);
    }
    return true;
  }

  /** Create a window (W) according to the configuration from a request.
   * Returns a pair of ID and W. Must be called on the UI thread.
   */
  public abstract Map.Entry<KQMLToken,W> createWindow(WindowConfig windowConfig);

  /** Return a KQML list of window ID tokens for all open windows. */
  KQMLList getWindowIDs() {
    KQMLList ret = new KQMLList();
    for (String id : id2win.keySet()) {
      ret.add(new KQMLToken(id));
    }
    return ret;
  }

  /** Return a KQMLPerformative describing the window with the given ID. */
  KQMLPerformative describeWindow(KQMLToken id) throws UnknownObject {
    W window = getWindow(id);
    WindowConfig windowConfig = WindowConfig.fromWindow(window);
    windowConfig.id = id;
    return windowConfig.describe();
  }

  /** Set window title, position, and/or dimensions according to config.
   */
  void configureWindow(KQMLPerformative config) throws MissingArgument, InvalidArgument, UnknownObject {
    // NOTE: this is called to handle a configure request, so the argument is
    // :what, not :id
    KQMLToken id = Args.getTypedArgument(config, ":what", KQMLToken.class);
    W window = getWindow(id);
    WindowConfig windowConfig = new WindowConfig(config);
    configureWindow(window, windowConfig);
  }

  public abstract void configureWindow(W window, WindowConfig config);

  /** Close the window with the given ID. */
  void closeWindow(KQMLToken id) throws UnknownObject {
    final W window = getWindow(id);
    closeWindow(window, id);
  }

  abstract void closeWindow(W window, KQMLToken id);

  /** Close and forget all windows, without reporting their closure. */
  void restart() {
    ArrayList<W> windows = new ArrayList<W>(win2id.keySet());
    win2id.clear();
    id2win.clear();
    for (W w : windows) {
      closeWindow(w, null);
    }
  }

  /** Report that a window was opened or closed. Also update id2win and win2id.
   * Do nothing if id is null (which happens on restart()).
   */
  void openedOrClosedWindow(KQMLToken id, boolean opened, boolean bySystem) {
    if (id == null) return;
    KQMLPerformative tell = new KQMLPerformative("tell");
    KQMLPerformative report = new KQMLPerformative("report");
    tell.setParameter(":content", report);
    KQMLPerformative cont = new KQMLPerformative(opened ? "opened" : "closed");
    report.setParameter(":content", cont);
    cont.setParameter(":what", id);
    cont.setParameter(":who", (bySystem ? "sys" : "usr"));
    module.send(tell);
    if (!opened) { // closed, forget this window
      try {
	win2id.remove(getWindow(id));
	id2win.remove(id.stringValue());
      } catch (Exception e) {} // swallow exceptions from getWindow that can't happen
    }
  }
}
