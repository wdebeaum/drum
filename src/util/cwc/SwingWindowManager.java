package TRIPS.util.cwc;

import java.util.AbstractMap;
import java.util.Map;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import TRIPS.KQML.*;

/** Handle window management KQML messages for windows created with Swing. */
public class SwingWindowManager extends WindowManager<JFrame> implements WindowListener {
  public SwingWindowManager(StandardCWCModule module) {
    super(module);
  }

  @Override
  public Map.Entry<KQMLToken,JFrame> createWindow(WindowConfig windowConfig) {
    JFrame window = new JFrame();
    window.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    window.addWindowListener(this);
    windowConfig.configure(window);
    KQMLToken id = nextID();
    id2win.put(id.stringValue(), window);
    win2id.put(window, id);
    return new AbstractMap.SimpleImmutableEntry<KQMLToken,JFrame>(id, window);
  }

  @Override
  public void configureWindow(JFrame window, WindowConfig config) {
    SwingUtilities.invokeLater(()->{
      try {
        config.configure(window);
      } catch (Exception e) {
	module.reportFailure(e);
      }
    });
  }

  @Override void closeWindow(JFrame window, KQMLToken id) {
    SwingUtilities.invokeLater(()->{
      try {
	window.dispose();
	openedOrClosedWindow(id, false, true);
      } catch (Exception e) {
	module.reportFailure(e);
      }
    });
  }

  @Override public void windowActivated(WindowEvent evt) { /* TODO? */ }

  @Override public void windowClosed(WindowEvent evt) {
    if (evt.getWindow() instanceof JFrame) {
      JFrame window = (JFrame)evt.getWindow();
      KQMLToken id = getID(window);
      if (id != null) { // didn't get windowClosing first
	openedOrClosedWindow(id, false, true);
      }
    }
  }

  @Override public void windowClosing(WindowEvent evt) {
    if (evt.getWindow() instanceof JFrame) {
      JFrame window = (JFrame)evt.getWindow();
      KQMLToken id = getID(window);
      openedOrClosedWindow(id, false, false);
    }
  }

  @Override public void windowDeactivated(WindowEvent evt) { /* TODO? */ }
  @Override public void windowDeiconified(WindowEvent evt) { /* TODO? */ }
  @Override public void windowIconified(WindowEvent evt) { /* TODO? */ }

  @Override public void windowOpened(WindowEvent evt) {
    if (evt.getWindow() instanceof JFrame) {
      JFrame window = (JFrame)evt.getWindow();
      KQMLToken id = getID(window);
      openedOrClosedWindow(id, true, true);
    }
  }
}
