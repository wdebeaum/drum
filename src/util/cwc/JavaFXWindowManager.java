package TRIPS.util.cwc;

import java.util.AbstractMap;
import java.util.Map;
import javafx.application.Platform;
import javafx.event.EventHandler;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;
import TRIPS.KQML.*;

/** Handle window management KQML messages for windows created with JavaFX. */
public class JavaFXWindowManager extends WindowManager<Stage> implements EventHandler<WindowEvent> {
  public JavaFXWindowManager(StandardCWCModule module) {
    super(module);
  }

  @Override
  public Map.Entry<KQMLToken,Stage> createWindow(WindowConfig windowConfig) {
    Stage window = new Stage();
    window.setOnCloseRequest(this);
    window.setOnShown(this);
    windowConfig.configure(window);
    KQMLToken id = nextID();
    id2win.put(id.stringValue(), window);
    win2id.put(window, id);
    return new AbstractMap.SimpleImmutableEntry<KQMLToken,Stage>(id, window);
  }

  @Override public void configureWindow(Stage window, WindowConfig config) {
    Platform.runLater(()->{
      try {
        config.configure(window);
      } catch (Exception e) {
	module.reportFailure(e);
      }
    });
  }

  @Override void closeWindow(Stage window, KQMLToken id) {
    Platform.runLater(()->{
      try {
	window.close();
	openedOrClosedWindow(id, false, true);
      } catch (Exception e) {
	module.reportFailure(e);
      }
    });
  }

  /** Handle window open/close events. */
  @Override public void handle(WindowEvent evt) {
    if (evt.getTarget() instanceof Stage) {
      Stage window = (Stage)evt.getTarget();
      KQMLToken id = getID(window);
      boolean opened = (evt.getEventType() == WindowEvent.WINDOW_SHOWN);
      // NOTE: we only get a close event when the *user* closes a window, not
      // when the system does it, so we use "opened" again for the bySystem
      // argument here
      openedOrClosedWindow(id, opened, opened);
    }
  }
}
