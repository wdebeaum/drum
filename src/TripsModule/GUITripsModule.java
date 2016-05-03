package TRIPS.TripsModule;

import javax.swing.JFrame;
import java.awt.event.WindowListener;
import java.awt.event.WindowEvent;
import TRIPS.util.GeometrySpec;

public class GUITripsModule extends StandardTripsModule {
  protected JFrame frame;
  protected String title = "Untitled TRIPS module";
  protected GeometrySpec geometry;
  protected String getDefaultGeometry() { return "320x240+0+0"; }

  // copy these three methods into your extending class, changing "GUITripsModule" to your class' name
  public static void main(String argv[]) {
    new GUITripsModule(argv, true).run();
  }
  public GUITripsModule(String argv[], boolean isApplication) {
    super(argv, isApplication);
  }
  public GUITripsModule(String argv[]) {
    this(argv, false);
  }

  // called by StandardTripsModule::run()
  public void init() {
    super.init();
    handleParameters();
    createFrame();
    sendSubscriptions();
    ready();
  }

  // override this to handle your parameters, and call super()
  protected void handleParameters() {
    String value;
    if ((value = getParameter("-title")) != null) {
      title = value;
    }
    if ((value = getParameter("-geometry")) != null) {
      geometry = new GeometrySpec(value);
    } else {
      geometry = new GeometrySpec(getDefaultGeometry());
    }	    
  }

  protected void createFrame() {
    frame = new JFrame(title);
    addWidgetsToFrame();
    frame.pack();
    geometry.setLocationOfFrame(frame);
    frame.setVisible(true);
  }

  protected void addWidgetsToFrame() {
    // e.g. frame.add(new JLabel("hi"));
  }

  protected void sendSubscriptions() {
    // e.g. sendMessage("(subscribe :content (tell &key :content (hello . *)))");
  }
}
