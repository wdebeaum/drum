package TRIPS.util.cwc;

import javax.swing.JFrame;
import javafx.application.Platform;
import javafx.stage.Stage;
import TRIPS.KQML.*;

/** The configuration of a window, including its title, position, and size. All
 * fields are nullable, and configuring a window will omit setting the fields
 * that are null.
 */
public class WindowConfig {
  public KQMLToken id; // not really part of the config, but convenient
  public String title;
  // NOTE: JavaFX has these as doubles, but I think most platforms use ints
  // under the hood, so I don't see the point in supporting fractional pixels
  public Integer left, top, width, height;

  /** Null constructor. */
  public WindowConfig() {}

  /** Construct a configuration from individual field values. */
  public WindowConfig(String title, Integer left, Integer top, Integer width, Integer height) {
    this.title = title;
    this.left = left;
    this.top = top;
    this.width = width;
    this.height = height;
    // TODO? check that width and height are positive
  }
  
  /** Extract configuration from an existing Swing window. */
  public WindowConfig(JFrame window) {
    this(
      window.getTitle(),
      new Integer(window.getX()),
      new Integer(window.getY()),
      new Integer(window.getWidth()),
      new Integer(window.getHeight())
    );
  }

  // FIXME does this have to be called from the UI thread?
  /** Extract configuration from an existing JavaFX window. */
  public WindowConfig(Stage window) {
    this(
      window.getTitle(),
      new Integer((int)window.getX()),
      new Integer((int)window.getY()),
      new Integer((int)window.getWidth()),
      new Integer((int)window.getHeight())
    );
  }

  /** Dynamically call either JFrame or Stage version of WindowConfig
   * constructor.
   */
  public static WindowConfig fromWindow(Object window) {
    // FIXME would be nice if we didn't have to explicitly mention specific W
    // types here, but Java is being obstinate
    if (window instanceof JFrame) {
      return new WindowConfig((JFrame)window);
    } else if (window instanceof Stage) {
      return new WindowConfig((Stage)window);
    } else {
      throw new RuntimeException("WindowConfig.fromWindow() expected a Stage or a JFrame, but got a " + window.getClass().getSimpleName());
    }
  }

  /** Extract configuration from a KQML description. */
  public WindowConfig(KQMLPerformative config) throws InvalidArgument {
    this(
      Args.getTypedArgument(config, ":title", String.class, null),
      Args.getTypedArgument(config, ":left", Integer.class, null),
      Args.getTypedArgument(config, ":top", Integer.class, null),
      Args.getTypedArgument(config, ":width", Integer.class, null),
      Args.getTypedArgument(config, ":height", Integer.class, null)
    );
  }

  /** Convert configuration to a KQML performative of the form:
   * (window :id foo :title "foo" :left # :top # :width # :height #)
   * with null values left out.
   */
  public KQMLPerformative describe() {
    KQMLPerformative desc = new KQMLPerformative("window");
    if (id	!= null) {
      desc.setParameter(":id",		id);
    }
    if (title	!= null) {
      desc.setParameter(":title",	new KQMLString(title));
    }
    if (left	!= null) {
      desc.setParameter(":left",	new KQMLToken(left.toString()));
    }
    if (top	!= null) {
      desc.setParameter(":top",		new KQMLToken(top.toString()));
    }
    if (width	!= null) {
      desc.setParameter(":width",	new KQMLToken(width.toString()));
    }
    if (height	!= null) {
      desc.setParameter(":height",	new KQMLToken(height.toString()));
    }
    return desc;
  }

  /** Apply configuration to an existing Swing window. Must be called from the
   * UI thread. */
  public void configure(JFrame window) {
    if (title	!= null) { window.setTitle	(title		); }
    // FIXME should be || not &&, but then need to get current settings for
    // unspecified parts
    if (left	!= null &&
	top	!= null) { window.setLocation	(left,	top	); }
    if (width	!= null &&
	height	!= null) { window.setSize	(width,	height	); }
  }

  /** Apply configuration to an existing JavaFX window. Must be called from the
   * UI thread. */
  public void configure(Stage window) {
    if (title	!= null) { window.setTitle	(title); }
    if (left	!= null) { window.setX		(left	.doubleValue()); }
    if (top	!= null) { window.setY		(top	.doubleValue()); }
    if (width	!= null) { window.setWidth	(width	.doubleValue()); }
    if (height	!= null) { window.setHeight	(height	.doubleValue()); }
  }
}
