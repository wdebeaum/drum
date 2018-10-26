package TRIPS.util.cwc;

import java.awt.Dimension;
import java.awt.Toolkit;
import java.util.regex.*;
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

  /** Get a WindowConfig from the command-line parameters of a
   * StandardCWCModule. (This would work with a regular old
   * StandardTripsModule, except that getParameter() is protected and we're not
   * in that package. Sigh.)
   */
  public static WindowConfig fromParameters(StandardCWCModule m) {
    String title = m.getParameter("-title");
    // title defaults to module name
    if (title == null) { title = m.getName(); }
    String geometry = m.getParameter("-geometry");
    Integer left = null, top = null, width = null, height = null;
    if (geometry != null) {
      Pattern p = Pattern.compile(
        "((?<w>\\d+)x(?<h>\\d+))?((?<x>[+-]\\d+)(?<y>[+-]\\d+))?",
	Pattern.CASE_INSENSITIVE
      );
      Matcher matcher = p.matcher(geometry);
      if (!matcher.matches()) {
	throw new RuntimeException("expected WxH+X+Y after -geometry, but got " + geometry);
      }
      boolean haveSize = (matcher.group("w") != null);
      if (haveSize) {
	width = Integer.valueOf(matcher.group("w"));
	height = Integer.valueOf(matcher.group("h"));
      }
      if (matcher.group("x") != null) {
	String xStr = matcher.group("x"), yStr = matcher.group("y");
	int x = Integer.parseInt(xStr), y = Integer.parseInt(yStr);
	boolean xNeg = xStr.startsWith("-"), yNeg = yStr.startsWith("-");
	// convert negative offsets (relative to bottom right corner) to
	// positive offsets (relative to top left corner)
	if (xNeg || yNeg) {
	  if (!haveSize) {
	    throw new RuntimeException("can't convert negative offsets in '-geometry " + geometry + "' to positive offsets without window dimensions");
	  }
	  Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
	  if (xNeg) { x += screenSize.getWidth() - width; }
	  if (yNeg) { y += screenSize.getHeight() - height; }
	}
	left = x; top = y; // convert int to Integer
      }
    }
    return new WindowConfig(title, left, top, width, height);
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
