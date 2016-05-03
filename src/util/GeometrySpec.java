/*
 * GeometrySpec.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 10 Mar 1998
 * Time-stamp: <Fri Jul 27 11:18:38 EDT 2007 ferguson>
 */

package TRIPS.util;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.GraphicsConfiguration;
import java.awt.Insets;
import java.awt.Window;
import TRIPS.util.IndexedString;

/**
 * A class for parsing and representing X geometry specifications, based
 * on the X Window System specification and the code in XParseGeom.c and
 * WMGeom.c in particular.
 *
 * A geometry specification is one of the following forms:
 * <PRE>
 *     =WxH?X?Y
 *     =WxH
 *     =?X?Y
 * </PRE>
 * where the leading equals sign is optional and the question mark
 * stands for either a plus sign (`=') or a minus sign (`-'). W stands for
 * width, H for height, X for the horizontal position, and Y for the vertical
 * position. Negative positions are interpreted relative to the right
 * and bottom of the screen.
 * 
 */
public class GeometrySpec {
    // Members
    /**
     * Width specified in geometry spec, if <B>widthSpecified</B> is true.
     */
    public int width;
    /**
     * Height specified in geometry spec, if <B>heightSpecified</B> is true.
     */
    public int height;
    /**
     * X coord specified in geometry spec, if <B>xSpecified</B> is true.
     */
    public int x;
    /**
     * Y coord specified in geometry spec, if <B>ySpecified</B> is true.
     */
    public int y;
    /**
     * True if width was specified in geometry spec.
     */
    public boolean widthSpecified;
    /**
     * True if height was specified in geometry spec.
     */
    public boolean heightSpecified;
    /**
     * True if x coord was specified in geometry spec.
     */
    public boolean xSpecified;
    /**
     * True if y coord was specified in geometry spec.
     */
    public boolean ySpecified;
    /**
     * True if x coord was preceded by a minus sign.
     */
    public boolean xNegative;
    /**
     * True if y coord was preceded by a minus sign.
     */
    public boolean yNegative;
    // Constructor
    /**
     * Parses given geometry spec and returns a new object describing it.
     *
     * @param	s	Geometry spec to parse
     */
    public GeometrySpec(String s) {
	IndexedString istr = new IndexedString(s);
	// Defaults
	widthSpecified = heightSpecified = xSpecified = ySpecified = false;
	// Skip whitespace
	istr.skipWhitespace();
	// Skip optional leading equals sign
	if (istr.peekChar() == '=') {
	    istr.nextChar();
	}
	// Width if given
	char ch = istr.peekChar();
	if (ch != '\0' && ch != '+' && ch != '-' && ch != 'x') {
	    width = istr.nextInt();
	    widthSpecified = true;
	}
	// Height if given
	ch = istr.peekChar();
	if (ch == 'x' || ch == 'X') {
	    istr.nextChar();
	    height = istr.nextInt();
	    heightSpecified = true;
	}
	// X if given
	ch = istr.peekChar();
	if (ch == '+' || ch == '-') {
	    istr.nextChar();
	    x = istr.nextInt();
	    xSpecified = true;
	    xNegative = (ch == '-');
	}
	// Y if given
	ch = istr.peekChar();
	if (ch == '+' || ch == '-') {
	    istr.nextChar();
	    y = istr.nextInt();
	    ySpecified = true;
	    yNegative = (ch == '-');
	}
	// String must now be at end, but we won't bother checking that...
    }
    /**
     * Sets the location of the given frame from the components of the
     * GeometrySpec. Negative X and Y coordinates are interpreted relative
     * to the right and bottom of the frame's screen, taking into account
     * its width and height, respectively.
     *
     * @param frame Frame of which to set location.
     */
    public void setLocationOfFrame(Frame frame) {
	// Now adjust negative coords for screen size
	Dimension winsize = frame.getSize();
	Toolkit toolkit = frame.getToolkit();
	GraphicsConfiguration gc = frame.getGraphicsConfiguration();
        Insets insets = toolkit.getScreenInsets(gc);
	Dimension scrsize = toolkit.getScreenSize();
	int xloc = x;
	int yloc = y;
	if (xSpecified && xNegative) {
	    xloc = scrsize.width - insets.right - x - winsize.width;
	}
	if (ySpecified && yNegative) {
	    yloc = scrsize.height - insets.bottom - y - winsize.height;
	}
	// TODO: what about top and right screen insets? what about frame insets (window decorations)?
	// can only get frame insets after pack() or show()
	frame.setLocation(xloc, yloc);
    }
    /**
     * Sets the size and location of the given frame from the components of the
     * GeometrySpec.
     * @see #setLocationOfFrame(Frame)
     */
    public void setBoundsOfFrame(Frame frame) {
      Dimension winsize = frame.getSize();
      int newWidth = winsize.width, newHeight = winsize.height;
      if (widthSpecified) newWidth = width;
      if (heightSpecified) newHeight = height;
      if (widthSpecified || heightSpecified)
	frame.setSize(width, height);
      if (xSpecified || ySpecified)
	setLocationOfFrame(frame);
    }
    /**
     * Sets the location of the given frame to the center of the screen.
     * The work is done by centerWindowOnScreen (Frames are Windows).
     * This method is defined for backward compatibility with ancient
     * TRIPS code and more recent code that has been copied from it.
     */
    public static void centerFrameOnScreen(Frame frame) {
	centerWindowOnScreen(frame);
    }
    /**
     * Sets the location of the given Window to the center of the screen.
     * Nothing to do with GeometrySpec's, really, but it sort of goes
     * with GeometrySpec.setLocationOfFrame().
     */
    public static void centerWindowOnScreen(Window window) {
	Dimension ssize = Toolkit.getDefaultToolkit().getScreenSize();
	Dimension fsize = window.getSize();
	int x = (ssize.width - fsize.width) / 2;
	int y = (ssize.height - fsize.height) / 2;
	window.setLocation(x, y);
    }
}
