
// $Id: ColumnLayout.java,v 1.1.1.1 2005/01/14 19:48:08 ferguson Exp $

package TRIPS.Facilitator;//added for testing

import java.awt.*;

/**
 * ColumnLayout lays out components in a column.  At 
 * construction time, both horizontal orientation and vertical 
 * orientation may be specified, along with the gap to use 
 * between components.<p>
 *
 * Horizontal orientation must be one of the following:
 * <dl>
 * <dd> LEFT 
 * <dd> CENTER  
 * <dd> RIGHT
 * </dl>
 *
 * Vertical orientation must be one of the following:
 * <dl>
 * <dd> TOP
 * <dd> CENTER  
 * <dd> BOTTOM
 * </dl>
 *
 * @version 1.0, Apr 1 1996
 * @author  David Geary
 * @see     Orientation
 * @see     RowLayout
 */
public class ColumnLayout implements LayoutManager {
    static private int _defaultGap = 5;

    private int         gap;
    private Orientation horizontalOrientation;
    private Orientation verticalOrientation;

    public ColumnLayout() {
        this(Orientation.CENTER, 
             Orientation.CENTER, _defaultGap);
    }
    public ColumnLayout(int gap) {
        this(Orientation.CENTER, Orientation.CENTER, gap);
    }
    public ColumnLayout(Orientation horizontalOrient, 
                        Orientation verticalOrient) {
        this(horizontalOrient, verticalOrient, _defaultGap);
    }
    public ColumnLayout(Orientation horizontalOrient, 
                        Orientation verticalOrient, int gap) {
        Assert.notFalse(gap >= 0);
        Assert.notFalse(
            horizontalOrient == Orientation.LEFT   ||
            horizontalOrient == Orientation.CENTER ||
            horizontalOrient == Orientation.RIGHT);
        Assert.notFalse(
            verticalOrient   == Orientation.TOP    ||
            verticalOrient   == Orientation.CENTER ||
            verticalOrient   == Orientation.BOTTOM);

        this.gap                   = gap;
        this.verticalOrientation   = verticalOrient;
        this.horizontalOrientation = horizontalOrient;
    }

    public void addLayoutComponent(String name, 
                                   Component comp) {
    }
    public void removeLayoutComponent(Component comp) { 
    }

    public Dimension preferredLayoutSize(Container target) {
        Insets    insets      = target.getInsets();
        Dimension dim         = new Dimension(0,0);
        int       ncomponents = target.getComponentCount();
        Component comp;
        Dimension d;

        for (int i = 0 ; i < ncomponents ; i++) {
            comp = target.getComponent(i);

            if(comp.isVisible()) {
                d = comp.getPreferredSize();
                if(i > 0) 
                    dim.height += gap;

                dim.height += d.height;
                dim.width   = Math.max(d.width, dim.width);
            }
        }
        dim.width  += insets.left + insets.right;
        dim.height += insets.top  + insets.bottom;
        return dim;
    }
    public Dimension minimumLayoutSize(Container target) {
        Insets    insets      = target.getInsets();
        Dimension dim         = new Dimension(0,0);
        int       ncomponents = target.getComponentCount();
        Component comp;
        Dimension d;

        for (int i = 0 ; i < ncomponents ; i++) {
            comp = target.getComponent(i);

            if(comp.isVisible()) {
                d = comp.getMinimumSize();

                dim.width  = Math.max(d.width, dim.width);
                dim.height += d.height;

                if(i > 0) dim.height += gap;
            }
        }
        dim.width  += insets.left + insets.right;
        dim.height += insets.top  + insets.bottom;

        return dim;
    }
    public void layoutContainer(Container target) {
        Insets    insets        = target.getInsets();
        int       top           = insets.top;
        int       left          = 0;
        int       ncomponents   = target.getComponentCount();
        Dimension preferredSize = target.getPreferredSize();
        Dimension targetSize    = target.getSize();
        Component comp;
        Dimension ps;

        if(verticalOrientation == Orientation.CENTER)
            top += (targetSize.height/2) - 
                   (preferredSize.height/2);
        else if(verticalOrientation == Orientation.BOTTOM)
            top = targetSize.height - preferredSize.height + 
                  insets.top;

        for (int i = 0 ; i < ncomponents ; i++) {
            comp = target.getComponent(i);
            left = insets.left;

            if(comp.isVisible()) {
                ps = comp.getPreferredSize();

                if(horizontalOrientation == Orientation.CENTER)
                    left = (targetSize.width/2) - (ps.width/2);
                else if(
                  horizontalOrientation == Orientation.RIGHT) {
                    left = targetSize.width - ps.width - 
                           insets.right;
                }
                comp.setBounds(left,top,ps.width,ps.height);
                top += ps.height + gap;
            }
        }
    }
}

/**
 * A simple assertion mechanism for asserting validity of
 * arguments.<p>
 *
 * @version 1.0, Apr 1 1996
 * @author  David Geary
 */
class Assert {
    static public void notFalse(boolean b) 
                       throws IllegalArgumentException {
        if(b == false) 
            throw new IllegalArgumentException(
                            "boolean expression false");
    }
	static public void isTrue(boolean b) { 
		notFalse(b); 
	}
	static public void isFalse(boolean b) { 
		if(b != false)
            throw new IllegalArgumentException(
                            "boolean expression true");
	}
    static public void notNull(Object obj) 
                       throws IllegalArgumentException {
        if(obj == null) 
            throw new IllegalArgumentException("null argument");
    }
    static public void notFalse(boolean b, String s) 
                               throws IllegalArgumentException {
        if(b == false)
            throw new IllegalArgumentException(s);
    }
	static public void isTrue(boolean b, String msg) { 
		notFalse(b, msg); 
	}
	static public void isFalse(boolean b, String s) { 
		if(b != false)
            throw new IllegalArgumentException(s);
	}
    static public void notNull(Object obj, String s) 
                               throws IllegalArgumentException {
        if(obj == null) 
            throw new IllegalArgumentException(s);
    }
}
