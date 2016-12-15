/*
 * LED.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 20 Apr 2000
 * $Id: LED.java,v 1.2 2016/12/14 19:41:26 wdebeaum Exp $
 */

package TRIPS.Facilitator;

import java.net.URL;
import javax.swing.JLabel;
import javax.swing.ImageIcon;
import java.awt.Image;
import TRIPS.util.ImageLoader;

public class LED extends JLabel {
    //
    // Constants
    //
    public static final int OFF = 0;
    public static final int RED = 1;
    public static final int YELLOW = 2;
    public static final int GREEN = 3;
    public static final int PINK = 4;
    protected static final String[] iconFilenames = {
	"images/LEDoff.gif",
	"images/LEDred.gif",
	"images/LEDyellow.gif",
	"images/LEDgreen.gif",
	"images/LEDpink.gif"
    };
    //
    // Class variables
    //
    protected static ImageIcon[] icons;
    static {
	int nicons = iconFilenames.length;
	icons = new ImageIcon[nicons];
	for (int i=0; i < nicons; i++) {
	    //Debug.debug("LED.<classinit>: loading file: \"" + iconFilenames[i] + "\"");
	    URL url = LED.class.getResource(iconFilenames[i]);
	    //Debug.debug("LED.<classinit>: url=" + url);
	    if (url == null) {
		//Debug.error("couldn't locate icon: " + iconFilenames[i]);
	    } else {
		Image img = ImageLoader.loadImageResource(url);
		//Debug.debug("LED.<classinit>: icons[" + i + "]=" + img);
		icons[i] = new ImageIcon(img);
	    }
	}
    }
    //
    // Fields
    //
    protected int color;
    protected boolean state = false;
    //
    // Constructors
    //
    public LED() {
	this(OFF, false);
    }
    public LED(int color) {
	this(color, false);
    }
    public LED(int color, boolean state) {
	this.color = color;
	this.state = state;
	if (state) {
	    setIcon(icons[color]);
	} else {
	    setIcon(icons[OFF]);
	}
	//setHorizontalAlignment(JLabel.CENTER);
    }
    //
    // Methods
    //
    public boolean getState() {
	return state;
    }
    public void setState(boolean state) {
	//System.err.println("LED.setState: state=" + state);
	this.state = state;
	if (state) {
	    setIcon(icons[color]);
	} else {
	    setIcon(icons[OFF]);
	}
	//System.err.println("LED.setState: done");
    }
    public int getColor() {
	return color;
    }
    public void setColor(int color) {
	//System.err.println("LED.setColor: color=" + color);
	this.color = color;
	// And if we're turned on, change color now
	if (state) {
	    setIcon(icons[color]);
	}
	//System.err.println("LED.setColor: done");
    }
    //
    // Testing
    //
    /*
    import javax.swing.JFrame;
    import java.awt.FlowLayout;
    public static void main(String argv[]) {
    	JFrame frame = new JFrame();
    	frame.getContentPane().setLayout(new FlowLayout());
    	frame.getContentPane().add(new LED(RED));
	frame.getContentPane().add(new LED(YELLOW));
	frame.getContentPane().add(new LED(GREEN));
	frame.pack();
	frame.setVisible(true);
    }
    */
}
