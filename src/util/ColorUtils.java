/*
 * ColorUtils.java
 *
 * George Ferguson, ferguson@cs.rochester.edu,  6 Apr 1999
 * Time-stamp: <Wed Sep 13 16:59:46 EDT 2000 ferguson>
 */

package TRIPS.util;

import java.awt.Color;
import java.util.Hashtable;

public class ColorUtils {
    // Fields
    static private Hashtable systemColors;
    // Class initialization
    static {
	systemColors = new Hashtable();
	systemColors.put("black", Color.black);
	systemColors.put("blue", Color.blue);
	systemColors.put("cyan", Color.cyan);
	systemColors.put("darkgray", Color.darkGray);
	systemColors.put("gray", Color.gray);
	systemColors.put("green", Color.green);
	systemColors.put("lightgray", Color.lightGray);
	systemColors.put("magenta", Color.magenta);
	systemColors.put("orange", Color.orange);
	systemColors.put("pink", Color.pink);
	systemColors.put("red", Color.red);
	systemColors.put("white", Color.white);
	systemColors.put("yellow", Color.yellow);
	systemColors.put("forestgreen", new Color(0x238E23));
	systemColors.put("khaki", new Color(0x9F9F5F));
	systemColors.put("firebrick", new Color(0x8E2323));
	systemColors.put("lightblue", new Color(0xC0D9D9));
	systemColors.put("brown",new Color(0xA62A2A));
    }
    // Methods
    static public Color stringToColor(String s) {
	Color c = (Color)systemColors.get(s.toLowerCase());
	if (c == null) {
	    try {
		c = Color.decode(s);
	    } catch (NumberFormatException ex) {
		System.err.println("ColorUtils.stringToColor: bad color spec: " + c);
	    }
	}
	return c;
    }
}
