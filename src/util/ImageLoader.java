/*
 * ImageLoader.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 20 Apr 2000
 * Time-stamp: <Thu Apr 20 17:23:08 EDT 2000 ferguson>
 */

package TRIPS.util;

import java.net.URL;
import java.awt.Toolkit;
import java.awt.Image;
import java.awt.image.ImageObserver;

/**
 * This class loads images without resorting to a MediaTracker, and
 * so can be used by static methods including class initialization code.
 */
public class ImageLoader {
    public static Image loadImageResource(URL url) {
	//System.err.println("loading image resource: " + url);
	Image img = Toolkit.getDefaultToolkit().createImage(url);
	//System.err.println("img=" + img);
	Toolkit.getDefaultToolkit().prepareImage(img, -1, -1, null);
	while (true) {
	    int status = Toolkit.getDefaultToolkit().checkImage(img, -1, -1, null);
	    //System.err.print("status=" + status);
	    //if ((status & ImageObserver.ABORT) != 0) { System.err.print(" ABORT"); }
	    //if ((status & ImageObserver.ALLBITS) != 0) { System.err.print(" ALLBITS"); }
	    //if ((status & ImageObserver.ERROR) != 0) { System.err.print(" ERROR"); }
	    //if ((status & ImageObserver.FRAMEBITS) != 0) { System.err.print(" FRAMEBITS"); }
	    //if ((status & ImageObserver.HEIGHT) != 0) { System.err.print(" HEIGHT"); }
	    //if ((status & ImageObserver.PROPERTIES) != 0) { System.err.print(" PROPERTIES"); }
	    //if ((status & ImageObserver.SOMEBITS) != 0) { System.err.print(" SOMEBITS"); }
	    //if ((status & ImageObserver.WIDTH) != 0) { System.err.print(" WIDTH"); }
	    //System.err.println();
	    //
	    if ((status & ImageObserver.ALLBITS) != 0) {
		//System.err.println("done!");
		break;
	    } else if ((status & (ImageObserver.ABORT | ImageObserver.ERROR)) != 0) {
		//System.err.println("error!");
		break;
	    }
	    try {
		Thread.currentThread().sleep(100);
	    } catch (InterruptedException ex) {
	    }
	}
	return img;
    }
}
