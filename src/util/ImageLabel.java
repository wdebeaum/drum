/*
 * ImageLabel.java: Simple component that displays an Image
 *
 * George Ferguson, ferguson@cs.rochester.edu,  1 Apr 2000
 * Time-stamp: <Sat Apr  1 15:46:39 EST 2000 ferguson>
 */

package TRIPS.util;

import java.awt.*;
import java.awt.image.*;

/**
 * An ImageLabel is a component that displays an image. You could do this
 * with a JLabel from javax.swing.
 *
 */
public class ImageLabel extends Component {
    //
    // Fields
    //
    Image image;
    Insets insets;
    //
    // Constructor(s)
    //
    /**
     * Creates a new ImageLabel displaying the given image.
     * Insets for the label are set to 0 on all sides.
     *
     * @param	image	Image to display
     * @see	setInsets, getInsets
     */
    public ImageLabel(Image image) {
	this.image = image;
	this.insets = new Insets(0, 0, 0, 0);
    }
    //
    // Methods to get and set parameters
    //
    public void setImage(Image image) {
	this.image = image;
	invalidate();
    }
    public Image getImage() {
	return image;
    }
    public void setInsets(Insets insets) {
	this.insets = insets;
	invalidate();
    }
    public Insets getInsets() {
	return insets;
    }
    //
    // Component methods
    //
    public Dimension getPreferredSize() {
	int width = image.getWidth(this);
	int height = image.getHeight(this);
	return new Dimension(width, height);
    }
    public Dimension getMinimumSize() {
	return getPreferredSize();
    }
    public void paint(Graphics g) {
	g.drawImage(image, insets.left, insets.top, this);
    }
    //
    // Testing
    //
    public static void main(String argv[]) {
	if (argv.length == 0) {
	    System.err.println("usage: java ImageLabel image-file");
	    System.exit(1);
	}
	Frame frame = new Frame();
	frame.setLayout(new BorderLayout());
	MediaTracker tracker = new MediaTracker(frame);
	Image image = Toolkit.getDefaultToolkit().getImage(argv[0]);
	tracker.addImage(image, 1);
	try {
	    tracker.waitForID(1);
	} catch (InterruptedException ex) {
	}
	if (image == null) {
	    System.err.println("failed to load image: " + argv[0]);
	    System.exit(1);
	}
	ImageLabel label = new ImageLabel(image);
	frame.add(label, "North");
	Button button = new Button("Hello");
	frame.add(button, "South");
	frame.pack();
	frame.setVisible(true);
    }
}
