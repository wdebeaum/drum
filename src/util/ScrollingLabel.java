/*
 * ScrollingLabel.java: Component that scrolls its label
 *
 * George Ferguson, ferguson@cs.rochester.edu,  1 Apr 2000
 * Time-stamp: <Wed Jan 28 11:21:26 EST 2004 ferguson>
 */

package TRIPS.util;

import java.awt.BorderLayout; // pureley for testing
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import javax.swing.JButton; // purely for testing
import javax.swing.JLabel;
import javax.swing.JFrame; // purely for testing

public class ScrollingLabel extends JLabel implements Runnable {
    //
    // Fields
    //
    String text;
    int columns;
    int speed; 		// ms between steps
    Insets insets;

    private int startpos = 0;
    private int textlen;
    //
    // Constructor(s)
    //
    public ScrollingLabel() {
	this("", 20, 500);
    }
    public ScrollingLabel(String text) {
	this(text, 20, 500);
    }
    public ScrollingLabel(String text, int columns) {
	this(text, columns, 500);
    }
    public ScrollingLabel(String text, int columns, int speed) {
	super(text);
	this.text = text;
	this.columns = columns;
	this.speed = speed;
	this.textlen = text.length();
	this.insets = new Insets(2, 2, 2, 2);
	start();
    }
    //
    // Finalize method
    //
    public void finalize() {
	// Kill the thread when we are cleaned up
	// Hmmm, I think this will never be called as long as the thread is
	// running. I'll leave it here for future reference anyway.
	stop();
    }
    //
    // Methods to start and stop the scrolling thread
    //
    protected boolean running;
    public void start() {
	if (!running) {
	    // Fire up a new thread
	    running = true;
	    new Thread(this).start();
	}
    }
    public void stop() {
	// Tell the thread to stop (next time it checks)
	running = false;
    }
    //
    // Methods to get and set parameters
    //
    public void setSpeed(int speed) {
	this.speed = speed;
    }
    public int getSpeed() {
	return speed;
    }
    public void setColumns(int columns) {
	this.columns = columns;
    }
    public int getColumns() {
	return columns;
    }
    public void setInsets(Insets insets) {
	this.insets = insets;
	// Need to force resize to happen? invalidate?
    }
    public Insets getInsets() {
	return insets;
    }
    //
    // Label methods
    //
    public Dimension getPreferredSize() {
	Font f = getFont();
	//System.err.println("ScrollingLabel.getPreferredSize: f="  + f);
	FontMetrics fm = getFontMetrics(f);
	//System.err.println("ScrollingLabel.getPreferredSize: fm="  + fm);
	int width = fm.charWidth('0') * columns + insets.left + insets.right;
	int height = fm.getMaxAscent() + fm.getMaxDescent() + insets.top + insets.bottom;
	//System.err.println("ScrollingLabel.getPreferredSize: width=" + width);
	//System.err.println("ScrollingLabel.getPreferredSize: height=" + height);
	return new Dimension(width, height);
    }
    public Dimension getMinimumSize() {
	return getPreferredSize();
    }
    public void setText(String text) {
	super.setText(text);
	this.text = text;
	this.textlen = text.length();
	this.startpos = 0;
    }
    //
    // Runnable method
    //
    public void run() {
	while (running) {
	    // Sleep
	    try {
		Thread.currentThread().sleep(speed);
	    } catch (InterruptedException ex) {
	    }
	    // Increment counter
	    startpos += 1;
	    if (startpos == textlen) {
		startpos = 0;
	    }
	    // Repaint
	    repaint();
	}
    }
    //
    // Label method
    //
    public void paint(Graphics g) {
	Dimension size = getSize();
	String t = text.substring(startpos) + text.substring(0, startpos);
	//System.err.println("ScrollingLabel.paint: t=\"" + t + "\"");
	g.clearRect(0, 0, size.width, size.height);
	Font f = getFont();
	FontMetrics fm = getFontMetrics(f);
	g.setFont(f);
	g.setColor(getForeground());
	g.setClip(insets.left, insets.top,
		  size.width - insets.left - insets.right,
		  size.height - insets.top - insets.bottom);
	g.drawString(t, insets.left, fm.getMaxAscent() + insets.top);
    }
    //
    // Testing
    //
    public static void main(String argv[]) {
	if (argv.length == 0) {
	    System.err.println("usage: java ScrollingLabel text-to-scroll");
	    System.exit(1);
	}
	JFrame frame = new JFrame();
	frame.getContentPane().setLayout(new BorderLayout());
	ScrollingLabel label = new ScrollingLabel(argv[0]);
	frame.getContentPane().add(label, "North");
	JButton button = new JButton("Hello");
	frame.getContentPane().add(button, "South");
	frame.pack();
	frame.setVisible(true);
    }
}
