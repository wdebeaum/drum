/**
 * @(#)JSizedButton.java
 * 
 * Dave Costello, costello@cs.rochester.edu, 04 Feb 1999
 * Time-stamp: <Fri Mar  9 15:58:01 EST 2001 ferguson>
 */

package TRIPS.util;

import java.awt.Dimension;
import javax.swing.JButton;
// For testing with main()
import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 * Specialized button that takes dimension object or width and height 
 * as argument to constructor to force the button to be sized the way
 * I want it.
 * <p>
 * NOTE:Dimensions are in pixels *not* rows/cols.
 */
public class JSizedButton extends JButton {

    //members
    private Dimension preferSize;

    //constructors
    /**
     * constructs a button with d size
     */
    public JSizedButton(Dimension d) {
	preferSize = d;
    }
    /**
     * constructs a button with size width and height
     */
    public JSizedButton(int width, int height){
	preferSize = new Dimension(width,height);
    }
    /**
     * constructs a button with d size and Label label
     */
    public JSizedButton(Dimension d, String label) {
	preferSize = d;
	this.setText(label);
    }
    /**
     * constructs a window with size width,height and Label label
     */
    public JSizedButton(int width, int height, String label){
	preferSize = new Dimension(width,height);
	this.setText(label);
    }    
    /**
     * constructs a regular autosized JButton
     */
    public JSizedButton(){
	preferSize = super.getPreferredSize();
    }    
    /**
     * constructs a regular autosized JButton with a label
     */
    public JSizedButton(String label){
	preferSize = super.getPreferredSize();
	this.setText(label);
    }   

    // --= public methods =--

    //overridden method in JButton
    public Dimension getPreferredSize(){
	return preferSize;
    }

    public void setPreferredSize(Dimension d) {
	preferSize = d;
    }

    public void setPreferredSize(int width, int height) {
	preferSize = new Dimension(width,height);
    }

    //main for testing	
    public static void main(String args[]) {

	Dimension d = new Dimension(100,100);
	JFrame f = new JFrame("test button");
	JPanel p1 = new JPanel();
	JPanel p2 = new JPanel();
	JPanel p3 = new JPanel();
	JPanel p4 = new JPanel();

	//test constructors
	JSizedButton b1 = new JSizedButton(d);
	JSizedButton b2 = new JSizedButton(75,75);	
	JSizedButton b3 = new JSizedButton(d,"Hi There");
	JSizedButton b4 = new JSizedButton(90,60,"Hi There");

	// add buttons to their own panels
	p1.add(b1);
	p2.add(b2);
	p3.add(b3);
	p4.add(b4);

	// add the button panels to the frame
	f.add(p1, "North");
	f.add(p2, "South");
	f.add(p3, "East");
	f.add(p4, "West");

	f.pack();
	f.setVisible(true);
    }



}


