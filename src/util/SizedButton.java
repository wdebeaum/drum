/**
 * SizedButton.java
 * 
 * Dave Costello, costello@cs.rochester.edu, 04 Feb 1999
 * Time-stamp: <Tue Feb 23 16:00:06 EST 1999 ferguson>
 *
 * specialized button that takes dimension object or width and height 
 * as argument to constructor to force the button to be sized the way
 * I want it.
 * NOTE:Dimensions are in pixels *not* rows/cols.
 */

package TRIPS.util;

import java.awt.*;

public class SizedButton extends Button {

    //members
    private Dimension preferSize;

    //constructors
    /**
     * constructs a button with d size
     */
    public SizedButton(Dimension d) {
	preferSize = d;
    }
    /**
     * constructs a button with size width and height
     */
    public SizedButton(int width, int height){
	preferSize = new Dimension(width,height);
    }
    /**
     * constructs a button with d size and Label label
     */
    public SizedButton(Dimension d, String label) {
	preferSize = d;
	this.setLabel(label);
    }
    /**
     * constructs a window with size width,height and Label label
     */
    public SizedButton(int width, int height, String label){
	preferSize = new Dimension(width,height);
	this.setLabel(label);
    }    
    /**
     * constructs a regular autosized Button
     */
    public SizedButton(){
	preferSize = super.getPreferredSize();
    }    
    /**
     * constructs a regular autosized Button with a label
     */
    public SizedButton(String label){
	preferSize = super.getPreferredSize();
	this.setLabel(label);
    }   

    // --= public methods =--

    //overridden method in Button
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
	Frame f = new Frame("test button");
	Panel p1 = new Panel();
	Panel p2 = new Panel();
	Panel p3 = new Panel();
	Panel p4 = new Panel();

	//test constructors
	SizedButton b1 = new SizedButton(d);
	SizedButton b2 = new SizedButton(75,75);	
	SizedButton b3 = new SizedButton(d,"Hi There");
	SizedButton b4 = new SizedButton(90,60,"Hi There");

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


