/*
 * ModemDisplay.java
 *
 * David Costello, costello@cs.rochester.edu,  8 Jul 1999
 * $Id: ModemDisplay.java,v 1.7 2016/12/16 16:39:14 wdebeaum Exp $
 *
 */

package TRIPS.Facilitator;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JMenuBar;
import javax.swing.border.BevelBorder;
import java.awt.Color;
import java.awt.Font;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Hashtable;
import TRIPS.util.GeometrySpec;
import TRIPS.KQML.KQMLPerformative;

public class ModemDisplay extends JFrame implements FacilitatorDisplay, ActionListener {
    //
    // Constants
    //
    protected final static String DEFAULT_GEOMETRY = "254x200-0+0";
    protected final static int ROW_NAME_WIDTH = 120;
    protected final static int ROW_LED_WIDTH   = 25;
    protected final static int ROW_HEIGHT      = 20;
    protected final static Color BACKGROUND_COLOR = Color.lightGray;
    protected final static Color HEADING_COLOR = Color.black;
    protected static final Font HEADING_FONT =
	new Font("Dialog", Font.BOLD, 12);
    protected final static Color ENTRY_COLOR = Color.blue.darker();
    protected static final Font ENTRY_FONT =
	new Font("Dialog", Font.BOLD, 12);
    //
    // Fields
    //
    private String title = "TRIPS Facilitator";
    private boolean iconic = false;
    private GeometrySpec geometry;
    private boolean showMenuBar = true;
    private Facilitator facilitator;
    private JPanel mainpanel;
    private JLabel statusLine;
    protected Hashtable entries = new Hashtable();
    TrafficViewer trafficViewer = null;
    StatusViewer statusViewer = null;
    //
    // Constructors
    //
    public ModemDisplay(Facilitator facilitator) {
	init(facilitator);
	showMessageTraffic();
    }
    public ModemDisplay(Facilitator facilitator,Boolean hideTraffic) {
	init(facilitator);
	if( !hideTraffic.booleanValue() ) showMessageTraffic();
    }
    // 
    // Main Constructor code...
    //
    private void init(Facilitator facilitator) {
	this.facilitator = facilitator;
	handleParameters();
	createWidgets();
	setTitle(title);
	setSize(geometry.width, geometry.height);
	geometry.setLocationOfFrame(this);
	setVisible(true);
    }
    //
    // Methods
    //
    private void handleParameters() {
	String value;
	if ((value = facilitator.getParameter("-title")) != null) {
	    title = value;
	}
	if ((value = facilitator.getParameter("-iconic")) != null) {
	    iconic = Boolean.getBoolean(value);
	}
	if ((value = facilitator.getParameter("-geometry")) != null) {
	    geometry = new GeometrySpec(value);
	} else {
	    geometry = new GeometrySpec(DEFAULT_GEOMETRY);
	}	    
	if ((value = facilitator.getParameter("-showMenuBar")) != null) {
	    showMenuBar = new Boolean(value).booleanValue();
	}
    }
    // Setup the main panel of the display
    private void createWidgets(){
	mainpanel = new JPanel();
	//set a ColumnLayout top,center, gap 0
	mainpanel.setLayout(new ColumnLayout(Orientation.LEFT,
					     Orientation.TOP,
					     0));
	//set the panel's color
	mainpanel.setBackground(BACKGROUND_COLOR);
	// create a scrollpane with the mainpanel as it's viewport
	JScrollPane scrollpane =
	    new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
			    JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
	scrollpane.setColumnHeaderView(createHeaders());
	scrollpane.getViewport().setView(mainpanel);
	// the JFrame's LayoutManager
	this.getContentPane().setLayout(new BorderLayout());
	// add the scrollpane to the display frame
	getContentPane().add(scrollpane,"Center");
	if (showMenuBar) {
	    // create a menu bar
	    createMenus();
	}
	statusLine = new JLabel(" ");
	getContentPane().add(statusLine, "South");
    }
    // Create the column headers for the display
    protected JPanel createHeaders() {
	JPanel panel = new JPanel();
	// Tighten up the insets (matches ModemDisplayInfo)
	panel.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 0));
	panel.setBorder(new BevelBorder(BevelBorder.LOWERED));
	panel.setBackground(BACKGROUND_COLOR);
	// Add labels
	createHeaderLabel(panel, "Client", ROW_NAME_WIDTH);
	createHeaderLabel(panel, "ST", ROW_LED_WIDTH);
	createHeaderLabel(panel, "SD", ROW_LED_WIDTH);
	createHeaderLabel(panel, "RD", ROW_LED_WIDTH);
	// Done
	return panel;
    }
    public void createHeaderLabel(JPanel panel, String text, int width) {
	Color label_color  = Color.black;
	JLabel label = new JLabel(text, JLabel.LEFT);
	label.setPreferredSize(new Dimension(width, ROW_HEIGHT));
	label.setForeground(HEADING_COLOR);
	label.setFont(HEADING_FONT);
	panel.add(label);
    }
    private void createMenus() {
	JMenuBar menubar = new JMenuBar();
	JMenu menu;
	JMenuItem item;
        // Main
	menu = new JMenu("Control");
	addMenuItem("Quit TRIPS", menu);
	menubar.add(menu);
        // Control menu 
	menu = new JMenu("View");
	addMenuItem("Message Traffic", menu);
	addMenuItem("Module Status", menu);
	menubar.add(menu);
        // Debug
	menu = new JMenu("Debug");
	addMenuItem("Dump Registry", menu);
	addMenuItem("Dump Groups", menu);
	addMenuItem("Dump Subscriptions", menu);
	menubar.add(menu);
	// Add menubar to frame
	setJMenuBar(menubar);
    }
    JMenuItem addMenuItem(String text, JMenu menu) {
	JMenuItem item = new JMenuItem(text);
	item.setActionCommand(text);
	item.addActionListener(this);
	menu.add(item);
	return item;
    }

    /**
     * added by Nate Blaylock 9/25/07
     * adds a menu item to a menu in alphabetical order -- this is a hack to make components
     * show up in the same order each time (it gets to be a pain searching the list for them
     * startpos tells you where to start alphabetizing (so you don't mix with the
     *  start content like "Show all"
     */
    void addModemDisplayEntryAlphabetically(JPanel mainpanel, ModemDisplayEntry entry, int startpos) {
	String name = entry.getName();
	// go through menu and find right place
	for (int i = startpos; i < mainpanel.getComponentCount(); i++) {
	    String entryName = mainpanel.getComponent(i).getName();
	    if (name.compareToIgnoreCase(entryName) < 0) {
		mainpanel.add(entry, i);
		return;
	    }
	} // end for
	
	// if we made it out here, this item is last alphabetically
	mainpanel.add(entry);
    } // end addMenuItemAlphabetically

    //
    // Display methods
    //
    public void add(String name) {
    	name = name.toUpperCase();
	Debug.debug("ModemDisplay.add: name=" + name);
	if (name.equals("STDIN")) {
	    // Special case to not show stdin in display
	    return;
	}
	ModemDisplayEntry entry = new ModemDisplayEntry(name);
	Debug.debug("ModemDisplay.add: entry=" + entry);
	// Save entry for this name
	entries.put(name, entry);
	// Add components to the display
	Debug.debug("ModemDisplay.add: adding entry to mainpanel");
	//mainpanel.add(entry); NATE- make list alphabetica
	addModemDisplayEntryAlphabetically(mainpanel, entry, 0);
	// Make sure the panel updates the display properly
	mainpanel.revalidate();
	if (trafficViewer != null) {
	    trafficViewer.add(name);
	}
	if (statusViewer != null) {
	    statusViewer.add(name);
	}
	Debug.debug("ModemDisplay.add: done");
    }
    public void remove(String name) {
    	name = name.toUpperCase();
	Debug.debug("ModemDisplay.remove: name=" + name);
	if (name.equals("STDIN")) {
	    // Special case to not show stdin in display
	    return;
	}
	ModemDisplayEntry entry = (ModemDisplayEntry)entries.get(name);
	if (entry == null) {
	    Debug.error("ModemDisplay.remove: no entry for name \"" + name + "\"");
	    return;
	}
	Debug.debug("ModemDisplay.remove: entry=" + entry);
	// Clear entry for this name
	entries.remove(name);
	// Remove the client from the display
	Debug.debug("ModemDisplay.remove: removing entry from mainpanel");
	mainpanel.remove(entry);
	// Make sure the panel updates the display properly
	mainpanel.revalidate();
	mainpanel.repaint();
	if (trafficViewer != null) {
	    trafficViewer.remove(name);
	}
	if (statusViewer != null) {
	    statusViewer.remove(name);
	}
	Debug.debug("ModemDisplay.remove: done");
    }
    public void changeName(String oldname, String newname) {
    	oldname = oldname.toUpperCase();
    	newname = newname.toUpperCase();
	Debug.debug("ModemDisplay.changeName: oldname=" + oldname + ", newname=" + newname);
	ModemDisplayEntry entry = (ModemDisplayEntry)entries.get(oldname);
	if (entry == null) {
	    Debug.error("ModemDisplay.changeName: no entry for oldname \"" + oldname + "\"");
	    return;
	}
	Debug.debug("ModemDisplay.changeName: entry=" + entry);
	// Update registry
	entries.remove(oldname);
	entries.put(newname, entry);
	// Set name on entry
	entry.setName(newname);
	if (trafficViewer != null) {
	    trafficViewer.changeName(oldname, newname);
	}
	if (statusViewer != null) {
	    statusViewer.changeName(oldname, newname);
	}
	Debug.debug("ModemDisplay.changeName: done");
    }
    public void indicateMessageSent(String sender,
				    KQMLPerformative msg) {
	indicateMessage(sender, msg, ModemDisplayEntry.SD);
	if (trafficViewer != null) {
	    trafficViewer.indicateMessageSent(sender, msg);
	}
    }
    public void indicateMessageReceived(String sender,
				        KQMLPerformative msg) {
	indicateMessage(sender, msg, ModemDisplayEntry.RD);
	if (trafficViewer != null) {
	    trafficViewer.indicateMessageReceived(sender, msg);
	}
    }
    protected void indicateMessage(String name, KQMLPerformative msg, int which) {
    	name = name.toUpperCase();
	if (name.equals("STDIN")) {
	    // Special case to not show stdin in display
	    return;
	}
	ModemDisplayEntry entry = (ModemDisplayEntry)entries.get(name);
	if (entry == null) {
	    //Debug.error("ModemDisplay.indicateMessage: no entry for name \"" + name + "\"");
	    return;
	}
	String verb = msg.getVerb();
	int color;
	if (verb.equalsIgnoreCase("ERROR") || verb.equalsIgnoreCase("SORRY")) {
	    color = LED.RED;
	} else {
	    color = LED.GREEN;
	}
	entry.indicate(which, color);
    }
    private static boolean statusVerbIs(String status, String verb) {
        return status.equalsIgnoreCase(verb) ||
	       status.toUpperCase().startsWith("(" + verb);
    }
    public void indicateStatus(String name, String status) {
	if (name.equals("STDIN")) {
	    // Special case to not show stdin in display
	    return;
	} else if (name.equalsIgnoreCase("system")) {
	    // Special case for system status line
	    statusLine.setText(status);
	    return;
	}
	ModemDisplayEntry entry = (ModemDisplayEntry)entries.get(name);
	if (entry == null) {
	    //Debug.error("ModemDisplay.indicateStatus: no entry for name \"" + name + "\"");
	    return;
	}
	if (statusViewer != null) {
	    statusViewer.indicateStatus(name, status);
	}
	int color;
	if (statusVerbIs(status, "READY") ||
	    statusVerbIs(status, "OK")) {
	    color = LED.GREEN;
	} else if (statusVerbIs(status, "CONNECTED")) {
	    color = LED.PINK;
	} else if (statusVerbIs(status, "WAITING")) {
	    color = LED.YELLOW;
	} else if (statusVerbIs(status, "UNCHANGED")) {
	    return; // don't change the color
	} else {
	    color = LED.RED;
	}
	entry.set(ModemDisplayEntry.ST, color);
	entry.setToolTipText(status);
    }
    //
    // ActionListener method
    //
    public void actionPerformed(ActionEvent e){
	String cmd = e.getActionCommand();  
	if (cmd.equals("Quit TRIPS")) {
	    facilitator.exit(0);
	} else if (cmd.equals("Dump Groups")) {
	    facilitator.dumpGroups();
	} else if (cmd.equals("Dump Registry")) {
	    facilitator.dumpRegistry();
	} else if (cmd.equals("Dump Subscriptions")) {
	    facilitator.dumpSubscriptions();
	} else if (cmd.equals("Message Traffic")) {
	    showMessageTraffic();
	} else if (cmd.equals("Module Status")) {
	    showModuleStatus();
	}
    }
    //
    // Other Methods
    //
    public void hideWindow() {
	setVisible(false);
	if (trafficViewer != null) {
	    trafficViewer.setVisible(false);
	}
	if (statusViewer != null) {
	    statusViewer.setVisible(false);
	}
    }
    public void showWindow() {
	setVisible(true);
	if (trafficViewer != null) {
	    trafficViewer.setVisible(true);
	}
	if (statusViewer != null) {
	    statusViewer.setVisible(true);
	}
    }
    //
    // Message Traffic Viewer
    void showMessageTraffic() {
	if (trafficViewer == null) {
	    trafficViewer = new TrafficViewer(facilitator);
	}
	trafficViewer.setVisible(true);
    }
    //
    // Module Status Viewer
    void showModuleStatus() {
	if (statusViewer == null) {
	    statusViewer = new StatusViewer(facilitator);
	}
	statusViewer.setVisible(true);
    }
}

class ModemDisplayEntry extends JPanel {
    //
    // Constants
    //
    public static final int ST = 0;
    public static final int SD = 1;
    public static final int RD = 2;
    //
    // Fields
    //
    JLabel label;
    FlashLED[] leds = new FlashLED[3];
    //
    // Constructor
    //
    public ModemDisplayEntry(String name) {
	setLayout(new FlowLayout(FlowLayout.LEFT, 5, 0));
	setBorder(new BevelBorder(BevelBorder.LOWERED));
	setBackground(ModemDisplay.BACKGROUND_COLOR);
	// Name label
  	label = new JLabel(name, JLabel.LEFT);
	label.setForeground(ModemDisplay.ENTRY_COLOR);
	label.setFont(ModemDisplay.ENTRY_FONT);
	label.setPreferredSize(new Dimension(ModemDisplay.ROW_NAME_WIDTH,
					     ModemDisplay.ROW_HEIGHT));
	add(label);
	// LED labels
	for (int i=0; i < 3; i++) {
	    leds[i] = new FlashLED();
	    leds[i].setPreferredSize(new Dimension(ModemDisplay.ROW_LED_WIDTH,
						   ModemDisplay.ROW_HEIGHT));
	    add(leds[i]);
	}
    }
    //
    // Methods
    //
    public void set(int which, int color) {
	leds[which].setColor(color);
	leds[which].setState(true);
    }
    public void indicate(int which, int color) {
	leds[which].flash(color);
    }
    public void setName(String name) {
	label.setText(name);
    }
    public String getName() {
	return label.getText();
    }
}

