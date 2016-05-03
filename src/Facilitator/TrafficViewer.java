/*
 * TrafficViewer.java
 *
 * George Ferguson, ferguson@cs.rochester.edu,  8 Jan 2004
 * Time-stamp: <Fri Jun  1 15:45:07 EDT 2007 ferguson>
 *
 * TODO:
 * - resize history array
 */

package TRIPS.Facilitator;

import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.util.CircularBuffer;

/**
 * This class implements the Display interface, and so could be used as a
 * standalone viewer. But it's usually called up by the ModemPanelDisplay.
 */
public class TrafficViewer extends JFrame implements FacilitatorDisplay, ActionListener, ItemListener {

    protected static final int MAX_TEXT_AREA_LENGTH = 1024*1024;

    Facilitator facilitator;
    JTextArea textArea;
    JMenu viewMenu;
    //
    // Constructors
    //
    public TrafficViewer(Facilitator facilitator) {
	this.facilitator = facilitator;
	setTitle("TRIPS Message Traffic");
	setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	createMenus();
        textArea = new JTextArea(40, 80);
        textArea.setFont(new Font("Monospaced", Font.BOLD, 12));
        textArea.setEditable(true);
	textArea.setLineWrap(true);
	textArea.setWrapStyleWord(true);
	JScrollPane scroller = new JScrollPane(textArea);
        getContentPane().add(scroller); 
	pack();
	setVisible(true);
    }
    //
    // Methods
    //
    void createMenus() {
	JMenuBar menubar = new JMenuBar();
	JMenu menu;
	// File
	menu = new JMenu("File");
	addMenuItem("Save As...", menu);
	menu.addSeparator();
	addMenuItem("Close", menu);
	menubar.add(menu);
	// View
	viewMenu = new JMenu("View");
	addMenuItem("Clear History", viewMenu);
	menu.addSeparator();
	addMenuItem("Show All", viewMenu);
	addMenuItem("Show None", viewMenu);
	menu.addSeparator();
	Enumeration names = facilitator.getRegistry().enumerateNames();
	while (names.hasMoreElements()) {
	    String name = (String)names.nextElement();
	    addClient(name);
	}
	menubar.add(viewMenu);
	// Add to frame
	setJMenuBar(menubar);
    }
    JMenuItem addMenuItem(String text, JMenu menu) {
	JMenuItem item = new JMenuItem(text);
	item.setActionCommand(text);
	item.addActionListener(this);
	menu.add(item);
	return item;
    }
    JCheckBoxMenuItem addCheckBoxMenuItem(String text, JMenu menu) {
	JCheckBoxMenuItem item = new JCheckBoxMenuItem(text, true);
	item.setActionCommand(text);
	item.addItemListener(this);
	addMenuItemAlphabetically(menu,item,3); // Nate: alphabetize components on menu
	//menu.add(item);
	return item;
    }

    /**
     * added by Nate Blaylock 9/25/07
     * adds a menu item to a menu in alphabetical order -- this is a hack to make components
     * show up in the same order each time (it gets to be a pain searching the list for them
     * startpos tells you where to start alphabetizing (so you don't mix with the
     *  start content like "Show all"
     */
    void addMenuItemAlphabetically(JMenu menu, JMenuItem item, int startpos) {
	String text = item.getActionCommand();
	// go through menu and find right place
	for (int i = startpos; i < menu.getItemCount(); i++) {
	    String itemText = menu.getItem(i).getActionCommand();
	    if (text.compareToIgnoreCase(itemText) < 0) {
		menu.insert(item, i);
		return;
	    }
	} // end for
	
	// if we made it out here, this item is last alphabetically
	menu.add(item);
    } // end addMenuItemAlphabetically


    //
    // Managing the client menu and enabled list
    //
    List clientList = new LinkedList();
    void addClient(String name) {
	//System.err.println("addClient: name=" + name);
	JCheckBoxMenuItem menuItem = addCheckBoxMenuItem(name, viewMenu);
	ClientRecord rec = new ClientRecord(name, true, menuItem);
	clientList.add(rec);
	//System.err.println("addClient: done");
    }
    ClientRecord findClient(String name) {
	Iterator clients = clientList.iterator();
	while (clients.hasNext()) {
	    ClientRecord rec = (ClientRecord)clients.next();
	    if (rec.name.equals(name)) {
		return rec;
	    }
	}
	return null;
    }
    void removeClient(String name) {
	Iterator clients = clientList.iterator();
	while (clients.hasNext()) {
	    ClientRecord rec = (ClientRecord)clients.next();
	    if (rec.name.equals(name)) {
		clients.remove();
		viewMenu.remove(rec.menuItem);
		break;
	    }
	}
    }
    void renameClient(String oldname, String newname) {
	Iterator clients = clientList.iterator();
	while (clients.hasNext()) {
	    ClientRecord rec = (ClientRecord)clients.next();
	    if (rec.name.equals(oldname)) {
		rec.name = newname;
		rec.menuItem.setText(newname);
		break;
	    }
	}
    }
    void setAllClientsState(boolean state) {
	Iterator clients = clientList.iterator();
	while (clients.hasNext()) {
	    ClientRecord rec = (ClientRecord)clients.next();
	    rec.enabled = state;
	    rec.menuItem.setSelected(state);
	}
    }
    //
    // Managing the history list
    //
    CircularBuffer history = new CircularBuffer(100);
    void addToHistory(HistoryEntry entry) {
	history.add(entry);
    }
    void redrawTextArea() {
	textArea.setText("");
	Iterator entries = history.iterator();
	while (entries.hasNext()) {
	    HistoryEntry entry = (HistoryEntry)entries.next();
	    maybeShowEntry(entry);
	}
    }
    void maybeShowEntry(HistoryEntry entry) {
	String sender = entry.sender;
	ClientRecord rec = findClient(sender.toUpperCase());
	if (rec != null && rec.enabled) {
	    KQMLPerformative msg = entry.msg;
	    boolean sent = entry.sent;
	    String tag = (sent ? "From" : "To");
	    textArea.append(tag + " " + sender + ":\n" + msg.toString() + "\n\n");
	    // Prevent JTextArea from growing inexorably
	    Document doc = textArea.getDocument();
	    int len = doc.getLength();
	    if (len > MAX_TEXT_AREA_LENGTH) {
		try {
		    textArea.setText(doc.getText(len/2, len/2));
		} catch (BadLocationException ex) {
		}
	    }
	    // Maybe better way to make bottom stuff visible...
	    textArea.setCaretPosition(doc.getLength()); 
	}
    }
    //
    // FacilitatorDisplay methods
    //
    public void add(String name) {
	//System.err.println("add: name=" + name);
    	name = name.toUpperCase();
	if (findClient(name) == null) {
	    addClient(name);
	}
	//System.err.println("add: done");
    }
    public void remove(String name) {
	//System.err.println("remove: name=" + name);
    	name = name.toUpperCase();
	removeClient(name);
	//System.err.println("remove: done");
    }
    public void changeName(String oldname, String newname) {
	//System.err.println("changeName: oldname=" + oldname + ", newname=" + newname);
    	oldname = oldname.toUpperCase();
    	newname = newname.toUpperCase();
	renameClient(oldname, newname);
	//System.err.println("changeName: done");
    }
    public void indicateMessageSent(String sender,
				    KQMLPerformative msg) {
	indicateMessage(sender, msg, true);
    }
    public void indicateMessageReceived(String sender,
				        KQMLPerformative msg) {
	indicateMessage(sender, msg, false);
    }
    protected void indicateMessage(String name, KQMLPerformative msg, boolean sent) {
    	name = name.toUpperCase();
	HistoryEntry entry = new HistoryEntry(name, msg, sent);
	addToHistory(entry);
	maybeShowEntry(entry);
    }
    public void indicateStatus(String name, String status) {
	// Not used in LoggerDisplay
    }
    //
    // ActionListener method
    //
    JFileChooser fileChooser;
    public void actionPerformed(ActionEvent e){
	String cmd = e.getActionCommand();  
	if (cmd.equals("Close")) {
	    // File > Close
	    setVisible(false);
	    dispose();
	} else if (cmd.equals("Save As...")) {
	    // File > Save As
	    if (fileChooser == null) {
		fileChooser = new JFileChooser();
	    }
	    int returnVal = fileChooser.showSaveDialog(this);
	    if (returnVal == JFileChooser.APPROVE_OPTION) {
		File file = fileChooser.getSelectedFile();
		try {
		    FileWriter out = new FileWriter(file);
		    // For large amounts of text, this must be a bad idea...
		    out.write(textArea.getText());
		    out.close();
		} catch (IOException ex) {
		    JOptionPane.showMessageDialog(this, file.getPath() + ":\n " + ex.getMessage(), "Error saving file", JOptionPane.ERROR_MESSAGE);
		}
	    }
	} else if (cmd.equals("Clear History")) {
	    // View > Clear History
	    textArea.setText("");
	    history.clear();
	} else if (cmd.equals("Show All")) {
	    // View > Show All
	    setAllClientsState(true);
	} else if (cmd.equals("Show None")) {
	    // View > Show None
	    setAllClientsState(false);
	}
    }
    //
    // ItemListener method
    //
    public void itemStateChanged(ItemEvent e) { 
	JCheckBoxMenuItem item = (JCheckBoxMenuItem)(e.getItemSelectable());
	String name = item.getText();
	ClientRecord rec = findClient(name);
	rec.enabled = item.isSelected();
	redrawTextArea();
    }
    //
    // Other Methods
    //
    public void hideWindow() {
	setVisible(false);
    }
    public void showWindow() {
	setVisible(true);
    }
}

class ClientRecord {
    public String name;
    public boolean enabled;
    public JCheckBoxMenuItem menuItem;
    public ClientRecord(String name, boolean enabled, JCheckBoxMenuItem menuItem) {
	this.name = name;
	this.enabled = enabled;
	this.menuItem = menuItem;
    }
}

class HistoryEntry {
    public String sender;
    public KQMLPerformative msg;
    public boolean sent;
    public HistoryEntry(String sender, KQMLPerformative msg, boolean sent) {
	this.sender = sender;
	this.msg = msg;
	this.sent = sent;
    }
}

