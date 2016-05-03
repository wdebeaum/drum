/*
 * SelectorPanel.java
 *
 * Time-stamp: <Sun May 10 14:49:26 CDT 2015 lgalescu>
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>,  8 Feb 2010
 */

package TRIPS.DrumGUI;

import java.awt.BorderLayout;
import java.awt.EventQueue;
import java.awt.Font;
import java.awt.GridLayout;
import java.util.Arrays;
import java.util.Vector;

import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public class SelectorPanel extends JPanel 
    implements ListSelectionListener
 {
    private Display parent;
    private DataSet data;

    private JList list;
    private volatile DefaultListModel listModel;

    /**
     * Flag signaling whether selection operations should be updating the dataset.
     */
    private boolean pushingSelections = true;

    public SelectorPanel(Display parent, DataSet data) {
        super(new BorderLayout());

        this.parent = parent;
        this.data = data;

        listModel = new DefaultListModel();

        //Create the list and put it in a scroll pane.
        list = new JList(listModel);
        list.addListSelectionListener(this);

        list.setFont(new Font("Monaco", Font.PLAIN, parent.getFontSize()));

        //list.setVisibleRowCount(20);
        JScrollPane listPane = new JScrollPane(list);

        JPanel listContainer = new JPanel(new GridLayout(1, 1));
        listContainer.add(listPane);

        add(listContainer, BorderLayout.CENTER);
    }

    protected DataSet getDataset() {
        return data;
    }

    /**
     * Sets the selection mode.
     * 
     * @param mode
     */
    protected void setSelectionMode(int mode) {
        list.setSelectionMode(mode);
    }

    /**
     * Show selected item (without the side effect of pushing selections onto
     * the dataset).
     * 
     * @param item
     * 
     * @see #pushSelections()
     */
    protected void showSelected(String item) {
        boolean updating = isPushingSelections();
        setPushingSelections(false);
        int n = find(item);
        Debug.debug("Showing selection: " + n);
        list.setSelectedIndex(n);
        list.ensureIndexIsVisible(n);
        if (updating)
            setPushingSelections(true);
    }

    /**
     * Getter for {@link #pushingSelections}.
     */
    private synchronized boolean isPushingSelections() {
        Debug.debug("isPushingSelections: " + pushingSelections);
        return pushingSelections;
    }

    /**
     * Setter for {@link #pushingSelections}.
     */
    private synchronized void setPushingSelections(boolean value) {
        Debug.debug("Pushing selections: " + value);
        pushingSelections = value;
    }

    protected void setPushSelectionEnabled(boolean value) {
        setPushingSelections(value);
    }

    /**
     * Reloads data into the model. Should be used sparingly.
     */
    protected void refresh() {
        clear();
        add(data.getFilenames().toArray(new String[0]));
        Debug.debug("Displayed " + listModel.size() + " items");
    }

    /**
     * Adds a single element to the {@link #listModel}.
     * 
     * @param item
     */
    private void add(final String item) {
        // we do this on the EDT, since it updates the GUI
        try {
            if (EventQueue.isDispatchThread())
                listModel.addElement(item);
            else
                // NB: if this item is part of a list added with #add(String[]),
                // there won't be a new Runnable thread created for each item!
                EventQueue.invokeAndWait(new Runnable() {
                    public void run() {
                        listModel.addElement(item);
                    }
                });
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Adds a list of elements to the {@link #listModel}.
     * 
     * @param listData
     */
    private void add(final String[] listData) {
        try {
            if (EventQueue.isDispatchThread())
                for (String item : listData) {
                    add(item);
                }
            else
                // we create a thread here so we don't create one for each item
                // in #add(String)
                EventQueue.invokeAndWait(new Runnable() {
                    public void run() {
                        for (String item : listData) {
                            add(item);
                        }
                    }
                });
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    protected String get(int n) {
        return (String) listModel.get(n);
    }

    protected int find(String item) {
        return listModel.indexOf(item);
    }

    protected void clear() {
        listModel.clear();
    }

    protected Vector<Integer> getSelection() {
        Vector<Integer> result = new Vector<Integer>();
        for (int i = 0; i < listModel.size(); i++) {
            if (list.isSelectedIndex(i)) {
                result.add(i);
            }
        }
        return result;
    }

    /**
     * Clears selections.
     */
    protected void clearSelections() {
        list.clearSelection();
    }

    protected void pushSelections() {
        if (!isPushingSelections()) {
            return;
        }
        data.clearSelection();
        boolean haveSelection = false;
        for (Object item : list.getSelectedValues()) {
            data.select((String) item);
            haveSelection = true;
        }
        if (haveSelection) {
            parent.setState(Display.State.DATASET);
        } else {
            parent.unsetState(Display.State.DATASET);
        }
        Debug.warn("Selection: " + Arrays.toString(data.getSelection()));
    }

    //This method is required by ListSelectionListener.
    public void valueChanged(ListSelectionEvent e) {
        // Debug.debug("getValueIsAdjusting: " + e.getValueIsAdjusting());
        if (!e.getValueIsAdjusting()) {
            pushSelections();
        }
    }

    protected void setFontSize(int size) {
        Font f = list.getFont();
        Font newFont = new Font(f.getName(), f.getStyle(), size);
        list.setFont(newFont);
    }

}
