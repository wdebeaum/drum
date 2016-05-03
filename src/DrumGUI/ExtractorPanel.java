/*
 * ExtractorPanel.java
 *
 * Time-stamp: <Sun May 10 15:46:13 CDT 2015 lgalescu>
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>, 18 Feb 2010
 */

package TRIPS.DrumGUI;

import java.awt.BorderLayout;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.HashSet;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;

public class ExtractorPanel extends JPanel 
        implements TreeSelectionListener, KeyListener
{
    private TextPanel textPanel;

    /** Display type.
     * In TREE mode, a 3-level representation is used: root->type->item. 
     * In LIST mode, a 2-level representation is used: root->item. 
     * In either mode, the root is not visible, so perceptually the TREE mode
     * results in a 2-level presentation, and the LIST mode results in a flat 
     * presentation.
     */
    private enum Mode {
        TREE, LIST
    };

    static private Mode DEFAULT_MODE = Mode.TREE;
    private Mode mode = DEFAULT_MODE;

    private ExtractionTree xTree;

    private ArrayList<Extraction> items;
    private ArrayList<Extraction> filteredItems;

    private boolean isFiltered = false;

    public ExtractorPanel(TextPanel textPanel) {
        super(new BorderLayout());

        this.textPanel = textPanel;

        items = new ArrayList<Extraction>();
        filteredItems = new ArrayList<Extraction>();
        xTree = new ExtractionTree((TreeSelectionListener) this);

        JTree tree = xTree.getTree();
        JScrollPane scrollPane = new JScrollPane(tree);
        add(scrollPane, BorderLayout.CENTER);

        tree.setFocusable(false); 
        tree.addKeyListener(this);
    }

    protected void setMode(Mode mode) {
        this.mode = mode;
    }

    protected void setMode(String mode) {
        try {
            Debug.warn("ExtractorPanel: Setting mode: " + mode);
            setMode(Mode.valueOf(mode.toUpperCase()));
        } catch (IllegalArgumentException ex) {
            Debug.error("ExtractorPanel: Illegal mode: " + mode);
        } catch (NullPointerException ex) {
            Debug.error("ExtractorPanel: null mode");
        }
    }

    protected void addItem(Extraction item) {
        if (item == null) {
            return;
        }
        items.add(item);
        addItemToTree(item);
        // flash the item in text
        textPanel.setHighlight(item.getOffset(item.getStart()), item.getOffset(item.getEnd()));
        try {
            Thread.sleep(150);
        } catch (Exception e) {
            Debug.error(e);
        }
        textPanel.removeHighlight();
    }

    private DefaultMutableTreeNode addItemToTree(Extraction item) {
        if (mode == Mode.TREE) {
            String type = item.getExType();
            DefaultMutableTreeNode typeNode = xTree.findNode(type);
            if (typeNode == null) {
                typeNode = xTree.addObject(null, type);
            }
            return xTree.addObject(typeNode, item);
        } else {
            return xTree.addObject(null, item);
        }
    }

    private void switchMode(Mode mode) {
        if (this.mode == mode)
            return; // nop
        Extraction selected = null;
        DefaultMutableTreeNode selectedNode = xTree.getCurrentSelection();
        if ((selectedNode != null) && selectedNode.isLeaf())
            selected = (Extraction) xTree.getCurrentSelectionObject();
        xTree.clear();
        setMode(mode);
        ArrayList<Extraction> list =
                isFiltered ? filteredItems : items;
        for (Extraction ex : list)
            addItemToTree(ex);
        if (selected != null)
            xTree.setCurrentSelection(selected);
    }

    private void showAll() {
        if (!isFiltered)
            return; // nop
        Extraction selected = null;
        DefaultMutableTreeNode selectedNode = xTree.getCurrentSelection();
        if ((selectedNode != null) && selectedNode.isLeaf())
            selected = (Extraction) xTree.getCurrentSelectionObject();
        xTree.clear();
        for (Extraction ex : items)
            addItemToTree(ex);
        if (selected != null)
            xTree.setCurrentSelection(selected);
        filteredItems.clear();
        isFiltered = false;
    }

    private void filterByUttnum() {
        DefaultMutableTreeNode selectedNode = xTree.getCurrentSelection();
        if ((selectedNode == null) || !selectedNode.isLeaf())
            return; // nop
        Extraction selected = (Extraction) xTree.getCurrentSelectionObject();
        if (selected == null) {
            Debug.error("/EP/ selected: null");
            return;
        }
        int uttnum = selected.getUttnum();
        xTree.clear();
        for (Extraction ex : items)
            if (ex.getUttnum() == uttnum) {
                filteredItems.add(ex);
                addItemToTree(ex);
                if (ex.equals(selected)) {
                    xTree.setCurrentSelection(ex);
                }
            }
        isFiltered = true;
    }

    private void filterByNeighborhood() {
        DefaultMutableTreeNode selectedNode = xTree.getCurrentSelection();
        if ((selectedNode == null) || !selectedNode.isLeaf())
            return; // nop
        Extraction selected = (Extraction) xTree.getCurrentSelectionObject();
        if (selected == null) {
            Debug.error("/EP/ selected: null");
            return;
        }
        HashSet<String> sVars = selected.findAllVars();
        xTree.clear();
        for (Extraction ex : items) {
            HashSet<String> xVars = ex.findAllVars();
            xVars.retainAll(sVars); // ie, xVars ^ sVars != []
            if (!xVars.isEmpty()) {
                filteredItems.add(ex);
                addItemToTree(ex);
                if (ex.equals(selected)) {
                    xTree.setCurrentSelection(ex);
                }
            }
        }
        isFiltered = true;
    }

    protected void clear() {
        items.clear();
        xTree.clear();
    }

    protected void update() {
        textPanel.setHighlight(10, 100);
    }

    /** TreeSelectionListener methods
     */
    public void valueChanged(TreeSelectionEvent e) {
        // Returns the last path element of the selection.
        // This method is useful only when the selection model allows a single selection.
        DefaultMutableTreeNode node = xTree.getCurrentSelection();
        if (node == null)
            return; // nop
        if (node.isLeaf()) {
            Extraction item = (Extraction) node.getUserObject();
            textPanel.setHighlight(item.getOffset(item.getStart()), item.getOffset(item.getEnd()));
        } else {
            textPanel.removeHighlight();
        }
    }

    /** Turn on/off operations on tree.
     */
    protected void setTreeOperationsEnabled(boolean enabled) {
        xTree.getTree().setFocusable(enabled);
    }

    /** Show help alert */
    private void showHelp() {
    };

    /** Hide help alert */
    private void hideHelp() {
    };


    /** Handle the key typed event. */
    public void keyTyped(KeyEvent e) {
    }

    /** Handle the key-pressed event. */
    public void keyPressed(KeyEvent e) {
        int keycode = e.getKeyCode();
        // Debug.debug("keyPressed: e=" + e);
        if (e.isMetaDown()) {
            if (keycode == KeyEvent.VK_N) {
                // show only extractions connected to selection
                filterByNeighborhood();
            } else if (keycode == KeyEvent.VK_U) {
                // show only extractions for current utterance
                filterByUttnum();
            } else if (keycode == KeyEvent.VK_A) {
                // show all extractions
                showAll();
            } else if (keycode == KeyEvent.VK_T) {
                // switch to tree mode
                switchMode(Mode.TREE);
            } else if (keycode == KeyEvent.VK_L) {
                // switch to list mode
                switchMode(Mode.LIST);
            }
        } else if (e.isShiftDown() && keycode == KeyEvent.VK_SLASH) {
            // help
            showHelp();
        } else if (keycode == KeyEvent.VK_ESCAPE) {
            // help
            hideHelp();
        }
    }

    /** Handle the key-released event. */
    public void keyReleased(KeyEvent e) {
    }

    protected void setFontSize(int size) {
        xTree.setFontSize(size);
    }

}
