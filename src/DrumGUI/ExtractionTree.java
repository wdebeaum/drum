/*
 * ExtractionTree.java
 *
 * Time-stamp: <Mon Nov 30 15:40:18 CST 2015 lgalescu>
 * 
 * Author: Lucian Galescu <lgalescu@ihmc.us>, 18 Feb 2010
 */

package TRIPS.DrumGUI;

/* A 3-level tree: 
 * - level 1: root (should be invisible)
 * - level 2: type nodes
 * - level 3: extraction nodes: can be clicked & hovered over to reveal info
 */

import java.awt.Font;
import java.awt.event.MouseEvent;
import java.util.Enumeration;

import javax.swing.JTree;
import javax.swing.ToolTipManager;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

public class ExtractionTree
{
    protected DefaultMutableTreeNode rootNode;
    protected DefaultTreeModel treeModel;
    protected JTree tree;

    public ExtractionTree(TreeSelectionListener selectionListener) {
        rootNode = new DefaultMutableTreeNode();
        treeModel = new DefaultTreeModel(rootNode);
        tree = new JTree(treeModel) {
		public String getToolTipText(MouseEvent e) {
		    DefaultMutableTreeNode tip = null;
		    TreePath path = getPathForLocation(e.getX(), e.getY());
		    if (path != null) 
			tip = (DefaultMutableTreeNode) path.getLastPathComponent();
		    if (tip == null) return null;
		    if (tip.isLeaf()) {
			Extraction ex = (Extraction) tip.getUserObject();
			if (ex == null) return null; // should never happen
			String s = ex.toString();
			if (s.equals("")) return null;
			String newline = System.getProperty("line.separator");
			s = s.replaceAll(newline, "<br>");
			int fontSize = getFont().getSize()-1;
			//FIXME: i should make sure the tooltip fits on the screen!
			// it now does on my machine by virtue of having a maximum fontSize
			s = "<html><p width=\""+50*fontSize+"\" style=\"font-size:"+fontSize+"\">" + s + "</p></html>";
			return s;
		    }
		    return null;
		}
	    };
        tree.getSelectionModel().setSelectionMode
                (TreeSelectionModel.SINGLE_TREE_SELECTION);
	tree.addTreeSelectionListener(selectionListener);
        tree.setShowsRootHandles(true);
	tree.setRootVisible(false);
	// this is not doing anything for the Mac LAF:
	// tree.putClientProperty("JTree.lineStyle", "Horizontal");
	// we want row height to be determined automatically -- doesn't work!
	// tree.setRowHeight(-1);
	//Enable tool tips & make them stay on
	ToolTipManager.sharedInstance().registerComponent(tree);
	ToolTipManager.sharedInstance().setDismissDelay(Integer.MAX_VALUE);

    }

    /** Get tree member */
    public JTree getTree() {
	return tree;
    }
    
    /** Remove all nodes except the root node. */
    public void clear() {
        rootNode.removeAllChildren();
        treeModel.reload();
    }

    /** Remove the currently selected node. */
    public void removeCurrentNode() {
	DefaultMutableTreeNode currentNode = getCurrentSelection();
	if (currentNode == null) return;
	MutableTreeNode parent = (MutableTreeNode)(currentNode.getParent());
	if (parent != null) {
	    treeModel.removeNodeFromParent(currentNode);
	    return;
	    // FIXME: remove parent if childless?
	}
        // toolkit.beep();
    }

    /** Add child to the currently selected node. */
    public DefaultMutableTreeNode addObject(Object child) {
        DefaultMutableTreeNode parentNode = null;
        TreePath parentPath = tree.getSelectionPath();

        if (parentPath == null) {
            parentNode = rootNode;
        } else {
            parentNode = (DefaultMutableTreeNode)
                         (parentPath.getLastPathComponent());
        }

        return addObject(parentNode, child, true);
    }

    public DefaultMutableTreeNode addObject(DefaultMutableTreeNode parent,
                                            Object child) {
        return addObject(parent, child, true);
    }

    public DefaultMutableTreeNode addObject(DefaultMutableTreeNode parent,
                                            Object child, 
                                            boolean shouldBeVisible) {
        DefaultMutableTreeNode childNode = 
                new DefaultMutableTreeNode(child);

        if (parent == null) {
            parent = rootNode;
        }
	
        treeModel.insertNodeInto(childNode, parent, 
                                 parent.getChildCount());

        if (shouldBeVisible) {
            tree.scrollPathToVisible(new TreePath(childNode.getPath()));
        }
        return childNode;
    }

    /** Recursively look for an object */
    public DefaultMutableTreeNode searchNodeChildren(Object object, DefaultMutableTreeNode node) {
        if (node == null) {
            return null;
        }
        for (Enumeration e = node.children(); e.hasMoreElements();) {
            node = (DefaultMutableTreeNode) e.nextElement();
            if (object.equals(node.getUserObject())) {
                return node;
            }
            if (!node.isLeaf()) {
                DefaultMutableTreeNode result = searchNodeChildren(object, node);
                if (result != null)
                    return result;
            }
        }
        return null;
    }

    public DefaultMutableTreeNode findNode(Object object) {
	return searchNodeChildren(object, rootNode);
    }

    public DefaultMutableTreeNode getCurrentSelection() {
	return (DefaultMutableTreeNode) tree.getLastSelectedPathComponent();
    }

    public Object getCurrentSelectionObject() {
	DefaultMutableTreeNode node = getCurrentSelection();
	if (node == null) return null; 
	return node.getUserObject();
    }

    public void setCurrentSelection(Object object) {
	DefaultMutableTreeNode node = findNode(object);
	if (node != null) 
	    tree.setSelectionPath(new TreePath(node.getPath()));
	else
            Debug.error("Selected item not found: " + object);
    }

    protected void setFontSize(int size) {
	Font f = tree.getFont();
	Font newFont = new Font(f.getName(), f.getStyle(), size);
        tree.setFont(newFont);
	// adjust row heights
	tree.setRowHeight(0);
    }

}
