/*
 * TagOptionsInput.java
 *
 * Time-stamp: <Thu Mar 26 10:27:35 CDT 2015 lgalescu>
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>, 21 Feb 2010
 */

package TRIPS.DrumGUI;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import TRIPS.KQML.KQMLList;

public class TagOptionsInput extends JFrame 
        implements ComponentListener, ActionListener
{
    private Display parent;

    private JTextArea inputArea;

    static final int MIN_WIDTH = 333;
    static final int MIN_HEIGHT = 222;

    /**
     * Constructor specifying parent window.
     */
    public TagOptionsInput(Display parent) {
        super("Tag Options");

        this.parent = parent;

        createWidgets();
        pack();
        setSize(MIN_WIDTH, MIN_HEIGHT);
        addComponentListener(this);
        // put it in the middle of the screen
        // FIXME: this should be relative to main frame
        setLocationRelativeTo(null);
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        setVisible(true);

        // set initial contents of input area
        setText(parent.getTagOptions());
    }

    /** Gets the contents of the text area. */
    public String getText() {
        return inputArea.getText();
    }

    /** Sets the contents of the text area. */
    public void setText(String text) {
        inputArea.setText(text);
    }

    private void createWidgets() {
        // inputArea: a text area in a scroll pane
        inputArea = new JTextArea(5, 40);
        inputArea.setLineWrap(true);
        inputArea.setWrapStyleWord(true);
        inputArea.setEditable(true);
        JScrollPane inputPanel = new JScrollPane(inputArea);
        inputPanel.setBorder(BorderFactory.createTitledBorder("Type text:"));

        // buttons: cancel & save
        JButton cancelButton = new JButton("Cancel");
        JButton clearButton = new JButton("Clear");
        JButton saveButton = new JButton("Save");
        cancelButton.setActionCommand("cancel");
        cancelButton.addActionListener(this);
        clearButton.setActionCommand("clear");
        clearButton.addActionListener(this);
        saveButton.setActionCommand("save");
        saveButton.addActionListener(this);
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS));
        buttonPanel.add(cancelButton);
        buttonPanel.add(Box.createHorizontalGlue());
        buttonPanel.add(clearButton);
        buttonPanel.add(Box.createHorizontalGlue());
        buttonPanel.add(saveButton);

        // add everything to frame
        Container pane = getContentPane();
        pane.add(inputPanel, BorderLayout.CENTER);
        pane.add(buttonPanel, BorderLayout.PAGE_END);
    }

    protected void cancel() {
        dispose();
    }

    protected void done() {
        dispose();
        parent.flipState(Display.State.TAGOPT);
    }

    // button actions
    public void actionPerformed(ActionEvent e) {
        String cmd = e.getActionCommand();
        if (cmd.equals("cancel")) {
            done();
        } else if (cmd.equals("clear")) {
            setText(null);
        } else if (cmd.equals("save")) {
            String text = getText();
            if (text.equals("")) {
                JOptionPane.showMessageDialog(this, "Returning to defaults!");
            }
            try {
                KQMLList tagOptions = KQMLList.fromString("(" + text + ")");
                parent.setTagOptions(tagOptions);
                done();
            } catch (Exception ex) {
                ex.printStackTrace();
                JOptionPane.showMessageDialog(this, "Invalid format!");
                inputArea.requestFocusInWindow();
                return;
            }
        }
    }

    public void componentResized(ComponentEvent e) {
        int width = getWidth();
        int height = getHeight();
        // we check if either the width or the height are below minimum
        boolean resize = false;
        if (width < MIN_WIDTH) {
            resize = true;
            width = MIN_WIDTH;
        }
        if (height < MIN_HEIGHT) {
            resize = true;
            height = MIN_HEIGHT;
        }
        if (resize) {
            setSize(width, height);
        }
    }
    public void componentMoved(ComponentEvent e) {}
    public void componentShown(ComponentEvent e) {}
    public void componentHidden(ComponentEvent e) {}

}
