/*
 * TextPanel.java
 *
 * Time-stamp: <Mon Mar  2 12:34:53 CST 2015 lgalescu>
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>, 17 Feb 2010
 */

package TRIPS.DrumGUI;

import java.awt.BorderLayout;
import java.awt.Color;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

public class TextPanel extends JPanel {
    private JTextPane textPane;
    private JScrollPane textScrollPane;
    private StyledDocument doc;

    private boolean documentEdited = false;
    private DocumentListener editDocumentListener;

    private String savedText = null;
    
    final String fontFamily = "Lucida Sans Unicode";

    private int highlightStart = 0;
    private int highlightLength = 0;

    public TextPanel() {
	super(new BorderLayout());
	
	textPane = new JTextPane();
	textScrollPane = new JScrollPane(textPane);
	doc = textPane.getStyledDocument();
	
	// fit
	add(textScrollPane);

	// attributes
	textPane.setEditable(false);

	// styles
	addStyles(doc);
	doc.setLogicalStyle(0, doc.getStyle("logical"));

	// listeners
	editDocumentListener = new DocumentListener() {
		public void insertUpdate(DocumentEvent e) {
		    documentEdited = true;
		}
		public void removeUpdate(DocumentEvent e) {
		    documentEdited = true;
		}
		public void changedUpdate(DocumentEvent e){}
	    };
    }

    protected void setEditable(boolean editable) {
	textPane.setEditable(editable);
	if (editable) {
	    savedText = getText();
	    doc.addDocumentListener(editDocumentListener);
	} else {
	    doc.removeDocumentListener(editDocumentListener);
	    documentEdited = false;
	    savedText = null;
	}
    }
			       
    private void addStyles(StyledDocument doc) {
        // Logical style.
	Style s = doc.addStyle("logical", null);
        StyleConstants.setFontFamily(s, fontFamily);
 
        // Character attributes.
        s = doc.addStyle("bold", null);
        StyleConstants.setBold(s, true);
        s = doc.addStyle("italic", null);
        StyleConstants.setItalic(s, true);
        s = doc.addStyle("underline", null);
        StyleConstants.setUnderline(s, true);
        s = doc.addStyle("red", null);
        StyleConstants.setForeground(s, Color.red);
        s = doc.addStyle("blue", null);
        StyleConstants.setForeground(s, Color.blue);
    }

    protected void setText(String text) {
	removeHighlight();
	try {
            doc.insertString(0, text, null);  // uses default style
        } catch(BadLocationException e) {
            Debug.error("/TP/ Bad location: " + e.getMessage());
        }
	textPane.setCaretPosition(0);
    }

    protected void discardEdits() {
	clear();
	setText(savedText);
    }

    protected String getText() {
	return textPane.getText();
    }

    protected String getText(int start, int end) {
	return textPane.getText().substring(start, end);
    }

    protected void clear() {
	try {
            doc.remove(0, doc.getLength());  
        } catch(BadLocationException e) {
            Debug.error("/TP/ Bad location: " + e.getMessage());
        }	
    }

    protected void setHighlight(int start, int end) {
	unhighlight();
	highlightStart = start;
	highlightLength = end-start; // no +1 needed!
	highlight();
    }
    protected void removeHighlight() {
	unhighlight();
	highlightStart = highlightLength = 0;
    }

    private void highlight() {
	if (highlightLength <= 0) {
	    return;
	}
	textPane.setCaretPosition(highlightStart);
	doc.setCharacterAttributes(highlightStart, highlightLength,
				   doc.getStyle("red"), 
				   false);
    }
    private void unhighlight() {
	SimpleAttributeSet sas = new SimpleAttributeSet();
	doc.setCharacterAttributes(highlightStart, highlightLength, 
				   sas,
				   true);
    }

    protected boolean isEdited() {
	return documentEdited;
    }

    protected void setFontSize(int size) {
        StyleConstants.setFontSize(doc.getStyle("logical"), size);
    }

}
