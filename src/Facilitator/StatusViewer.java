/*
 * StatusViewer.java
 *
 * William de Beaumont
 * 2016-12-14
 */

package TRIPS.Facilitator;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;
import java.awt.Component;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableCellRenderer;
import TRIPS.KQML.KQMLPerformative;

/**
 * Displays the detailed status string of each connected TRIPS module. Could be
 * used as a standalone viewer, but it's usually part of ModemDisplay. See also
 * TrafficViewer.
 */
public class StatusViewer extends JFrame implements FacilitatorDisplay {
  private final static String[] cha = { "Module Name", "Module Status" };
  final static Vector<String> columnHeadings =
    new Vector<String>(Arrays.asList(cha));
  /** A 2 column by n row vector of vectors of strings representing the module
   * status table.
   */
  Vector<Vector<String>> tableData;
  /** Compares rows of tableData. */
  final static Comparator<Vector<String>> rowComparator =
    new Comparator<Vector<String>>() {
      @Override
      public int compare(Vector<String> row1, Vector<String> row2) {
	return row1.get(0).compareTo(row2.get(0));
      }
    };
  class NonEditableTableModel extends DefaultTableModel {
    public NonEditableTableModel(Vector data, Vector columnNames) {
      super(data, columnNames);
    }
    @Override
    public boolean isCellEditable(int row, int column) { return false; }
  }
  NonEditableTableModel tableModel;
  JTable table;

  public StatusViewer(Facilitator facilitator) {
    setTitle("TRIPS Module Statuses");
    setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    tableData = new Vector<Vector<String>>();
    Registry registry = facilitator.getRegistry();
    Enumeration clients = registry.enumerateClients();
    while (clients.hasMoreElements()) {
      Sendable client = (Sendable)clients.nextElement();
      Vector<String> row = new Vector<String>();
      row.add(registry.lookupNameOfClient(client));
      row.add(facilitator.clientStatuses.get(client));
      tableData.add(row);
    }
    Collections.sort(tableData, rowComparator);
    tableModel = new NonEditableTableModel(tableData, columnHeadings);
    table = new JTable(tableModel);
    setNameColumnWidth();
    JScrollPane scroller = new JScrollPane(table);
    getContentPane().add(scroller);
    pack();
    setVisible(true);
  }

  // support standalone
  public StatusViewer(Facilitator facilitator, Boolean hideTraffic) {
    this(facilitator);
  }

  /** Set the max/preferred widths of the name column to the maximum of the
   * preferred widths of the cells.
   */
  void setNameColumnWidth() {
    // first, get the width of the heading:
    TableColumn column = table.getColumnModel().getColumn(0);
    TableCellRenderer r = table.getTableHeader().getDefaultRenderer();
    // this annoyingly doesn't work for headings like it does for regular cells:
    //Component c = table.prepareRenderer(r, -1, 0);
    // but this does:
    Component c =
      r.getTableCellRendererComponent(table, cha[0], false, false, -1, 0);
    int width = c.getPreferredSize().width;
    // then, get the widths of the cells:
    int numRows = table.getRowCount();
    for (int i = 0; i < numRows; i++) {
      r = table.getCellRenderer(i, 0);
      c = table.prepareRenderer(r, i, 0);
      int cellWidth = c.getPreferredSize().width;
      width = Math.max(width, cellWidth);
    }
    width += table.getIntercellSpacing().width;
    column.setMaxWidth(width);
    column.setPreferredWidth(width);
  }

  /** Return the index of the row whose Module Name value is name. If there is
   * no such row, return -i-1 where i is the index where such a row should be
   * inserted.
   */
  int findRow(String name) {
    Vector<String> search = new Vector<String>();
    search.add(name);
    return Collections.binarySearch(tableData, search, rowComparator);
  }

  /** Return the status string for the named module. */
  public String getStatus(String name) {
    int i = findRow(name);
    if (i < 0) {
      Debug.warn("no module named " + name + " connected.");
      return "";
    } else {
      return tableData.get(i).get(0);
    }
  }

  //
  // FacilitatorDisplay methods
  //

  @Override
  public void add(String name) {
    int i = findRow(name);
    if (i >= 0) {
      Debug.warn("module named " + name + " already connected.");
    } else {
      i = -i-1; // convert to insertion point
      String[] ra = { name, "CONNECTED" };
      tableModel.insertRow(
        i,
	new Vector<String>(Arrays.asList(ra))
      );
      setNameColumnWidth();
    }
  }

  @Override
  public void remove(String name) {
    int i = findRow(name);
    if (i < 0) {
      Debug.warn("no module named " + name + " connected.");
    } else {
      tableModel.removeRow(i);
      setNameColumnWidth();
    }
  }

  // never actually called?
  @Override
  public void changeName(String oldname, String newname) {
    // remove the row with the old name
    int i = findRow(oldname);
    if (i < 0) {
      Debug.warn("no module named " + oldname + " connected.");
    } else {
      Vector<String> row = tableData.get(i);
      tableModel.removeRow(i);
      // find the place to put it back with the new name
      int j = findRow(newname);
      if (j >= 0) {
	Debug.warn("module named " + newname + " already connected.");
	tableModel.insertRow(i, row); // put it back without changing the name
      } else {
	j = -j-1; // convert to insertion point
	// set the name and put the row back
	row.set(0, newname);
	tableModel.insertRow(j, row);
	setNameColumnWidth();
      }
    }
  }

  // these two do nothing and are never called, but are required by the
  // FacilitatorDisplay interface
  @Override
  public void indicateMessageSent(String sender, KQMLPerformative msg) {}
  @Override
  public void indicateMessageReceived(String receiver, KQMLPerformative msg) {}

  // the whole point of this class
  @Override
  public void indicateStatus(String name, String status) {
    int i = findRow(name);
    if (i < 0) {
      Debug.warn("no module named " + name + " connected.");
    } else {
      tableModel.setValueAt(status, i, 1);
    }
  }

  @Override
  public void showWindow() {
    setVisible(true);
  }

  @Override
  public void hideWindow() {
    setVisible(false);
  }
}
