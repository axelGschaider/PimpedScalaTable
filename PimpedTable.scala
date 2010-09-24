/* PimpedScalaTable is a wrapper arround scalas swing.table or Javas JTable (haven't decided jet ;) ) that is meant to evercome all shortcomings I find lacking in scalas swing.table 


Copyright (C) 2010 Axel Gschaider

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, see <http://www.gnu.org/licenses/>. 


If you would like to obtain this programm under another license, feel free to contact me. Probably I won't charge you for commercial projects. Just would like to receive a courtesy call.
 
axel[dot]gschaider[at]gmx[dot]at
or http://github.com/axelGschaider
*/

package at.axelGschaider.pimpedTable

import scala.swing._
import javax.swing.JTable
import scala.swing.event._
import javax.swing.table.AbstractTableModel
import javax.swing.table.TableRowSorter
import javax.swing.event.{ListSelectionEvent, TableColumnModelListener, ChangeEvent, TableColumnModelEvent}
import java.awt.event.{MouseEvent, MouseAdapter}
import java.util.EventObject
import java.util.Comparator
import java.awt.Color

/*trait Row[A] {
  val data:A
  val isExpandable:Boolean = false
  def expandedData():List[A] = List.empty[A]
  var expanded:Boolean = false
  //val internalComparator:Option[Comparator[A]] = None
}*/

trait Row[A] {
  val data:A
}

case class DeadTree[A](data:A) extends Row[A]
case class Tree[A](data:A, leafData:List[A], internal:Option[Comparator[A]], var expanded:Boolean = false) extends Row[A] {
  var leaves:List[Leaf[A]] = leafData.map(x => Leaf(x, this))
}
case class Leaf[A](data:A, father:Tree[A]) extends Row[A]

/*trait UnexpandAbleRow[A] extends Row[A] {
  val isExpandable:Boolean = false
  def expandedData():List[A] = List.empty[A]
  var expanded:Boolean = false
}

trait ExpandableRow[A] extends Row[A] {
  val isExpandable:Boolean = false
  var expanded:Boolean = false

}*/

/*trait ExpandedData[A] extends Row[A]{
  val father:A
}*/




trait TableBehaviourClient {
  def paintExpandColumn:Boolean = false
  def moveColumn(oldIndex:Int, newIndex:Int):Unit
  def reordering_=(allowe:Boolean)
  def reordering:Boolean
}

case class TableBehaviourWorker(x: TableBehaviourClient) extends MouseAdapter with TableColumnModelListener {
  
  private var columnValue:Int = -1
  private var columnNewValue:Int = -1
  private var marginChanged:Boolean = false
  
  def columnSelectionChanged(e: ListSelectionEvent) {}
  def columnMarginChanged(e: ChangeEvent) {
    marginChanged = true
  }
  def columnMoved(e: TableColumnModelEvent) {
    if(columnValue == -1) columnValue = e.getFromIndex
    columnNewValue = e.getToIndex
  }
  def columnRemoved(e: TableColumnModelEvent) {}
  def columnAdded(e: TableColumnModelEvent) {}

  override def mouseReleased(e:MouseEvent) {
    var takeNewData = false
    if(x.paintExpandColumn) {
      if(columnValue != -1 && (columnValue == 0 || columnNewValue == 0)) {
        x.moveColumn(columnNewValue, columnValue)
        columnNewValue = -1
        columnValue = -1 
        println("fixed it")
      } 
    }
    if(columnValue != -1 && columnValue != columnNewValue) {
      println(columnValue + " -> " + columnNewValue)
      takeNewData = true
    }

    if(marginChanged) {
      println("Margin changed")
      takeNewData = true
    }

    if(takeNewData) {
      println("take new data")
    }

    columnNewValue = -1
    columnValue = -1
    marginChanged = false
  }
}


trait ColumnDescription[-A,+B] {
  val name:String

  def extractValue(x:A):B

  val isSortable:Boolean = false

  val ignoreWhileExpanding:Boolean = false

  val paintGroupColourWhileExpanding:Boolean = false

  def renderComponent(data:A, isSelected: Boolean, focused: Boolean, isExpanded: Boolean):Component

  def comparator: Option[Comparator[_ <: B]]
}

class PimpedTableModel[A,B](dat:List[Row[A]], columns:List[ColumnDescription[A,B]], var paintExpander:Boolean = false) extends AbstractTableModel {
  private var lokalData = dat
  
  private def expOffset = if (paintExpander) 1 else 0

  def data = lokalData

  def data_=(d: List[Row[A]]) = {
    lokalData = d
  }

  def getRowCount():Int = data.length

  def getColumnCount():Int = columns.length + expOffset


  override def getValueAt(row:Int, column:Int): java.lang.Object = {
    if(row < 0 || column < 0 || column >= columns.length + expOffset || row >= data.length) {
      throw new Error("Bad Table Index: row " + row + " column " + column)
    }
    
    if(paintExpander && column == 0) {
      //data(row).isExpandable.asInstanceOf[java.lang.Object]
      data(row) match {
        case Tree(_,_,_,_) => true.asInstanceOf[java.lang.Object]
        case _           => false.asInstanceOf[java.lang.Object]
      }
    }
    else {
      (columns(column - expOffset) extractValue data(row).data).asInstanceOf[java.lang.Object]
    }
  }

  override def getColumnName(column: Int): String = {
    if(paintExpander && column == 0) {
      ""
    } else {
      columns(column - expOffset).name
    }
  }
}

class ConvenientPimpedTable[A, B](dat:List[A], columns:List[ColumnDescription[A,B]]) extends PimpedTable[A, B](dat.map(x => new Row[A] {val data = x}),columns) 

case class PimpedTableSelectionEvent(s:Table) extends TableEvent(s)

class PimpedTable[A, B](initData:List[Row[A]], columns:List[ColumnDescription[A,B]]) extends Table with TableBehaviourClient {

  private var fallbackDat = initData
  private var filteredDat = fallbackDat
  
  private var expandColumn:Boolean = false
  override def paintExpandColumn:Boolean = expandColumn
  def paintExpandColumn_=(paint:Boolean) = {
    expandColumn = paint
    //TODO
    fallbackData = fallbackData
  }
  def moveColumn(oldIndex:Int, newIndex:Int) = peer.moveColumn(oldIndex, newIndex)
  def reordering_=(allowe:Boolean) = peer.getTableHeader setReorderingAllowed allowe
  def reordering:Boolean = peer.getTableHeader.getReorderingAllowed
  private val behaviourWorker = TableBehaviourWorker(this)
  peer.getColumnModel().addColumnModelListener(behaviourWorker)
  peer.getTableHeader().addMouseListener(behaviourWorker)


  private var tableModel:PimpedTableModel[A,B] = new PimpedTableModel(filteredData, columns, paintExpandColumn)
  private var savedFilter:Option[(A => Boolean)] = None

  private var blockSelectionEvents:Boolean = false

  val sorter = new TableRowSorter[PimpedTableModel[A,B]]() 
  this.peer.setRowSorter(sorter)

  private def fillSorter = {
    val offset = if(paintExpandColumn) 1 else 0

    columns.zipWithIndex filter {x => x match {
      case (colDes,_) => colDes.isSortable
    }} foreach {x => x match {
          case (colDes,i) => { colDes.comparator match {
            case None    => {}
            case Some(c) => sorter.setComparator(i+offset, c)
          }
        }
      }
    }
  }
  
  private def fallbackData = fallbackDat
  private def fallbackData_=(d:List[Row[A]]) = {
    fallbackDat = d
    savedFilter match {
      case Some(f) => filteredData = fallbackData.filter(x => f(x.data))
      case None    => filteredData = fallbackData
    }

  }

   
  def filteredData = filteredDat
  private def filteredData_= (d:List[Row[A]]) = {
    var sel = selectedData()
    blockSelectionEvents = true
    filteredDat = d
    tableModel =  new PimpedTableModel(filteredData, columns, paintExpandColumn)
    this.model = tableModel
    sorter setModel tableModel
    fillSorter
    if(sel.size!=0 && !sel.map(select(_)).forall(x => x)) {
      blockSelectionEvents = false
      publish(PimpedTableSelectionEvent(this))
    }
    
    if(paintExpandColumn) {
      val col = peer.getTableHeader.getColumnModel.getColumn(0)
      col setWidth 30
      col setMinWidth 30
      col setMaxWidth 30
    }

    blockSelectionEvents = false
  }

  def data:List[Row[A]] = fallbackData

  def data_=(da:List[Row[A]])= {
    fallbackData = da
    /*var sel = selectedData()
    blockSelectionEvents = true
    fallbackData = da
    //triggerDataChange()
    tableModel =  new PimpedTableModel(filteredData, columns)
    this.model = tableModel
    sorter setModel tableModel
    fillSorter
    if(sel.size!=0 && !sel.map(select(_)).forall(x => x)) {
      //System.out.println("something changed")
      blockSelectionEvents = false
      publish(TableRowsSelected(this, Range(0,0), true))
    }
    blockSelectionEvents = false*/
  }

  def isFiltered() = savedFilter match {
    case None    => false
    case Some(_) => true
  }

  def filter(p: (A) => Boolean):Unit = {
    savedFilter = Some(p)
    filteredData = fallbackData.filter(x => p(x.data))
  }

  def unfilter():Unit = {
    savedFilter = None
    filteredData = fallbackData
  }

  def select(x:A):Boolean = {
    filteredData.map(_.data).indexOf(x) match {
      case -1 => {false}
      case i  => {
        this.selection.rows += i
        true
      }
    }
  }

  def unselect(x:A):Boolean = {
    filteredData.map(_.data).indexOf(x) match {
      case -1 => {false}
      case i  => {
        this.selection.rows -= i
        true
      }
    }
  }
  
  listenTo(this.selection)
  reactions += {
    case e@TableRowsSelected(_,_,true) => if(!blockSelectionEvents) publish(PimpedTableSelectionEvent(this))
  }

  def unselectAll():Unit = this.selection.rows.clear

  override  def   rendererComponent(isSelected: Boolean, focused: Boolean, row: Int, column: Int): Component = {
    rendererComponentForPeerTable(isSelected, focused, row, column)
  }
  
  private def convertedRow(row:Int) = this.peer.convertRowIndexToModel(row)
  private def convertedColumn(column:Int) = this.peer.convertColumnIndexToModel(column)
  
  def rendererComponentForPeerTable(isSelected: Boolean, focused: Boolean, row: Int, column: Int): Component = {
    if(paintExpandColumn) {
      if(column == 0) {
        //TODO
        new Label() {
          opaque = true
          background = Color.RED
        }
      }
      else {
        val c = convertedColumn(column)-1
        if(c >= 0) columns(c).renderComponent(filteredData(convertedRow(row)).data, isSelected, focused, /*TODO*/false)
        else {/*println("Föhlah: " + c); */new Label("")}
      }
    } else {
      columns(convertedColumn(column)).renderComponent(filteredData(convertedRow(row)).data, isSelected, focused, /*TODO*/false)
    }
  }
  
  def selectedData():List[A] = this.selection.rows.toList.map(i => filteredData(convertedRow(i)).data)

  

  fallbackData = initData

}

