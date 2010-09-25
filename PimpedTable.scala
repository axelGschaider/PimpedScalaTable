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
import javax.swing.RowSorter.SortKey
import javax.swing.event.{ListSelectionEvent, TableColumnModelListener, ChangeEvent, TableColumnModelEvent}
import java.awt.event.{MouseEvent, MouseAdapter}
import java.util.EventObject
import java.util.Comparator
import java.awt.Color
import scala.annotation.unchecked.uncheckedVariance

/*trait Row[A] {
  val data:A
  val isExpandable:Boolean = false
  def expandedData():List[A] = List.empty[A]
  var expanded:Boolean = false
  //val internalComparator:Option[Comparator[A]] = None
}*/

trait ColumnValue[-A] {
  val father:Row[_ >: A]
}

trait Row[+A] {
  val data:A
}

case class DeadTree[A](data:A) extends Row[A]
case class LivingTree[A](data:A, leafData:List[A], internal:Option[Comparator[Leaf[A]]], var expanded:Boolean = false) extends Row[A] {
  var leaves:List[Leaf[A]] = leafData.map(x => Leaf(x, this))
}
case class Leaf[A](data:A, father:LivingTree[A]) extends Row[A]


trait TableBehaviourClient {
  def paintExpandColumn:Boolean = false
  def moveColumn(oldIndex:Int, newIndex:Int):Unit
  def reordering_=(allowe:Boolean)
  def reordering:Boolean
}

trait ComparatorRelais[+B] extends java.util.Comparator[B @uncheckedVariance]

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


trait ColumnDescription[-A, +B] {
  val name:String

  def extractValue(x:Row[A]):B

  val isSortable:Boolean = false

  val ignoreWhileExpanding:Boolean = false

  val paintGroupColourWhileExpanding:Boolean = false

  def renderComponent(data:Row[A], isSelected: Boolean, focused: Boolean, isExpanded: Boolean):Component

  def comparator: Option[ComparatorRelais[B]]
  //def comparator: /*Option[Comparator[ColumnValue[_ <: A]]]*/Option[Comparator[_ <: B]]
}

/*class ExtractorRelais[A,B](descr:ColumnDescription[A,B]) {
  def ex(x:Row[A]):ColumnValue[B] = descr extractValue x
}*/

class PimpedTableModel[A,B <: ColumnValue[A] ](dat:List[Row[A]], columns:List[ColumnDescription[A,B]], var paintExpander:Boolean = false) extends AbstractTableModel {
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
        case LivingTree(_,_,_,_) => true.asInstanceOf[java.lang.Object]
        case _                   => false.asInstanceOf[java.lang.Object]
      }
    }
    else {
      (columns(column - expOffset) extractValue data(row)/*.data*/).asInstanceOf[java.lang.Object]
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

class PimpedColumnComparator[A,B <: ColumnValue[A]](comp:Comparator[B], col:ColumnDescription[A,B], colIndex:Int, mother:SortInfo) extends Comparator[B] {
  override def compare(o1:B, o2:B):Int = internalCompare(o1,o2) match {
    case Some(i) => i
    case None    => comp.compare(o1, o2)
  }
  
  private def internalCompare(v1:B, v2:B):Option[Int] = (v1.father, v2.father) match {
    case(t:LivingTree[A], l:Leaf[A]) => Some(handleLivingtree(t,l))
    case(l:Leaf[A], t:LivingTree[A]) => Some(handleLivingtree(t,l) * (-1))
    case(d:DeadTree[A], l:Leaf[A]) => Some(comp.compare(v1, col extractValue l.father))
    case(l:Leaf[A], d:DeadTree[A]) => Some(comp.compare(col extractValue l.father, v2))
    case(l1:Leaf[A], l2:Leaf[A])   => Some(handleTwoLeaves(l1,l2))
    case(_,_) => None
    
  }

  private def handleTwoLeaves(l1:Leaf[A], l2:Leaf[A]):Int = (l1.father, l2.father) match {
    case (f1, f2) if f1 == f2 => f1.internal match {
      case None    => comp.compare(col extractValue l1, col extractValue l2)
      case Some(c) => mother.getSortOrder match {
        case Descending => c.compare(l1, l2) * (-1)
        case _          => c.compare(l1, l2)
      }
    }
    case (f1,f2) => comp.compare(col extractValue f1, col extractValue f2)
  }

  private def handleLivingtree(t:LivingTree[A], l:Leaf[A]):Int = (t, l.father) match {
    case (t1, t2) if t1 == t2 => over
    case (t1, t2)             => comp.compare(col extractValue t1, col extractValue t2)
  }

  private def over = mother.getSortOrder match {
    case Descending => 1
    case _          => -1
  }

}

sealed trait SortOrder
case object Ascending extends SortOrder
case object Descending extends SortOrder
case object NoSort extends SortOrder

class SortInfo(sorter:TableRowSorter[_], val column:Int ) {

  def getSortOrder():SortOrder = {
    val i = sorter.getSortKeys().iterator
    var ret:SortOrder = NoSort

    while(i.hasNext) {
      val key = i.next
      if(key.getColumn == column) {
        val ord = key.getSortOrder
        if(ord == javax.swing.SortOrder.ASCENDING) ret = Ascending
        else if(ord == javax.swing.SortOrder.DESCENDING) ret = Descending
      }
    }

    ret
  }
}

class ConvenientPimpedTable[A, B <: ColumnValue[A]](dat:List[A], columns:List[ColumnDescription[A,B]]) extends PimpedTable[A, B](dat.map(x => new Row[A] {val data = x}),columns) 

case class PimpedTableSelectionEvent(s:Table) extends TableEvent(s)

class PimpedTable[A, B <: ColumnValue[A]](initData:List[Row[A]], columns:List[ColumnDescription[A,B]]) extends Table with TableBehaviourClient {

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
  sorter.setSortsOnUpdates(true)
  this.peer.setRowSorter(sorter)

  private def fillSorter = {
    val offset = if(paintExpandColumn) {
                    sorter.setSortable(0, false)
                    1
                 } else 0

    columns.zipWithIndex filter {x => x match {
      case (colDes,_) => colDes.isSortable
    }} foreach {x => x match {
      case (colDes,i) => { colDes.comparator match {
        case None    => sorter.setSortable(i, true)
        case Some(c) => {
          sorter.setComparator(i+offset, new PimpedColumnComparator[A,B](c, colDes, i+offset, new SortInfo(sorter, i+offset)))
          sorter.setSortable(i,true)
        }
            }
          }
        }
    }

    columns.zipWithIndex foreach {
      x => x match {
        case (colDes,i) => if (!colDes.isSortable) sorter.setSortable(i, false)
      }
    }
  }
  
  def refresh() = {
    data = data
  }

  private def fallbackData = fallbackDat
  private def fallbackData_=(d:List[Row[A]]) = {
    fallbackDat = d
    savedFilter match {
      case Some(f) => filteredData = fallbackData.filter(x => f(x.data))
      case None    => filteredData = fallbackData
    }

  }

  private def expandData(l:List[Row[A]]):List[Row[A]] = l match {
    case List() => List.empty[Row[A]]
    case _      => l.head match {
      case a@LivingTree(_,_,_,true) => (l.head +: a.leaves) ++ expandData(l.tail)
      case _                        => l.head +: expandData(l.tail)
    }
  }
   
  def filteredData = filteredDat
  private def filteredData_= (d:List[Row[A]]) = {
    var sel = selectedData()
    var sortkeys = sorter.getSortKeys
    blockSelectionEvents = true
    filteredDat = expandData(d)
    tableModel =  new PimpedTableModel(filteredData, columns, paintExpandColumn)
    this.model = tableModel
    sorter setModel tableModel
    fillSorter
    //sorter.sort()
    if(sel.size!=0 && !sel.map(select(_)).forall(x => x)) {
      blockSelectionEvents = false
      publish(PimpedTableSelectionEvent(this))
    }

    sorter setSortKeys sortkeys
    
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
          filteredData(convertedRow(row)) match {
            case DeadTree(_)         => background = Color.BLACK//RED
            case LivingTree(_,_,_,_) => background = Color.GREEN
            case Leaf(_,_)           => background = Color.WHITE
          }
          if(isSelected) {
            background = Color.BLUE
          }
        }
      }
      else {
        val c = convertedColumn(column)-1
        if(c >= 0) columns(c).renderComponent(filteredData(convertedRow(row)), isSelected, focused, /*TODO*/false)
        else {/*println("Föhlah: " + c); */new Label("")}
      }
    } else {
      columns(convertedColumn(column)).renderComponent(filteredData(convertedRow(row)), isSelected, focused, /*TODO*/false)
    }
  }
  
  def selectedData():List[A] = this.selection.rows.toList.map(i => filteredData(convertedRow(i)).data)

  

  fallbackData = initData

}

