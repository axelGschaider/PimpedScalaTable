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
import java.util.Comparator

trait Row[A] {
  val data:A
  val isExpandAble:Boolean = false
  def expand():List[ExpandedData[A]] = List.empty[ExpandedData[A]]
}

trait ExpandedData[A] extends Row[A]{
  val father:A
}


trait ColumnDescription[-A,+B] {
  val name:String;

  def extractValue(x:A):B;

  val isSortable:Boolean = false;

  val ignoreWhileExpanding:Boolean = false;

  def renderComponent(data:A, isSelected: Boolean, focused: Boolean):Component

  def comparator: Option[Comparator[_ <: B]]
}

class PimpedTableModel[A,B](dat:List[Row[A]], columns:List[ColumnDescription[A,B]]) extends AbstractTableModel {
  private var lokalData = dat

  def data = lokalData

  def data_=(d: List[Row[A]]) = {
    lokalData = d
  }

  def getRowCount():Int = data.length

  def getColumnCount():Int = columns.length


  override def getValueAt(row:Int, column:Int): java.lang.Object = {
    if(row < 0 || column < 0 || column >= columns.length || row >= data.length) {
      throw new Error("Bad Table Index: row " + row + " column " + column)
    }
    //(columns(column) extractValue data(row)).toString
    //null
    (columns(column) extractValue data(row).data).asInstanceOf[java.lang.Object]
  }

  def getNiceValue(row:Int, column:Int): B = {
    if(row < 0 || column < 0 || column >= columns.length || row >= data.length) {
      throw new Error("Bad Table Index: row " + row + " column " + column)
    }
    columns(column) extractValue data(row).data
  }

  override def getColumnName(column: Int): String = columns(column).name
}

class ConvenientPimpedTable[A, B](dat:List[A], columns:List[ColumnDescription[A,B]]) extends PimpedTable[A, B](dat.map(x => new Row[A] {val data = x}),columns)

case class PimpedTableSelectionEvent(s:Table) extends TableEvent(s)

class PimpedTable[A, B](initData:List[Row[A]], columns:List[ColumnDescription[A,B]]) extends Table {

  private var fallbackDat = initData
  private var filteredDat = fallbackDat

  private var tableModel:PimpedTableModel[A,B] = new PimpedTableModel(filteredData, columns)
  private var savedFilter:Option[(A => Boolean)] = None

  //private var lokalFiltered:Boolean = false
  private var blockSelectionEvents:Boolean = false

  val sorter = new TableRowSorter[PimpedTableModel[A,B]]() 
  this.peer.setRowSorter(sorter)

  private def fillSorter = {
    columns.zipWithIndex filter {x => x match {
      case (colDes,_) => colDes.isSortable
    }} foreach {x => x match {
          case (colDes,i) => { colDes.comparator match {
            case None    => {}
            case Some(c) => sorter.setComparator(i, c)
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
    tableModel =  new PimpedTableModel(filteredData, columns)
    this.model = tableModel
    sorter setModel tableModel
    fillSorter
    if(sel.size!=0 && !sel.map(select(_)).forall(x => x)) {
      //System.out.println("something changed")
      blockSelectionEvents = false
      publish(PimpedTableSelectionEvent(this))
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

  /*private def triggerDataChange() = {
    
  }*/

  def isFiltered() = savedFilter match {
    case None    => false
    case Some(_) => true
  }

  def filter(p: (A) => Boolean):Unit = {
    //data = dat.filter(x => p(x.data))
    savedFilter = Some(p)
    filteredData = fallbackData.filter(x => p(x.data))
  }

  def unfilter():Unit = {
    /*data = dat  
    lokalFiltered = false*/
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

  /*override def publish(e: Event):Unit = {
    //println("there something going on")
    //blockSelectionEvents = true
    super.publish(e)
    //blockSelectionEvents = false
  }*/

  def unselectAll():Unit = this.selection.rows.clear

  override  def   rendererComponent(isSelected: Boolean, focused: Boolean, row: Int, column: Int): Component = {
    rendererComponentForPeerTable(isSelected, focused, row, column)
  }
  
  def rendererComponentForPeerTable(isSelected: Boolean, focused: Boolean, row: Int, column: Int): Component = {
    columns(column).renderComponent(filteredData(this.peer.convertRowIndexToModel(row)).data, isSelected, focused)
  }
  
  def selectedData():List[A] = this.selection.rows.toList.map(i => filteredData(this.peer.convertRowIndexToModel(i)).data)

  fallbackData = initData

}

