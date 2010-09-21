/* PimpedScalaTable is a wrapper arround scalas swing.table or Javas JTable (haven't decided jet ;) ) that is meant to evercome all shortcomings I find lacking in scalas swing.table 


Copyright (C) 2010 Axel Gschaider

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, see <http://www.gnu.org/licenses/>. 


If you would like to obtain this programm under another license, feel free to contact me: 
axel[dot]gschaider[at]gmx[dot]at
or http://github.com/axelGschaider
*/

package at.axelGschaider.pimpedTable

import scala.swing._
import javax.swing.JTable
import javax.swing.table.AbstractTableModel
import javax.swing.table.TableRowSorter
import java.util.Comparator

trait Row[A] {
  val data:A
  val isExpandAble:Boolean = false
  def expand():List[A] = List.empty[A]
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

class PimpedTable[A, B](dat:List[Row[A]], columns:List[ColumnDescription[A,B]]) extends Table {

  private var lokalData = dat
  private var tableModel:PimpedTableModel[A,B] = new PimpedTableModel(data, columns)

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

  def data:List[Row[A]] = lokalData

  def data_=(d:List[Row[A]])= {
    lokalData = d
    triggerDataChange()
  }

  private def triggerDataChange() = {
    tableModel =  new PimpedTableModel(data, columns)
    this.model = tableModel
    sorter setModel tableModel
    fillSorter
  }

  /*def filter(p: (RowData[A]) => Boolean):Unit = {
    
  }*/

  /*def unfilter():Unit = {
    
  }*/
  override  def   rendererComponent(isSelected: Boolean, focused: Boolean, row: Int, column: Int): Component = {
    rendererComponentForPeerTable(isSelected, focused, row, column)
  }
  
  def rendererComponentForPeerTable(isSelected: Boolean, focused: Boolean, row: Int, column: Int): Component = {
    columns(column).renderComponent(data(this.peer.convertRowIndexToModel(row)).data, isSelected, focused)
  }
  
  def selectedData():List[A] = this.selection.rows.toList.map(i => data(this.peer.convertRowIndexToModel(i)).data)

  data = dat

}

