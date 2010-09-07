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

trait ColumnData[A] {
  val isExpandAble:Boolean = false
  def expand():List[A] = List.empty[A]
}

trait ColumnDescription[A,B] {
  val name:String;

  def extractValue(x:A):B;

  val isSortable:Boolean = false;

  def firstIsBigger(x:B, y:B):Boolean;

  def firstValueIsBigger(x:A, y:A):Boolean = firstIsBigger(extractValue(x), extractValue(y))  

  val ignoreWhileExpanding:Boolean = false;

}

class PimpedTable[A, B](data:List[ColumnData[A]], columns:List[ColumnDescription[A,B]]) {
  
  

}

