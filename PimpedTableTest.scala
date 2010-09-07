/* PimpedScalaTable is a wrapper arround scalas swing.table or Javas JTable (haven't decided jet ;) ) that is meant to evercome all shortcomings I find lacking in scalas swing.table 


Copyright (C) 2010 Axel Gschaider

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, see <http://www.gnu.org/licenses/>. 


If you would like to obtain this programm under another license, feel free to contact me: 
axel[dot]gschaider[at]gmx[dot]at
or http://github.com/axelGschaider
*/

import at.axelGschaider.pimpedTable._

case class Container(n:Int, s:String)

sealed abstract class Value

case class IntValue(i:Int) extends Value
case class StringValue(s:String) extends Value

case class Data(i:Int, s:String) extends ColumnData[Data] {
   override val isExpandAble = true;
   override def expand() = (1 to 5).toList.map(x => Data(i, s+x.toString))
}




sealed abstract class MyColumns[Data, Value] extends ColumnDescription[Data, Value] {
  def firstIsBigger(x:Value, y:Value) = (x, y) match {
    case (IntValue(i1), IntValue(i2))       => i1 > i2
    case (StringValue(s1), StringValue(s2)) => true
    case _                                  => false
  }
}


case class StringColumn(val name:String) extends MyColumns[Data, Value]  {
  def extractValue(x:Data) = StringValue(x.s)
}

case class IntColumn(val name:String) extends MyColumns[Data, Value] {
  def extractValue(x:Data) = IntValue(x.i)
  override val ignoreWhileExpanding = true
}


object Test {
  val data = (0 to 10).toList.map(x => Data(x, x.toString + "xxx")) 
  val columns = IntColumn("some int") :: StringColumn("some string") :: List.empty[MyColumns[Data,Value]]
  //val table = new PimpedTable(data, TheStringColumn :: TheIntColumn :: List[ColumnDescription[Data, Value]].empty)
  //val t = new PimpedTable(data,List.empty[ColumnDescription[Data,Value]] ++ TheIntColumn)
  val table = new PimpedTable(data, columns) 
}


