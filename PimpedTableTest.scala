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
import scala.swing._
import scala.swing.GridBagPanel.Fill
import event._
import java.awt.Color
import java.util.Comparator



case class Data(i:Int, s:String)

sealed trait Value 

case class IntValue(i:Int) extends Value 
case class StringValue(s:String) extends Value 

case class RowData(data:Data) extends Row[Data]

sealed trait MyColumns[+Value] extends ColumnDescription[Data, Value] {
  /*def firstIsBigger(x:Data, y:Data) = (this extractValue x,this extractValue y) match {
    case (IntValue(i1),    IntValue(i2))    => i1 > i2
    case (StringValue(s1), StringValue(s2)) => s1 > s2
  }*/
  
  override val isSortable = true

  def renderComponent(data:Data, isSelected: Boolean, focused: Boolean):Component = {
    
    val l = new Label()

    extractValue(data) match {
      case StringValue(s) => l.text = s
      case IntValue(i)    => l.text = i.toString
    }

    if(isSelected) {
      l.background = Color.BLUE
      l.foreground = Color.GREEN
      //println(l.text + ": I am elected!!!!!")
    }

    l

  }

}

case class StringColumn(name:String) extends MyColumns[StringValue] {
  def extractValue(x:Data) = StringValue(x.s)

  def comparator = Some(new Comparator[StringValue] {
    def compare(o1:StringValue, o2:StringValue):Int = (o1,o2) match {
      case (StringValue(s1), StringValue(s2)) => if(s1 < s2) -1
                                                 else if (s1 > s2) 1
                                                 else 0
    }
  })

}

case class IntColumn(name:String) extends MyColumns[IntValue] {
  def extractValue(x:Data) = IntValue(x.i)

  def comparator = Some(new Comparator[IntValue] {
    
    def compare(o1:IntValue, o2:IntValue):Int = (o1,o2) match {
      case (IntValue(i1), IntValue(i2)) => if(i1 < i2) -1
                                           else if (i1 > i2) 1
                                           else 0
    }
  })

}


object Test extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "Table Test"
    
    //DO SOME INIT
    //MainFleetSummaryDistributer registerClient this

    //SET SIZE AND LOCATION
    val framewidth = 640
    val frameheight = 480

    val data:List[RowData] = (0 to 50).toList.map(x => RowData(Data(x,/* (100-x).toString + */"xxx"))) 
    val columns:List[MyColumns[Value]] = List(new IntColumn("some int"), new StringColumn("some string") )
    val table = new PimpedTable(data, columns) {
      showGrid = true
      gridColor = Color.BLACK
      selectionBackground = Color.RED
      selectionForeground = Color.GREEN
    }

    
    val screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
    location = new java.awt.Point((screenSize.width - framewidth)/2, (screenSize.height - frameheight)/2)
    minimumSize = new java.awt.Dimension(framewidth, frameheight)
    
    val buttonPannel = new GridBagPanel() {
      add(new Button("Test"),
          new Constraints() {
            grid = (0,0)
            gridheight = 1
            gridwidth = 1
            weightx = 1
            weighty = 1
            fill = Fill.Both
          })
    }
    
    val tablePane = new ScrollPane(table) {
      horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.AsNeeded
      verticalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.AsNeeded
      viewportView = table
    }
    contents = new SplitPane(Orientation.Vertical, buttonPannel, tablePane)

    listenTo(table.selection)
    reactions += {
      case TableRowsSelected(`table`, range, adjusting) => {
        
      }
    }
    
  }

  
}


