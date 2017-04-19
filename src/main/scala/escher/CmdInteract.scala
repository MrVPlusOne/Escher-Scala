package escher

/**
  * Created by weijiayi on 19/04/2017.
  */
object CmdInteract {

  def printTable(elements: IS[IS[String]], spacing: Int): Unit = {
    val rows = elements.length
    val cols = elements.head.length

    val colWidths = (0 until cols).map(c => (0 until rows).map(r => elements(r)(c).length).max)
    elements.foreach(row => {
      for(c <- row.indices){
        val str = row(c)
        print(str)
        print(" " * (colWidths(c) + spacing - str.length))
      }
      println()
    })
  }
}
