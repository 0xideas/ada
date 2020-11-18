package plotting
import scala.collection.mutable.ListBuffer

class Chart(chars : ListBuffer[ListBuffer[String]],
            top: Double, bottom: Double, left: Int, right: Int, width: Int = 150, height: Int = 40) {

    def render(): String = {
        val plotMax = chars.reverse.tail.map(row => {row.length - row.reverse.map(e => if(e != " ") 1 else 0).indexOf(1)}).max
        //println("\n" + plotMax.toString())
        right.toString.toList.zipWithIndex.map{ case(c, i) => chars(height-1)(plotMax- right.toString.length + i) = c.toString}
        (plotMax until width).map(i => chars(height-1)(i) = " ")
        chars.map(row => row.mkString("")).mkString("\n")
    } 
    def bucket(x: Double): Int = (((top - x)/(top -bottom))*(height-0.001)).toInt

    def placeValue(value: Double, xLoc: Int, symbol: String): Unit = {
        val yLoc = bucket(value)
        if(chars(yLoc)(xLoc) == " ") chars(yLoc)(xLoc) = symbol
        else if(yLoc+1 < height && chars(yLoc + 1)(xLoc) == " ") chars(yLoc+1)(xLoc) = symbol
        else if(chars(yLoc - 1)(xLoc) == " ") chars(yLoc-1)(xLoc) = symbol
    }

    def plotLine(data: List[Double], label:Option[String], symbol:String = "-"): Chart = {
        val printLabel = label != None
        val incr = math.max(1, math.ceil(data.length.toDouble/(width.toDouble)).toInt)
        data.zipWithIndex
            .filter{case(x, i) => i%incr == 0}
            .map(_._1)
            .zipWithIndex
            .tail
            .filter{case(x, i) => bucket(x) > 0 && bucket(x) < height}
            .map{ case(y, xLoc) => {
                placeValue(y, xLoc, symbol)
                if(xLoc == 10 && printLabel) chars(((bucket(y) + 1) % height))(xLoc) = label.get
            } } 
        this
    } 
}
object Chart{
    def apply(top: Double, bottom: Double, left: Int, right: Int, width: Int = 150, height: Int = 40): Chart = {
        val chars= ListBuffer.fill(height-1)(ListBuffer("|") ++: ListBuffer.fill(width-1)(" ")) :+ (ListBuffer("|") ++: ListBuffer.fill(width-1)("_"))
        top.toString.toList.zipWithIndex.map{ case(c, i) => chars(0)(i) = c.toString}
        (bottom.toString + "/" + left.toString).toList.zipWithIndex.map{ case(c, i) => chars(height-1)(i) = c.toString}
        new Chart(chars, top, bottom, left, right, width, height)
    
    }
}