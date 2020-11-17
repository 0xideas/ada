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

    def plotLine(data: List[Double], label:Option[String], symbol:String = "-"): Chart = {
        val printLabel = label == None
        val incr = math.max(1, math.ceil(data.length.toDouble/(width.toDouble)).toInt)
        data.zipWithIndex
            .filter{case(x, i) => i%incr == 0}
            .map(_._1)
            .zipWithIndex
            .tail
            .filter{case(x, i) => bucket(x) > 0 && bucket(x) < height}
            .map{ case(x, i) => {
                if(chars(bucket(x))(i) == " ") chars(bucket(x))(i) = symbol
                if(i == 10 && printLabel) chars(((bucket(x) + 1) % height))(i) = label.get
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